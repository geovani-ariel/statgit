#' Utilidades internas do pacote git4stats
#'
#' Funcoes auxiliares para diagnostico, mensagens e interacao segura com Git.
#'
#' @keywords internal
"_PACKAGE"

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0 || (is.character(x) && !nzchar(x[1]))) {
    return(y)
  }

  x
}

time_ago <- function(time) {
  if (length(time) == 0 || is.na(time)) return("")
  diff <- as.numeric(difftime(Sys.time(), time, units = "mins"))
  if (diff < 2) return("agora mesmo")
  if (diff < 60) return(sprintf("há %d min", as.integer(diff)))
  diff_hours <- diff / 60
  if (diff_hours < 24) return(sprintf("há %d h", as.integer(diff_hours)))
  diff_days <- diff_hours / 24
  if (diff_days < 30) return(sprintf("há %d dias", as.integer(diff_days)))
  format(time, "%d/%m/%Y")
}

normalize_project_path <- function(path = ".") {
  as.character(fs::path_abs(path))
}

active_project_path <- function(default = ".") {
  if (rstudioapi::isAvailable() && nzchar(rstudioapi::getActiveProject() %||% "")) {
    return(rstudioapi::getActiveProject())
  }

  normalize_project_path(default)
}

project_context <- function(path = ".") {
  current_path <- normalize_project_path(path)
  repo_path <- git_repo_root(current_path)
  repo_ref <- repo_path %||% current_path
  rproj_files <- find_rproj_files(repo_ref)

  list(
    current_path = current_path,
    repo_path = repo_path,
    repo_ref = repo_ref,
    is_repo_root = !is.null(repo_path) && identical(repo_path, current_path),
    rproj_files = rproj_files,
    rproj_path = if (length(rproj_files) > 0) rproj_files[[1]] else NULL
  )
}

find_rproj_files <- function(path = ".") {
  files <- list.files(
    path = normalize_project_path(path),
    pattern = "\\.[Rr]proj$",
    full.names = TRUE
  )

  sort(files)
}

is_rstudio_project <- function(path = ".") {
  length(find_rproj_files(path)) > 0
}


build_git_diagnosis <- function(path = ".") {
  context <- project_context(path)
  git_ok <- git_installed()
  repo_remotes <- repo_remote_info(context$repo_ref)
  identity <- git_identity(context$repo_ref)
  repo_name <- if (nrow(repo_remotes) > 0) repo_remotes$name[[1]] else NULL
  repo_url <- if (nrow(repo_remotes) > 0) repo_remotes$url[[1]] else NULL

  diagnosis <- structure(
    list(
      path = context$current_path,
      repo_path = context$repo_path,
      is_repo_root = context$is_repo_root,
      rproj_path = context$rproj_path,
      git_installed = git_ok,
      user_name = if (git_ok) git_global_config("user.name") else NULL,
      user_email = if (git_ok) git_global_config("user.email") else NULL,
      identity = identity,
      is_rstudio_project = length(context$rproj_files) > 0,
      has_repo = FALSE,
      has_commits = FALSE,
      branch = NULL,
      has_remote = FALSE,
      remote_name = repo_name,
      remote_url = repo_url,
      remote_is_github = looks_like_github_url(repo_url),
      status = empty_status_table(),
      status_counts = list(
        staged = 0L,
        new = 0L,
        modified = 0L,
        deleted = 0L,
        conflicted = 0L,
        total = 0L
      )
    ),
    class = "git4stats_diagnosis"
  )

  if (git_ok) {
    diagnosis$has_repo <- !is.null(context$repo_path)

    if (diagnosis$has_repo) {
      diagnosis$has_commits <- repo_has_commits(context$repo_path)
      diagnosis$branch <- repo_current_branch(context$repo_path)
      diagnosis$has_remote <- nrow(repo_remotes) > 0
      diagnosis$status <- repo_status_table(context$repo_path)
      diagnosis$status_counts <- status_counts(diagnosis$status)
    }
  }

  diagnosis
}

format_item <- function(ok, text, value = NULL) {
  prefix <- if (isTRUE(ok)) "[OK]" else "[!]"
  details <- if (!is.null(value) && nzchar(value)) paste0(": ", value) else ""
  paste0(prefix, " ", text, details)
}

next_step_message <- function(diagnosis) {
  if (!isTRUE(diagnosis$git_installed)) {
    return("Instale o Git e rode check_git_setup() novamente.")
  }

  if (!isTRUE(diagnosis$has_repo)) {
    return("Clique em 'Inicializar Git' na aba 'Git e GitHub'.")
  }

  if (!isTRUE(diagnosis$identity$complete)) {
    return("Configure nome e email no Git antes de seguir para o primeiro commit.")
  }

  if (!isTRUE(diagnosis$has_commits)) {
    return("Use a aba 'Controle Fino (Diffs)' para salvar a primeira versão do projeto.")
  }

  if (!isTRUE(diagnosis$has_remote)) {
    return("Você já pode preencher a URL do GitHub na aba 'Git e GitHub'.")
  }

  if (diagnosis$status_counts$total > 0) {
    return("Se as mudan\u00e7as estiverem corretas, fa\u00e7a um novo commit ao terminar esta etapa.")
  }

  if (isTRUE(diagnosis$remote_is_github)) {
    return("Se quiser enviar seu histórico ao GitHub, clique em 'Enviar Histórico (Push)'.")
  }

  "Seu projeto parece em ordem. Continue analisando e fa\u00e7a commits por etapas l\u00f3gicas."
}

print_named_list <- function(items) {
  for (line in items) {
    cat(line, "\n", sep = "")
  }
}

diagnosis_lines <- function(diagnosis) {
  lines <- c(
    "Diagn\u00f3stico Git do projeto",
    "",
    if (diagnosis$git_installed) {
      format_item(TRUE, "Git instalado")
    } else {
      "[!] Git n\u00e3o foi encontrado neste computador."
    },
    if (!is.null(diagnosis$user_name)) {
      format_item(TRUE, "Nome configurado", diagnosis$user_name)
    } else {
      "[!] Nome global do Git ainda n\u00e3o foi configurado."
    },
    if (!is.null(diagnosis$user_email)) {
      format_item(TRUE, "Email configurado", diagnosis$user_email)
    } else {
      "[!] Email global do Git ainda n\u00e3o foi configurado."
    },
    if (isTRUE(diagnosis$identity$complete)) {
      format_item(TRUE, "Identidade ativa para commits", paste(diagnosis$identity$name, "<", diagnosis$identity$email, ">", sep = " "))
    } else {
      "[!] Nome e email ainda n\u00e3o est\u00e3o completos para criar commits."
    },
    if (diagnosis$is_rstudio_project) {
      format_item(TRUE, "Projeto RStudio detectado", basename(diagnosis$rproj_path))
    } else {
      "[!] Nenhum arquivo .Rproj foi encontrado nesta pasta."
    },
    if (diagnosis$has_repo) {
      format_item(TRUE, "Reposit\u00f3rio Git inicializado")
    } else {
      "[!] Esta pasta ainda n\u00e3o usa Git."
    }
  )

  if (diagnosis$has_repo) {
    lines <- c(
      lines,
      if (!is.null(diagnosis$branch)) {
        format_item(TRUE, "Branch atual", diagnosis$branch)
      } else {
        "[!] Ainda n\u00e3o h\u00e1 branch ativa porque falta o primeiro commit."
      },
      if (diagnosis$has_commits) {
        format_item(TRUE, "J\u00e1 existe pelo menos um commit")
      } else {
        "[!] Ainda n\u00e3o existe nenhum commit neste projeto."
      },
      if (diagnosis$has_remote) {
        format_item(TRUE, "Remote configurado", paste0(diagnosis$remote_name, " -> ", diagnosis$remote_url))
      } else {
        "[!] Ainda n\u00e3o h\u00e1 remote configurado."
      }
    )

    counts <- diagnosis$status_counts
    if (counts$total == 0) {
      lines <- c(lines, format_item(TRUE, "Sem arquivos pendentes no momento"))
    } else {
      lines <- c(
        lines,
        if (counts$new > 0) {
          paste0("[!] Existem ", counts$new, " arquivo(s) novo(s) fora do hist\u00f3rico.")
        },
        if (counts$modified > 0) {
          paste0("[!] Existem ", counts$modified, " arquivo(s) modificados.")
        },
        if (counts$staged > 0) {
          paste0("[!] Existem ", counts$staged, " arquivo(s) j\u00e1 preparados para commit.")
        },
        if (counts$deleted > 0) {
          paste0("[!] Existem ", counts$deleted, " arquivo(s) marcados como removidos.")
        },
        if (counts$conflicted > 0) {
          paste0("[!] Existem ", counts$conflicted, " conflito(s) que precisam de aten\u00e7\u00e3o.")
        }
      )
    }
  }

  c(lines, "", paste("Pr\u00f3ximo passo recomendado:", next_step_message(diagnosis)))
}

print_git_setup <- function(diagnosis) {
  print_named_list(diagnosis_lines(diagnosis))
}

ensure_suggested_package <- function(pkg, feature) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop(
      glue::glue(
        "Para usar {feature}, instale o pacote sugerido '{pkg}' com install.packages(\"{pkg}\")."
      ),
      call. = FALSE
    )
  }
}

merge_gitignore_lines <- function(existing, additions) {
  merged <- existing

  for (line in additions) {
    if (!(line %in% merged)) {
      merged <- c(merged, line)
    }
  }

  merged
}

default_gitignore_lines <- function() {
  c(
    ".Rhistory",
    ".RData",
    ".Ruserdata",
    ".Rproj.user/",
    ".httr-oauth",
    ".DS_Store",
    "Thumbs.db"
  )
}

generated_output_lines <- function() {
  c(
    "*.html",
    "*.pdf",
    "*.docx"
  )
}

data_gitignore_lines <- function() {
  c(
    "data/raw/*",
    "!data/raw/README.md",
    "data/processed/*",
    "!data/processed/README.md"
  )
}

project_readme_template <- function(project_name = "Nome do projeto", template = "analise_exploratoria") {
  template_title <- project_template_label(template)
  template_goal <- switch(
    template,
    trabalho_disciplina = "Registrar a pergunta da disciplina, os dados usados e os resultados que precisam entrar na entrega.",
    iniciacao_cientifica = "Documentar a pergunta de pesquisa, o plano de analise e os entregaveis da investigacao.",
    tcc = "Explicar o problema central do TCC, a estrategia metodologica e o cronograma de escrita.",
    artigo_quarto = "Descrever a pergunta do artigo, os dados e a estrategia para reproducao do manuscrito.",
    analise_exploratoria = "Descrever aqui o objetivo da exploracao e quais decisoes dependem desta analise.",
    projeto_grupo = "Explicar o objetivo do grupo, a divisao de tarefas e como combinar resultados."
  )

  c(
    paste("#", project_name),
    "",
    paste("Template inicial:", template_title),
    "",
    "## Objetivo",
    "",
    template_goal,
    "",
    "## Estrutura",
    "",
    "- `R/`: fun\u00e7\u00f5es auxiliares",
    "- `scripts/`: scripts de an\u00e1lise",
    "- `data/raw/`: dados originais",
    "- `data/processed/`: dados tratados",
    "- `reports/`: relat\u00f3rios",
    "- `figs/`: figuras geradas",
    "",
    "## Como reproduzir",
    "",
    "1. Abra o arquivo `.Rproj`",
    "2. Execute os scripts na ordem indicada",
    "3. Gere o relat\u00f3rio final"
  )
}

data_readme_template <- function(kind = c("raw", "processed")) {
  kind <- match.arg(kind)
  label <- if (kind == "raw") "originais" else "tratados"

  c(
    paste("# Dados", label),
    "",
    "Coloque aqui apenas um resumo ou instru\u00e7\u00f5es de obten\u00e7\u00e3o dos dados.",
    "Os arquivos reais ficam fora do versionamento para evitar enviar dados sens\u00edveis ou pesados."
  )
}

rstudio_project_file_template <- function(project_name = "meu-projeto") {
  c(
    "Version: 1.0",
    "",
    "RestoreWorkspace: No",
    "SaveWorkspace: No",
    "AlwaysSaveHistory: Default",
    "",
    "EnableCodeIndexing: Yes",
    "UseSpacesForTab: Yes",
    "NumSpacesForTab: 2",
    "Encoding: UTF-8",
    "",
    "RnwWeave: Sweave",
    "LaTeX: pdfLaTeX",
    "",
    paste("# Projeto:", project_name)
  )
}

quarto_report_template <- function(project_name, title) {
  c(
    "---",
    paste0("title: \"", title, "\""),
    "format: html",
    "---",
    "",
    paste("Projeto:", project_name),
    "",
    "## Objetivo",
    "",
    "Descreva aqui o objetivo deste relatorio.",
    "",
    "## Proximos passos",
    "",
    "- Atualizar os dados",
    "- Rodar a analise",
    "- Revisar as figuras"
  )
}

rmarkdown_report_template <- function(project_name, title) {
  c(
    "---",
    paste0("title: \"", title, "\""),
    "output: html_document",
    "---",
    "",
    paste("Projeto:", project_name),
    "",
    "## Objetivo",
    "",
    "Escreva aqui a pergunta principal deste relatorio."
  )
}

r_script_template <- function(description) {
  c(
    paste("#", description),
    "",
    "# Carregue pacotes e dados aqui.",
    ""
  )
}

quarto_config_template <- function(project_name) {
  c(
    "project:",
    paste0("  title: \"", project_name, "\""),
    "",
    "format:",
    "  html:",
    "    toc: true",
    ""
  )
}

bib_template <- function() {
  c(
    "@article{exemplo2026,",
    "  title = {Titulo do artigo de referencia},",
    "  author = {Sobrenome, Nome},",
    "  journal = {Revista Exemplo},",
    "  year = {2026}",
    "}"
  )
}

contributing_template <- function() {
  c(
    "# Como colaborar",
    "",
    "1. Abra sempre o arquivo `.Rproj`.",
    "2. Combine nomes curtos e consistentes para scripts e relatarios.",
    "3. Faça commits pequenos e com mensagens objetivas.",
    "4. Antes de mexer em `data/`, alinhe com o restante do grupo."
  )
}

markdown_note_template <- function(title, template = "analise_exploratoria") {
  c(
    paste("#", title),
    "",
    paste("Projeto baseado no template:", project_template_label(template)),
    "",
    "Escreva aqui o contexto deste arquivo."
  )
}

path_title <- function(path) {
  file_name <- fs::path_ext_remove(fs::path_file(path))
  words <- gsub("[-_]+", " ", file_name)
  words <- trimws(words)

  if (!nzchar(words)) {
    return("Novo arquivo")
  }

  paste(toupper(substring(words, 1, 1)), substring(words, 2), sep = "")
}

capture_action_output <- function(expr) {
  utils::capture.output(result <- force(expr))
}

wizard_action_button <- function(id, label, enabled = TRUE) {
  if (isTRUE(enabled)) {
    return(shiny::actionButton(id, label))
  }

  shiny::tags$button(
    id = id,
    type = "button",
    class = "btn btn-default action-button",
    disabled = "disabled",
    label
  )
}

wizard_step_note <- function(title, text, ok = TRUE) {
  cls <- if (isTRUE(ok)) "wizard-step-ok" else "wizard-step-warn"
  shiny::div(
    class = cls,
    shiny::strong(title),
    shiny::tags$br(),
    text
  )
}
