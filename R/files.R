#' Copia ou move um arquivo para dentro do projeto
#'
#' @param source Caminho do arquivo original.
#' @param destination Pasta de destino dentro do projeto.
#' @param path Caminho do projeto.
#' @param move Se `TRUE`, move o arquivo em vez de copiar.
#' @param add_to_git Se `TRUE`, prepara o arquivo no Git apos importar.
#' @param overwrite Se `TRUE`, permite substituir arquivo existente no destino.
#'
#' @return Uma lista com o resultado da importacao.
#' @export
file_import <- function(
  source,
  destination = "data/raw",
  path = ".",
  move = FALSE,
  add_to_git = FALSE,
  overwrite = FALSE
) {
  project_path <- normalize_project_path(path)
  source_path <- normalize_project_path(source)
  destination <- normalize_project_destination(destination)
  destination_dir <- fs::path(project_path, destination)
  target_path <- fs::path(destination_dir, fs::path_file(source_path))

  if (!fs::file_exists(source_path)) {
    stop("O arquivo de origem nao existe.", call. = FALSE)
  }

  if (!is_within_project(target_path, project_path)) {
    stop("O destino precisa ficar dentro do projeto.", call. = FALSE)
  }

  if (fs::file_exists(target_path) && !isTRUE(overwrite)) {
    stop("Ja existe um arquivo com esse nome no destino.", call. = FALSE)
  }

  fs::dir_create(destination_dir, recurse = TRUE)

  if (isTRUE(move)) {
    fs::file_move(source_path, target_path)
    action <- "movido"
  } else {
    fs::file_copy(source_path, target_path, overwrite = overwrite)
    action <- "copiado"
  }

  git_added <- FALSE
  if (isTRUE(add_to_git) && is_git_repo(project_path)) {
    repo <- git_repo_root(project_path)
    relative_target <- relative_project_path(target_path, repo)
    gert::git_add(files = relative_target, repo = repo)
    git_added <- TRUE
  }

  relative_path <- relative_project_path(target_path, project_path)

  cli::cli_alert_success(glue::glue("Arquivo {action} para {relative_path}."))
  if (isTRUE(git_added)) {
    cli::cli_inform("O arquivo tambem foi preparado para o proximo commit.")
  }

  invisible(list(
    ok = TRUE,
    source = source_path,
    path = normalize_project_path(target_path),
    relative_path = relative_path,
    moved = isTRUE(move),
    git_added = git_added
  ))
}

choose_project_file <- function() {
  if (!rstudio_available()) {
    return(NULL)
  }

  tryCatch(
    rstudioapi::selectFile(caption = "Escolha um arquivo para importar"),
    error = function(e) NULL
  )
}

choose_directory <- function() {
  if (!rstudio_available()) {
    return(NULL)
  }

  tryCatch(
    rstudioapi::selectDirectory(caption = "Escolha uma pasta"),
    error = function(e) NULL
  )
}

choose_rproj_file <- function() {
  if (!rstudio_available()) {
    return(NULL)
  }

  tryCatch(
    rstudioapi::selectFile(
      caption = "Escolha um projeto (.Rproj)",
      filter = "R Project files (*.Rproj)",
      existing = TRUE
    ),
    error = function(e) NULL
  )
}

normalize_project_destination <- function(destination) {
  destination <- trimws(destination %||% "")
  if (!nzchar(destination)) {
    destination <- "."
  }

  destination <- gsub("\\\\", "/", destination)
  invalid <- grepl("^(/|~|[A-Za-z]:)", destination) ||
    grepl("(^|/)\\.\\.($|/)", destination)

  if (isTRUE(invalid)) {
    stop("Use uma pasta de destino relativa dentro do projeto.", call. = FALSE)
  }

  destination
}

is_within_project <- function(path, project_path) {
  path <- normalize_project_path(path)
  project_path <- normalize_project_path(project_path)
  identical(path, project_path) || startsWith(path, paste0(project_path, .Platform$file.sep))
}

relative_project_path <- function(path, project_path) {
  path <- normalize_existing_path(path)
  project_path <- normalize_existing_path(project_path)

  as.character(fs::path_rel(path, start = project_path))
}

normalize_existing_path <- function(path) {
  path <- normalize_project_path(path)

  if (file.exists(path)) {
    return(normalizePath(path, winslash = "/", mustWork = TRUE))
  }

  path
}

#' Encontra os arquivos de dados e scripts mais recentes na pasta Downloads
#'
#' @return Um vetor de caminhos de arquivos.
#' @export
find_recent_downloads <- function() {
  downloads_dir <- path.expand("~/Downloads")
  if (!fs::dir_exists(downloads_dir)) {
    return(character())
  }
  
  # List files inside downloads
  files <- tryCatch(fs::dir_info(downloads_dir), error = function(e) NULL)
  if (is.null(files) || nrow(files) == 0) {
    return(character())
  }
  
  # Filter: keep only files (not directories), ignore hidden files, keep only specific extensions
  valid_exts <- c("csv", "xlsx", "rds", "r", "qmd", "rmd", "txt", "tsv")
  
  files <- files[files$type == "file", ]
  files <- files[!startsWith(basename(files$path), "."), ]
  files$ext <- tolower(fs::path_ext(files$path))
  files <- files[files$ext %in% valid_exts, ]
  
  if (nrow(files) == 0) {
    return(character())
  }
  
  # Sort by modification_time (newest first)
  files <- files[order(files$modification_time, decreasing = TRUE), ]
  
  head(as.character(files$path), 5)
}

default_file_template <- function(type = "R") {
  templates <- list(
    R = paste0(
      "# Script R\n",
      "# Criado em ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n"
    ),
    Rmd = paste(
      "---",
      "title: \"Relatório\"",
      "author: \"Seu Nome\"",
      "date: \"`r Sys.Date()`\"",
      "output: html_document",
      "---",
      "",
      "# Introdução",
      "",
      sep = "\n"
    ),
    qmd = paste(
      "---",
      "title: \"Relatório\"",
      "author: \"Seu Nome\"",
      "date: today",
      "format: html",
      "---",
      "",
      "# Introdução",
      "",
      sep = "\n"
    ),
    md = "# Título\n\n",
    txt = "",
    csv = "coluna1,coluna2,coluna3\nvalor1,valor2,valor3\n"
  )

  templates[[type]] %||% ""
}

git_path_is_tracked <- function(path, repo = ".") {
  relative_path <- trimws(path %||% "")
  if (!nzchar(relative_path)) {
    return(FALSE)
  }

  result <- run_git(c("ls-files", "--", relative_path), path = repo)
  result$status == 0L && any(nzchar(trimws(result$output)))
}

is_protected_project_item <- function(path_to_delete) {
  normalized_path <- gsub("\\\\", "/", trimws(path_to_delete %||% ""))
  if (!nzchar(normalized_path)) {
    return(FALSE)
  }

  basename_path <- basename(normalized_path)

  identical(basename_path, ".git") ||
    identical(basename_path, ".gitignore") ||
    identical(basename_path, ".Rproj.user") ||
    grepl("\\.Rproj$", basename_path)
}

file_delete_info <- function(path_to_delete, path = ".") {
  project_path <- normalize_project_path(path)
  relative_path <- trimws(path_to_delete %||% "")

  if (!nzchar(relative_path)) {
    return(list(
      ok = FALSE,
      reason = "empty_path",
      message = "Selecione um arquivo ou pasta para deletar."
    ))
  }

  full_path <- fs::path(project_path, relative_path)

  if (!is_within_project(full_path, project_path)) {
    return(list(
      ok = FALSE,
      reason = "outside_project",
      message = "O item precisa estar dentro do projeto."
    ))
  }

  if (isTRUE(is_protected_project_item(relative_path))) {
    return(list(
      ok = FALSE,
      reason = "protected_file",
      message = glue::glue("'{relative_path}' é um arquivo crítico e não pode ser deletado.")
    ))
  }

  exists <- fs::file_exists(full_path) || fs::dir_exists(full_path)
  if (!isTRUE(exists)) {
    return(list(
      ok = FALSE,
      reason = "not_found",
      message = glue::glue("'{relative_path}' não foi encontrado no projeto.")
    ))
  }

  item_kind <- if (fs::dir_exists(full_path)) "directory" else "file"
  item_type_label <- if (identical(item_kind, "directory")) "Pasta" else "Arquivo"

  was_tracked <- FALSE
  repo <- git_repo_root(project_path)
  if (!is.null(repo)) {
    was_tracked <- git_path_is_tracked(
      relative_project_path(full_path, repo),
      repo = repo
    )
  }

  list(
    ok = TRUE,
    relative_path = relative_path,
    full_path = full_path,
    item_kind = item_kind,
    item_type_label = item_type_label,
    label = sprintf("%s '%s'", item_type_label, relative_path),
    was_tracked = was_tracked
  )
}

#' Cria um novo arquivo no projeto
#'
#' @param filename Nome do arquivo (ex: "script.R", "relatorio.qmd")
#' @param type Tipo de arquivo: "R", "Rmd", "qmd", "md", "txt", "csv"
#' @param destination Pasta dentro do projeto (ex: "scripts", "reports")
#' @param path Caminho do projeto
#' @param content Conteúdo inicial do arquivo. Se `NULL`, usa o template padrão.
#' @param open_in_rstudio Se TRUE, abre o arquivo no RStudio editor
#'
#' @return Lista com resultado da criação
#' @export
file_create <- function(filename, type = "R", destination = ".", path = ".", content = NULL, open_in_rstudio = TRUE) {
  project_path <- normalize_project_path(path)
  destination_dir <- normalize_project_destination(destination)
  full_dir <- fs::path(project_path, destination_dir)
  filename <- trimws(filename %||% "")
  file_path <- fs::path(full_dir, filename)

  # Validações
  if (!nzchar(filename)) {
    cli::cli_alert_danger("Nome do arquivo não pode estar vazio.")
    return(invisible(list(ok = FALSE, reason = "empty_filename")))
  }

  if (!is_within_project(file_path, project_path)) {
    cli::cli_alert_danger("O arquivo precisa ficar dentro do projeto.")
    return(invisible(list(ok = FALSE, reason = "outside_project")))
  }

  if (fs::file_exists(file_path)) {
    cli::cli_alert_danger(glue::glue("O arquivo '{filename}' já existe."))
    return(invisible(list(ok = FALSE, reason = "file_exists")))
  }

  file_content <- content
  if (is.null(file_content)) {
    file_content <- default_file_template(type)
  } else {
    file_content <- paste(as.character(file_content), collapse = "\n")
  }

  # Criar diretório se necessário
  fs::dir_create(full_dir, recurse = TRUE)

  # Escrever arquivo
  tryCatch({
    writeLines(file_content, file_path, useBytes = TRUE)

    cli::cli_alert_success(glue::glue("Arquivo '{filename}' criado com sucesso."))

    # Abrir no RStudio se solicitado
    if (isTRUE(open_in_rstudio) && rstudio_available()) {
      tryCatch(
        rstudioapi::navigateToFile(file_path),
        error = function(e) NULL
      )
    }

    invisible(list(
      ok = TRUE,
      filename = filename,
      path = file_path,
      relative_path = relative_project_path(file_path, project_path),
      type = type
    ))
  }, error = function(e) {
    cli::cli_alert_danger("Erro ao criar arquivo.")
    cli::cli_inform(conditionMessage(e))
    invisible(list(ok = FALSE, reason = "creation_error", error = conditionMessage(e)))
  })
}

#' Deleta um arquivo ou pasta no projeto
#'
#' @param path_to_delete Caminho relativo do arquivo/pasta a deletar (ex: "scripts/old_analysis.R")
#' @param path Caminho do projeto
#'
#' @return Lista com resultado da deleção
#' @export
file_delete <- function(path_to_delete, path = ".") {
  info <- file_delete_info(path_to_delete, path = path)
  if (!isTRUE(info$ok)) {
    cli::cli_alert_danger(info$message)
    return(invisible(info))
  }

  # Deletar
  tryCatch({
    if (identical(info$item_kind, "directory")) {
      fs::dir_delete(info$full_path)
    } else {
      fs::file_delete(info$full_path)
    }

    cli::cli_alert_success(glue::glue("{info$item_type_label} '{info$relative_path}' deletado."))

    if (isTRUE(info$was_tracked)) {
      cli::cli_inform("⚠️  Este item estava rastreado no Git. Você pode recuperá-lo do histórico se necessário.")
    }

    invisible(list(
      ok = TRUE,
      path = info$relative_path,
      item_type = info$item_kind,
      was_tracked = info$was_tracked
    ))
  }, error = function(e) {
    cli::cli_alert_danger("Erro ao deletar item.")
    cli::cli_inform(conditionMessage(e))
    invisible(list(ok = FALSE, reason = "deletion_error", error = conditionMessage(e)))
  })
}

#' Renomeia um arquivo ou pasta no projeto
#' @export
file_rename <- function(source, target, path = ".") {
  project_path <- normalize_project_path(path)
  source_path <- fs::path(project_path, source)
  target_path <- fs::path(project_path, target)

  if (!fs::file_exists(source_path) && !fs::dir_exists(source_path)) {
    cli::cli_alert_danger(glue::glue("Origem '{source}' não encontrada."))
    return(invisible(list(ok = FALSE, output = glue::glue("Item não encontrado: {source}"))))
  }

  if (fs::file_exists(target_path) || fs::dir_exists(target_path)) {
    cli::cli_alert_danger(glue::glue("O destino '{target}' já existe."))
    return(invisible(list(ok = FALSE, output = glue::glue("Destino já existe: {target}"))))
  }

  tryCatch({
    target_dir <- fs::path_dir(target_path)
    if (!fs::dir_exists(target_dir)) {
      fs::dir_create(target_dir, recurse = TRUE)
    }

    fs::file_move(source_path, target_path)
    cli::cli_alert_success(glue::glue("Item renomeado para '{target}'."))

    invisible(list(ok = TRUE, output = glue::glue("Renomeado: {source} -> {target}")))
  }, error = function(e) {
    cli::cli_alert_danger("Erro ao renomear item.")
    invisible(list(ok = FALSE, output = conditionMessage(e)))
  })
}
