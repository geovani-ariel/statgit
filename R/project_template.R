#' Cria uma estrutura basica para projeto estatistico
#'
#' @param path Caminho do projeto.
#' @param include_data Se `FALSE`, cria READMEs nas pastas de dados para
#'   orientar o uso sem versionar os arquivos reais.
#' @param template Template inicial do projeto.
#' @param extra_files Vetor opcional de arquivos extras, sempre com caminhos
#'   relativos dentro do projeto.
#'
#' @return Uma lista com diretorios e arquivos criados.
#' @export
use_stats_project <- function(
  path = ".",
  include_data = TRUE,
  template = c(
    "analise_exploratoria",
    "trabalho_disciplina",
    "iniciacao_cientifica",
    "tcc",
    "artigo_quarto",
    "projeto_grupo"
  ),
  extra_files = character()
) {
  template <- match.arg(template)
  project_path <- normalize_project_path(path)
  project_name <- basename(project_path)
  extra_files <- normalize_extra_project_files(extra_files)

  starter_files <- c(
    template_starter_files(template, project_name),
    extra_project_files(extra_files, project_name, template)
  )

  directories <- unique(c(
    base_project_directories(),
    dirname(names(starter_files))
  ))
  directories <- directories[nzchar(directories) & directories != "."]

  created_dirs <- ensure_project_directories(project_path, directories)
  created_files <- character()

  readme_path <- fs::path(project_path, "README.md")
  if (!fs::file_exists(readme_path)) {
    writeLines(
      project_readme_template(project_name = project_name, template = template),
      readme_path,
      useBytes = TRUE
    )
    created_files <- c(created_files, "README.md")
  }

  if (isFALSE(include_data)) {
    starter_files <- c(
      starter_files,
      setNames(list(data_readme_template("raw")), "data/raw/README.md"),
      setNames(list(data_readme_template("processed")), "data/processed/README.md")
    )
  }

  created_files <- c(
    created_files,
    create_project_files(project_path, starter_files)
  )

  if (length(created_dirs) == 0 && length(created_files) == 0) {
    cli::cli_alert_info("A estrutura basica do projeto ja existia. Nada foi sobrescrito.")
  } else {
    cli::cli_alert_success(
      glue::glue(
        "Estrutura do projeto criada com o template '{project_template_label(template)}'."
      )
    )
  }

  invisible(list(
    path = project_path,
    template = template,
    created_dirs = created_dirs,
    created_files = unique(created_files),
    include_data = include_data,
    extra_files = extra_files
  ))
}

#' Cria um projeto estatistico organizado com .Rproj e estrutura inicial
#'
#' @param path Caminho da pasta do projeto.
#' @param template Template inicial do projeto.
#' @param include_data Se `FALSE`, cria README nas pastas de dados para manter
#'   os arquivos reais fora do Git.
#' @param initialize_git Se `TRUE`, inicializa um repositorio Git no projeto.
#' @param open Se `TRUE`, tenta abrir o projeto no RStudio ao final.
#' @param extra_files Vetor opcional de arquivos extras, com um caminho
#'   relativo por elemento.
#'
#' @return Uma lista com o resumo da criacao.
#' @export
create_stats_project <- function(
  path,
  template = c(
    "analise_exploratoria",
    "trabalho_disciplina",
    "iniciacao_cientifica",
    "tcc",
    "artigo_quarto",
    "projeto_grupo"
  ),
  include_data = TRUE,
  initialize_git = TRUE,
  open = FALSE,
  extra_files = character()
) {
  template <- match.arg(template)
  project_path <- normalize_project_path(path)
  fs::dir_create(project_path, recurse = TRUE)

  structure_result <- use_stats_project(
    path = project_path,
    include_data = include_data,
    template = template,
    extra_files = extra_files
  )
  rproj_result <- create_rstudio_project_file(project_path)
  gitignore_result <- create_r_gitignore(project_path, include_data = include_data)
  git_result <- if (isTRUE(initialize_git)) init_git_project(project_path) else NULL
  open_result <- if (isTRUE(open)) open_stats_project(project_path) else NULL

  invisible(list(
    path = project_path,
    template = template,
    created_dirs = structure_result$created_dirs,
    created_files = unique(c(
      structure_result$created_files,
      rproj_result$created_files,
      fs::path_file(gitignore_result$path)
    )),
    include_data = include_data,
    initialize_git = initialize_git,
    git = git_result,
    open = open_result
  ))
}

create_rstudio_project_file <- function(path = ".", project_name = NULL) {
  project_path <- normalize_project_path(path)
  project_name <- project_name %||% basename(project_path)
  rproj_path <- fs::path(project_path, paste0(project_name, ".Rproj"))

  if (fs::file_exists(rproj_path)) {
    return(invisible(list(
      path = rproj_path,
      created_files = character(),
      project_name = project_name
    )))
  }

  writeLines(rstudio_project_file_template(project_name), rproj_path, useBytes = TRUE)

  invisible(list(
    path = rproj_path,
    created_files = fs::path_file(rproj_path),
    project_name = project_name
  ))
}

base_project_directories <- function() {
  c(
    "R",
    "scripts",
    "data",
    "data/raw",
    "data/processed",
    "reports",
    "figs"
  )
}

ensure_project_directories <- function(project_path, directories) {
  created_dirs <- character()

  for (dir in unique(directories)) {
    dir_path <- fs::path(project_path, dir)
    if (!fs::dir_exists(dir_path)) {
      fs::dir_create(dir_path, recurse = TRUE)
      created_dirs <- c(created_dirs, dir)
    }
  }

  created_dirs
}

create_project_files <- function(project_path, files) {
  if (length(files) == 0) {
    return(character())
  }

  created_files <- character()

  for (relative_path in names(files)) {
    full_path <- fs::path(project_path, relative_path)
    parent_dir <- dirname(full_path)

    if (!fs::dir_exists(parent_dir)) {
      fs::dir_create(parent_dir, recurse = TRUE)
    }

    if (!fs::file_exists(full_path)) {
      writeLines(files[[relative_path]], full_path, useBytes = TRUE)
      created_files <- c(created_files, relative_path)
    }
  }

  created_files
}

normalize_extra_project_files <- function(extra_files = character()) {
  extra_files <- trimws(as.character(extra_files))
  extra_files <- extra_files[nzchar(extra_files)]

  if (length(extra_files) == 0) {
    return(character())
  }

  invalid <- grepl("^(/|~|[A-Za-z]:)", extra_files) |
    grepl("(^|/|\\\\)\\.\\.($|/|\\\\)", extra_files)

  if (any(invalid)) {
    stop(
      "Arquivos extras devem usar caminhos relativos dentro do projeto, sem '..'.",
      call. = FALSE
    )
  }

  unique(gsub("\\\\", "/", extra_files))
}

project_template_label <- function(template) {
  switch(
    template,
    trabalho_disciplina = "Trabalho de disciplina",
    iniciacao_cientifica = "Iniciacao cientifica",
    tcc = "TCC",
    artigo_quarto = "Artigo com Quarto",
    analise_exploratoria = "Analise exploratoria",
    projeto_grupo = "Projeto em grupo"
  )
}

template_starter_files <- function(template, project_name) {
  switch(
    template,
    trabalho_disciplina = c(
      setNames(list(r_script_template("Preparacao dos dados para a disciplina")), "scripts/01-preparacao.R"),
      setNames(list(r_script_template("Analise principal do trabalho")), "scripts/02-analise.R"),
      setNames(list(quarto_report_template(project_name, "Relatorio final da disciplina")), "reports/relatorio-final.qmd")
    ),
    iniciacao_cientifica = c(
      setNames(list(r_script_template("Limpeza e organizacao dos dados da pesquisa")), "scripts/01-limpeza.R"),
      setNames(list(r_script_template("Modelos e tabelas da pesquisa")), "scripts/02-modelagem.R"),
      setNames(list(quarto_report_template(project_name, "Plano de trabalho")), "reports/plano-de-trabalho.qmd"),
      setNames(list(quarto_report_template(project_name, "Relatorio parcial")), "reports/relatorio-parcial.qmd")
    ),
    tcc = c(
      setNames(list(r_script_template("Preparacao do banco do TCC")), "scripts/01-preparacao.R"),
      setNames(list(r_script_template("Resultados do TCC")), "scripts/02-resultados.R"),
      setNames(list(quarto_report_template(project_name, "Roteiro do TCC")), "reports/tcc.qmd")
    ),
    artigo_quarto = c(
      setNames(list(quarto_config_template(project_name)), "_quarto.yml"),
      setNames(list(quarto_report_template(project_name, "Artigo")), "reports/artigo.qmd"),
      setNames(list(bib_template()), "refs.bib")
    ),
    analise_exploratoria = c(
      setNames(list(r_script_template("Exploracao inicial dos dados")), "scripts/01-exploracao.R"),
      setNames(list(quarto_report_template(project_name, "Notas de exploracao")), "reports/notas.qmd")
    ),
    projeto_grupo = c(
      setNames(list(r_script_template("Configuracao compartilhada do grupo")), "scripts/00-setup.R"),
      setNames(list(quarto_report_template(project_name, "Andamento do projeto")), "reports/andamento.qmd"),
      setNames(list(contributing_template()), "CONTRIBUTING.md")
    )
  )
}

extra_project_files <- function(extra_files, project_name, template) {
  if (length(extra_files) == 0) {
    return(list())
  }

  stats::setNames(
    lapply(extra_files, default_extra_file_contents, project_name = project_name, template = template),
    extra_files
  )
}

default_extra_file_contents <- function(relative_path, project_name, template) {
  ext <- tolower(fs::path_ext(relative_path))
  title <- path_title(relative_path)

  if (identical(ext, "qmd")) {
    return(quarto_report_template(project_name, title))
  }

  if (identical(ext, "rmd")) {
    return(rmarkdown_report_template(project_name, title))
  }

  if (identical(ext, "r")) {
    return(r_script_template(glue::glue("{title} do projeto {project_name}")))
  }

  if (identical(ext, "md")) {
    return(markdown_note_template(title, template))
  }

  if (identical(ext, "yml") || identical(ext, "yaml")) {
    return(c("# Arquivo de configuracao inicial", ""))
  }

  ""
}
