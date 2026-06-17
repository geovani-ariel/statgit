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
import_project_file <- function(
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
