#' Formata um arquivo suportado com styler
#'
#' @param path Caminho do arquivo. Quando `NULL`, usa o arquivo ativo no editor.
#'
#' @return Invisivelmente, uma lista com o resultado da formatacao.
#' @export
format_active_file <- function(path = NULL) {
  ensure_suggested_package("styler", "o formatador de codigo")

  target_path <- resolve_style_document_path(path)
  editor <- matching_source_document(target_path)

  if (!is.null(editor$id)) {
    save_source_document(editor$id)
  }

  run_styler_file(target_path)

  if (!is.null(editor$id)) {
    refresh_source_document_from_disk(target_path, editor$id)
  }

  cli::cli_alert_success(glue::glue("Arquivo processado pelo formatador: {fs::path_file(target_path)}."))

  invisible(list(
    ok = TRUE,
    path = target_path,
    refreshed_editor = !is.null(editor$id)
  ))
}

#' Formata os arquivos suportados de um projeto
#'
#' @param path Caminho da pasta do projeto.
#'
#' @return Invisivelmente, uma lista com os arquivos formatados.
#' @export
format_project_files <- function(path = ".") {
  ensure_suggested_package("styler", "o formatador de codigo")

  project_path <- normalize_project_path(path)
  files <- find_styleable_files(project_path)

  if (length(files) == 0) {
    cli::cli_alert_info("Nenhum arquivo suportado para formatacao foi encontrado.")
    return(invisible(list(
      ok = TRUE,
      path = project_path,
      styled_files = character()
    )))
  }

  editor <- matching_source_document(files)

  if (!is.null(editor$id)) {
    save_source_document(editor$id)
  }

  for (file in files) {
    run_styler_file(file)
  }

  if (!is.null(editor$id)) {
    refresh_source_document_from_disk(editor$path, editor$id)
  }

  cli::cli_alert_success(glue::glue(
    "{length(files)} arquivo(s) processado(s) pelo formatador no projeto."
  ))

  invisible(list(
    ok = TRUE,
    path = project_path,
    styled_files = files
  ))
}

resolve_style_document_path <- function(path = NULL) {
  document_path <- path %||% active_source_document_path()

  if (is.null(document_path) || !nzchar(document_path)) {
    stop(
      "Abra e salve um arquivo .R, .Rmd, .Rmarkdown, .qmd ou .Rprofile antes de formatar.",
      call. = FALSE
    )
  }

  document_path <- normalize_project_path(document_path)

  if (!fs::file_exists(document_path)) {
    stop("O arquivo informado para formatacao nao existe.", call. = FALSE)
  }

  if (!is_styleable_file(document_path)) {
    stop(
      "O formatador suporta arquivos .R, .Rmd, .Rmarkdown, .qmd e .Rprofile.",
      call. = FALSE
    )
  }

  document_path
}

is_styleable_file <- function(path) {
  file_name <- tolower(fs::path_file(path))
  extension <- tolower(fs::path_ext(path))

  identical(file_name, ".rprofile") ||
    extension %in% c("r", "rmd", "rmarkdown", "qmd")
}

find_styleable_files <- function(path = ".") {
  project_path <- normalize_project_path(path)

  if (!fs::dir_exists(project_path)) {
    stop("A pasta informada para formatacao nao existe.", call. = FALSE)
  }

  files <- list.files(
    path = project_path,
    recursive = TRUE,
    all.files = TRUE,
    full.names = TRUE,
    include.dirs = FALSE
  )

  files <- normalize_project_path(files)
  files <- files[!is_excluded_style_path(files)]
  files <- files[vapply(files, is_styleable_file, logical(1))]

  unique(sort(files))
}

is_excluded_style_path <- function(path) {
  normalized <- gsub("\\\\", "/", path)
  grepl("/(\\.git|\\.Rproj\\.user|renv|packrat)/", normalized)
}

matching_source_document <- function(paths) {
  context <- source_editor_context()

  if (is.null(context)) {
    return(list(id = NULL, path = NULL))
  }

  context_path <- context$path %||% ""
  if (!nzchar(context_path)) {
    return(list(id = NULL, path = NULL))
  }

  context_path <- as.character(normalize_project_path(context_path))
  candidate_paths <- as.character(normalize_project_path(paths))

  if (!(context_path %in% candidate_paths)) {
    return(list(id = NULL, path = NULL))
  }

  list(
    id = context$id %||% NULL,
    path = context_path
  )
}

refresh_source_document_from_disk <- function(path, id) {
  if (!rstudio_available()) {
    return(invisible(FALSE))
  }

  contents <- paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")

  tryCatch(
    {
      rstudioapi::setDocumentContents(contents, id = id)
      save_source_document(id)
      TRUE
    },
    error = function(e) FALSE
  )
}

run_styler_file <- function(path) {
  before <- readLines(path, warn = FALSE, encoding = "UTF-8")
  warnings <- character()
  cache_dir <- fs::path_temp("git4stats-styler-cache")
  fs::dir_create(cache_dir)
  old_cache_dir <- Sys.getenv("R_USER_CACHE_DIR", unset = NA_character_)
  Sys.setenv(R_USER_CACHE_DIR = cache_dir)
  on.exit({
    if (is.na(old_cache_dir)) {
      Sys.unsetenv("R_USER_CACHE_DIR")
    } else {
      Sys.setenv(R_USER_CACHE_DIR = old_cache_dir)
    }
  }, add = TRUE)

  result <- tryCatch(
    withCallingHandlers(
      styler::style_file(path),
      warning = function(w) {
        warnings <<- c(warnings, conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    ),
    error = function(e) e
  )

  if (inherits(result, "error")) {
    stop(conditionMessage(result), call. = FALSE)
  }

  after <- readLines(path, warn = FALSE, encoding = "UTF-8")
  changed <- !identical(before, after)

  if (length(warnings) > 0 && !changed) {
    stop(
      paste(
        c(
          "O styler nao conseguiu formatar este arquivo.",
          warnings
        ),
        collapse = "\n"
      ),
      call. = FALSE
    )
  }

  invisible(list(
    path = path,
    changed = changed,
    warnings = warnings
  ))
}
