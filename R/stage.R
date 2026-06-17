#' Prepara arquivos para o proximo commit
#'
#' @param files Caminhos dos arquivos relativos ao repositorio.
#' @param path Caminho do projeto.
#'
#' @return Invisivelmente, uma lista com o resultado.
#' @export
git_stage <- function(files, path = ".") {
  git_file_action(files, path, action = "stage")
}

#' Remove arquivos da preparacao para commit
#'
#' @param files Caminhos dos arquivos relativos ao repositorio.
#' @param path Caminho do projeto.
#'
#' @return Invisivelmente, uma lista com o resultado.
#' @export
git_unstage <- function(files, path = ".") {
  git_file_action(files, path, action = "unstage")
}

#' Descarta mudancas locais de arquivos
#'
#' @param files Caminhos dos arquivos relativos ao repositorio.
#' @param path Caminho do projeto.
#'
#' @return Invisivelmente, uma lista com o resultado.
#' @export
git_discard <- function(files, path = ".") {
  git_file_action(files, path, action = "discard")
}

#' Cria commit com arquivos ja preparados
#'
#' @param message Mensagem do commit.
#' @param path Caminho do projeto.
#'
#' @return Invisivelmente, uma lista com o resultado.
#' @export
git_commit <- function(message = "Atualiza projeto", path = ".") {
  project_path <- normalize_project_path(path)
  repo <- git_repo_root(project_path)

  if (is.null(repo)) {
    cli::cli_alert_danger("Este projeto ainda nao usa Git.")
    return(invisible(list(ok = FALSE, reason = "repo_missing")))
  }

  message <- trimws(message %||% "")
  if (!nzchar(message)) {
    cli::cli_alert_warning("Escreva uma mensagem para o commit.")
    return(invisible(list(ok = FALSE, reason = "message_missing")))
  }

  identity <- git_identity(repo)
  if (!isTRUE(identity$complete)) {
    cli::cli_alert_warning("O Git precisa de nome e email antes do commit.")
    return(invisible(list(ok = FALSE, reason = "missing_identity")))
  }

  status <- repo_status_table(repo)
  if (!any(status$staged, na.rm = TRUE)) {
    cli::cli_alert_info("Nenhum arquivo esta preparado para commit.")
    return(invisible(list(ok = TRUE, committed = FALSE, reason = "nothing_staged")))
  }

  signature <- sprintf("%s <%s>", identity$name, identity$email)
  commit_id <- tryCatch(
    gert::git_commit(
      message = message,
      author = signature,
      committer = signature,
      repo = repo
    ),
    error = function(e) e
  )

  if (inherits(commit_id, "error")) {
    cli::cli_alert_danger("Nao foi possivel criar o commit.")
    cli::cli_inform(conditionMessage(commit_id))
    return(invisible(list(ok = FALSE, reason = "commit_failed")))
  }

  cli::cli_alert_success(glue::glue('Commit criado: "{message}"'))
  invisible(list(
    ok = TRUE,
    committed = TRUE,
    commit = as.character(commit_id),
    message = message,
    path = repo
  ))
}

git_file_action <- function(files, path = ".", action = c("stage", "unstage", "discard")) {
  action <- match.arg(action)
  project_path <- normalize_project_path(path)
  repo <- git_repo_root(project_path)

  if (is.null(repo)) {
    cli::cli_alert_danger("Este projeto ainda nao usa Git.")
    return(invisible(list(ok = FALSE, reason = "repo_missing")))
  }

  files <- normalize_git_file_selection(files)
  if (length(files) == 0) {
    cli::cli_alert_warning("Selecione pelo menos um arquivo.")
    return(invisible(list(ok = FALSE, reason = "files_missing")))
  }

  result <- switch(
    action,
    stage = run_git(c("add", "--", files), path = repo),
    unstage = unstage_git_files(files, repo),
    discard = discard_git_files(files, repo)
  )
  if (result$status != 0L) {
    cli::cli_alert_danger(git_file_action_failure(action))
    cli::cli_inform(result$output)
    return(invisible(list(ok = FALSE, reason = paste0(action, "_failed"), output = result$output)))
  }

  cli::cli_alert_success(git_file_action_success(action, length(files)))
  invisible(list(ok = TRUE, action = action, files = files, path = repo))
}

unstage_git_files <- function(files, repo) {
  result <- run_git(c("restore", "--staged", "--", files), path = repo)
  if (result$status == 0L) {
    return(result)
  }

  run_git(c("rm", "--cached", "-r", "--", files), path = repo)
}

discard_git_files <- function(files, repo) {
  run_git(c("restore", "--staged", "--", files), path = repo)
  restore_result <- run_git(c("restore", "--", files), path = repo)
  clean_result <- run_git(c("clean", "-f", "--", files), path = repo)

  if (restore_result$status == 0L || clean_result$status == 0L) {
    return(list(status = 0L, output = c(restore_result$output, clean_result$output)))
  }

  list(status = restore_result$status, output = c(restore_result$output, clean_result$output))
}

normalize_git_file_selection <- function(files) {
  files <- unique(trimws(as.character(files %||% character())))
  files[nzchar(files)]
}

git_file_action_success <- function(action, n) {
  switch(
    action,
    stage = glue::glue("{n} arquivo(s) preparado(s) para commit."),
    unstage = glue::glue("{n} arquivo(s) removido(s) da preparacao."),
    discard = glue::glue("Mudancas descartadas em {n} arquivo(s).")
  )
}

git_file_action_failure <- function(action) {
  switch(
    action,
    stage = "Nao foi possivel preparar os arquivos.",
    unstage = "Nao foi possivel remover os arquivos da preparacao.",
    discard = "Nao foi possivel descartar as mudancas."
  )
}
