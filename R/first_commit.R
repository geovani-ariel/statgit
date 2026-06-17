#' Faz o primeiro commit do projeto
#'
#' Adiciona os arquivos do projeto ao historico local e cria um commit inicial.
#'
#' @param message Mensagem do commit.
#' @param path Caminho do projeto.
#'
#' @return Uma lista com o resultado da operacao.
#' @export
git_commit_all <- function(message = "Primeiro commit", path = ".") {
  project_path <- normalize_project_path(path)

  if (!git_installed()) {
    cli::cli_alert_danger("Git n\u00e3o foi encontrado. Instale o Git antes de continuar.")
    return(invisible(list(ok = FALSE, reason = "git_missing")))
  }

  if (!is_git_repo(project_path)) {
    cli::cli_alert_danger("Este projeto ainda n\u00e3o usa Git.")
    cli::cli_inform("Rode git_init() primeiro.")
    return(invisible(list(ok = FALSE, reason = "repo_missing")))
  }

  identity <- git_identity(project_path)
  author_signature <- if (isTRUE(identity$complete)) {
    sprintf("%s <%s>", identity$name, identity$email)
  } else {
    NULL
  }

  if (!isTRUE(identity$complete)) {
    cli::cli_alert_warning("O Git precisa de nome e email antes do primeiro commit.")
    cli::cli_inform(c(
      "Configure pelo menos uma destas opc\u00f5es:",
      '  git config user.name "Seu Nome"',
      '  git config user.email "seu@email.com"',
      "ou, para todos os projetos:",
      '  git config --global user.name "Seu Nome"',
      '  git config --global user.email "seu@email.com"'
    ))
    return(invisible(list(ok = FALSE, reason = "missing_identity")))
  }

  repo <- git_repo_root(project_path)
  status_before <- repo_status_table(repo)
  buckets_before <- status_breakdown(status_before)
  only_staged <- length(buckets_before$staged) > 0 &&
    length(buckets_before$new) == 0 &&
    length(buckets_before$modified) == 0 &&
    length(buckets_before$deleted) == 0

  if (nrow(status_before) == 0) {
    cli::cli_alert_info("N\u00e3o h\u00e1 nada novo para commitar neste projeto.")
    return(invisible(list(ok = TRUE, committed = FALSE, reason = "nothing_to_commit")))
  }

  add_result <- tryCatch(
    {
      gert::git_add(files = ".", repo = repo)
      TRUE
    },
    error = function(e) e
  )

  if (inherits(add_result, "error")) {
    cli::cli_alert_danger("N\u00e3o foi poss\u00edvel preparar os arquivos para o commit.")
    cli::cli_inform(conditionMessage(add_result))
    return(invisible(list(ok = FALSE, reason = "git_add_failed")))
  }

  if (length(buckets_before$conflicted) > 0) {
    cli::cli_alert_danger("Existem conflitos pendentes. Resolva isso antes de criar o commit.")
    return(invisible(list(ok = FALSE, reason = "has_conflicts")))
  }

  commit_id <- tryCatch(
    gert::git_commit(
      message = message,
      author = author_signature,
      committer = author_signature,
      repo = repo
    ),
    error = function(e) e
  )

  if (inherits(commit_id, "error")) {
    cli::cli_alert_danger("N\u00e3o foi poss\u00edvel criar o commit.")
    cli::cli_inform(conditionMessage(commit_id))
    return(invisible(list(ok = FALSE, reason = "commit_failed")))
  }

  if (only_staged) {
    cli::cli_alert_success("Os arquivos que j\u00e1 estavam preparados foram salvos no hist\u00f3rico.")
  } else {
    cli::cli_alert_success("Arquivos adicionados ao hist\u00f3rico.")
  }
  cli::cli_alert_success(glue::glue('Commit criado: "{message}"'))
  cli::cli_inform(c(
    "Isso significa que o projeto agora tem um primeiro ponto salvo.",
    "Voc\u00ea pode voltar a esta vers\u00e3o no futuro se algo quebrar."
  ))

  invisible(list(
    ok = TRUE,
    committed = TRUE,
    commit = as.character(commit_id),
    message = message,
    path = repo
  ))
}
