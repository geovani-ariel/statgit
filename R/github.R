#' Verifica se o projeto esta pronto para falar com o GitHub
#'
#' Faz uma checagem simples do remote e tenta validar o acesso ao GitHub sem
#' pedir token em texto aberto.
#'
#' @param path Caminho do projeto.
#' @param remote Nome do remote a ser testado.
#'
#' @return Uma lista com o resultado da verificacao.
#' @export
github_check <- function(path = ".", remote = "origin") {
  project_path <- normalize_project_path(path)

  if (!git_installed()) {
    cli::cli_alert_danger("Git n\u00e3o foi encontrado. Instale o Git antes de continuar.")
    return(invisible(list(ok = FALSE, reason = "git_missing", path = project_path)))
  }

  if (!is_git_repo(project_path)) {
    cli::cli_alert_danger("Esta pasta ainda n\u00e3o usa Git.")
    cli::cli_inform("V\u00e1 na aba 'Git e GitHub' e clique em 'Inicializar Git'.")
    return(invisible(list(ok = FALSE, reason = "repo_missing", path = project_path)))
  }

  remote_info <- remote_by_name(project_path, remote = remote)
  if (is.null(remote_info)) {
    cli::cli_alert_warning(glue::glue("Nenhum remote chamado '{remote}' foi encontrado."))
    cli::cli_inform("V\u00e1 na aba 'Git e GitHub' e conecte a URL do reposit\u00f3rio.")
    return(invisible(list(ok = FALSE, reason = "remote_missing", remote = remote)))
  }

  if (!isTRUE(remote_info$is_github[[1]])) {
    cli::cli_alert_warning("O remote existe, mas n\u00e3o parece apontar para o GitHub.")
    cli::cli_inform(remote_info$url[[1]])
    return(invisible(list(
      ok = FALSE,
      reason = "not_github",
      remote = remote,
      remote_url = remote_info$url[[1]]
    )))
  }

  result <- run_git(c("ls-remote", "--exit-code", remote, "HEAD"), path = project_path)
  output <- paste(result$output, collapse = "\n")

  if (result$status == 0L) {
    cli::cli_alert_success("Conex\u00e3o com o GitHub validada para este remote.")
    cli::cli_inform("Se voc\u00ea j\u00e1 tiver commits locais, clique em 'Push'.")
    return(invisible(list(
      ok = TRUE,
      remote = remote,
      remote_url = remote_info$url[[1]],
      output = result$output
    )))
  }

  if (grepl("authentication|permission denied|could not read username|repository not found", output, ignore.case = TRUE)) {
    cli::cli_alert_warning("O reposit\u00f3rio foi encontrado, mas o acesso foi negado pelo GitHub.")
    cli::cli_inform("Verifique se voc\u00ea tem permiss\u00e3o no reposit\u00f3rio e se est\u00e1 autenticado. Se usar o GitHub Desktop, confirme que est\u00e1 conectado \u00e0 conta correta.")
  } else {
    cli::cli_alert_warning("N\u00e3o foi poss\u00edvel confirmar o acesso ao GitHub agora.")
    cli::cli_inform(output)
  }

  invisible(list(
    ok = FALSE,
    reason = "auth_check_failed",
    remote = remote,
    remote_url = remote_info$url[[1]],
    output = result$output
  ))
}

#' Conecta o projeto local a um repositorio no GitHub
#'
#' Adiciona ou atualiza um remote sem criar tokens nem gravar credenciais.
#'
#' @param remote_url URL do repositorio GitHub.
#' @param path Caminho do projeto.
#' @param remote Nome do remote.
#' @param replace Se `TRUE`, atualiza a URL quando o remote ja existe.
#'
#' @return Uma lista com o resultado da operacao.
#' @export
github_connect <- function(remote_url, path = ".", remote = "origin", replace = FALSE) {
  project_path <- normalize_project_path(path)

  if (!git_installed()) {
    cli::cli_alert_danger("Git n\u00e3o foi encontrado. Instale o Git antes de continuar.")
    return(invisible(list(ok = FALSE, reason = "git_missing", path = project_path)))
  }

  if (!is_git_repo(project_path)) {
    cli::cli_alert_danger("Esta pasta ainda n\u00e3o usa Git.")
    cli::cli_inform("V\u00e1 na aba 'Git e GitHub' e clique em 'Inicializar Git'.")
    return(invisible(list(ok = FALSE, reason = "repo_missing", path = project_path)))
  }

  if (!looks_like_github_url(remote_url)) {
    cli::cli_alert_danger("A URL informada n\u00e3o parece ser de um reposit\u00f3rio GitHub.")
    return(invisible(list(ok = FALSE, reason = "invalid_remote_url", remote_url = remote_url)))
  }

  existing <- remote_by_name(project_path, remote = remote)
  cmd <- if (is.null(existing)) {
    c("remote", "add", remote, remote_url)
  } else if (isTRUE(replace)) {
    c("remote", "set-url", remote, remote_url)
  } else {
    cli::cli_alert_info(glue::glue("O remote '{remote}' j\u00e1 est\u00e1 configurado."))
    cli::cli_inform(existing$url[[1]])
    cli::cli_inform("Use replace = TRUE se quiser trocar a URL.")
    return(invisible(list(
      ok = TRUE,
      changed = FALSE,
      remote = remote,
      remote_url = existing$url[[1]]
    )))
  }

  result <- run_git(cmd, path = project_path)
  if (result$status != 0L) {
    cli::cli_alert_danger("N\u00e3o foi poss\u00edvel configurar o remote GitHub.")
    cli::cli_inform(result$output)
    return(invisible(list(ok = FALSE, reason = "remote_config_failed", output = result$output)))
  }

  cli::cli_alert_success(glue::glue("Remote '{remote}' configurado para o GitHub."))
  cli::cli_inform(c(
    remote_url,
    "Pr\u00f3ximo passo: clique em 'Testar acesso ao GitHub' e depois em 'Push'."
  ))

  invisible(list(
    ok = TRUE,
    changed = TRUE,
    remote = remote,
    remote_url = remote_url
  ))
}

#' Envia o historico local para o remote pela primeira vez
#'
#' Faz um `git push -u` de forma guiada e com mensagens amigaveis.
#'
#' @param path Caminho do projeto.
#' @param remote Nome do remote.
#' @param branch Branch a ser enviada. Se `NULL`, usa a branch atual.
#'
#' @return Uma lista com o resultado da operacao.
#' @export
git_push <- function(path = ".", remote = "origin", branch = NULL) {
  project_path <- normalize_project_path(path)
  diagnosis <- build_git_diagnosis(project_path)

  if (!diagnosis$git_installed) {
    cli::cli_alert_danger("Git n\u00e3o foi encontrado. Instale o Git antes de continuar.")
    return(invisible(list(ok = FALSE, reason = "git_missing", path = project_path)))
  }

  if (!diagnosis$has_repo) {
    cli::cli_alert_danger("Esta pasta ainda n\u00e3o usa Git.")
    cli::cli_inform("V\u00e1 na aba 'Git e GitHub' e clique em 'Inicializar Git'.")
    return(invisible(list(ok = FALSE, reason = "repo_missing", path = project_path)))
  }

  if (!diagnosis$has_commits) {
    cli::cli_alert_warning("Ainda n\u00e3o existe nenhum commit para enviar.")
    cli::cli_inform("V\u00e1 para a aba 'Git e GitHub', escreva uma mensagem de commit e salve uma vers\u00e3o antes de enviar.")
    return(invisible(list(ok = FALSE, reason = "no_commits", path = project_path)))
  }

  remote_info <- remote_by_name(project_path, remote = remote)
  if (is.null(remote_info)) {
    cli::cli_alert_warning(glue::glue("Nenhum remote chamado '{remote}' foi encontrado."))
    cli::cli_inform("V\u00e1 na aba 'Git e GitHub' e conecte a URL do reposit\u00f3rio primeiro.")
    return(invisible(list(ok = FALSE, reason = "remote_missing", remote = remote)))
  }

  branch <- branch %||% diagnosis$branch
  if (is.null(branch) || !nzchar(branch)) {
    cli::cli_alert_danger("N\u00e3o foi poss\u00edvel descobrir a branch atual.")
    return(invisible(list(ok = FALSE, reason = "branch_missing")))
  }

  if (diagnosis$status_counts$total > 0) {
    cli::cli_alert_info("Existem arquivos pendentes no projeto. O push vai enviar apenas os commits j\u00e1 salvos.")
  }

  result <- run_git(c("push", "-u", remote, branch), path = project_path)
  output <- paste(result$output, collapse = "\n")

  if (result$status != 0L) {
    if (grepl("authentication|permission denied|could not read username|repository not found", output, ignore.case = TRUE)) {
      cli::cli_alert_warning("O GitHub recusou o envio. Confirme que voc\u00ea est\u00e1 autenticado e tem permiss\u00e3o de escrita neste reposit\u00f3rio.")
    } else {
      cli::cli_alert_danger("N\u00e3o foi poss\u00edvel enviar os commits para o GitHub.")
    }
    cli::cli_inform(output)
    return(invisible(list(
      ok = FALSE,
      reason = "push_failed",
      remote = remote,
      branch = branch,
      output = result$output
    )))
  }

  cli::cli_alert_success(glue::glue("Commits enviados para {remote}/{branch}."))
  cli::cli_inform("Agora seu hist\u00f3rico local tamb\u00e9m est\u00e1 no GitHub.")

  invisible(list(
    ok = TRUE,
    remote = remote,
    branch = branch,
    output = result$output
  ))
}

#' Sincroniza o historico local com o remote
#'
#' Executa `git pull --rebase` antes de enviar os commits locais com `git push`.
#'
#' @param path Caminho do projeto.
#' @param remote Nome do remote.
#' @param branch Branch a ser sincronizada. Se `NULL`, usa a branch atual.
#'
#' @return Uma lista com o resultado da operacao.
#' @export
git_pull <- function(path = ".", remote = "origin", branch = NULL) {
  project_path <- normalize_project_path(path)
  diagnosis <- build_git_diagnosis(project_path)

  if (!diagnosis$git_installed) {
    cli::cli_alert_danger("Git n\u00e3o foi encontrado. Instale o Git antes de continuar.")
    return(invisible(list(ok = FALSE, reason = "git_missing", path = project_path)))
  }

  if (!diagnosis$has_repo) {
    cli::cli_alert_danger("Esta pasta ainda n\u00e3o usa Git.")
    return(invisible(list(ok = FALSE, reason = "repo_missing", path = project_path)))
  }

  remote_info <- remote_by_name(project_path, remote = remote)
  if (is.null(remote_info)) {
    cli::cli_alert_warning(glue::glue("Nenhum remote chamado '{remote}' foi encontrado."))
    return(invisible(list(ok = FALSE, reason = "remote_missing", remote = remote)))
  }

  branch <- branch %||% diagnosis$branch
  if (is.null(branch) || !nzchar(branch)) {
    cli::cli_alert_danger("N\u00e3o foi poss\u00edvel descobrir a branch atual.")
    return(invisible(list(ok = FALSE, reason = "branch_missing")))
  }

  remote_branch <- run_git(c("ls-remote", "--exit-code", "--heads", remote, branch), path = project_path)
  if (remote_branch$status != 0L) {
    cli::cli_alert_warning(glue::glue("A branch {remote}/{branch} ainda n\u00e3o existe no remote."))
    return(invisible(list(ok = FALSE, reason = "remote_branch_missing", remote = remote, branch = branch)))
  }

  pull_result <- run_git(c("pull", "--rebase", remote, branch), path = project_path)
  if (pull_result$status != 0L) {
    cli::cli_alert_danger("N\u00e3o foi poss\u00edvel baixar as mudan\u00e7as do remote.")
    cli::cli_inform(pull_result$output)
    return(invisible(list(ok = FALSE, reason = "pull_failed", remote = remote, branch = branch, output = pull_result$output)))
  }

  cli::cli_alert_success(glue::glue("Mudan\u00e7as baixadas de {remote}/{branch}."))
  invisible(list(ok = TRUE, remote = remote, branch = branch, output = pull_result$output))
}

git_sync <- function(path = ".", remote = "origin", branch = NULL) {
  project_path <- normalize_project_path(path)
  diagnosis <- build_git_diagnosis(project_path)

  if (!diagnosis$git_installed) {
    cli::cli_alert_danger("Git n\u00e3o foi encontrado. Instale o Git antes de continuar.")
    return(invisible(list(ok = FALSE, reason = "git_missing", path = project_path)))
  }

  if (!diagnosis$has_repo) {
    cli::cli_alert_danger("Esta pasta ainda n\u00e3o usa Git.")
    return(invisible(list(ok = FALSE, reason = "repo_missing", path = project_path)))
  }

  if (!diagnosis$has_commits) {
    cli::cli_alert_warning("Ainda n\u00e3o existe nenhum commit para sincronizar.")
    return(invisible(list(ok = FALSE, reason = "no_commits", path = project_path)))
  }

  remote_info <- remote_by_name(project_path, remote = remote)
  if (is.null(remote_info)) {
    cli::cli_alert_warning(glue::glue("Nenhum remote chamado '{remote}' foi encontrado."))
    return(invisible(list(ok = FALSE, reason = "remote_missing", remote = remote)))
  }

  branch <- branch %||% diagnosis$branch
  if (is.null(branch) || !nzchar(branch)) {
    cli::cli_alert_danger("N\u00e3o foi poss\u00edvel descobrir a branch atual.")
    return(invisible(list(ok = FALSE, reason = "branch_missing")))
  }

  remote_branch <- run_git(c("ls-remote", "--exit-code", "--heads", remote, branch), path = project_path)
  pull_output <- character()

  if (remote_branch$status == 0L) {
    pull_result <- run_git(c("pull", "--rebase", remote, branch), path = project_path)
    pull_output <- pull_result$output

    if (pull_result$status != 0L) {
      cli::cli_alert_danger("N\u00e3o foi poss\u00edvel baixar e reaplicar mudan\u00e7as do remote.")
      cli::cli_inform(pull_result$output)
      return(invisible(list(
        ok = FALSE,
        reason = "pull_failed",
        remote = remote,
        branch = branch,
        output = pull_result$output
      )))
    }
  } else {
    cli::cli_inform(glue::glue("A branch {remote}/{branch} ainda n\u00e3o existe. Pulando pull antes do push."))
  }

  push_result <- git_push(path = project_path, remote = remote, branch = branch)
  if (!isTRUE(push_result$ok)) {
    return(invisible(list(
      ok = FALSE,
      reason = "push_failed_after_pull",
      remote = remote,
      branch = branch,
      pull_output = pull_output,
      push_result = push_result
    )))
  }

  cli::cli_alert_success(glue::glue("Projeto sincronizado com {remote}/{branch}."))

  invisible(list(
    ok = TRUE,
    remote = remote,
    branch = branch,
    pull_output = pull_output,
    push_output = push_result$output
  ))
}
