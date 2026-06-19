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
  remote <- normalize_remote_name(remote)

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

  result <- run_git(c("ls-remote", remote), path = project_path)
  output <- paste(result$output, collapse = "\n")

  if (result$status == 0L) {
    cli::cli_alert_success("Conex\u00e3o com o GitHub validada para este remote.")
    if (length(result$output) == 0L) {
      cli::cli_inform("O remote est\u00e1 acess\u00edvel, mas ainda n\u00e3o tem refs publicadas. Fa\u00e7a o primeiro Push para publicar a branch.")
    } else {
      cli::cli_inform("Se voc\u00ea j\u00e1 tiver commits locais, clique em 'Push'.")
    }
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
  remote <- normalize_remote_name(remote)

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
      reason = "remote_exists",
      remote = remote,
      remote_url = existing$url[[1]],
      requested_remote_url = remote_url
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

#' Desconecta o projeto local do remote GitHub atual
#'
#' Remove um remote configurado sem apagar o historico local do repositorio.
#'
#' @param path Caminho do projeto.
#' @param remote Nome do remote.
#'
#' @return Uma lista com o resultado da operacao.
#' @export
github_disconnect <- function(path = ".", remote = "origin") {
  project_path <- normalize_project_path(path)
  remote <- normalize_remote_name(remote)

  if (!git_installed()) {
    cli::cli_alert_danger("Git n\u00e3o foi encontrado. Instale o Git antes de continuar.")
    return(invisible(list(ok = FALSE, reason = "git_missing", path = project_path)))
  }

  if (!is_git_repo(project_path)) {
    cli::cli_alert_danger("Esta pasta ainda n\u00e3o usa Git.")
    return(invisible(list(ok = FALSE, reason = "repo_missing", path = project_path)))
  }

  existing <- remote_by_name(project_path, remote = remote)
  if (is.null(existing)) {
    cli::cli_alert_warning(glue::glue("Nenhum remote chamado '{remote}' foi encontrado."))
    return(invisible(list(ok = FALSE, reason = "remote_missing", remote = remote)))
  }

  result <- run_git(c("remote", "remove", remote), path = project_path)
  if (result$status != 0L) {
    cli::cli_alert_danger("N\u00e3o foi poss\u00edvel desconectar o remote GitHub.")
    cli::cli_inform(result$output)
    return(invisible(list(ok = FALSE, reason = "remote_remove_failed", output = result$output)))
  }

  cli::cli_alert_success(glue::glue("Remote '{remote}' desconectado deste projeto."))

  invisible(list(
    ok = TRUE,
    changed = TRUE,
    remote = remote,
    remote_url = existing$url[[1]]
  ))
}

github_repo_browse_url <- function(remote_url) {
  url <- trimws(remote_url %||% "")
  if (!looks_like_github_url(url)) {
    return(NULL)
  }

  if (grepl("^git@github\\.com:", url, ignore.case = TRUE)) {
    path <- sub("^git@github\\.com:", "", url, ignore.case = TRUE)
    path <- sub("\\.git$", "", path, ignore.case = TRUE)
    return(paste0("https://github.com/", path))
  }

  if (grepl("^ssh://git@github\\.com/", url, ignore.case = TRUE)) {
    path <- sub("^ssh://git@github\\.com/", "", url, ignore.case = TRUE)
    path <- sub("\\.git$", "", path, ignore.case = TRUE)
    return(paste0("https://github.com/", path))
  }

  if (grepl("^https?://github\\.com/", url, ignore.case = TRUE)) {
    return(sub("\\.git$", "", url, ignore.case = TRUE))
  }

  NULL
}

#' Abre o repositório GitHub configurado no navegador
#'
#' Converte a URL do remote em uma URL web navegável e abre no navegador padrão.
#'
#' @param path Caminho do projeto.
#' @param remote Nome do remote.
#'
#' @return Uma lista com o resultado da operação.
#' @export
github_open_repo <- function(path = ".", remote = "origin") {
  project_path <- normalize_project_path(path)
  remote <- normalize_remote_name(remote)

  if (!git_installed()) {
    cli::cli_alert_danger("Git n\u00e3o foi encontrado. Instale o Git antes de continuar.")
    return(invisible(list(ok = FALSE, reason = "git_missing", path = project_path)))
  }

  if (!is_git_repo(project_path)) {
    cli::cli_alert_danger("Esta pasta ainda n\u00e3o usa Git.")
    return(invisible(list(ok = FALSE, reason = "repo_missing", path = project_path)))
  }

  remote_info <- remote_by_name(project_path, remote = remote)
  if (is.null(remote_info)) {
    cli::cli_alert_warning(glue::glue("Nenhum remote chamado '{remote}' foi encontrado."))
    return(invisible(list(ok = FALSE, reason = "remote_missing", remote = remote)))
  }

  browse_url <- github_repo_browse_url(remote_info$url[[1]])
  if (is.null(browse_url)) {
    cli::cli_alert_warning("O remote existe, mas n\u00e3o parece apontar para uma URL web naveg\u00e1vel do GitHub.")
    cli::cli_inform(remote_info$url[[1]])
    return(invisible(list(
      ok = FALSE,
      reason = "non_browsable_remote",
      remote = remote,
      remote_url = remote_info$url[[1]]
    )))
  }

  utils::browseURL(browse_url)
  cli::cli_alert_success("Reposit\u00f3rio aberto no navegador.")

  invisible(list(
    ok = TRUE,
    remote = remote,
    remote_url = remote_info$url[[1]],
    browse_url = browse_url
  ))
}

#' Busca atualizações do remote sem alterar a branch local
#'
#' Executa `git fetch --prune` para atualizar as referências remotas.
#'
#' @param path Caminho do projeto.
#' @param remote Nome do remote.
#'
#' @return Uma lista com o resultado da operação.
#' @export
git_fetch <- function(path = ".", remote = "origin") {
  project_path <- normalize_project_path(path)
  remote <- normalize_remote_name(remote)
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

  result <- run_git(c("fetch", "--prune", remote), path = project_path)
  if (result$status != 0L) {
    cli::cli_alert_danger("N\u00e3o foi poss\u00edvel buscar atualiza\u00e7\u00f5es do remote.")
    cli::cli_inform(result$output)
    return(invisible(list(
      ok = FALSE,
      reason = "fetch_failed",
      remote = remote,
      output = result$output
    )))
  }

  sync_status <- repo_sync_status(project_path, remote = remote, branch = diagnosis$branch)
  cli::cli_alert_success(glue::glue("Atualiza\u00e7\u00f5es de '{remote}' carregadas."))

  invisible(list(
    ok = TRUE,
    remote = remote,
    output = result$output,
    sync_status = sync_status
  ))
}

clone_target_name <- function(remote_url) {
  url <- trimws(remote_url %||% "")
  if (!nzchar(url)) {
    return(NULL)
  }

  cleaned <- sub("/+$", "", url)
  cleaned <- sub("\\.git$", "", cleaned, ignore.case = TRUE)
  name <- basename(cleaned)
  if (!nzchar(name) || identical(name, ".") || identical(name, "/")) {
    return(NULL)
  }

  name
}

#' Clona um repositório Git em uma pasta local
#'
#' @param remote_url URL ou caminho do repositório remoto.
#' @param path Pasta base onde o clone será criado.
#' @param directory Nome opcional da pasta de destino.
#' @param open Se `TRUE`, tenta abrir o projeto clonado no RStudio ao final.
#'
#' @return Uma lista com o resultado da operação.
#' @export
git_clone_repo <- function(remote_url, path = ".", directory = NULL, open = FALSE) {
  base_path <- normalize_project_path(path)
  remote_url <- trimws(remote_url %||% "")
  directory <- trimws(directory %||% "")

  if (!git_installed()) {
    cli::cli_alert_danger("Git n\u00e3o foi encontrado. Instale o Git antes de continuar.")
    return(invisible(list(ok = FALSE, reason = "git_missing", path = base_path)))
  }

  if (!nzchar(remote_url)) {
    cli::cli_alert_danger("Informe a URL do reposit\u00f3rio antes de clonar.")
    return(invisible(list(ok = FALSE, reason = "missing_remote_url", path = base_path)))
  }

  if (!fs::dir_exists(base_path)) {
    cli::cli_alert_danger("A pasta de destino informada n\u00e3o existe.")
    return(invisible(list(ok = FALSE, reason = "missing_base_dir", path = base_path)))
  }

  target_name <- if (nzchar(directory)) directory else clone_target_name(remote_url)
  if (is.null(target_name) || !nzchar(target_name)) {
    cli::cli_alert_danger("N\u00e3o foi poss\u00edvel inferir o nome da pasta de destino.")
    return(invisible(list(ok = FALSE, reason = "missing_directory_name", path = base_path)))
  }

  if (grepl("(^|/|\\\\)\\.\\.($|/|\\\\)", target_name) || grepl("^(/|~|[A-Za-z]:)", target_name)) {
    cli::cli_alert_danger("O nome da pasta de destino precisa ser relativo e seguro.")
    return(invisible(list(ok = FALSE, reason = "invalid_directory_name", directory = target_name)))
  }

  target_path <- normalize_project_path(fs::path(base_path, target_name))
  if (fs::dir_exists(target_path) || fs::file_exists(target_path)) {
    cli::cli_alert_warning("A pasta de destino j\u00e1 existe.")
    return(invisible(list(
      ok = FALSE,
      reason = "target_exists",
      path = target_path
    )))
  }

  result <- run_git(c("clone", remote_url, target_path))
  if (result$status != 0L) {
    cli::cli_alert_danger("N\u00e3o foi poss\u00edvel clonar o reposit\u00f3rio.")
    cli::cli_inform(result$output)
    return(invisible(list(
      ok = FALSE,
      reason = "clone_failed",
      remote_url = remote_url,
      path = target_path,
      output = result$output
    )))
  }

  cli::cli_alert_success(glue::glue("Reposit\u00f3rio clonado em '{target_path}'."))

  open_result <- NULL
  if (isTRUE(open)) {
    rproj_files <- find_rproj_files(target_path)
    if (length(rproj_files) == 1L) {
      open_result <- project_open(rproj_files[[1]])
    } else if (length(rproj_files) == 0L) {
      cli::cli_alert_info("Clone conclu\u00eddo, mas nenhum arquivo .Rproj foi encontrado para abertura autom\u00e1tica.")
      open_result <- invisible(list(
        ok = FALSE,
        opened = FALSE,
        reason = "missing_rproj",
        path = target_path
      ))
    } else {
      cli::cli_alert_info("Clone conclu\u00eddo, mas mais de um arquivo .Rproj foi encontrado. Abra o projeto desejado manualmente.")
      open_result <- invisible(list(
        ok = FALSE,
        opened = FALSE,
        reason = "multiple_rproj",
        path = target_path,
        rproj_files = rproj_files
      ))
    }
  }

  invisible(list(
    ok = TRUE,
    remote_url = remote_url,
    path = target_path,
    open = open_result
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
  remote <- normalize_remote_name(remote)
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

  sync_status <- repo_sync_status(project_path, remote = remote, branch = branch)
  if (isTRUE(sync_status$behind > 0L)) {
    cli::cli_alert_warning(glue::glue("Sua branch local est\u00e1 {sync_status$behind} commit(s) atr\u00e1s de {sync_status$upstream_branch}."))
    cli::cli_inform("Fa\u00e7a um Pull antes de enviar para evitar rejei\u00e7\u00e3o do push.")
    return(invisible(list(
      ok = FALSE,
      reason = "behind_remote",
      remote = remote,
      branch = branch,
      sync_status = sync_status
    )))
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
  remote <- normalize_remote_name(remote)
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

  sync_status <- repo_sync_status(project_path, remote = remote, branch = branch)
  if (!isTRUE(sync_status$remote_branch_exists)) {
    cli::cli_alert_warning(glue::glue("A branch {remote}/{branch} ainda n\u00e3o existe no remote."))
    return(invisible(list(ok = FALSE, reason = "remote_branch_missing", remote = remote, branch = branch)))
  }

  pull_result <- run_git(c("pull", "--rebase", remote, branch), path = project_path)
  if (pull_result$status != 0L) {
    cli::cli_alert_danger("N\u00e3o foi poss\u00edvel baixar as mudan\u00e7as do remote.")
    cli::cli_inform(pull_result$output)
    return(invisible(list(
      ok = FALSE,
      reason = pull_failure_reason(pull_result$output),
      remote = remote,
      branch = branch,
      output = pull_result$output
    )))
  }

  cli::cli_alert_success(glue::glue("Mudan\u00e7as baixadas de {remote}/{branch}."))
  invisible(list(ok = TRUE, remote = remote, branch = branch, output = pull_result$output))
}

pull_failure_reason <- function(output = character()) {
  message <- paste(output %||% character(), collapse = "\n")

  if (grepl("CONFLICT|could not apply|Resolve all conflicts manually", message, ignore.case = TRUE)) {
    return("pull_conflict")
  }

  "pull_failed"
}

pull_failure_next_step <- function(reason = "pull_failed") {
  if (identical(reason, "pull_conflict")) {
    return("Resolva os conflitos, rode 'git rebase --continue' ou 'git rebase --abort', e tente sincronizar novamente.")
  }

  NULL
}

git_sync <- function(path = ".", remote = "origin", branch = NULL) {
  project_path <- normalize_project_path(path)
  remote <- normalize_remote_name(remote)
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

  sync_status <- repo_sync_status(project_path, remote = remote, branch = branch)
  pull_output <- character()

  if (isTRUE(sync_status$remote_branch_exists)) {
    pull_result <- run_git(c("pull", "--rebase", remote, branch), path = project_path)
    pull_output <- pull_result$output

    if (pull_result$status != 0L) {
      failure_reason <- pull_failure_reason(pull_result$output)
      cli::cli_alert_danger("N\u00e3o foi poss\u00edvel baixar e reaplicar mudan\u00e7as do remote.")
      cli::cli_inform(pull_result$output)
      return(invisible(list(
        ok = FALSE,
        reason = failure_reason,
        remote = remote,
        branch = branch,
        output = pull_result$output,
        rebase_in_progress = identical(failure_reason, "pull_conflict"),
        next_step = pull_failure_next_step(failure_reason)
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
