#' Inicializa Git em um projeto
#'
#' Cria um repositorio Git com branch principal configurada de forma segura.
#'
#' @param path Caminho do projeto.
#' @param branch Nome da branch principal.
#'
#' @return Uma lista com o resultado da inicializacao.
#' @export
init_git_project <- function(path = ".", branch = "main") {
  project_path <- normalize_project_path(path)

  if (!git_installed()) {
    cli::cli_alert_danger("Git n\u00e3o foi encontrado. Instale o Git e tente novamente.")
    return(invisible(list(ok = FALSE, reason = "git_missing", path = project_path)))
  }

  if (is_git_repo(project_path)) {
    cli::cli_alert_success("Este projeto j\u00e1 usa Git.")
    cli::cli_inform("Nenhuma altera\u00e7\u00e3o foi feita.")
    return(invisible(list(ok = TRUE, created = FALSE, path = project_path)))
  }

  result <- run_git(c("init", "--initial-branch", branch, project_path))
  if (result$status != 0L) {
    cli::cli_alert_danger("N\u00e3o foi poss\u00edvel inicializar o reposit\u00f3rio Git.")
    cli::cli_inform(result$output)
    return(invisible(list(ok = FALSE, reason = "git_init_failed", output = result$output)))
  }

  cli::cli_alert_success("Reposit\u00f3rio Git criado.")
  cli::cli_alert_success(glue::glue("Branch principal configurada como {branch}."))
  cli::cli_inform(c(
    "Agora voc\u00ea pode criar seu primeiro commit com:",
    glue::glue('  first_commit(path = "{project_path}")')
  ))

  invisible(list(ok = TRUE, created = TRUE, branch = branch, path = project_path))
}
