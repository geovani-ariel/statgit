#' Configura nome e email global no Git
#' @export
setup_git_identity <- function(name, email) {
  if (!git_installed()) return(invisible(FALSE))
  
  res_name <- run_git(c("config", "--global", "user.name", name))
  res_email <- run_git(c("config", "--global", "user.email", email))
  
  if (res_name$status == 0L && res_email$status == 0L) {
    cli::cli_alert_success("Identidade Git configurada com sucesso!")
    return(invisible(TRUE))
  }
  
  cli::cli_alert_danger("Erro ao configurar identidade Git.")
  invisible(FALSE)
}
