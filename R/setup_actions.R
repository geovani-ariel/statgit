#' Configura nome e email global no Git
#'
#' @param name Nome a registrar em `user.name` na configuracao global do Git.
#' @param email Email a registrar em `user.email` na configuracao global do Git.
#'
#' @return Invisivelmente, `TRUE` se a configuracao foi aplicada, `FALSE` caso
#'   contrario.
#' @export
git_set_identity <- function(name, email) {
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
