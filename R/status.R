#' Mostra o estado Git do projeto em linguagem amigavel
#'
#' @param path Caminho do projeto.
#'
#' @return Invisivelmente, um data frame com o status do repositorio.
#' @export
git_status <- function(path = ".") {
  diagnosis <- build_git_diagnosis(path)

  if (!diagnosis$git_installed) {
    cli::cli_alert_danger("Git n\u00e3o foi encontrado. Instale o Git antes de continuar.")
    return(invisible(empty_status_table()))
  }

  if (!diagnosis$has_repo) {
    cli::cli_alert_warning("Esta pasta ainda n\u00e3o usa Git.")
    cli::cli_inform("V\u00e1 na aba 'Git e GitHub' e clique em 'Inicializar Git'.")
    return(invisible(empty_status_table()))
  }

  status_tbl <- diagnosis$status
  buckets <- status_breakdown(status_tbl)

  cat("Estado do projeto\n\n", sep = "")
  if (!diagnosis$is_repo_root) {
    cat("Voc\u00ea est\u00e1 em uma subpasta, mas o reposit\u00f3rio vale para o projeto inteiro.\n\n")
  }

  if (nrow(status_tbl) == 0) {
    cat("Nenhum arquivo pendente. Seu hist\u00f3rico local est\u00e1 em dia.\n\n")
    cat("Sugest\u00e3o:\nContinue trabalhando e fa\u00e7a um commit ao concluir uma etapa l\u00f3gica da an\u00e1lise.\n")
    return(invisible(status_tbl))
  }

  if (length(buckets$new) > 0) {
    cat("Arquivos novos ainda fora do hist\u00f3rico:\n", sep = "")
    print_named_list(paste0("- ", buckets$new))
    cat("\n")
  }

  if (length(buckets$modified) > 0) {
    cat("Arquivos modificados desde o \u00faltimo commit:\n", sep = "")
    print_named_list(paste0("- ", buckets$modified))
    cat("\n")
  }

  if (length(buckets$deleted) > 0) {
    cat("Arquivos removidos desde o \u00faltimo commit:\n", sep = "")
    print_named_list(paste0("- ", buckets$deleted))
    cat("\n")
  }

  if (length(buckets$staged) > 0) {
    cat("Arquivos prontos para commit (staged):\n", sep = "")
    print_named_list(paste0("- ", buckets$staged))
    cat("\n")
  }

  if (length(buckets$conflicted) > 0) {
    cat("Arquivos com conflito que precisam de aten\u00e7\u00e3o:\n", sep = "")
    print_named_list(paste0("- ", buckets$conflicted))
    cat("\n")
  }

  cat("Sugest\u00e3o:\nFa\u00e7a um commit ao concluir uma etapa da an\u00e1lise para salvar essa vers\u00e3o no hist\u00f3rico.\n")
  invisible(status_tbl)
}
