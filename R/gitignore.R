#' Cria ou atualiza um .gitignore para projetos R
#'
#' Adiciona padroes uteis para projetos R e RStudio, evitando linhas duplicadas.
#'
#' @param path Caminho do projeto.
#' @param include_data Controla se a pasta de dados sera ignorada.
#'
#' @return Uma lista com o caminho do arquivo e as linhas adicionadas.
#' @export
create_r_gitignore <- function(path = ".", include_data = NULL) {
  project_path <- normalize_project_path(path)
  gitignore_path <- fs::path(project_path, ".gitignore")

  existing <- if (fs::file_exists(gitignore_path)) {
    readLines(gitignore_path, warn = FALSE, encoding = "UTF-8")
  } else {
    character()
  }

  additions <- c(default_gitignore_lines(), generated_output_lines())

  if (isFALSE(include_data)) {
    additions <- c(additions, data_gitignore_lines())
  }

  merged <- merge_gitignore_lines(existing, additions)
  added_lines <- setdiff(merged, existing)

  writeLines(merged, gitignore_path, useBytes = TRUE)

  if (length(added_lines) == 0) {
    cli::cli_alert_info("O .gitignore j\u00e1 tinha as regras necess\u00e1rias.")
  } else {
    cli::cli_alert_success(glue::glue(
      "Arquivo .gitignore atualizado com {length(added_lines)} linha(s) nova(s)."
    ))
  }

  if (is.null(include_data)) {
    cli::cli_alert_warning(
      "Voc\u00ea ainda pode decidir depois se quer versionar dados. Use include_data = FALSE para ignorar data/raw/ e data/processed/."
    )
  } else if (isTRUE(include_data)) {
    cli::cli_inform("A pasta data/ continua dispon\u00edvel para versionamento.")
  } else {
    cli::cli_inform("Arquivos em data/raw/ e data/processed/ ser\u00e3o ignorados, mas os READMEs ficam vis\u00edveis.")
  }

  invisible(list(
    path = gitignore_path,
    added_lines = added_lines,
    include_data = include_data
  ))
}
