#' Lista arquivos com mudancas no projeto
#'
#' @param path Caminho do projeto.
#'
#' @return Um data frame com arquivos alterados.
#' @export
git_changed <- function(path = ".") {
  repo_status_table(path)
}

#' Mostra o diff de um arquivo do projeto
#'
#' @param file Caminho do arquivo relativo ao repositorio.
#' @param path Caminho do projeto.
#' @param staged Se `TRUE`, mostra mudancas preparadas para commit.
#' @param context Use `"changes"` para mostrar apenas os trechos alterados ou
#'   `"full"` para mostrar o arquivo com mais contexto.
#'
#' @return Invisivelmente, uma lista com o diff.
#' @export
git_diff <- function(file, path = ".", staged = FALSE, context = c("changes", "full")) {
  context <- match.arg(context)
  project_path <- normalize_project_path(path)
  repo <- git_repo_root(project_path)

  if (is.null(repo)) {
    cli::cli_alert_warning("Esta pasta ainda nao usa Git.")
    return(invisible(list(ok = FALSE, reason = "repo_missing", diff = character())))
  }

  file <- trimws(file %||% "")
  if (!nzchar(file)) {
    cli::cli_alert_warning("Escolha um arquivo para ver as mudancas.")
    return(invisible(list(ok = FALSE, reason = "file_missing", diff = character())))
  }

  args <- c("diff")
  if (isTRUE(staged)) {
    args <- c(args, "--staged")
  }
  if (identical(context, "full")) {
    args <- c(args, "-U999999")
  }
  args <- c(args, "--", file)

  result <- run_git(args, path = repo)
  if (result$status != 0L) {
    cli::cli_alert_danger("Nao foi possivel gerar o diff.")
    cli::cli_inform(result$output)
    return(invisible(list(ok = FALSE, reason = "diff_failed", output = result$output)))
  }

  diff_lines <- result$output
  if (length(diff_lines) == 0) {
    cli::cli_alert_info("Nenhuma mudanca encontrada para este arquivo.")
  } else {
    print_named_list(diff_lines)
  }

  invisible(list(
    ok = TRUE,
    file = file,
    staged = isTRUE(staged),
    diff = diff_lines
  ))
}

format_diff_for_panel <- function(diff_lines) {
  if (length(diff_lines) == 0) {
    return("Nenhuma mudanca encontrada para este arquivo.")
  }

  paste(diff_lines, collapse = "\n")
}

format_diff_for_panel_html <- function(diff_lines) {
  if (length(diff_lines) == 0) {
    return("<div class=\"tr-diff-empty\">Nenhuma mudanca encontrada para este arquivo.</div>")
  }

  lines <- vapply(diff_lines, diff_line_html, character(1))
  paste0("<div class=\"tr-diff\">", paste(lines, collapse = ""), "</div>")
}

diff_line_html <- function(line) {
  class <- diff_line_class(line)
  marker <- diff_line_marker(line)

  paste0(
    "<div class=\"tr-diff-line tr-diff-", class, "\">",
    "<span class=\"tr-diff-marker\">", escape_html(marker), "</span>",
    "<code>", escape_html(line), "</code>",
    "</div>"
  )
}

diff_line_class <- function(line) {
  if (startsWith(line, "@@")) {
    return("hunk")
  }
  if (startsWith(line, "diff --git") ||
    startsWith(line, "index ") ||
    startsWith(line, "--- ") ||
    startsWith(line, "+++ ")) {
    return("meta")
  }
  if (startsWith(line, "+")) {
    return("add")
  }
  if (startsWith(line, "-")) {
    return("remove")
  }

  "context"
}

diff_line_marker <- function(line) {
  if (startsWith(line, "+") && !startsWith(line, "+++")) {
    return("+")
  }
  if (startsWith(line, "-") && !startsWith(line, "---")) {
    return("-")
  }
  if (startsWith(line, "@@")) {
    return("@")
  }

  " "
}

escape_html <- function(x) {
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  x <- gsub("\"", "&quot;", x, fixed = TRUE)
  x <- gsub("'", "&#39;", x, fixed = TRUE)
  x
}
