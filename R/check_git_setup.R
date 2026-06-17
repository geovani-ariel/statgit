#' Diagnostica Git no projeto atual
#'
#' Verifica se Git esta instalado, se nome e email globais estao configurados
#' e qual e o estado basico do repositorio atual.
#'
#' @param path Caminho do projeto a ser analisado.
#'
#' @return Uma lista com o diagnostico e impressao amigavel no console.
#' @export
check_git_setup <- function(path = ".") {
  diagnosis <- build_git_diagnosis(path)
  print(diagnosis)
  invisible(diagnosis)
}

#' @export
print.git4stats_diagnosis <- function(x, ...) {
  print_git_setup(x)
  invisible(x)
}
