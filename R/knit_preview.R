#' Gera uma pre-visualizacao HTML de um relatorio
#'
#' Renderiza um arquivo `.Rmd`, `.Rmarkdown` ou `.qmd` em HTML e abre o
#' resultado no Viewer do RStudio quando disponivel.
#'
#' @param path Caminho do relatorio. Quando `NULL`, usa o arquivo aberto no
#'   editor do RStudio.
#' @param style Se `TRUE`, roda o formatador com `styler` antes de renderizar.
#'
#' @return Invisivelmente, uma lista com os caminhos de entrada e saida.
#' @export
preview_knit <- function(path = NULL, style = FALSE) {
  input_path <- resolve_preview_document_path(path)

  if (isTRUE(style)) {
    format_active_file(input_path)
  }

  output_path <- render_preview_document(input_path)

  open_preview_output(output_path)
  cli::cli_alert_success("Pre-visualizacao HTML gerada.")

  invisible(list(
    ok = TRUE,
    input = input_path,
    output = output_path
  ))
}

#' Inicia uma pre-visualizacao com atualizacao automatica
#'
#' Para arquivos `.qmd`, delega para `quarto preview`. Para `.Rmd` e
#' `.Rmarkdown`, abre um gadget que observa o arquivo salvo e rerenderiza
#' automaticamente.
#'
#' @param path Caminho do relatorio. Quando `NULL`, usa o arquivo aberto no
#'   editor do RStudio.
#' @param style Se `TRUE`, roda o formatador com `styler` antes de cada render.
#' @param interval_ms Intervalo de verificacao do arquivo, em milissegundos.
#'
#' @return Invisivelmente, um resumo da pre-visualizacao iniciada.
#' @export
live_preview_knit <- function(path = NULL, style = FALSE, interval_ms = 1500) {
  input_path <- resolve_preview_document_path(path)
  extension <- tolower(fs::path_ext(input_path))

  if (identical(extension, "qmd")) {
    return(start_quarto_live_preview(input_path))
  }

  start_rmarkdown_live_preview(
    path = input_path,
    style = style,
    interval_ms = interval_ms
  )
}

resolve_preview_document_path <- function(path = NULL) {
  document_path <- path %||% active_source_document_path()

  if (is.null(document_path) || !nzchar(document_path)) {
    stop(
      "Abra e salve um arquivo .Rmd, .Rmarkdown ou .qmd antes de usar o pre-visualizador.",
      call. = FALSE
    )
  }

  document_path <- normalize_project_path(document_path)

  if (!fs::file_exists(document_path)) {
    stop("O arquivo informado para pre-visualizacao nao existe.", call. = FALSE)
  }

  extension <- tolower(fs::path_ext(document_path))
  if (!extension %in% c("rmd", "rmarkdown", "qmd")) {
    stop(
      "O pre-visualizador suporta arquivos .Rmd, .Rmarkdown e .qmd.",
      call. = FALSE
    )
  }

  document_path
}

active_source_document_path <- function() {
  if (!rstudio_available()) {
    return(NULL)
  }

  context <- source_editor_context()
  if (is.null(context)) {
    return(NULL)
  }

  context_path <- context$path %||% ""
  if (!nzchar(context_path)) {
    return(NULL)
  }

  save_source_document(context$id %||% NULL)
  context_path
}

rstudio_available <- function() {
  isTRUE(rstudioapi::isAvailable())
}

source_editor_context <- function() {
  tryCatch(
    rstudioapi::getSourceEditorContext(),
    error = function(e) NULL
  )
}

save_source_document <- function(id = NULL) {
  if (!rstudio_available()) {
    return(invisible(FALSE))
  }

  tryCatch(
    {
      rstudioapi::documentSave(id = id)
      TRUE
    },
    error = function(e) FALSE
  )
}

render_preview_document <- function(path, output_dir = preview_output_dir()) {
  extension <- tolower(fs::path_ext(path))

  if (identical(extension, "qmd")) {
    return(render_quarto_preview(path, output_dir = output_dir))
  }

  render_rmarkdown_preview(path, output_dir = output_dir)
}

render_rmarkdown_preview <- function(path, output_dir = preview_output_dir()) {
  ensure_suggested_package("rmarkdown", "o pre-visualizador de knit")
  ensure_pandoc_available()

  output_file <- paste0(fs::path_ext_remove(fs::path_file(path)), ".html")

  rendered_file <- rmarkdown::render(
    input = path,
    output_format = "html_document",
    output_file = output_file,
    output_dir = output_dir,
    quiet = TRUE,
    envir = new.env(parent = globalenv())
  )

  normalize_project_path(rendered_file)
}

ensure_pandoc_available <- function() {
  if (isTRUE(rmarkdown::pandoc_available())) {
    return(invisible(TRUE))
  }

  stop(
    paste(
      "Pandoc nao foi encontrado.",
      "No RStudio isso normalmente funciona automaticamente.",
      "Fora dele, configure a variavel RSTUDIO_PANDOC ou instale o Pandoc."
    ),
    call. = FALSE
  )
}

render_quarto_preview <- function(path, output_dir = preview_output_dir()) {
  quarto_bin <- quarto_command()

  if (!nzchar(quarto_bin)) {
    stop(
      "Para pre-visualizar arquivos .qmd, instale o Quarto CLI ou use um arquivo .Rmd.",
      call. = FALSE
    )
  }

  output_file <- paste0(fs::path_ext_remove(fs::path_file(path)), ".html")

  result <- suppressWarnings(system2(
    quarto_bin,
    c("render", path, "--to", "html", "--output-dir", output_dir),
    stdout = TRUE,
    stderr = TRUE
  ))

  status <- as.integer(attr(result, "status") %||% 0L)
  rendered_path <- fs::path(output_dir, output_file)

  if (status != 0L || !fs::file_exists(rendered_path)) {
    stop(
      paste(
        c(
          "Nao foi possivel renderizar a pre-visualizacao do arquivo .qmd.",
          result
        ),
        collapse = "\n"
      ),
      call. = FALSE
    )
  }

  normalize_project_path(rendered_path)
}

quarto_command <- function() {
  Sys.which("quarto")
}

preview_output_dir <- function() {
  output_dir <- fs::path_temp("git4stats-preview")
  fs::dir_create(output_dir)
  output_dir
}

open_preview_output <- function(path) {
  preview_path <- normalize_project_path(path)

  if (rstudio_available()) {
    viewer_opened <- tryCatch(
      {
        rstudioapi::viewer(preview_path)
        TRUE
      },
      error = function(e) FALSE
    )

    if (viewer_opened) {
      return(invisible(preview_path))
    }
  }

  utils::browseURL(preview_path)
  invisible(preview_path)
}

start_quarto_live_preview <- function(path) {
  quarto_bin <- quarto_command()

  if (!nzchar(quarto_bin)) {
    stop(
      "Para live preview de arquivos .qmd, instale o Quarto CLI.",
      call. = FALSE
    )
  }

  run_background_command(
    command = quarto_bin,
    args = c("preview", path),
    wd = dirname(path)
  )

  cli::cli_alert_success("Live preview do Quarto iniciado.")

  invisible(list(
    ok = TRUE,
    mode = "quarto",
    input = path,
    command = c(quarto_bin, "preview", path)
  ))
}

start_rmarkdown_live_preview <- function(path, style = FALSE, interval_ms = 1500) {
  ensure_suggested_package("shiny", "o live preview")
  ensure_suggested_package("miniUI", "o live preview")
  ensure_suggested_package("rmarkdown", "o live preview")

  interval_ms <- as.integer(interval_ms)
  if (is.na(interval_ms) || interval_ms < 250L) {
    interval_ms <- 250L
  }

  output_dir <- preview_output_dir()
  resource_prefix <- live_preview_resource_prefix()
  shiny::addResourcePath(resource_prefix, output_dir)
  on.exit({
    try(shiny::removeResourcePath(resource_prefix), silent = TRUE)
  }, add = TRUE)

  initial_output <- render_live_preview_document(path, output_dir, style = style)

  ui <- miniUI::miniPage(
    miniUI::gadgetTitleBar("git4stats: live preview"),
    miniUI::miniContentPanel(
      shiny::fillCol(
        flex = c(0, 1),
        shiny::wellPanel(
          shiny::strong(basename(path)),
          shiny::tags$br(),
          shiny::textOutput("live_status"),
          shiny::conditionalPanel(
            condition = "output.live_error !== ''",
            shiny::tags$hr(),
            shiny::verbatimTextOutput("live_error")
          )
        ),
        shiny::uiOutput("live_frame")
      )
    )
  )

  server <- function(input, output, session) {
    values <- shiny::reactiveValues(
      output_path = initial_output,
      last_mtime = file_mtime_token(path),
      last_render = Sys.time(),
      status = "Aguardando alteracoes no arquivo salvo.",
      error = ""
    )

    output$live_status <- shiny::renderText(values$status)
    output$live_error <- shiny::renderText(values$error)

    output$live_frame <- shiny::renderUI({
      shiny::tags$iframe(
        src = live_preview_iframe_src(resource_prefix, values$output_path, values$last_render),
        style = "width: 100%; height: 100%; border: 0;"
      )
    })

    observe_live_preview <- function() {
      shiny::invalidateLater(interval_ms, session)

      current_mtime <- file_mtime_token(path)
      if (identical(current_mtime, values$last_mtime)) {
        return()
      }

      values$last_mtime <- current_mtime
      values$status <- "Alteracao detectada. Atualizando preview..."

      refreshed <- tryCatch(
        render_live_preview_document(path, output_dir, style = style),
        error = function(e) e
      )

      if (inherits(refreshed, "error")) {
        values$error <- conditionMessage(refreshed)
        values$status <- "Falha ao atualizar. Corrija o arquivo e salve novamente."
        return()
      }

      values$output_path <- refreshed
      values$last_render <- Sys.time()
      values$error <- ""
      values$status <- paste(
        "Preview atualizado em",
        format(values$last_render, "%H:%M:%S")
      )
    }

    shiny::observe({
      observe_live_preview()
    })

    shiny::observeEvent(input$done, {
      shiny::stopApp(invisible(list(
        ok = TRUE,
        mode = "rmarkdown",
        input = path,
        output = values$output_path
      )))
    })

    shiny::observeEvent(input$cancel, {
      shiny::stopApp(invisible(NULL))
    })
  }

  result <- shiny::runGadget(
    ui,
    server = server,
    viewer = shiny::dialogViewer("git4stats")
  )

  invisible(result %||% list(
    ok = TRUE,
    mode = "rmarkdown",
    input = path,
    output = initial_output
  ))
}

render_live_preview_document <- function(path, output_dir, style = FALSE) {
  if (isTRUE(style)) {
    format_active_file(path)
  }

  render_rmarkdown_preview(path, output_dir = output_dir)
}

live_preview_resource_prefix <- function() {
  paste0(
    "git4stats-preview-",
    format(as.integer(stats::runif(1, min = 1, max = .Machine$integer.max)), scientific = FALSE)
  )
}

live_preview_iframe_src <- function(resource_prefix, output_path, token) {
  file_name <- utils::URLencode(fs::path_file(output_path), reserved = TRUE)
  paste0("/", resource_prefix, "/", file_name, "?v=", as.numeric(token))
}

file_mtime_token <- function(path) {
  info <- file.info(path)

  if (!isTRUE(info$exists[[1]])) {
    return(NA_real_)
  }

  as.numeric(info$mtime[[1]])
}

run_background_command <- function(command, args, wd = NULL) {
  old_wd <- NULL

  if (!is.null(wd)) {
    old_wd <- getwd()
    setwd(wd)
    on.exit(setwd(old_wd), add = TRUE)
  }

  invisible(system2(command, args, wait = FALSE, stdout = FALSE, stderr = FALSE))
}
