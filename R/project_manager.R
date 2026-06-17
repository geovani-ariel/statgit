#' Lista projetos RStudio disponiveis em uma pasta
#'
#' @param path Pasta base da busca.
#' @param recursive Se `TRUE`, procura tambem em subpastas.
#'
#' @return Um data frame com nome, pasta e arquivo `.Rproj`.
#' @export
find_rstudio_projects <- function(path = ".", recursive = TRUE) {
  search_path <- normalize_project_path(path)

  if (!fs::dir_exists(search_path)) {
    stop("A pasta informada para busca de projetos nao existe.", call. = FALSE)
  }

  files <- list.files(
    path = search_path,
    pattern = "\\.[Rr]proj$",
    recursive = recursive,
    full.names = TRUE
  )

  files <- sort(normalize_project_path(files))

  data.frame(
    name = fs::path_ext_remove(fs::path_file(files)),
    project_dir = dirname(files),
    rproj_path = files,
    stringsAsFactors = FALSE
  )
}

#' Abre rapidamente um projeto RStudio
#'
#' @param path Caminho de uma pasta de projeto ou do proprio arquivo `.Rproj`.
#'
#' @return Uma lista com o resultado da operacao.
#' @export
open_stats_project <- function(path) {
  project_file <- resolve_rstudio_project_file(path)

  if (!rstudio_available()) {
    cli::cli_alert_info("Projeto localizado. Abra este arquivo .Rproj no RStudio para trocar de projeto.")
    cli::cli_inform(project_file)
    return(invisible(list(ok = TRUE, opened = FALSE, path = project_file)))
  }

  rstudio_open_project(project_file)

  invisible(list(ok = TRUE, opened = TRUE, path = project_file))
}

#' Addin visual para criar e abrir projetos RStudio
#'
#' @return Invisivelmente, o caminho do projeto criado ou aberto.
#' @export
project_manager_addin <- function() {
  ensure_suggested_package("shiny", "o gerenciador de projetos")
  ensure_suggested_package("miniUI", "o gerenciador de projetos")

  ui <- miniUI::miniPage(
    miniUI::gadgetTitleBar("git4stats: gerenciar projetos"),
    miniUI::miniContentPanel(
      shiny::fillCol(
        flex = c(1, 1),
        shiny::div(
          shiny::h4("1. Criar projeto organizado"),
          shiny::textInput("base_dir", "Pasta base", value = default_projects_directory()),
          shiny::textInput("project_name", "Nome do projeto", value = "meu-projeto"),
          shiny::selectInput("template", "Template", choices = project_template_choices()),
          shiny::checkboxInput("include_data", "Versionar a pasta data/", value = TRUE),
          shiny::checkboxInput("initialize_git", "Inicializar Git", value = TRUE),
          shiny::checkboxInput("open_after_create", "Abrir projeto ao criar", value = TRUE),
          shiny::textAreaInput(
            "extra_files",
            "Arquivos extras (um por linha)",
            value = "scripts/03-figuras.R\nreports/apresentacao.qmd",
            rows = 4
          ),
          shiny::actionButton("create_project", "Criar projeto"),
          shiny::verbatimTextOutput("create_result")
        ),
        shiny::div(
          shiny::h4("2. Abrir projeto existente"),
          shiny::textInput("search_root", "Buscar projetos em", value = default_projects_directory()),
          shiny::actionButton("refresh_projects", "Atualizar lista"),
          shiny::selectInput("project_choice", "Projetos encontrados", choices = character()),
          shiny::actionButton("open_project", "Abrir projeto selecionado"),
          shiny::verbatimTextOutput("open_result")
        )
      )
    )
  )

  server <- function(input, output, session) {
    values <- shiny::reactiveValues(
      create_result = "",
      open_result = ""
    )

    project_choices <- shiny::reactiveVal(named_project_choices(default_projects_directory()))

    capture_lines <- function(expr) {
      paste(utils::capture.output(force(expr)), collapse = "\n")
    }

    refresh_project_choices <- function(root) {
      project_choices(named_project_choices(root))
    }

    shiny::observe({
      choices <- project_choices()
      shiny::updateSelectInput(
        session,
        "project_choice",
        choices = choices,
        selected = if (length(choices) > 0) unname(choices[[1]]) else character()
      )
    })

    shiny::observeEvent(input$refresh_projects, {
      refresh_project_choices(input$search_root)
      values$open_result <- ""
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$create_project, {
      project_name <- trimws(input$project_name)

      if (!nzchar(project_name)) {
        values$create_result <- "Informe um nome para o projeto."
        return()
      }

      target_path <- fs::path(normalize_project_path(input$base_dir), project_name)
      extra_files <- split_extra_file_lines(input$extra_files)

      values$create_result <- capture_lines(
        create_stats_project(
          path = target_path,
          template = input$template,
          include_data = isTRUE(input$include_data),
          initialize_git = isTRUE(input$initialize_git),
          open = isTRUE(input$open_after_create),
          extra_files = extra_files
        )
      )

      refresh_project_choices(input$search_root)
    })

    shiny::observeEvent(input$open_project, {
      selected <- input$project_choice %||% ""

      if (!nzchar(selected)) {
        values$open_result <- "Nenhum projeto foi selecionado."
        return()
      }

      values$open_result <- capture_lines(open_stats_project(selected))
      shiny::stopApp(selected)
    })

    shiny::observeEvent(input$done, {
      shiny::stopApp(invisible(NULL))
    })

    shiny::observeEvent(input$cancel, {
      shiny::stopApp(invisible(NULL))
    })

    output$create_result <- shiny::renderText(values$create_result)
    output$open_result <- shiny::renderText(values$open_result)
  }

  shiny::runGadget(ui, server = server, viewer = shiny::dialogViewer("git4stats"))
  invisible(NULL)
}

resolve_rstudio_project_file <- function(path) {
  project_path <- normalize_project_path(path)

  if (fs::file_exists(project_path) && grepl("\\.[Rr]proj$", project_path)) {
    return(project_path)
  }

  if (!fs::dir_exists(project_path)) {
    stop("O caminho informado nao existe.", call. = FALSE)
  }

  rproj_files <- find_rproj_files(project_path)

  if (length(rproj_files) == 0) {
    stop("Nenhum arquivo .Rproj foi encontrado nesta pasta.", call. = FALSE)
  }

  if (length(rproj_files) > 1) {
    stop("Mais de um .Rproj foi encontrado. Informe o arquivo desejado.", call. = FALSE)
  }

  rproj_files[[1]]
}

default_projects_directory <- function() {
  current_project <- tryCatch(active_project_path(), error = function(e) NULL)

  if (!is.null(current_project) && fs::dir_exists(current_project)) {
    return(dirname(current_project))
  }

  path.expand("~")
}

named_project_choices <- function(root) {
  projects <- tryCatch(
    find_rstudio_projects(root),
    error = function(e) data.frame(
      name = character(),
      project_dir = character(),
      rproj_path = character(),
      stringsAsFactors = FALSE
    )
  )

  if (nrow(projects) == 0) {
    return(setNames(character(), character()))
  }

  labels <- paste(projects$name, "-", projects$project_dir)
  stats::setNames(projects$rproj_path, labels)
}

project_template_choices <- function() {
  choices <- c(
    "analise_exploratoria",
    "trabalho_disciplina",
    "iniciacao_cientifica",
    "tcc",
    "artigo_quarto",
    "projeto_grupo"
  )

  stats::setNames(choices, vapply(choices, project_template_label, character(1)))
}

split_extra_file_lines <- function(text) {
  if (is.null(text) || !nzchar(text)) {
    return(character())
  }

  normalize_extra_project_files(strsplit(text, "\n", fixed = TRUE)[[1]])
}

rstudio_open_project <- function(path) {
  rstudioapi::openProject(path)
}
