#' Lista projetos RStudio disponiveis em uma pasta
#'
#' @param path Pasta base da busca.
#' @param recursive Se `TRUE`, procura tambem em subpastas.
#'
#' @return Um data frame com nome, pasta e arquivo `.Rproj`.
#' @export
project_find <- function(path = ".", recursive = TRUE) {
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
project_open <- function(path) {
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
    miniUI::gadgetTitleBar("statgit"),
    miniUI::miniContentPanel(
      shiny::fillCol(
        flex = c(0, 1),
        shiny::selectInput("trackr_language", NULL, choices = trackr_language_choices(), selected = "en", width = "100%"),
        shiny::uiOutput("project_manager_content")
      )
    )
  )

  server <- function(input, output, session) {
    language <- shiny::reactive(input$trackr_language %||% "en")
    tr <- trackr_tr(language)
    values <- shiny::reactiveValues(
      create_result = "",
      open_result = "",
      clone_result = ""
    )

    project_choices <- shiny::reactiveVal(named_project_choices(default_projects_directory()))

    capture_lines <- function(expr) {
      paste(utils::capture.output(force(expr)), collapse = "\n")
    }

    refresh_project_choices <- function(root) {
      project_choices(named_project_choices(root))
    }

    output$project_manager_content <- shiny::renderUI({
      shiny::fillCol(
        flex = c(1, 1, 1),
        shiny::div(
          shiny::h4(tr("manager.create_title")),
          shiny::textInput("base_dir", tr("project.base_dir"), value = default_projects_directory()),
          shiny::textInput("project_name", tr("project.name"), value = tr("project.default_name")),
          shiny::selectInput("template", "Template", choices = project_template_choices()),
          shiny::checkboxInput("include_data", tr("project.include_data"), value = TRUE),
          shiny::checkboxInput("initialize_git", tr("project.initialize_git"), value = TRUE),
          shiny::checkboxInput("open_after_create", tr("project.open_after_create"), value = TRUE),
          shiny::textAreaInput(
            "extra_files",
            tr("project.extra_files"),
            value = "scripts/03-figuras.R\nreports/apresentacao.qmd",
            rows = 4
          ),
          shiny::actionButton("create_project", tr("project.create_button")),
          shiny::verbatimTextOutput("create_result")
        ),
        shiny::div(
          shiny::h4(tr("manager.open_title")),
          shiny::textInput("search_root", tr("manager.search_root"), value = default_projects_directory()),
          shiny::actionButton("refresh_projects", tr("changes.refresh")),
          shiny::selectInput("project_choice", tr("manager.found_projects"), choices = character()),
          shiny::actionButton("open_project", tr("manager.open_selected")),
          shiny::verbatimTextOutput("open_result")
        ),
        shiny::div(
          shiny::h4("Clonar repositório"),
          shiny::textInput("clone_url", "URL do repositório", value = ""),
          shiny::textInput("clone_base_dir", tr("project.base_dir"), value = default_projects_directory()),
          shiny::textInput("clone_dir", "Nome da pasta clonada (opcional)", value = ""),
          shiny::checkboxInput("clone_open_after", tr("project.open_after_create"), value = TRUE),
          shiny::actionButton("clone_project", "Clonar repositório"),
          shiny::verbatimTextOutput("clone_result")
        )
      )
    })

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
        values$create_result <- tr("manager.name_required")
        return()
      }

      target_path <- fs::path(normalize_project_path(input$base_dir), project_name)
      extra_files <- split_extra_file_lines(input$extra_files)

      values$create_result <- capture_lines(
        project_create(
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
        values$open_result <- tr("manager.none_selected")
        return()
      }

      values$open_result <- capture_lines(project_open(selected))
      shiny::stopApp(selected)
    })

    shiny::observeEvent(input$clone_project, {
      values$clone_result <- capture_lines(
        git_clone_repo(
          remote_url = input$clone_url,
          path = input$clone_base_dir,
          directory = input$clone_dir,
          open = isTRUE(input$clone_open_after)
        )
      )

      refresh_project_choices(input$search_root)
    })

    shiny::observeEvent(input$done, {
      shiny::stopApp(invisible(NULL))
    })

    shiny::observeEvent(input$cancel, {
      shiny::stopApp(invisible(NULL))
    })

    output$create_result <- shiny::renderText(values$create_result)
    output$open_result <- shiny::renderText(values$open_result)
    output$clone_result <- shiny::renderText(values$clone_result)
  }

  shiny::runGadget(ui, server = server, viewer = shiny::dialogViewer("statgit"))
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
    project_find(root),
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
