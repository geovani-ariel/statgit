#' Addin para verificar Git no projeto atual
#'
#' @export
addin_check_git <- function() {
  check_git_setup(active_project_path())
}

#' Addin para abrir o painel principal do git4stats
#'
#' @export
addin_git4stats <- function() {
  git4stats_panel(active_project_path())
}

#' Addin para abrir o wizard de configuracao Git
#'
#' @export
addin_git_setup_wizard <- function() {
  git_setup_wizard(active_project_path())
}

#' Addin para criar o primeiro commit
#'
#' @export
addin_first_commit <- function() {
  default_message <- "Primeiro commit"

  message <- default_message
  if (rstudioapi::isAvailable()) {
    prompt_result <- tryCatch(
      rstudioapi::showPrompt(
        title = "Primeiro commit",
        message = "Escreva uma mensagem curta para salvar esta vers\u00e3o:",
        default = default_message
      ),
      error = function(e) default_message
    )

    message <- prompt_result %||% default_message
  }

  first_commit(message = message, path = active_project_path())
}

#' Addin para ver o status Git do projeto atual
#'
#' @export
addin_git_status <- function() {
  git_status_pretty(active_project_path())
}

#' Addin para pre-visualizar o knit do relatorio ativo
#'
#' @export
addin_knit_preview <- function() {
  preview_knit()
}

#' Addin para live preview do relatorio ativo
#'
#' @export
addin_live_preview <- function() {
  live_preview_knit()
}

#' Addin para formatar o arquivo ativo
#'
#' @export
addin_format_active_file <- function() {
  format_active_file()
}

#' Addin para formatar os arquivos do projeto atual
#'
#' @export
addin_format_project <- function() {
  format_project_files(active_project_path())
}

#' Addin para criar e trocar rapidamente de projeto
#'
#' @export
addin_project_manager <- function() {
  project_manager_addin()
}

#' Wizard visual para configurar Git em um projeto RStudio
#'
#' @param path Caminho do projeto.
#'
#' @return Invisivelmente, o caminho analisado.
#' @export
git_setup_wizard <- function(path = ".") {
  ensure_suggested_package("shiny", "o assistente visual")
  ensure_suggested_package("miniUI", "o assistente visual")

  project_path <- normalize_project_path(path)

  ui <- miniUI::miniPage(
    miniUI::gadgetTitleBar("git4stats: configurar Git neste projeto"),
    shiny::tags$head(
      shiny::tags$style(shiny::HTML(
        ".wizard-step-ok, .wizard-step-warn {padding: 8px 10px; border-radius: 6px; margin-bottom: 8px;}
         .wizard-step-ok {background: #eef8ef; border: 1px solid #bfdcbf;}
         .wizard-step-warn {background: #fff7e6; border: 1px solid #f0d59b;}
         .wizard-overview {margin-bottom: 12px;}"
      ))
    ),
    miniUI::miniContentPanel(
      shiny::fillCol(
        flex = c(1, 1, 1, 1, 1, 1, 1, 1),
        shiny::uiOutput("wizard_overview"),
        shiny::div(
          shiny::h4("1. Diagn\u00f3stico"),
          shiny::p("Veja se Git j\u00e1 est\u00e1 pronto para uso neste projeto."),
          shiny::uiOutput("diagnose_button"),
          shiny::verbatimTextOutput("diagnosis")
        ),
        shiny::div(
          shiny::h4("2. Inicializar Git"),
          shiny::p("Transforma esta pasta em um reposit\u00f3rio local com branch principal main."),
          shiny::uiOutput("init_button"),
          shiny::verbatimTextOutput("init_result")
        ),
        shiny::div(
          shiny::h4("3. Criar .gitignore"),
          shiny::p("Evita versionar arquivos tempor\u00e1rios do RStudio e sa\u00eddas geradas automaticamente."),
          shiny::checkboxInput("ignore_data", "Ignorar data/raw/ e data/processed/", value = FALSE),
          shiny::uiOutput("gitignore_button"),
          shiny::verbatimTextOutput("gitignore_result")
        ),
        shiny::div(
          shiny::h4("4. Criar estrutura do projeto"),
          shiny::p("Organiza o projeto em pastas comuns para an\u00e1lise estat\u00edstica."),
          shiny::checkboxInput("template_data", "Versionar a pasta data/", value = TRUE),
          shiny::uiOutput("template_button"),
          shiny::verbatimTextOutput("template_result")
        ),
        shiny::div(
          shiny::h4("5. Primeiro commit"),
          shiny::p("Commit = salvar uma vers\u00e3o do projeto no hist\u00f3rico local."),
          shiny::textInput("commit_message", "Mensagem do commit", value = "Primeiro commit"),
          shiny::uiOutput("commit_button"),
          shiny::verbatimTextOutput("commit_result")
        ),
        shiny::div(
          shiny::h4("6. GitHub"),
          shiny::p("Aqui voc\u00ea liga o projeto local a um remote e testa o primeiro envio."),
          shiny::textInput("remote_url", "URL do reposit\u00f3rio GitHub", value = ""),
          shiny::checkboxInput("replace_remote", "Trocar a URL se o remote j\u00e1 existir", value = FALSE),
          shiny::uiOutput("connect_button"),
          shiny::uiOutput("auth_button"),
          shiny::uiOutput("push_button"),
          shiny::verbatimTextOutput("github_result")
        ),
        shiny::div(
          shiny::h4("7. Pr\u00f3ximos passos"),
          shiny::verbatimTextOutput("next_steps")
        )
      )
    )
  )

  shiny::runGadget(
    ui,
    server = git_setup_wizard_server(project_path),
    viewer = shiny::dialogViewer("git4stats")
  )
  invisible(project_path)
}

git_setup_wizard_server <- function(project_path) {
  force(project_path)

  function(input, output, session) {
    initial_diagnosis <- build_git_diagnosis(project_path)
    diagnosis_state <- shiny::reactiveVal(initial_diagnosis)
    values <- shiny::reactiveValues(
      diagnosis = paste(diagnosis_lines(initial_diagnosis), collapse = "\n"),
      init_result = "",
      gitignore_result = "",
      template_result = "",
      commit_result = "",
      github_result = ""
    )

    capture_lines <- function(expr) {
      paste(utils::capture.output(force(expr)), collapse = "\n")
    }

    refresh_diagnosis <- function() {
      current <- build_git_diagnosis(project_path)
      diagnosis_state(current)
      values$diagnosis <- paste(diagnosis_lines(current), collapse = "\n")
    }

    diagnosis_ready <- shiny::reactive({
      diagnosis_state()
    })

    output$wizard_overview <- shiny::renderUI({
      d <- diagnosis_ready()
      shiny::div(
        class = "wizard-overview",
        wizard_step_note(
          "Estado atual",
          next_step_message(d),
          ok = d$git_installed && d$has_repo
        ),
        wizard_step_note(
          "Commit",
          if (d$has_commits) {
            glue::glue("Branch atual: {d$branch %||% 'sem branch ativa'}.")
          } else {
            "Ainda falta criar o primeiro commit."
          },
          ok = d$has_commits
        ),
        wizard_step_note(
          "GitHub",
          if (d$has_remote) {
            paste("Remote atual:", d$remote_name, "->", d$remote_url)
          } else {
            "Ainda n\u00e3o existe remote configurado."
          },
          ok = d$has_remote
        )
      )
    })

    output$diagnose_button <- shiny::renderUI({
      wizard_action_button("diagnose", "Atualizar diagn\u00f3stico", enabled = TRUE)
    })

    output$init_button <- shiny::renderUI({
      d <- diagnosis_ready()
      wizard_action_button("init_repo", "Inicializar Git", enabled = d$git_installed && !d$has_repo)
    })

    output$gitignore_button <- shiny::renderUI({
      wizard_action_button("write_gitignore", "Criar ou atualizar .gitignore", enabled = TRUE)
    })

    output$template_button <- shiny::renderUI({
      wizard_action_button("write_template", "Criar estrutura", enabled = TRUE)
    })

    output$commit_button <- shiny::renderUI({
      d <- diagnosis_ready()
      wizard_action_button(
        "run_commit",
        "Fazer primeiro commit",
        enabled = d$has_repo && isTRUE(d$identity$complete) && d$status_counts$total > 0
      )
    })

    output$connect_button <- shiny::renderUI({
      d <- diagnosis_ready()
      wizard_action_button("connect_remote", "Conectar remote GitHub", enabled = d$has_repo)
    })

    output$auth_button <- shiny::renderUI({
      d <- diagnosis_ready()
      wizard_action_button("check_remote_auth", "Testar acesso ao GitHub", enabled = d$has_remote)
    })

    output$push_button <- shiny::renderUI({
      d <- diagnosis_ready()
      wizard_action_button(
        "run_push",
        "Enviar commits ao GitHub",
        enabled = d$has_remote && d$has_commits && !is.null(d$branch)
      )
    })

    shiny::observeEvent(input$diagnose, {
      refresh_diagnosis()
    }, ignoreInit = FALSE)

    shiny::observeEvent(input$init_repo, {
      values$init_result <- capture_lines(init_git_project(project_path))
      refresh_diagnosis()
    })

    shiny::observeEvent(input$write_gitignore, {
      values$gitignore_result <- capture_lines(
        create_r_gitignore(project_path, include_data = !isTRUE(input$ignore_data))
      )
    })

    shiny::observeEvent(input$write_template, {
      values$template_result <- capture_lines(
        use_stats_project(project_path, include_data = isTRUE(input$template_data))
      )
    })

    shiny::observeEvent(input$run_commit, {
      values$commit_result <- capture_lines(
        first_commit(message = input$commit_message, path = project_path)
      )
      refresh_diagnosis()
    })

    shiny::observeEvent(input$connect_remote, {
      values$github_result <- capture_lines(
        connect_github_repo(
          remote_url = input$remote_url,
          path = project_path,
          replace = isTRUE(input$replace_remote)
        )
      )
      refresh_diagnosis()
    })

    shiny::observeEvent(input$check_remote_auth, {
      values$github_result <- capture_lines(
        check_github_auth(path = project_path)
      )
      refresh_diagnosis()
    })

    shiny::observeEvent(input$run_push, {
      values$github_result <- capture_lines(
        push_first_time(path = project_path)
      )
      refresh_diagnosis()
    })

    output$diagnosis <- shiny::renderText(values$diagnosis)
    output$init_result <- shiny::renderText(values$init_result)
    output$gitignore_result <- shiny::renderText(values$gitignore_result)
    output$template_result <- shiny::renderText(values$template_result)
    output$commit_result <- shiny::renderText(values$commit_result)
    output$github_result <- shiny::renderText(values$github_result)
    output$next_steps <- shiny::renderText({
      d <- diagnosis_ready()
      paste(
        next_step_message(d),
        "",
        "Commit = salvar uma vers\u00e3o do projeto.",
        "Push = enviar commits para o GitHub.",
        "Pull = baixar mudan\u00e7as feitas por colegas.",
        sep = "\n"
      )
    })

    shiny::observeEvent(input$done, {
      shiny::stopApp(project_path)
    })

    shiny::observeEvent(input$cancel, {
      shiny::stopApp(invisible(NULL))
    })
  }
}
