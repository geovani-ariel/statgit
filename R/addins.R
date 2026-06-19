#' Addin para verificar Git no projeto atual
#'
#' @export
addin_check_git <- function() {
  git_check(active_project_path())
}

#' Addin para abrir o painel principal do statgit
#'
#' @export
addin_statgit <- function() {
  statgit(active_project_path())
}

#' Addin para abrir o wizard de configuracao Git
#'
#' @export
addin_git_setup_wizard <- function() {
  git_wizard(active_project_path())
}

#' Addin para criar o primeiro commit
#'
#' @export
addin_first_commit <- function() {
  tr <- trackr_tr()
  default_message <- tr("git.first_commit.default")

  message <- default_message
  if (rstudioapi::isAvailable()) {
    prompt_result <- tryCatch(
      rstudioapi::showPrompt(
        title = tr("git.first_commit.default"),
        message = tr("changes.commit_message"),
        default = default_message
      ),
      error = function(e) default_message
    )

    message <- prompt_result %||% default_message
  }

  git_commit_all(message = message, path = active_project_path())
}

#' Addin para ver o status Git do projeto atual
#'
#' @export
addin_git_status <- function() {
  git_status(active_project_path())
}

#' Addin para pre-visualizar o knit do relatorio ativo
#'
#' @export
addin_knit_preview <- function() {
  report_preview()
}

#' Addin para live preview do relatorio ativo
#'
#' @export
addin_live_preview <- function() {
  report_live_preview()
}

#' Addin para formatar o arquivo ativo
#'
#' @export
addin_format_active_file <- function() {
  code_format()
}

#' Addin para formatar os arquivos do projeto atual
#'
#' @export
addin_format_project <- function() {
  code_format_all(active_project_path())
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
git_wizard <- function(path = ".") {
  ensure_suggested_package("shiny", "o assistente visual")
  ensure_suggested_package("miniUI", "o assistente visual")

  project_path <- normalize_project_path(path)

  ui <- miniUI::miniPage(
    miniUI::gadgetTitleBar("statgit"),
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
        flex = c(0, 1),
        shiny::selectInput("trackr_language", NULL, choices = trackr_language_choices(), selected = "en", width = "100%"),
        shiny::uiOutput("wizard_content")
      )
    )
  )

  shiny::runGadget(
    ui,
    server = git_wizard_server(project_path),
    viewer = shiny::dialogViewer("statgit")
  )
  invisible(project_path)
}

git_wizard_server <- function(project_path) {
  force(project_path)

  function(input, output, session) {
    language <- shiny::reactive(input$trackr_language %||% "en")
    tr <- trackr_tr(language)
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

    output$wizard_content <- shiny::renderUI({
      shiny::fillCol(
        flex = c(1, 1, 1, 1, 1, 1, 1, 1),
        shiny::uiOutput("wizard_overview"),
        shiny::div(
          shiny::h4(tr("wizard.step.diagnosis")),
          shiny::p(tr("wizard.step.diagnosis_help")),
          shiny::uiOutput("diagnose_button"),
          shiny::verbatimTextOutput("diagnosis")
        ),
        shiny::div(
          shiny::h4(tr("wizard.step.init")),
          shiny::p(tr("wizard.step.init_help")),
          shiny::uiOutput("init_button"),
          shiny::verbatimTextOutput("init_result")
        ),
        shiny::div(
          shiny::h4(tr("wizard.step.gitignore")),
          shiny::p(tr("wizard.step.gitignore_help")),
          shiny::checkboxInput("ignore_data", tr("git.ignore_data"), value = FALSE),
          shiny::uiOutput("gitignore_button"),
          shiny::verbatimTextOutput("gitignore_result")
        ),
        shiny::div(
          shiny::h4(tr("wizard.step.structure")),
          shiny::p(tr("wizard.step.structure_help")),
          shiny::checkboxInput("template_data", tr("project.include_data"), value = TRUE),
          shiny::uiOutput("template_button"),
          shiny::verbatimTextOutput("template_result")
        ),
        shiny::div(
          shiny::h4(tr("wizard.step.commit")),
          shiny::p(tr("wizard.step.commit_help")),
          shiny::textInput("commit_message", tr("git.commit_label"), value = tr("git.first_commit.default")),
          shiny::uiOutput("commit_button"),
          shiny::verbatimTextOutput("commit_result")
        ),
        shiny::div(
          shiny::h4("6. GitHub"),
          shiny::p(tr("wizard.step.github_help")),
          shiny::textInput("remote_name", "Nome do remote", value = "origin"),
          shiny::textInput("remote_url", tr("git.remote_url"), value = ""),
          shiny::checkboxInput("replace_remote", tr("git.replace_remote"), value = FALSE),
          shiny::uiOutput("connect_button"),
          shiny::uiOutput("disconnect_button"),
          shiny::uiOutput("open_repo_button"),
          shiny::uiOutput("fetch_button"),
          shiny::uiOutput("auth_button"),
          shiny::uiOutput("push_button"),
          shiny::verbatimTextOutput("github_result")
        ),
        shiny::div(
          shiny::h4(tr("wizard.step.next")),
          shiny::verbatimTextOutput("next_steps")
        )
      )
    })

    output$wizard_overview <- shiny::renderUI({
      d <- diagnosis_ready()
      shiny::div(
        class = "wizard-overview",
        wizard_step_note(
          tr("wizard.current_state"),
          next_step_message(d),
          ok = d$git_installed && d$has_repo
        ),
        wizard_step_note(
          "Commit",
          if (d$has_commits) {
            tr("wizard.current_branch", d$branch %||% tr("common.current_branch_missing"))
          } else {
            tr("wizard.first_commit_missing")
          },
          ok = d$has_commits
        ),
        wizard_step_note(
          "GitHub",
          if (d$has_remote) {
            paste(tr("wizard.current_remote"), d$remote_name, sprintf("(%s)", remote_protocol_label(d$remote_url)), "->", d$remote_url)
          } else {
            tr("wizard.no_remote")
          },
          ok = d$has_remote
        ),
        wizard_step_note(
          "Sincronização",
          wizard_sync_note(d),
          ok = isTRUE(d$has_remote) && !isTRUE(d$sync_status$behind > 0L)
        )
      )
    })

    output$diagnose_button <- shiny::renderUI({
      wizard_action_button("diagnose", tr("wizard.refresh"), enabled = TRUE)
    })

    output$init_button <- shiny::renderUI({
      d <- diagnosis_ready()
      wizard_action_button("init_repo", tr("git.init"), enabled = d$git_installed && !d$has_repo)
    })

    output$gitignore_button <- shiny::renderUI({
      wizard_action_button("write_gitignore", tr("git.gitignore"), enabled = TRUE)
    })

    output$template_button <- shiny::renderUI({
      wizard_action_button("write_template", tr("wizard.create_structure"), enabled = TRUE)
    })

    output$commit_button <- shiny::renderUI({
      d <- diagnosis_ready()
      wizard_action_button(
        "run_commit",
        tr("wizard.first_commit"),
        enabled = d$has_repo && isTRUE(d$identity$complete) && d$status_counts$total > 0
      )
    })

    output$connect_button <- shiny::renderUI({
      d <- diagnosis_ready()
      wizard_action_button("connect_remote", tr("wizard.connect_remote"), enabled = d$has_repo)
    })

    output$disconnect_button <- shiny::renderUI({
      d <- diagnosis_ready()
      if (!d$has_remote) {
        return(NULL)
      }

      shiny::actionButton("disconnect_remote", "Desconectar remote", class = "btn-default")
    })

    output$auth_button <- shiny::renderUI({
      d <- diagnosis_ready()
      wizard_action_button("check_remote_auth", tr("wizard.test_github"), enabled = d$has_remote)
    })

    output$open_repo_button <- shiny::renderUI({
      d <- diagnosis_ready()
      if (!d$has_remote) {
        return(NULL)
      }

      shiny::actionButton("open_remote_repo", "Abrir repositório", class = "btn-default")
    })

    output$fetch_button <- shiny::renderUI({
      d <- diagnosis_ready()
      if (!d$has_remote) {
        return(NULL)
      }

      shiny::actionButton("fetch_remote", "Fetch", class = "btn-default")
    })

    output$push_button <- shiny::renderUI({
      d <- diagnosis_ready()
      wizard_action_button(
        "run_push",
        tr("wizard.push"),
        enabled = d$has_remote && d$has_commits && !is.null(d$branch) && !isTRUE(d$sync_status$behind > 0L)
      )
    })

    shiny::observeEvent(input$diagnose, {
      refresh_diagnosis()
    }, ignoreInit = FALSE)

    shiny::observeEvent(input$init_repo, {
      values$init_result <- capture_lines(git_init(project_path))
      refresh_diagnosis()
    })

    shiny::observeEvent(input$write_gitignore, {
      values$gitignore_result <- capture_lines(
        git_ignore(project_path, include_data = !isTRUE(input$ignore_data))
      )
    })

    shiny::observeEvent(input$write_template, {
      values$template_result <- capture_lines(
        project_organize(project_path, include_data = isTRUE(input$template_data))
      )
    })

    shiny::observeEvent(input$run_commit, {
      values$commit_result <- capture_lines(
        git_commit_all(message = input$commit_message, path = project_path)
      )
      refresh_diagnosis()
    })

    shiny::observeEvent(input$connect_remote, {
      values$github_result <- capture_lines(
        github_connect(
          remote_url = input$remote_url,
          path = project_path,
          remote = normalize_remote_name(input$remote_name),
          replace = isTRUE(input$replace_remote)
        )
      )
      refresh_diagnosis()
    })

    shiny::observeEvent(input$disconnect_remote, {
      shiny::showModal(
        shiny::modalDialog(
          title = "Desconectar remote?",
          shiny::p(sprintf(
            "Voc\u00EA quer remover a conex\u00E3o com o remote '%s' deste projeto?",
            normalize_remote_name(input$remote_name, default = diagnosis_ready()$remote_name %||% "origin")
          )),
          shiny::p("Isso remove apenas a liga\u00E7\u00E3o com o GitHub. O hist\u00F3rico Git local continua intacto."),
          footer = shiny::tagList(
            shiny::modalButton("Cancelar"),
            shiny::actionButton("confirm_disconnect_remote", "Desconectar remote", class = "btn-danger")
          )
        )
      )
    })

    shiny::observeEvent(input$confirm_disconnect_remote, {
      shiny::removeModal()
      values$github_result <- capture_lines(
        github_disconnect(
          path = project_path,
          remote = normalize_remote_name(input$remote_name, default = diagnosis_ready()$remote_name %||% "origin")
        )
      )
      refresh_diagnosis()
    })

    shiny::observeEvent(input$check_remote_auth, {
      values$github_result <- capture_lines(
        github_check(path = project_path, remote = normalize_remote_name(input$remote_name))
      )
      refresh_diagnosis()
    })

    shiny::observeEvent(input$open_remote_repo, {
      values$github_result <- capture_lines(
        github_open_repo(path = project_path, remote = normalize_remote_name(input$remote_name))
      )
    })

    shiny::observeEvent(input$fetch_remote, {
      values$github_result <- capture_lines(
        git_fetch(path = project_path, remote = normalize_remote_name(input$remote_name))
      )
      refresh_diagnosis()
    })

    shiny::observeEvent(input$run_push, {
      values$github_result <- capture_lines(
        git_push(path = project_path, remote = normalize_remote_name(input$remote_name))
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
        tr("wizard.next_commit"),
        tr("wizard.next_push"),
        tr("wizard.next_pull"),
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

wizard_sync_note <- function(diagnosis) {
  sync_status <- diagnosis$sync_status

  if (!isTRUE(diagnosis$has_remote)) {
    return("Remote ainda n\u00E3o configurado.")
  }
  if (!isTRUE(diagnosis$has_commits)) {
    return("Crie pelo menos um commit antes de sincronizar.")
  }
  if (is.null(diagnosis$branch)) {
    return("A branch atual ainda n\u00E3o foi identificada.")
  }
  if (!isTRUE(sync_status$remote_branch_exists)) {
    return("Primeiro push publicar\u00E1 esta branch no remote.")
  }
  if (isTRUE(sync_status$behind > 0L)) {
    return(sprintf("Sua branch local est\u00E1 %d commit(s) atr\u00E1s do remote. Fa\u00E7a Pull antes do Push.", sync_status$behind))
  }
  if (isTRUE(sync_status$ahead > 0L)) {
    return(sprintf("Sua branch local tem %d commit(s) pendente(s) para enviar.", sync_status$ahead))
  }

  "Branch local sincronizada com o remote."
}
