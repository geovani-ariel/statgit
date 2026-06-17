#' Abre o painel principal do git4stats
#'
#' Centraliza as acoes do pacote em uma interface Shiny com navegacao por
#' modulos.
#'
#' @param path Caminho do projeto.
#'
#' @return Invisivelmente, o caminho analisado.
#' @export
git4stats_panel <- function(path = active_project_path()) {
  ensure_suggested_package("shiny", "o painel principal")
  ensure_suggested_package("miniUI", "o painel principal")

  project_path <- normalize_project_path(path)

  shiny::runGadget(
    git4stats_panel_ui(project_path),
    server = git4stats_panel_server(project_path),
    viewer = shiny::dialogViewer("git4stats", width = 1100, height = 760)
  )

  invisible(project_path)
}

git4stats_panel_ui <- function(project_path) {
  miniUI::miniPage(
    miniUI::gadgetTitleBar("git4stats"),
    shiny::tags$head(
      shiny::tags$style(shiny::HTML(git4stats_panel_css()))
    ),
    miniUI::miniContentPanel(
      shiny::div(
        class = "g4s-shell",
        shiny::div(
          class = "g4s-sidebar",
          shiny::radioButtons(
            "module",
            label = NULL,
            choices = c(
              "Comecar" = "project",
              "Arquivos" = "files",
              "Salvar versoes" = "git",
              "Mudancas" = "changes",
              "GitHub" = "github",
              "Relatorios" = "reports",
              "Organizar codigo" = "format"
            ),
            selected = "project"
          )
        ),
        shiny::div(
          class = "g4s-main",
          shiny::uiOutput("project_summary"),
          shiny::uiOutput("module_ui")
        ),
        shiny::div(
          class = "g4s-log",
          shiny::h4("Resultado"),
          shiny::verbatimTextOutput("action_log"),
          shiny::actionButton("refresh_all", "Atualizar estado")
        )
      )
    )
  )
}

git4stats_panel_server <- function(project_path) {
  force(project_path)

  function(input, output, session) {
    initial_diagnosis <- build_git_diagnosis(project_path)
    diagnosis_state <- shiny::reactiveVal(initial_diagnosis)
    values <- shiny::reactiveValues(
      log = paste(diagnosis_lines(initial_diagnosis), collapse = "\n"),
      diff_html = format_diff_for_panel_html(character()),
      project_choices = named_project_choices(default_projects_directory())
    )

    capture_lines <- function(expr) {
      paste(utils::capture.output(force(expr)), collapse = "\n")
    }

    refresh_diagnosis <- function() {
      current <- build_git_diagnosis(project_path)
      diagnosis_state(current)
      current
    }

    set_log <- function(text) {
      values$log <- text
    }

    update_changed_file_inputs <- function(diagnosis) {
      files <- changed_file_choices(diagnosis$status)
      selected <- if (length(files) > 0) unname(files[[1]]) else character()

      shiny::updateSelectInput(session, "changes_file", choices = files, selected = selected)
      shiny::updateSelectInput(session, "changes_files", choices = files, selected = character())
      shiny::updateSelectInput(session, "git_commit_files", choices = files, selected = character())
    }

    refresh_panel_state <- function() {
      diagnosis <- refresh_diagnosis()
      update_changed_file_inputs(diagnosis)
      diagnosis
    }

    run_panel_action <- function(expr, refresh = TRUE) {
      result <- capture_lines(expr)
      set_log(result)
      if (isTRUE(refresh)) {
        refresh_panel_state()
      }
      invisible(result)
    }

    d <- shiny::reactive(diagnosis_state())

    output$project_summary <- shiny::renderUI({
      diagnosis <- d()
      stats <- panel_summary_items(diagnosis)

      shiny::div(
        class = "g4s-summary",
        shiny::div(
          class = "g4s-summary-main",
          shiny::strong("Projeto atual"),
          shiny::div(class = "g4s-path", diagnosis$current_path)
        ),
        shiny::div(
          class = "g4s-summary-grid",
          lapply(stats, function(item) {
            shiny::div(
              class = paste("g4s-pill", item$class),
              shiny::span(class = "g4s-pill-label", item$label),
              shiny::span(class = "g4s-pill-value", item$value)
            )
          })
        )
      )
    })

    output$module_ui <- shiny::renderUI({
      switch(
        input$module %||% "project",
        project = project_module_ui(),
        files = files_module_ui(),
        git = git_module_ui(d()),
        changes = changes_module_ui(d()),
        github = github_module_ui(d()),
        reports = reports_module_ui(),
        format = format_module_ui(),
        project_module_ui()
      )
    })

    output$action_log <- shiny::renderText(values$log)

    output$diff_view <- shiny::renderUI({
      shiny::HTML(values$diff_html)
    })

    output$changes_summary <- shiny::renderText({
      diagnosis <- d()
      counts <- diagnosis$status_counts

      paste(
        "Arquivos novos:", counts$new,
        "Arquivos modificados:", counts$modified,
        "Arquivos removidos:", counts$deleted,
        "Preparados para commit:", counts$staged,
        sep = "\n"
      )
    })

    shiny::observeEvent(input$refresh_all, {
      diagnosis <- refresh_panel_state()
      set_log(paste(diagnosis_lines(diagnosis), collapse = "\n"))
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$project_refresh, {
      values$project_choices <- named_project_choices(input$project_search_root)
      set_log("Lista de projetos atualizada.")
    }, ignoreInit = TRUE)

    shiny::observe({
      choices <- values$project_choices
      shiny::updateSelectInput(
        session,
        "project_choice",
        choices = choices,
        selected = if (length(choices) > 0) unname(choices[[1]]) else character()
      )
    })

    shiny::observeEvent(input$project_create, {
      project_name <- trimws(input$project_name %||% "")
      if (!nzchar(project_name)) {
        set_log("Informe um nome para o projeto.")
        return()
      }

      target_path <- fs::path(normalize_project_path(input$project_base_dir), project_name)
      extra_files <- split_extra_file_lines(input$project_extra_files)

      run_panel_action(
        create_stats_project(
          path = target_path,
          template = input$project_template,
          include_data = isTRUE(input$project_include_data),
          initialize_git = isTRUE(input$project_initialize_git),
          open = isTRUE(input$project_open_after_create),
          extra_files = extra_files
        ),
        refresh = FALSE
      )

      values$project_choices <- named_project_choices(input$project_search_root)
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$project_open, {
      selected <- input$project_choice %||% ""
      if (!nzchar(selected)) {
        set_log("Nenhum projeto foi selecionado.")
        return()
      }

      run_panel_action(open_stats_project(selected), refresh = FALSE)
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$project_structure, {
      run_panel_action(
        use_stats_project(
          path = project_path,
          include_data = isTRUE(input$project_structure_include_data),
          template = input$project_structure_template
        )
      )
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$file_choose_source, {
      selected <- choose_project_file()
      if (!is.null(selected) && nzchar(selected)) {
        shiny::updateTextInput(session, "file_source", value = selected)
      }
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$file_import, {
      run_panel_action(
        import_project_file(
          source = input$file_source,
          destination = input$file_destination,
          path = project_path,
          move = isTRUE(input$file_move),
          add_to_git = isTRUE(input$file_add_to_git),
          overwrite = isTRUE(input$file_overwrite)
        )
      )
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$git_diagnose, {
      diagnosis <- refresh_panel_state()
      set_log(paste(diagnosis_lines(diagnosis), collapse = "\n"))
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$git_init, {
      run_panel_action(init_git_project(project_path))
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$gitignore_write, {
      run_panel_action(
        create_r_gitignore(
          project_path,
          include_data = !isTRUE(input$gitignore_ignore_data)
        )
      )
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$git_status, {
      run_panel_action(git_status_pretty(project_path))
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$git_commit, {
      run_panel_action(
        first_commit(
          message = input$commit_message %||% "Primeiro commit",
          path = project_path
        )
      )
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$changes_refresh, {
      diagnosis <- refresh_panel_state()
      values$diff_html <- format_diff_for_panel_html(character())
      set_log("Lista de mudancas atualizada.")
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$changes_diff, {
      result <- git_diff_file(
        file = input$changes_file,
        path = project_path,
        staged = isTRUE(input$changes_staged),
        context = input$changes_diff_context %||% "changes"
      )
      diff_lines <- result$diff %||% character()
      values$diff_html <- format_diff_for_panel_html(diff_lines)
      set_log(format_diff_for_panel(diff_lines))
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$changes_stage, {
      run_panel_action(stage_files(input$changes_files, path = project_path))
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$changes_unstage, {
      run_panel_action(unstage_files(input$changes_files, path = project_path))
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$changes_discard, {
      if (!isTRUE(input$changes_confirm_discard)) {
        set_log("Marque a confirmacao antes de descartar mudancas locais.")
        return()
      }

      run_panel_action(discard_file_changes(input$changes_files, path = project_path))
      shiny::updateCheckboxInput(session, "changes_confirm_discard", value = FALSE)
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$changes_commit_selected, {
      selected <- normalize_git_file_selection(input$changes_files)
      if (length(selected) > 0) {
        stage_output <- capture_lines(stage_files(selected, path = project_path))
        commit_output <- capture_lines(commit_staged_files(input$changes_commit_message, path = project_path))
        set_log(paste(c(stage_output, commit_output), collapse = "\n"))
        refresh_panel_state()
      } else {
        run_panel_action(commit_staged_files(input$changes_commit_message, path = project_path))
      }
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$git_stage_commit_files, {
      run_panel_action(stage_files(input$git_commit_files, path = project_path))
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$git_commit_staged, {
      run_panel_action(commit_staged_files(input$guided_commit_message, path = project_path))
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$github_connect, {
      run_panel_action(
        connect_github_repo(
          remote_url = input$github_remote_url,
          path = project_path,
          replace = isTRUE(input$github_replace_remote)
        )
      )
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$github_auth, {
      run_panel_action(check_github_auth(path = project_path))
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$github_push, {
      run_panel_action(push_first_time(path = project_path))
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$report_preview, {
      run_panel_action(
        preview_knit(
          path = panel_optional_path(input$report_path),
          style = isTRUE(input$report_style)
        ),
        refresh = FALSE
      )
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$report_live_preview, {
      run_panel_action(
        live_preview_knit(
          path = panel_optional_path(input$report_path),
          style = isTRUE(input$report_style)
        ),
        refresh = FALSE
      )
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$format_active, {
      run_panel_action(
        format_active_file(panel_optional_path(input$format_path)),
        refresh = FALSE
      )
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$format_project, {
      run_panel_action(format_project_files(project_path), refresh = FALSE)
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$done, {
      shiny::stopApp(project_path)
    })

    shiny::observeEvent(input$cancel, {
      shiny::stopApp(invisible(NULL))
    })
  }
}

project_module_ui <- function() {
  shiny::tagList(
    panel_section(
      "Criar projeto organizado",
      shiny::textInput("project_base_dir", "Pasta base", value = default_projects_directory()),
      shiny::textInput("project_name", "Nome do projeto", value = "meu-projeto"),
      shiny::selectInput("project_template", "Template", choices = project_template_choices()),
      shiny::checkboxInput("project_include_data", "Versionar a pasta data/", value = TRUE),
      shiny::checkboxInput("project_initialize_git", "Inicializar Git", value = TRUE),
      shiny::checkboxInput("project_open_after_create", "Abrir projeto ao criar", value = TRUE),
      shiny::textAreaInput(
        "project_extra_files",
        "Arquivos extras (um por linha)",
        value = "scripts/03-figuras.R\nreports/apresentacao.qmd",
        rows = 4
      ),
      shiny::actionButton("project_create", "Criar projeto")
    ),
    panel_section(
      "Abrir projeto existente",
      shiny::textInput("project_search_root", "Buscar projetos em", value = default_projects_directory()),
      shiny::actionButton("project_refresh", "Atualizar lista"),
      shiny::selectInput("project_choice", "Projetos encontrados", choices = character()),
      shiny::actionButton("project_open", "Abrir projeto selecionado")
    ),
    panel_section(
      "Organizar projeto atual",
      shiny::selectInput("project_structure_template", "Template", choices = project_template_choices()),
      shiny::checkboxInput("project_structure_include_data", "Versionar a pasta data/", value = TRUE),
      shiny::actionButton("project_structure", "Criar estrutura no projeto atual")
    )
  )
}

files_module_ui <- function() {
  shiny::tagList(
    panel_section(
      "Trazer arquivo para o projeto",
      shiny::textInput("file_source", "Arquivo baixado ou salvo fora do projeto", value = ""),
      shiny::actionButton("file_choose_source", "Escolher arquivo"),
      shiny::selectInput(
        "file_destination",
        "Pasta de destino",
        choices = c(
          "Dados originais" = "data/raw",
          "Dados tratados" = "data/processed",
          "Relatorios" = "reports",
          "Scripts" = "scripts",
          "Figuras" = "figs"
        ),
        selected = "data/raw"
      ),
      shiny::checkboxInput("file_move", "Mover em vez de copiar", value = FALSE),
      shiny::checkboxInput("file_overwrite", "Substituir se ja existir", value = FALSE),
      shiny::checkboxInput("file_add_to_git", "Preparar arquivo para a proxima versao", value = FALSE),
      shiny::actionButton("file_import", "Colocar no projeto")
    )
  )
}

git_module_ui <- function(diagnosis) {
  files <- changed_file_choices(diagnosis$status)

  shiny::tagList(
    panel_section(
      "Diagnostico",
      shiny::actionButton("git_diagnose", "Atualizar diagnostico"),
      shiny::actionButton("git_status", "Ver status")
    ),
    panel_section(
      "Configurar Git",
      panel_action_button("git_init", "Inicializar Git", enabled = diagnosis$git_installed && !diagnosis$has_repo),
      shiny::checkboxInput("gitignore_ignore_data", "Ignorar data/raw/ e data/processed/", value = FALSE),
      shiny::actionButton("gitignore_write", "Criar ou atualizar .gitignore")
    ),
    panel_section(
      "Commit rapido",
      shiny::textInput("commit_message", "Mensagem do commit", value = "Primeiro commit"),
      panel_action_button(
        "git_commit",
        "Preparar tudo e fazer commit",
        enabled = diagnosis$has_repo && isTRUE(diagnosis$identity$complete) &&
          diagnosis$status_counts$total > 0
      )
    ),
    panel_section(
      "Salvar versao guiada",
      shiny::selectInput("git_commit_files", "Arquivos para preparar", choices = files, multiple = TRUE),
      shiny::actionButton("git_stage_commit_files", "Preparar selecionados"),
      shiny::textInput("guided_commit_message", "Mensagem da versao", value = "Atualiza projeto"),
      panel_action_button(
        "git_commit_staged",
        "Salvar arquivos preparados",
        enabled = diagnosis$has_repo && isTRUE(diagnosis$identity$complete) &&
          diagnosis$status_counts$staged > 0
      )
    )
  )
}

changes_module_ui <- function(diagnosis) {
  files <- changed_file_choices(diagnosis$status)

  shiny::tagList(
    panel_section(
      "Arquivos com mudancas",
      shiny::actionButton("changes_refresh", "Atualizar lista"),
      shiny::selectInput("changes_file", "Arquivo", choices = files),
      shiny::checkboxInput("changes_staged", "Ver apenas mudancas ja preparadas", value = FALSE),
      shiny::selectInput(
        "changes_diff_context",
        "Modo do diff",
        choices = c("So trechos alterados" = "changes", "Arquivo com mais contexto" = "full"),
        selected = "changes"
      ),
      shiny::actionButton("changes_diff", "Ver mudancas")
    ),
    panel_section(
      "Preparar e salvar versao",
      shiny::selectInput("changes_files", "Arquivos", choices = files, multiple = TRUE),
      shiny::actionButton("changes_stage", "Preparar selecionados"),
      shiny::actionButton("changes_unstage", "Remover da preparacao"),
      shiny::textInput("changes_commit_message", "Mensagem da versao", value = "Atualiza projeto"),
      shiny::actionButton("changes_commit_selected", "Salvar selecionados")
    ),
    panel_section(
      "Descartar com cuidado",
      shiny::checkboxInput(
        "changes_confirm_discard",
        "Entendo que isso apaga mudancas locais dos arquivos selecionados",
        value = FALSE
      ),
      shiny::actionButton("changes_discard", "Descartar mudancas selecionadas")
    ),
    panel_section(
      "Resumo",
      shiny::verbatimTextOutput("changes_summary")
    ),
    panel_section(
      "Diff visual",
      shiny::uiOutput("diff_view")
    )
  )
}

github_module_ui <- function(diagnosis) {
  shiny::tagList(
    panel_section(
      "Remote GitHub",
      shiny::textInput("github_remote_url", "URL do repositorio GitHub", value = ""),
      shiny::checkboxInput("github_replace_remote", "Trocar a URL se o remote ja existir", value = FALSE),
      panel_action_button("github_connect", "Conectar remote", enabled = diagnosis$has_repo)
    ),
    panel_section(
      "Validar e enviar",
      panel_action_button("github_auth", "Testar acesso", enabled = diagnosis$has_remote),
      panel_action_button(
        "github_push",
        "Enviar commits",
        enabled = diagnosis$has_remote && diagnosis$has_commits && !is.null(diagnosis$branch)
      )
    )
  )
}

reports_module_ui <- function() {
  panel_section(
    "Relatorios",
    shiny::textInput("report_path", "Arquivo de relatorio", value = ""),
    shiny::checkboxInput("report_style", "Formatar antes do preview", value = FALSE),
    shiny::actionButton("report_preview", "Preview"),
    shiny::actionButton("report_live_preview", "Live preview")
  )
}

format_module_ui <- function() {
  panel_section(
    "Formatacao",
    shiny::textInput("format_path", "Arquivo para formatar", value = ""),
    shiny::actionButton("format_active", "Formatar arquivo"),
    shiny::actionButton("format_project", "Formatar projeto atual")
  )
}

panel_section <- function(title, ...) {
  shiny::div(
    class = "g4s-section",
    shiny::h3(title),
    ...
  )
}

panel_action_button <- function(id, label, enabled = TRUE) {
  if (isTRUE(enabled)) {
    return(shiny::actionButton(id, label))
  }

  shiny::tags$button(
    id = id,
    type = "button",
    class = "btn btn-default action-button",
    disabled = "disabled",
    label
  )
}

panel_optional_path <- function(path) {
  path <- trimws(path %||% "")
  if (nzchar(path)) path else NULL
}

panel_summary_items <- function(diagnosis) {
  list(
    list(
      label = ".Rproj",
      value = if (diagnosis$is_rstudio_project) basename(diagnosis$rproj_path) else "ausente",
      class = if (diagnosis$is_rstudio_project) "ok" else "warn"
    ),
    list(
      label = "Git",
      value = if (diagnosis$has_repo) "ativo" else "nao iniciado",
      class = if (diagnosis$has_repo) "ok" else "warn"
    ),
    list(
      label = "Commits",
      value = if (diagnosis$has_commits) "sim" else "nao",
      class = if (diagnosis$has_commits) "ok" else "warn"
    ),
    list(
      label = "Remote",
      value = if (diagnosis$has_remote) diagnosis$remote_name else "ausente",
      class = if (diagnosis$has_remote) "ok" else "warn"
    ),
    list(
      label = "Pendencias",
      value = as.character(diagnosis$status_counts$total),
      class = if (diagnosis$status_counts$total == 0) "ok" else "warn"
    )
  )
}

changed_file_choices <- function(status_tbl) {
  if (nrow(status_tbl) == 0) {
    return(stats::setNames(character(), character()))
  }

  files <- unique(status_tbl$file)
  labels <- vapply(files, function(file) {
    rows <- status_tbl[status_tbl$file == file, , drop = FALSE]
    states <- paste0(rows$status, ifelse(rows$staged, " preparado", ""))
    paste(file, paste0("(", paste(unique(states), collapse = ", "), ")"))
  }, character(1))

  stats::setNames(files, labels)
}

git4stats_panel_css <- function() {
  "
  .g4s-shell {
    display: grid;
    grid-template-columns: 190px minmax(360px, 1fr) 330px;
    gap: 14px;
    height: calc(100vh - 76px);
    min-height: 620px;
  }
  .g4s-sidebar, .g4s-main, .g4s-log {
    border: 1px solid #d8dee4;
    border-radius: 6px;
    background: #fff;
    padding: 12px;
    overflow: auto;
  }
  .g4s-sidebar .radio {
    margin: 0 0 8px 0;
    padding: 8px;
    border-radius: 6px;
  }
  .g4s-sidebar .radio:hover {
    background: #f6f8fa;
  }
  .g4s-summary {
    border: 1px solid #d8dee4;
    border-radius: 6px;
    padding: 12px;
    margin-bottom: 12px;
    background: #f6f8fa;
  }
  .g4s-path {
    color: #57606a;
    font-size: 12px;
    word-break: break-all;
    margin-top: 4px;
  }
  .g4s-summary-grid {
    display: grid;
    grid-template-columns: repeat(5, minmax(78px, 1fr));
    gap: 8px;
    margin-top: 10px;
  }
  .g4s-pill {
    border-radius: 6px;
    border: 1px solid #d8dee4;
    padding: 6px 8px;
    background: #fff;
  }
  .g4s-pill.ok {
    border-color: #b6d7bd;
    background: #eef8ef;
  }
  .g4s-pill.warn {
    border-color: #eac54f;
    background: #fff8c5;
  }
  .g4s-pill-label {
    display: block;
    font-size: 11px;
    color: #57606a;
  }
  .g4s-pill-value {
    display: block;
    font-weight: 600;
    word-break: break-word;
  }
  .g4s-section {
    border: 1px solid #d8dee4;
    border-radius: 6px;
    padding: 12px;
    margin-bottom: 12px;
  }
  .g4s-section h3 {
    font-size: 16px;
    margin: 0 0 12px 0;
  }
  .g4s-section .form-group {
    margin-bottom: 10px;
  }
  .g4s-log pre {
    min-height: 480px;
    white-space: pre-wrap;
    word-break: break-word;
  }
  .g4s-diff {
    border: 1px solid #d8dee4;
    border-radius: 6px;
    overflow: auto;
    background: #f6f8fa;
    font-size: 12px;
  }
  .g4s-diff-line {
    display: grid;
    grid-template-columns: 28px minmax(0, 1fr);
    min-height: 22px;
    border-bottom: 1px solid rgba(216, 222, 228, 0.65);
  }
  .g4s-diff-line:last-child {
    border-bottom: 0;
  }
  .g4s-diff-line code {
    display: block;
    padding: 3px 8px;
    color: #24292f;
    background: transparent;
    white-space: pre;
    font-family: Menlo, Monaco, Consolas, 'Liberation Mono', monospace;
  }
  .g4s-diff-marker {
    padding: 3px 0;
    text-align: center;
    color: #57606a;
    user-select: none;
    font-family: Menlo, Monaco, Consolas, 'Liberation Mono', monospace;
  }
  .g4s-diff-add {
    background: #dafbe1;
  }
  .g4s-diff-add .g4s-diff-marker {
    color: #1a7f37;
    background: #aceebb;
  }
  .g4s-diff-remove {
    background: #ffebe9;
  }
  .g4s-diff-remove .g4s-diff-marker {
    color: #cf222e;
    background: #ffcecb;
  }
  .g4s-diff-hunk {
    background: #ddf4ff;
  }
  .g4s-diff-hunk .g4s-diff-marker {
    color: #0969da;
    background: #b6e3ff;
  }
  .g4s-diff-meta {
    background: #f6f8fa;
  }
  .g4s-diff-meta code {
    color: #57606a;
    font-weight: 600;
  }
  .g4s-diff-context {
    background: #fff;
  }
  .g4s-diff-empty {
    border: 1px dashed #d8dee4;
    border-radius: 6px;
    padding: 14px;
    color: #57606a;
    background: #f6f8fa;
  }
  @media (max-width: 900px) {
    .g4s-shell {
      grid-template-columns: 1fr;
      height: auto;
    }
    .g4s-summary-grid {
      grid-template-columns: repeat(2, minmax(120px, 1fr));
    }
  }
  "
}
