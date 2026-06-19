#' Abre o painel principal do statgit
#'
#' Centraliza as acoes do pacote em uma interface Shiny com navegacao por
#' modulos.
#'
#' @param path Caminho do projeto.
#'
#' @return Invisivelmente, o caminho analisado.
#' @export
statgit <- function(path = active_project_path()) {
  ensure_suggested_package("shiny", "o painel principal")
  ensure_suggested_package("miniUI", "o painel principal")

  project_path <- normalize_project_path(path)
  initial_diagnosis <- build_git_diagnosis(project_path)
  
  default_module <- "overview"
  if (initial_diagnosis$is_rstudio_project) {
    if (!initial_diagnosis$has_repo) {
      default_module <- "overview"
    } else if (initial_diagnosis$status_counts$total > 0) {
      default_module <- "overview"
    }
  }

  shiny::runGadget(
    trackR_panel_ui(project_path, default_module),
    server = trackR_panel_server(project_path, initial_diagnosis),
    viewer = shiny::paneViewer(minHeight = "maximize")
  )

  invisible(project_path)
}

trackR_panel_ui <- function(project_path, default_module = "project") {
  miniUI::miniPage(
    shiny::tags$head(
      shiny::tags$style(shiny::HTML(trackR_panel_css())),
      shiny::tags$script(shiny::HTML("
        $(document).on('shiny:value', function(event) {
          if (event.name === 'action_log') {
            setTimeout(function() {
              var logDiv = document.querySelector('#action_log');
              if (logDiv) {
                logDiv.scrollTop = logDiv.scrollHeight;
              }
            }, 50);
          }
        });
        function trackrSwitchTab(tab) {
          document.querySelectorAll('.tr-preview-tab').forEach(function(b) { b.classList.remove('active'); });
          document.querySelectorAll('.tr-preview-pane').forEach(function(p) { p.classList.remove('active'); });
          document.getElementById('tab_btn_' + tab).classList.add('active');
          document.getElementById('pane_' + tab).classList.add('active');
          if (tab === 'diff') {
            Shiny.setInputValue('act_diff_trigger', Math.random());
          }
        }
      "))
    ),
    miniUI::miniContentPanel(
      shiny::div(
        class = "tr-shell",
        shiny::div(
          class = "tr-sidebar",
          shiny::radioButtons(
            "module",
            label = NULL,
            choiceNames = list(
              shiny::HTML(paste("<div class='tr-nav-item'>", shiny::icon("heartbeat"), "Vis\u00E3o Geral</div>")),
              shiny::HTML(paste("<div class='tr-nav-item'>", shiny::icon("folder"), "Gerenciar Projeto</div>")),
              shiny::HTML(paste("<div class='tr-nav-item'>", shiny::icon("file-alt"), "Arquivos e C\u00F3digo</div>")),
              shiny::HTML(paste("<div class='tr-nav-item'>", shiny::icon("github"), "Git e GitHub</div>"))
            ),
            choiceValues = c("overview", "project", "files", "git"),
            selected = default_module
          )
        ),
        shiny::div(
          class = "tr-main",
          shiny::uiOutput("project_summary"),
          shiny::uiOutput("module_ui"),
          shiny::uiOutput("terminal_panel")
        )
      )
    )
  )
}

trackR_panel_server <- function(project_path, initial_diagnosis = NULL) {
  force(project_path)

  function(input, output, session) {
    panel_project_path <- function(path) {
      normalized <- normalize_project_path(path)

      if (fs::file_exists(normalized) && grepl("\\.[Rr]proj$", normalized)) {
        return(dirname(normalized))
      }

      normalized
    }

    project_path_state <- shiny::reactiveVal(panel_project_path(project_path))
    current_panel_path <- function() {
      project_path_state()
    }
    current_rstudio_project <- function() {
      if (!rstudioapi::isAvailable()) {
        return(NULL)
      }

      active <- trimws(rstudioapi::getActiveProject() %||% "")
      if (!nzchar(active)) {
        return(NULL)
      }

      panel_project_path(active)
    }

    project_choices <- shiny::reactiveVal(named_project_choices(default_projects_directory()))
    
    shiny::observeEvent(input$project_base_dir, {
      path <- trimws(input$project_base_dir %||% "")
      if (nzchar(path) && fs::dir_exists(path)) {
        project_choices(named_project_choices(path))
      }
    })

    if (is.null(initial_diagnosis)) {
      initial_diagnosis <- build_git_diagnosis(current_panel_path())
    }
    diagnosis_state <- shiny::reactiveVal(initial_diagnosis)
    values <- shiny::reactiveValues(
      log = paste(diagnosis_lines(initial_diagnosis), collapse = "\n"),
      diff_html = format_diff_for_panel_html(character()),
      inline_diff_html = "",
      pending_delete = NULL,
      pending_remote_disconnect = NULL,
      synced_remote_name = initial_diagnosis$remote_name %||% "origin",
      synced_remote_url = initial_diagnosis$remote_url %||% ""
    )

    capture_lines <- function(expr) {
      paste(utils::capture.output(force(expr)), collapse = "\n")
    }

    refresh_diagnosis <- function(update_inputs = FALSE) {
      current <- build_git_diagnosis(current_panel_path())
      previous <- shiny::isolate(diagnosis_state())
      changed <- !identical(current, previous)

      if (changed) {
        diagnosis_state(current)
      }
      if (isTRUE(update_inputs) && changed) {
        update_git_config_inputs(current)
        update_changed_file_inputs(current)
      }

      current
    }

    set_log <- function(text) {
      values$log <- text
    }

    update_git_config_inputs <- function(diagnosis) {
      values$synced_remote_name <- diagnosis$remote_name %||% "origin"
      values$synced_remote_url <- diagnosis$remote_url %||% ""
      shiny::updateTextInput(
        session,
        "github_remote_name",
        value = values$synced_remote_name
      )
      shiny::updateTextInput(
        session,
        "github_remote_url",
        value = values$synced_remote_url
      )
    }

    update_changed_file_inputs <- function(diagnosis) {
      files <- changed_file_choices(diagnosis$status)
      selected <- if (length(files) > 0) unname(files[[1]]) else character()

      shiny::updateSelectInput(session, "changes_file", choices = files, selected = selected)
      shiny::updateSelectInput(session, "changes_files", choices = files, selected = character())
      shiny::updateSelectInput(session, "git_commit_files", choices = files, selected = character())
    }

    clone_followup_notification <- function(result) {
      open_result <- result$open %||% NULL
      if (is.null(open_result) || isTRUE(open_result$ok)) {
        return(NULL)
      }

      if (identical(open_result$reason, "multiple_rproj")) {
        return(list(
          message = "Clone conclu\u00EDdo. H\u00E1 mais de um .Rproj; escolha manualmente qual projeto abrir.",
          type = "warning"
        ))
      }

      if (identical(open_result$reason, "missing_rproj")) {
        return(list(
          message = "Clone conclu\u00EDdo. Nenhum .Rproj foi encontrado; abra a pasta clonada ou crie um projeto nela.",
          type = "message"
        ))
      }

      NULL
    }

    refresh_panel_state <- function() {
      refresh_diagnosis(update_inputs = TRUE)
    }

    sync_panel_project <- function(path, log_message = NULL) {
      panel_path <- panel_project_path(path)
      project_path_state(panel_path)
      diagnosis <- refresh_panel_state()

      shiny::updateTextInput(
        session,
        "project_to_open",
        value = diagnosis$rproj_path %||% panel_path
      )
      shiny::updateTextInput(session, "project_base_dir", value = dirname(panel_path))
      shiny::updateTextInput(session, "project_clone_base_dir", value = dirname(panel_path))
      project_choices(named_project_choices(dirname(panel_path)))

      set_log(log_message %||% paste(diagnosis_lines(diagnosis), collapse = "\n"))
      invisible(diagnosis)
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
    requested_remote_name <- function() {
      diagnosis <- shiny::isolate(diagnosis_state())
      normalize_remote_name(
        input$github_remote_name,
        default = diagnosis$remote_name %||% "origin"
      )
    }

    current_remote_name <- function() {
      diagnosis <- shiny::isolate(diagnosis_state())

      if (isTRUE(diagnosis$has_remote) && !is.null(diagnosis$remote_name)) {
        return(diagnosis$remote_name)
      }

      requested_remote_name()
    }

    output$project_summary <- shiny::renderUI({
      diagnosis <- d()
      # Force reactivity on both diagnosis and module
      shiny::req(diagnosis)
      current_module <- input$module %||% "overview"

      stats <- if (identical(current_module, "git")) {
        git_status_badge_items(diagnosis)
      } else {
        panel_summary_items(diagnosis)
      }

      project_name <- if (diagnosis$is_rstudio_project) basename(diagnosis$rproj_path) else basename(diagnosis$current_path)
      active_rstudio_project <- current_rstudio_project()
      context_warning <- project_context_warning_ui(
        panel_path = current_panel_path(),
        active_rstudio_path = active_rstudio_project
      )

      shiny::div(
        class = "tr-summary",
        shiny::div(
          class = "tr-summary-main",
          shiny::div(
            class = "tr-summary-header",
            shiny::div(
              class = "tr-summary-title-wrap",
              style = "margin-bottom: 4px;",
              shiny::strong(project_name, style = "font-size: 22px; color: #2563EB; letter-spacing: -0.5px;")
            ),
            shiny::div(
              class = "tr-summary-grid",
              lapply(stats, function(item) {
                pill_class <- paste("tr-pill", item$class, if (isTRUE(item$interactive)) "interactive" else "")
                onclick <- if (isTRUE(item$interactive)) {
                  "Shiny.setInputValue('navigate_to_files', true, {priority: 'event'});"
                } else {
                  ""
                }

                shiny::div(
                  class = pill_class,
                  title = item$title,
                  onclick = if (nzchar(onclick)) onclick else NULL,
                  if (nzchar(item$label)) shiny::span(class = "tr-pill-label", item$label),
                  shiny::span(class = "tr-pill-value", item$value)
                )
              })
            )
          ),
          shiny::div(class = "tr-path", style = "font-size: 12px; color: #71717A;", diagnosis$current_path),
          context_warning
        )
      )
    })

    output$module_ui <- shiny::renderUI({
      switch(
        input$module %||% "overview",
        overview = overview_module_ui(d()),
        project = project_module_ui(),
        files = files_module_ui(d()),
        git = git_module_ui(d()),
        overview_module_ui(d())
      )
    })

    output$action_log <- shiny::renderText(values$log)

    output$terminal_panel <- shiny::renderUI({
      if (input$module == "overview") {
        shiny::tags$details(
          class = "tr-log",
          shiny::tags$summary(
            class = "tr-log-summary",
            shiny::tags$span(shiny::icon("terminal"), " Registro de a\u00E7\u00F5es"),
            shiny::tags$span(class = "tr-log-hint", "Clique para ver detalhes")
          ),
          shiny::div(
            class = "tr-log-content",
            shiny::verbatimTextOutput("action_log"),
            shiny::div(
              style = "display: flex; gap: 10px;",
              shiny::tags$div(class = "tr-tooltip", `data-tooltip` = "Recarrega informa\u00E7\u00F5es do projeto", shiny::actionButton("refresh_all", "Atualizar diagn\u00F3stico", class = "btn-default btn-sm")),
              shiny::tags$div(class = "tr-tooltip", `data-tooltip` = "Abre o projeto que est\u00E1 aberto no RStudio", shiny::actionButton("refresh_project_path", "Usar projeto ativo no RStudio", class = "btn-default btn-sm", icon = shiny::icon("sync")))
            )
          )
        )
      } else {
        NULL
      }
    })

    output$diff_view <- shiny::renderUI({
      shiny::HTML(values$diff_html)
    })

    output$project_structure_preview <- shiny::renderUI({
      shiny::HTML(render_project_tree_html(input$project_template %||% "analise_exploratoria", isTRUE(input$project_include_data)))
    })
    
    output$project_organize_diff <- shiny::renderUI({
      shiny::req(d())
      panel_path <- current_panel_path()
      shiny::HTML(render_project_organize_diff_html(
        panel_path,
        input$project_structure_template %||% "analise_exploratoria",
        isTRUE(input$project_structure_include_data)
      ))
    })
    
    output$project_organize_status <- shiny::renderUI({
      shiny::req(d())
      diagnosis <- d()
      path <- diagnosis$current_path
      if (is.null(path) || length(path) == 0) return(NULL)
      folders <- c("data", "data/raw", "data/processed", "reports", "scripts", "figs")
      
      items <- lapply(folders, function(f) {
        exists <- fs::dir_exists(fs::path(path, f))
        icon_html <- if (exists) {
          paste0("<span style='margin-right: 8px;'>", panel_icon_html("check", "#9333EA"), "</span>")
        } else {
          paste0("<span style='margin-right: 8px;'>", panel_icon_html("times", "#EF4444"), "</span>")
        }
        label <- if (exists) {
          paste0("<span style='color: #EDEDED;'>", f, "/</span> <span style='color: #71717A; font-size: 11px;'>(J\u00E1 existe)</span>")
        } else {
          paste0("<span style='color: #A1A1AA;'>", f, "/</span> <span style='color: #71717A; font-size: 11px;'>(Ser\u00E1 criada)</span>")
        }
        shiny::HTML(paste0("<div style='margin-bottom: 6px; font-size: 13px;'>", icon_html, label, "</div>"))
      })
      
      shiny::div(
        style = "background: #0D0D0D; border: 1px solid #2D2D2D; padding: 14px; border-radius: 8px; margin-bottom: 15px;",
        shiny::h5("Estado das pastas no projeto atual:", style = "margin-top: 0; margin-bottom: 10px; font-weight: 600; color: #EDEDED;"),
        items
      )
    })
    
    output$recent_projects_list <- shiny::renderUI({
      choices <- project_choices()
      if (length(choices) == 0) {
        return(shiny::p("Nenhum projeto encontrado na pasta base.", style = "color: #71717A; font-style: italic; font-size: 13px;"))
      }
      
      items <- lapply(names(choices), function(label) {
        path <- choices[[label]]
        proj_name <- fs::path_ext_remove(fs::path_file(path))
        proj_dir <- dirname(path)
        
        js_path <- gsub("\\\\", "\\\\\\\\", path)
        js_path <- gsub("'", "\\\\'", js_path)
        
        shiny::tags$div(
          class = "tr-recent-project-card",
          onclick = sprintf("Shiny.setInputValue('project_to_open', '%s', {priority: 'event'});", js_path),
          shiny::div(class = "tr-recent-name", proj_name),
          shiny::div(class = "tr-recent-dir", proj_dir)
        )
      })
      
      shiny::div(
        class = "tr-recent-projects-grid",
        shiny::h5("Projetos detectados na pasta base:", style = "margin-top: 0; margin-bottom: 12px; font-weight: 600; color: #EDEDED;"),
        items
      )
    })



    output$recent_files_explorer <- shiny::renderUI({
      panel_path <- current_panel_path()
      shiny::req(panel_path, nzchar(panel_path))
      shiny::req(d())
      shiny::HTML(render_project_files_explorer_html(panel_path, d(), input$selected_project_item %||% ""))
    })

    output$file_content_preview <- shiny::renderUI({
      panel_path <- current_panel_path()
      shiny::req(panel_path, nzchar(panel_path))
      shiny::HTML(render_file_content_preview_html(panel_path, input$selected_project_item %||% ""))
    })

    output$file_actions_panel <- shiny::renderUI({
      panel_path <- current_panel_path()
      shiny::req(panel_path, nzchar(panel_path))
      file_actions_panel_ui(panel_path, d(), input$selected_project_item %||% "")
    })

    output$file_diff_inline <- shiny::renderUI({
      if (is.null(values$inline_diff_html) || !nzchar(values$inline_diff_html)) {
        return(shiny::div(
          class = "tr-preview-empty",
          shiny::div(class = "tr-preview-empty-icon", shiny::icon("code-branch")),
          shiny::div(class = "tr-preview-empty-title", "Nenhuma altera\u00E7\u00E3o neste arquivo"),
          shiny::div(class = "tr-preview-empty-hint", "Selecione um arquivo com modifica\u00E7\u00F5es na lista \u00E0 esquerda.")
        ))
      }
      shiny::div(class = "tr-inline-diff", shiny::HTML(values$inline_diff_html))
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

    shiny::observeEvent(input$navigate_to_files, {
      shiny::updateRadioButtons(session, "module", selected = "files")
      shiny::showNotification("Navegando para Arquivos e C\u00F3digo", type = "message", duration = 2)
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$refresh_all, {
      diagnosis <- refresh_panel_state()
      set_log(paste(diagnosis_lines(diagnosis), collapse = "\n"))
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$refresh_project_path, {
      new_path <- active_project_path()
      sync_panel_project(
        new_path,
        log_message = paste("Painel sincronizado com a pasta atual:", panel_project_path(new_path))
      )
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$project_choose_base, {
      selected <- choose_directory()
      if (!is.null(selected) && nzchar(selected)) {
        shiny::updateTextInput(session, "project_base_dir", value = selected)
      }
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$project_choose_existing, {
      selected <- choose_rproj_file()
      if (!is.null(selected) && nzchar(selected)) {
        shiny::updateTextInput(session, "project_to_open", value = selected)
      }
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$project_choose_clone_base, {
      selected <- choose_directory()
      if (!is.null(selected) && nzchar(selected)) {
        shiny::updateTextInput(session, "project_clone_base_dir", value = selected)
      }
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$project_create, {
      project_name <- trimws(input$project_name %||% "")
      if (!nzchar(project_name)) {
        shiny::showNotification("Preencha o campo 'Nome do projeto' para continuar.", type = "error")
        return()
      }

      target_path <- fs::path(normalize_project_path(input$project_base_dir), project_name)
      extra_files <- split_extra_file_lines(input$project_extra_files)

      result <- project_create(
        path = target_path,
        template = input$project_template,
        include_data = isTRUE(input$project_include_data),
        initialize_git = isTRUE(input$project_initialize_git),
        open = isTRUE(input$project_open_after_create),
        extra_files = extra_files
      )

      set_log(capture_lines(result))
      sync_panel_project(result$path %||% target_path, log_message = values$log)
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$git_save_identity, {
      name <- trimws(input$git_user_name %||% "")
      email <- trimws(input$git_user_email %||% "")
      if (!nzchar(name) || !nzchar(email)) {
        shiny::showNotification("Preencha seu nome completo e email para identificar seus commits.", type = "error")
        return()
      }
      run_panel_action(git_set_identity(name, email))
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$project_open, {
      selected <- input$project_to_open %||% ""
      if (!nzchar(selected)) {
        shiny::showNotification("Informe o caminho do projeto no campo acima ou clique em 'Procurar...'.", type = "error")
        return()
      }

      result <- project_open(selected)
      set_log(capture_lines(result))
      sync_panel_project(result$path %||% selected, log_message = values$log)
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$project_clone, {
      remote_url <- trimws(input$project_clone_url %||% "")
      if (!nzchar(remote_url)) {
        shiny::showNotification("Informe a URL do reposit\u00F3rio para clonar.", type = "error")
        return()
      }

      result <- git_clone_repo(
        remote_url = remote_url,
        path = input$project_clone_base_dir,
        directory = input$project_clone_dir,
        open = isTRUE(input$project_clone_open_after)
      )

      set_log(capture_lines(result))
      if (isTRUE(result$ok)) {
        sync_panel_project(result$path %||% input$project_clone_base_dir, log_message = values$log)
        followup <- clone_followup_notification(result)
        if (!is.null(followup)) {
          shiny::showNotification(followup$message, type = followup$type, duration = 6)
        }
      }
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$project_structure, {
      panel_path <- current_panel_path()
      run_panel_action(
        project_organize(
          path = panel_path,
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
      panel_path <- current_panel_path()
      shiny::removeModal()
      run_panel_action(
        file_import(
          source = input$file_source,
          destination = input$file_destination,
          path = panel_path,
          move = isTRUE(input$file_move),
          add_to_git = isTRUE(input$file_add_to_git),
          overwrite = isTRUE(input$file_overwrite)
        )
      )
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$file_create_type, {
      shiny::updateTextAreaInput(
        session,
        "file_create_content",
        value = ""
      )
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$selected_project_item, {
      values$inline_diff_html <- ""

      selected <- trimws(input$selected_project_item %||% "")

      # Dispara diff automaticamente para arquivos com mudan\u00E7as
      if (nzchar(selected)) {
        panel_path <- current_panel_path()
        shiny::isolate({
          result <- tryCatch(
            git_diff(file = selected, path = panel_path, staged = FALSE, context = "changes"),
            error = function(e) list(diff = character())
          )
          diff_lines <- result$diff %||% character()
          if (length(diff_lines) > 0) {
            values$inline_diff_html <- format_diff_for_panel_html(diff_lines)
          }
        })
      }
      if (!nzchar(selected)) {
        return()
      }

      panel_path <- current_panel_path()
      selected_full_path <- fs::path(panel_path, selected)
      all_items <- project_item_choices(panel_path)

      shiny::updateSelectInput(
        session,
        "file_delete_path",
        choices = unique(c("Selecione..." = "", stats::setNames(all_items, all_items), stats::setNames(selected, selected))),
        selected = selected
      )

      shiny::updateSelectInput(
        session,
        "rename_source",
        choices = unique(c(stats::setNames(all_items, all_items), stats::setNames(selected, selected))),
        selected = selected
      )
      shiny::updateTextInput(session, "rename_target", value = selected)

      if (fs::file_exists(selected_full_path)) {
        ext <- tolower(fs::path_ext(selected))
        if (ext %in% c("r", "rmd", "qmd")) {
          format_files <- project_format_file_choices(panel_path)
          shiny::updateSelectInput(
            session,
            "format_path",
            choices = unique(c("Nenhum script (busque na pasta)" = "", stats::setNames(format_files, format_files), stats::setNames(selected, selected))),
            selected = selected
          )
        }
      }
    }, ignoreInit = TRUE)

    # --- Explorador unificado: modais e acoes contextuais ---
    shiny::observeEvent(input$open_create_modal, {
      shiny::showModal(create_modal_ui())
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$open_import_modal, {
      shiny::showModal(import_modal_ui())
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$act_format, {
      selected <- trimws(input$selected_project_item %||% "")
      if (!nzchar(selected)) return()
      run_panel_action(code_format(fs::path(current_panel_path(), selected)), refresh = FALSE)
      shiny::showNotification("Arquivo formatado.", type = "message", duration = 2)
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$act_rename, {
      selected <- trimws(input$selected_project_item %||% "")
      target <- trimws(input$act_rename_target %||% "")
      if (!nzchar(selected) || !nzchar(target)) {
        shiny::showNotification("Selecione um arquivo e informe o novo nome.", type = "error")
        return()
      }
      if (identical(selected, target)) {
        shiny::showNotification("O novo nome \u00E9 igual ao atual. Informe um nome diferente.", type = "warning")
        return()
      }
      run_panel_action(file_rename(selected, target, path = current_panel_path()))
      shiny::showNotification("Arquivo renomeado.", type = "message", duration = 2)
    }, ignoreInit = TRUE)

    run_diff_for_selected <- function() {
      selected <- trimws(input$selected_project_item %||% "")
      if (!nzchar(selected)) return()
      result <- git_diff(file = selected, path = current_panel_path(), staged = FALSE, context = "changes")
      diff_lines <- result$diff %||% character()
      values$inline_diff_html <- format_diff_for_panel_html(diff_lines)
    }

    shiny::observeEvent(input$act_diff, {
      run_diff_for_selected()
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$act_diff_trigger, {
      run_diff_for_selected()
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$act_commit, {
      selected <- trimws(input$selected_project_item %||% "")
      if (!nzchar(selected)) return()
      message <- trimws(input$act_commit_message %||% "")
      if (!nzchar(message)) {
        shiny::showNotification("A mensagem do commit n\u00E3o pode estar vazia. Descreva brevemente o que voc\u00EA alterou.", type = "error")
        return()
      }
      panel_path <- current_panel_path()
      stage_output <- capture_lines(git_stage(selected, path = panel_path))
      commit_output <- capture_lines(git_commit(message, path = panel_path))
      set_log(paste(c(stage_output, commit_output), collapse = "\n"))
      refresh_panel_state()
      values$inline_diff_html <- ""
      shiny::showNotification("Vers\u00E3o salva com sucesso.", type = "message", duration = 2)
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$act_delete, {
      selected <- trimws(input$selected_project_item %||% "")
      if (!nzchar(selected)) return()
      info <- file_delete_info(selected, path = current_panel_path())
      if (!isTRUE(info$ok)) {
        set_log(info$message)
        shiny::showNotification(info$message, type = "error")
        return()
      }

      values$pending_delete <- info$relative_path

      tracked_warning <- if (isTRUE(info$was_tracked)) {
        shiny::div(
          style = "margin-top: 12px; padding: 12px; border-radius: 8px; background: #3B1D1F; color: #FECACA; border: 1px solid #7F1D1D;",
          "Este item est\u00E1 no hist\u00F3rico do Git. Ap\u00F3s excluir, a remo\u00E7\u00E3o aparecer\u00E1 como mudan\u00E7a pendente no projeto."
        )
      } else {
        NULL
      }

      shiny::showModal(
        shiny::modalDialog(
          title = "Excluir item?",
          shiny::p(sprintf("Voc\u00EA quer deletar %s?", info$label)),
          shiny::p("Esta a\u00E7\u00E3o remove o item do disco local."),
          tracked_warning,
          if (isTRUE(info$was_tracked)) {
            shiny::checkboxInput("file_delete_remove_from_git", "Registrar a remo\u00E7\u00E3o no Git (incluir no pr\u00F3ximo commit)", value = TRUE)
          },
          easyClose = TRUE,
          footer = shiny::tagList(
            shiny::modalButton("Cancelar"),
            shiny::actionButton("confirm_file_delete", "Excluir", class = "btn-danger")
          )
        )
      )
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$file_create, {
      panel_path <- current_panel_path()
      shiny::removeModal()
      run_panel_action(
        file_create(
          filename = input$file_create_name,
          type = input$file_create_type,
          destination = input$file_create_destination,
          path = panel_path,
          content = input$file_create_content,
          open_in_rstudio = TRUE
        )
      )
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$file_delete_browse, {
      selected <- choose_project_file()
      if (!is.null(selected) && nzchar(selected)) {
        panel_path <- current_panel_path()
        rel_path <- tryCatch(relative_project_path(selected, panel_path), error = function(e) selected)
        choices <- project_item_choices(panel_path)
        shiny::updateSelectInput(
          session,
          "file_delete_path",
          choices = unique(c("Selecione..." = "", stats::setNames(choices, choices), stats::setNames(rel_path, rel_path))),
          selected = rel_path
        )
      }
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$file_delete, {
      info <- file_delete_info(input$file_delete_path, path = current_panel_path())
      if (!isTRUE(info$ok)) {
        set_log(info$message)
        shiny::showNotification(info$message, type = "error")
        return()
      }

      values$pending_delete <- info$relative_path

      tracked_warning <- if (isTRUE(info$was_tracked)) {
        shiny::div(
          style = "margin-top: 12px; padding: 12px; border-radius: 8px; background: #3B1D1F; color: #FECACA; border: 1px solid #7F1D1D;",
          "Este item est\u00E1 no hist\u00F3rico do Git. Ap\u00F3s excluir, a remo\u00E7\u00E3o aparecer\u00E1 como mudan\u00E7a pendente no projeto."
        )
      } else {
        NULL
      }

      shiny::showModal(
        shiny::modalDialog(
          title = "Excluir item?",
          shiny::p(sprintf("Voc\u00EA quer deletar %s?", info$label)),
          shiny::p("Esta a\u00E7\u00E3o remove o item do disco local."),
          tracked_warning,
          if (isTRUE(info$was_tracked)) {
            shiny::checkboxInput("file_delete_remove_from_git", "Registrar a remo\u00E7\u00E3o no Git (incluir no pr\u00F3ximo commit)", value = TRUE)
          },
          easyClose = TRUE,
          footer = shiny::tagList(
            shiny::modalButton("Cancelar"),
            shiny::actionButton("confirm_file_delete", "Excluir", class = "btn-danger")
          )
        )
      )
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$confirm_file_delete, {
      shiny::removeModal()

      selected <- trimws(values$pending_delete %||% "")
      if (!nzchar(selected)) {
        return()
      }

      run_panel_action(
        file_delete(
          selected,
          path = current_panel_path(),
          remove_from_git = isTRUE(input$file_delete_remove_from_git)
        )
      )
      values$pending_delete <- NULL
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$git_diagnose, {
      diagnosis <- refresh_panel_state()
      set_log(paste(diagnosis_lines(diagnosis), collapse = "\n"))
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$git_init, {
      run_panel_action(git_init(current_panel_path()))
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$gitignore_write, {
      run_panel_action(
        git_ignore(
          current_panel_path(),
          include_data = !isTRUE(input$gitignore_ignore_data)
        )
      )
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$git_status, {
      run_panel_action(git_status(current_panel_path()))
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$git_commit, {
      run_panel_action(
        git_commit_all(
          message = input$commit_message %||% "Primeiro commit",
          path = current_panel_path()
        )
      )
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$changes_refresh, {
      diagnosis <- refresh_panel_state()
      values$diff_html <- format_diff_for_panel_html(character())
      shiny::showNotification("Lista de arquivos com mudan\u00E7as atualizada.", type = "message", duration = 3)
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$changes_diff, {
      panel_path <- current_panel_path()
      result <- git_diff(
        file = input$changes_file,
        path = panel_path,
        staged = isTRUE(input$changes_staged),
        context = input$changes_diff_context %||% "changes"
      )
      diff_lines <- result$diff %||% character()
      values$diff_html <- format_diff_for_panel_html(diff_lines)
      set_log(format_diff_for_panel(diff_lines))
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$changes_stage, {
      run_panel_action(git_stage(input$changes_files, path = current_panel_path()))
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$changes_unstage, {
      run_panel_action(git_unstage(input$changes_files, path = current_panel_path()))
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$changes_discard, {
      shiny::showModal(
        shiny::modalDialog(
          title = "Descartar altera\u00E7\u00F5es?",
          "As altera\u00E7\u00F5es dos arquivos selecionados ser\u00E3o removidas permanentemente. Esta a\u00E7\u00E3o n\u00E3o pode ser desfeita.",
          footer = shiny::tagList(
            shiny::modalButton("Cancelar"),
            shiny::actionButton("confirm_discard_action", "Descartar altera\u00E7\u00F5es", class = "btn-danger")
          )
        )
      )
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$confirm_discard_action, {
      shiny::removeModal()
      run_panel_action(git_discard(input$changes_files, path = current_panel_path()))
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$changes_commit_selected, {
      selected <- normalize_git_file_selection(input$changes_files)
      if (length(selected) > 0) {
        panel_path <- current_panel_path()
        stage_output <- capture_lines(git_stage(selected, path = panel_path))
        commit_output <- capture_lines(git_commit(input$changes_commit_message, path = panel_path))
        set_log(paste(c(stage_output, commit_output), collapse = "\n"))
        refresh_panel_state()
      } else {
        run_panel_action(git_commit(input$changes_commit_message, path = current_panel_path()))
      }
    }, ignoreInit = TRUE)

    # Observers of removed buttons git_stage_commit_files and git_commit_staged were deleted.

    shiny::observeEvent(input$github_connect, {
      remote_name <- requested_remote_name()
      run_panel_action(
        github_connect(
          remote_url = input$github_remote_url,
          path = current_panel_path(),
          remote = remote_name,
          replace = isTRUE(input$github_replace_remote)
        )
      )
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$github_disconnect, {
      values$pending_remote_disconnect <- list(
        remote = current_remote_name(),
        url = trimws(input$github_remote_url %||% "")
      )

      shiny::showModal(
        shiny::modalDialog(
          title = "Desconectar remote?",
          shiny::p(sprintf(
            "Voc\u00EA quer remover a conex\u00E3o com o remote '%s' deste projeto?",
            values$pending_remote_disconnect$remote
          )),
          shiny::p("Isso remove apenas a liga\u00E7\u00E3o com o GitHub. O hist\u00F3rico Git local continua intacto."),
          if (nzchar(values$pending_remote_disconnect$url)) {
            shiny::tags$p(
              shiny::strong("URL atual: "),
              values$pending_remote_disconnect$url
            )
          },
          footer = shiny::tagList(
            shiny::modalButton("Cancelar"),
            shiny::actionButton("confirm_github_disconnect", "Desconectar remote", class = "btn-danger")
          )
        )
      )
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$confirm_github_disconnect, {
      shiny::removeModal()
      pending <- values$pending_remote_disconnect
      if (is.null(pending)) {
        return(NULL)
      }

      run_panel_action(github_disconnect(path = current_panel_path(), remote = pending$remote))
      values$pending_remote_disconnect <- NULL
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$github_auth, {
      run_panel_action(github_check(path = current_panel_path(), remote = current_remote_name()))
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$github_open_repo, {
      run_panel_action(github_open_repo(path = current_panel_path(), remote = current_remote_name()), refresh = FALSE)
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$github_fetch, {
      run_panel_action(git_fetch(path = current_panel_path(), remote = current_remote_name()))
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$github_pull, {
      run_panel_action(git_pull(path = current_panel_path(), remote = current_remote_name()))
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$github_push, {
      run_panel_action(git_push(path = current_panel_path(), remote = current_remote_name()))
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$github_sync, {
      run_panel_action(git_sync(path = current_panel_path(), remote = current_remote_name()))
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$format_browse, {
      file <- tryCatch(rstudioapi::selectFile(caption = "Selecionar script para formatar", filter = "R/Quarto files (*.R *.Rmd *.qmd)", existing = TRUE), error = function(e) NULL)
      if (!is.null(file) && nzchar(file)) {
        run_panel_action(code_format(file), refresh = FALSE)
      }
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$rename_browse, {
      file <- tryCatch(rstudioapi::selectFile(caption = "Selecionar arquivo/pasta para renomear", existing = TRUE), error = function(e) NULL)
      if (!is.null(file) && nzchar(file)) {
        panel_path <- current_panel_path()
        rel_path <- tryCatch(relative_project_path(file, panel_path), error = function(e) file)
        all_items <- list.files(path = panel_path, all.files = FALSE, recursive = TRUE, include.dirs = TRUE, full.names = FALSE)
        shiny::updateSelectInput(session, "rename_source", choices = unique(c(rel_path, all_items)), selected = rel_path)
        shiny::updateTextInput(session, "rename_target", value = rel_path)
      }
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$rename_source, {
      selected <- trimws(input$rename_source %||% "")
      if (nzchar(selected)) {
        shiny::updateTextInput(session, "rename_target", value = selected)
      }
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$format_active, {
      run_panel_action(
        code_format(panel_optional_path(input$format_path)),
        refresh = FALSE
      )
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$format_project, {
      run_panel_action(code_format_all(current_panel_path()), refresh = FALSE)
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$rename_execute, {
      source <- panel_optional_path(input$rename_source)
      target <- panel_optional_path(input$rename_target)
      if (!is.null(source) && nzchar(source) && !is.null(target) && nzchar(target)) {
        run_panel_action(file_rename(source, target, path = current_panel_path()))
      } else {
        shiny::showNotification("Selecione um item na lista e informe o novo nome antes de renomear.", type = "error")
      }
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$done, {
      shiny::stopApp(current_panel_path())
    })

    shiny::observeEvent(input$cancel, {
      shiny::stopApp(invisible(NULL))
    })
  }
}

statgit_panel_ui <- function(project_path, default_module = "project") {
  trackR_panel_ui(project_path, default_module = default_module)
}

statgit_panel_server <- function(project_path, initial_diagnosis = NULL) {
  trackR_panel_server(project_path, initial_diagnosis = initial_diagnosis)
}

project_module_ui <- function() {
  panel_section(
    "Gerenciar Projeto",
    shiny::tabsetPanel(
      id = "project_tabs",
      type = "pills",
      shiny::tabPanel(
        "Criar",
        shiny::br(),
        shiny::div(
          class = "tr-project-layout",
          shiny::div(
            style = "display: flex; flex-direction: column; gap: 15px;",
            shiny::div(
              shiny::tags$label("Pasta base", style = "font-weight: 600; font-size: 14px; margin-bottom: 6px; display: block; color: #EDEDED;"),
              shiny::div(
                style = "display: flex; gap: 10px; align-items: stretch;",
                shiny::div(
                  style = "flex-grow: 1;",
                  shiny::textInput("project_base_dir", label = NULL, value = default_projects_directory(), width = "100%")
                ),
                shiny::actionButton("project_choose_base", "Procurar...")
              )
            ),
            shiny::textInput("project_name", "Nome do projeto", value = "meu-projeto"),
            shiny::div(
              class = "tr-checkbox-group",
              shiny::checkboxInput("project_include_data", "Versionar a pasta data/", value = TRUE),
              shiny::checkboxInput("project_initialize_git", "Inicializar Git", value = TRUE),
              shiny::checkboxInput("project_open_after_create", "Abrir projeto ao criar", value = TRUE)
            ),
            shiny::tags$details(
              shiny::tags$summary("Op\u00E7\u00F5es Avan\u00E7adas", style = "margin-bottom: 8px; cursor: pointer; color: #A1A1AA; font-weight: 600; outline: none;"),
              shiny::textAreaInput(
                "project_extra_files",
                "Arquivos extras (um por linha)",
                value = "scripts/03-figuras.R\nreports/apresentacao.qmd",
                rows = 4
              )
            ),
            shiny::div(
              style = "margin-top: 20px;",
              shiny::actionButton("project_create", "Criar projeto", class = "btn-primary", style = "width: 100%; font-weight: 600; font-size: 15px; height: 42px !important;")
            )
          ),
          shiny::div(
            shiny::tags$label("Modelo de Projeto", style = "font-weight: 600; font-size: 14px; margin-bottom: 16px; display: block; color: #EDEDED;"),
            project_template_cards_ui("project_template", selected = "analise_exploratoria"),
            shiny::div(
              class = "tr-tree-card",
              shiny::h5("Pr\u00E9via da Estrutura a ser Criada:", style = "margin-top: 0; margin-bottom: 12px; font-weight: 600; color: #EDEDED;"),
              shiny::uiOutput("project_structure_preview")
            )
          )
        )
      ),
      shiny::tabPanel(
        "Abrir",
        shiny::br(),
        shiny::div(
          class = "tr-project-layout",
          shiny::div(
            style = "display: flex; flex-direction: column; gap: 15px;",
            shiny::div(
              shiny::tags$label("Caminho do Projeto (.Rproj)", style = "font-weight: 600; font-size: 14px; margin-bottom: 6px; display: block; color: #EDEDED;"),
              shiny::div(
                style = "display: flex; gap: 10px; align-items: stretch; margin-bottom: 15px;",
                shiny::div(
                  style = "flex-grow: 1;",
                  shiny::textInput("project_to_open", label = NULL, value = "", width = "100%")
                ),
                shiny::actionButton("project_choose_existing", "Procurar...")
              )
            ),
            shiny::div(
              style = "margin-top: 20px;",
              shiny::actionButton("project_open", "Abrir projeto", class = "btn-primary", style = "width: 100%; font-weight: 600; font-size: 15px; height: 42px !important;")
            )
          ),
          shiny::div(
            shiny::uiOutput("recent_projects_list")
          )
        )
      ),
      shiny::tabPanel(
        "Clonar",
        shiny::br(),
        shiny::div(
          class = "tr-project-layout",
          shiny::div(
            style = "display: flex; flex-direction: column; gap: 15px;",
            shiny::div(
              shiny::tags$label("URL do reposit\u00F3rio", style = "font-weight: 600; font-size: 14px; margin-bottom: 6px; display: block; color: #EDEDED;"),
              shiny::textInput("project_clone_url", label = NULL, value = "", width = "100%", placeholder = "Ex: https://github.com/usuario/projeto.git")
            ),
            shiny::div(
              shiny::tags$label("Pasta base de destino", style = "font-weight: 600; font-size: 14px; margin-bottom: 6px; display: block; color: #EDEDED;"),
              shiny::div(
                style = "display: flex; gap: 10px; align-items: stretch;",
                shiny::div(
                  style = "flex-grow: 1;",
                  shiny::textInput("project_clone_base_dir", label = NULL, value = default_projects_directory(), width = "100%")
                ),
                shiny::actionButton("project_choose_clone_base", "Procurar...")
              )
            ),
            shiny::textInput("project_clone_dir", "Nome da pasta clonada (opcional)", value = "", placeholder = "Se vazio, usa o nome do reposit\u00F3rio"),
            shiny::checkboxInput("project_clone_open_after", "Abrir projeto clonado se houver .Rproj \u00FAnico", value = TRUE),
            shiny::div(
              style = "margin-top: 20px;",
              shiny::actionButton("project_clone", "Clonar reposit\u00F3rio", class = "btn-primary", style = "width: 100%; font-weight: 600; font-size: 15px; height: 42px !important;")
            )
          ),
          shiny::div(
            class = "tr-tree-card",
            shiny::h5("O que acontece no clone", style = "margin-top: 0; margin-bottom: 12px; font-weight: 600; color: #EDEDED;"),
            shiny::tags$ul(
              style = "padding-left: 18px; margin: 0; color: #D4D4D8; font-size: 13px; line-height: 1.6;",
              shiny::tags$li("Baixa o reposit\u00F3rio remoto para a pasta escolhida."),
              shiny::tags$li("Mant\u00E9m o hist\u00F3rico Git e o remote configurado."),
              shiny::tags$li("Se houver um \u00FAnico arquivo .Rproj, o pacote pode tentar abrir o projeto ao final.")
            )
          )
        )
      ),
      shiny::tabPanel(
        "Organizar",
        shiny::br(),
        shiny::div(
          class = "tr-project-layout tr-project-layout--3col",
          shiny::div(
            style = "display: flex; flex-direction: column; gap: 12px;",
            shiny::div(
              class = "tr-checkbox-group",
              shiny::checkboxInput("project_structure_include_data", "Incluir pastas data/raw e data/processed", value = TRUE)
            ),
            shiny::tags$div(class = "tr-tooltip", `data-tooltip` = "Cria uma estrutura padr\u00E3o com pastas para dados, an\u00E1lise e resultados", shiny::actionButton("project_structure", "Organizar estrutura", class = "btn-primary", style = "width: 100%; font-weight: 600; font-size: 15px; height: 42px !important;")),
            shiny::uiOutput("project_organize_status"),
            shiny::tags$label("Modelo de estrutura", style = "font-weight: 600; font-size: 14px; margin-top: 4px; margin-bottom: 2px; display: block; color: #EDEDED;"),
            project_template_cards_ui("project_structure_template", selected = "analise_exploratoria")
          ),
          shiny::div(
            style = "display: flex; flex-direction: column; gap: 12px;",
            shiny::div(
              class = "tr-organize-diff",
              shiny::uiOutput("project_organize_diff")
            ),
            shiny::div(
              style = "padding: 12px; border-radius: 8px; background: #111827; border: 1px solid #1F2937; color: #D1D5DB; font-size: 13px; line-height: 1.5;",
              shiny::icon("shield-halved"),
              " Organizar \u00E9 seguro: cria apenas o que falta. ",
              shiny::tags$b("Seus arquivos existentes n\u00E3o s\u00E3o movidos nem alterados.")
            )
          )
        )
      )
    )
  )
}

files_module_ui <- function(diagnosis) {
  panel_section(
    "Arquivos e C\u00F3digo",
    shiny::div(
      class = "tr-explorer",
      # Linha de cima: \u00E1rvore + a\u00E7\u00F5es
      shiny::div(
        class = "tr-explorer-top",
        shiny::div(
          class = "tr-explorer-tree",
          shiny::div(
            class = "tr-explorer-toolbar",
            shiny::actionButton("open_create_modal", shiny::tagList(shiny::icon("plus"), " Criar"), class = "btn-primary btn-sm"),
            shiny::actionButton("open_import_modal", shiny::tagList(shiny::icon("file-import"), " Importar"), class = "btn-default btn-sm"),
            shiny::actionButton("act_rename", shiny::tagList(shiny::icon("pen"), " Renomear"), class = "btn-default btn-sm"),
            shiny::tags$div(class = "tr-tooltip", `data-tooltip` = "Padroniza o estilo do c\u00F3digo", shiny::actionButton("act_format", shiny::tagList(shiny::icon("wand-magic-sparkles"), " Formatar"), class = "btn-default btn-sm")),
            shiny::actionButton("act_delete", shiny::tagList(shiny::icon("trash"), " Excluir"), class = "btn-danger btn-sm")
          ),
          shiny::div(
            class = "tr-explorer-tree-body",
            shiny::uiOutput("recent_files_explorer")
          )
        ),
        shiny::div(
          class = "tr-explorer-actions",
          shiny::uiOutput("file_actions_panel")
        )
      ),
      # Linha de baixo: preview + diff (largura total)
      shiny::div(
        class = "tr-explorer-content",
        shiny::div(
          class = "tr-preview-tabs",
          shiny::tags$button(
            class = "tr-preview-tab active",
            id = "tab_btn_code",
            onclick = "trackrSwitchTab('code')",
            shiny::icon("code"), " C\u00F3digo"
          ),
          shiny::tags$button(
            class = "tr-preview-tab",
            id = "tab_btn_diff",
            onclick = "trackrSwitchTab('diff')",
            shiny::icon("exchange-alt"), " Diff"
          )
        ),
        shiny::div(
          class = "tr-preview-pane active",
          id = "pane_code",
          shiny::uiOutput("file_content_preview")
        ),
        shiny::div(
          class = "tr-preview-pane",
          id = "pane_diff",
          shiny::uiOutput("file_diff_inline")
        )
      )
    )
  )
}

import_modal_ui <- function() {
  shiny::modalDialog(
    title = "Importar arquivo para o projeto",
    easyClose = TRUE,
    shiny::div(
      style = "display: flex; flex-direction: column; gap: 15px;",
      shiny::div(
        shiny::tags$label("Arquivo de origem", style = "font-weight: 600; font-size: 14px; margin-bottom: 6px; display: block; color: #EDEDED;"),
        shiny::div(
          style = "display: flex; gap: 10px; align-items: stretch;",
          shiny::div(style = "flex-grow: 1;", shiny::textInput("file_source", label = NULL, value = "", width = "100%")),
          shiny::actionButton("file_choose_source", "Escolher...")
        )
      ),
      shiny::selectInput(
        "file_destination",
        "Pasta de destino no projeto",
        choices = c(
          "Dados originais (data/raw)" = "data/raw",
          "Dados tratados (data/processed)" = "data/processed",
          "Relat\u00F3rios Quarto (reports)" = "reports",
          "Scripts R (scripts)" = "scripts",
          "Figuras/Gr\u00E1ficos (figs)" = "figs"
        ),
        selected = "data/raw"
      ),
      shiny::div(
        class = "tr-checkbox-group",
        shiny::checkboxInput("file_move", "Mover em vez de copiar arquivo", value = FALSE),
        shiny::checkboxInput("file_overwrite", "Substituir arquivo existente", value = FALSE),
        shiny::checkboxInput("file_add_to_git", "Incluir no pr\u00F3ximo commit (Git)", value = TRUE)
      )
    ),
    footer = shiny::tagList(
      shiny::modalButton("Cancelar"),
      shiny::actionButton("file_import", "Importar arquivo", class = "btn-primary")
    )
  )
}

create_modal_ui <- function() {
  shiny::modalDialog(
    title = "Criar novo arquivo",
    easyClose = TRUE,
    shiny::div(
      style = "display: flex; flex-direction: column; gap: 15px;",
      shiny::div(
        shiny::tags$label("Nome do arquivo", style = "font-weight: 600; font-size: 14px; margin-bottom: 6px; display: block; color: #EDEDED;"),
        shiny::textInput("file_create_name", label = NULL, placeholder = "ex: script.R, relatorio.qmd", value = "")
      ),
      shiny::selectInput(
        "file_create_type",
        "Tipo de arquivo",
        choices = c("Script R" = "R", "RMarkdown" = "Rmd", "Quarto" = "qmd", "Markdown" = "md", "CSV" = "csv"),
        selected = "R"
      ),
      shiny::selectInput(
        "file_create_destination",
        "Pasta de destino no projeto",
        choices = c("Scripts R" = "scripts", "Relat\u00F3rios" = "reports", "Dados" = "data/raw", "Figuras" = "figs", "Raiz do projeto" = "."),
        selected = "scripts"
      ),
      shiny::textAreaInput("file_create_content", "Conte\u00FAdo inicial (opcional)", value = "", rows = 8, width = "100%")
    ),
    footer = shiny::tagList(
      shiny::modalButton("Cancelar"),
      shiny::actionButton("file_create", "Criar arquivo", class = "btn-primary")
    )
  )
}

git_module_ui <- function(diagnosis) {
  remote_name <- diagnosis$remote_name %||% "origin"
  remote_url <- diagnosis$remote_url %||% ""
  remote_protocol <- remote_protocol_label(remote_url)
  sync_status <- diagnosis$sync_status %||% list(
    has_upstream = FALSE,
    upstream_branch = NULL,
    remote_branch_exists = FALSE,
    ahead = 0L,
    behind = 0L,
    can_compare = FALSE
  )
  pending_changes <- diagnosis$status_counts$total %||% 0L

  commit_enabled <- diagnosis$has_repo &&
    isTRUE(diagnosis$identity$complete) &&
    pending_changes > 0
  push_enabled <- diagnosis$has_remote &&
    diagnosis$has_commits &&
    !is.null(diagnosis$branch)
  has_remote_url <- nzchar(remote_url)

  panel_section(
    "Git e GitHub",
    shiny::div(
      class = "tr-git-dashboard",
      git_next_action_ui(
        diagnosis = diagnosis,
        remote_name = remote_name,
        remote_url = remote_url,
        remote_protocol = remote_protocol,
        sync_status = sync_status,
        commit_enabled = commit_enabled,
        push_enabled = push_enabled,
        pending_changes = pending_changes,
        has_remote_url = has_remote_url
      ),
      shiny::div(
        class = "tr-git-config",
        shiny::div(class = "tr-git-config-title", "Configura\u00E7\u00E3o"),
        git_config_rows_ui(
          diagnosis = diagnosis,
          remote_name = remote_name,
          remote_url = remote_url,
          remote_protocol = remote_protocol,
          sync_status = sync_status,
          has_remote_url = has_remote_url
        )
      )
    )
  )
}

git_next_action_ui <- function(diagnosis, remote_name, remote_url, remote_protocol, sync_status, commit_enabled, push_enabled, pending_changes, has_remote_url) {
  if (!isTRUE(diagnosis$identity$complete)) {
    return(git_action_panel_ui(
      title = "Configurar identidade",
      message = "Seu nome e email identificam quem fez cada altera\u00E7\u00E3o no projeto.",
      body = shiny::tagList(
        shiny::div(
          class = "tr-git-form-grid",
          shiny::textInput("git_user_name", "Seu Nome Completo", value = diagnosis$identity$name %||% ""),
          shiny::textInput("git_user_email", "Seu Email Acad\u00EAmico/Profissional", value = diagnosis$identity$email %||% "")
        ),
        panel_action_button("git_save_identity", "Salvar identidade", class = "btn-primary", enabled = TRUE, tooltip = "Seu nome aparece em todos os commits que voc\u00EA fizer")
      )
    ))
  }

  if (!isTRUE(diagnosis$git_installed)) {
    return(git_action_panel_ui(
      title = "Instalar Git",
      message = "Git n\u00E3o foi encontrado neste computador.",
      body = git_notice_ui("Instale o Git para criar hist\u00F3rico de vers\u00F5es neste projeto.")
    ))
  }

  if (!isTRUE(diagnosis$has_repo)) {
    return(git_action_panel_ui(
      title = "Ativar Git local",
      message = "Este projeto ainda n\u00E3o tem Git ativado.",
      body = shiny::tagList(
        panel_action_button("git_init", "Inicializar Git", enabled = TRUE, class = "btn-primary", tooltip = "Ativa o Git neste projeto para salvar vers\u00F5es"),
        shiny::div(class = "tr-checkbox-group", shiny::checkboxInput("gitignore_ignore_data", "Ignorar data/raw/ e data/processed/", value = TRUE)),
        shiny::actionButton("gitignore_write", "Criar ou atualizar .gitignore", class = "btn-default")
      )
    ))
  }

  if (pending_changes > 0) {
    return(git_action_panel_ui(
      title = "Salvar vers\u00E3o local",
      message = sprintf("Voc\u00EA tem %d mudan\u00E7a(s) pendente(s).", pending_changes),
      body = shiny::tagList(
        shiny::textInput("commit_message", "Mensagem do commit", value = "", placeholder = "Ex: Adiciona an\u00E1lise descritiva, Corrige importa\u00E7\u00E3o dos dados"),
        panel_action_button(
          "git_commit",
          "Preparar tudo e fazer commit",
          enabled = commit_enabled,
          class = "btn-primary",
          tooltip = "Salva um ponto de salvamento (snapshot) do seu projeto"
        )
      )
    ))
  }

  if (!isTRUE(diagnosis$has_remote)) {
    return(git_action_panel_ui(
      title = "Conectar GitHub",
      message = "Salve seu projeto no GitHub para ter backup e poder compartilhar.",
      body = git_notice_ui("Preencha o nome e a URL do remote na se\u00E7\u00E3o 'Configura\u00E7\u00E3o' logo ao lado.")
    ))
  }

  if (!isTRUE(diagnosis$has_commits)) {
    return(git_action_panel_ui(
      title = "Aguardar mudan\u00E7as",
      message = "O projeto est\u00E1 pronto. Edite ou crie arquivos para fazer seu primeiro commit.",
      body = disabled_reason_ui(TRUE, "Nenhuma mudan\u00E7a detectada ainda.")
    ))
  }

  if (isTRUE(sync_status$behind > 0L)) {
    return(git_action_panel_ui(
      title = "Atualizar branch local",
      message = sprintf("Seu reposit\u00F3rio local est\u00E1 %d commit(s) atr\u00E1s do remote.", sync_status$behind),
      body = shiny::div(
        class = "tr-git-action-row",
        panel_action_button("github_open_repo", "Abrir reposit\u00F3rio", enabled = has_remote_url, class = "btn-default", tooltip = "Abre a p\u00E1gina do reposit\u00F3rio no navegador"),
        panel_action_button("github_fetch", "Fetch", enabled = diagnosis$has_remote, class = "btn-default", tooltip = "Busca atualiza\u00E7\u00F5es do remote sem alterar sua branch local"),
        panel_action_button("github_pull", "Pull", enabled = diagnosis$has_remote, class = "btn-primary", tooltip = "Baixa as mudan\u00E7as do GitHub antes de continuar"),
        panel_action_button("github_sync", "Pull + Push", enabled = push_enabled, class = "btn-default", tooltip = "Atualiza e tenta sincronizar em uma s\u00F3 a\u00E7\u00E3o")
      )
    ))
  }

  git_action_panel_ui(
    title = if (push_enabled) "Sincronizar com GitHub" else "Projeto em dia",
    message = git_next_action_message(sync_status),
    body = shiny::tagList(
      shiny::div(
        class = "tr-remote-summary",
        shiny::strong(sprintf("Remote atual: %s", remote_name)),
        shiny::span(sprintf("%s \u2022 %s", remote_protocol, remote_url), class = "tr-remote-summary-meta")
      ),
      shiny::div(
        class = "tr-git-action-row",
        panel_action_button("github_open_repo", "Abrir reposit\u00F3rio", enabled = has_remote_url, class = "btn-default", tooltip = "Abre a p\u00E1gina do reposit\u00F3rio no navegador"),
        panel_action_button("github_fetch", "Fetch", enabled = diagnosis$has_remote, class = "btn-default", tooltip = "Busca atualiza\u00E7\u00F5es do remote sem alterar sua branch local"),
        panel_action_button("github_pull", "Pull", enabled = diagnosis$has_remote, class = "btn-default", tooltip = "Baixa as mudan\u00E7as do GitHub"),
        panel_action_button("github_push", "Push", enabled = push_enabled, class = "btn-primary", tooltip = "Envia seus commits para o GitHub"),
        panel_action_button("github_sync", "Pull + Push", enabled = push_enabled, class = "btn-default", tooltip = "Baixa e envia mudan\u00E7as em uma s\u00F3 a\u00E7\u00E3o")
      )
    )
  )
}

git_next_action_message <- function(sync_status) {
  if (isTRUE(sync_status$ahead > 0L) && isTRUE(sync_status$behind > 0L)) {
    return(sprintf("Sua branch local est\u00E1 %d commit(s) \u00E0 frente e %d atr\u00E1s do remote.", sync_status$ahead, sync_status$behind))
  }

  if (isTRUE(sync_status$ahead > 0L)) {
    return(sprintf("Sua branch local tem %d commit(s) pendente(s) para enviar.", sync_status$ahead))
  }

  if (isTRUE(sync_status$behind > 0L)) {
    return(sprintf("Sua branch local est\u00E1 %d commit(s) atr\u00E1s do remote.", sync_status$behind))
  }

  if (!isTRUE(sync_status$has_upstream) && isTRUE(sync_status$remote_branch_exists)) {
    return("O remote existe, mas esta branch ainda n\u00E3o tem upstream configurado localmente.")
  }

  if (!isTRUE(sync_status$remote_branch_exists)) {
    return("Nenhuma mudan\u00E7a pendente. O primeiro push vai publicar esta branch no remote.")
  }

  "Nenhuma mudan\u00E7a pendente. Seu hist\u00F3rico local est\u00E1 atualizado."
}

git_action_panel_ui <- function(title, message, body) {
  shiny::div(
    class = "tr-git-next-action",
    shiny::div(class = "tr-git-next-label", "Pr\u00F3xima a\u00E7\u00E3o"),
    shiny::h5(title),
    shiny::p(message),
    body
  )
}

git_config_rows_ui <- function(diagnosis, remote_name, remote_url, remote_protocol, sync_status, has_remote_url) {
  identity_value <- if (isTRUE(diagnosis$identity$complete)) {
    sprintf("%s <%s>", diagnosis$identity$name, diagnosis$identity$email)
  } else {
    "pendente"
  }

  shiny::div(
    class = "tr-git-config-list",
    git_config_row_ui(
      title = "Identidade",
      value = identity_value,
      complete = isTRUE(diagnosis$identity$complete),
      details = if (isTRUE(diagnosis$identity$complete)) {
        shiny::tagList(
          shiny::div(
            class = "tr-git-form-grid",
            shiny::textInput("git_user_name", "Seu Nome Completo", value = diagnosis$identity$name %||% ""),
            shiny::textInput("git_user_email", "Seu Email Acad\u00EAmico/Profissional", value = diagnosis$identity$email %||% "")
          ),
          panel_action_button("git_save_identity", "Salvar identidade", class = "btn-primary", enabled = TRUE, tooltip = "Seu nome aparece em todos os commits que voc\u00EA fizer")
        )
      }
    ),
    git_config_row_ui(
      title = "Git local",
      value = if (diagnosis$has_repo) paste("Branch", diagnosis$branch %||% "sem branch") else "N\u00E3o inicializado",
      complete = diagnosis$has_repo,
      blocked = !diagnosis$git_installed
    ),
    git_config_row_ui(
      title = "GitHub",
      value = if (has_remote_url) sprintf("%s (%s)", remote_name, remote_protocol) else "Remote n\u00E3o configurado",
      complete = diagnosis$has_remote,
      blocked = !diagnosis$has_repo,
      details = if (diagnosis$has_repo && has_remote_url) {
        shiny::tagList(
          shiny::textInput("github_remote_name", "Nome do remote", value = remote_name, placeholder = "Ex: origin, upstream"),
          shiny::textInput("github_remote_url", "URL do reposit\u00F3rio GitHub", value = remote_url, placeholder = "Ex: https://github.com/seu-usuario/seu-projeto"),
          shiny::div(class = "tr-remote-summary-meta", sprintf("Tipo da conex\u00E3o: %s", remote_protocol)),
          if (has_remote_url) shiny::checkboxInput("github_replace_remote", "Trocar URL conectada", value = FALSE),
          shiny::div(
            class = "tr-git-action-row",
            panel_action_button("github_connect", if (has_remote_url) "Reconectar / atualizar remote" else "Conectar remote", enabled = TRUE, class = "btn-primary", tooltip = "Configura a conex\u00E3o com o reposit\u00F3rio do GitHub"),
            panel_action_button("github_open_repo", "Abrir reposit\u00F3rio", enabled = has_remote_url, class = "btn-default", tooltip = "Abre a p\u00E1gina do reposit\u00F3rio no navegador"),
            panel_action_button("github_disconnect", "Desconectar remote", enabled = has_remote_url, class = "btn-default", tooltip = "Remove a conex\u00E3o com o reposit\u00F3rio GitHub sem apagar o hist\u00F3rico local")
          )
        )
      } else if (diagnosis$has_repo) {
        shiny::tagList(
          shiny::textInput("github_remote_name", "Nome do remote", value = remote_name, placeholder = "Ex: origin, upstream"),
          shiny::textInput("github_remote_url", "URL do reposit\u00F3rio GitHub", value = remote_url, placeholder = "Ex: https://github.com/seu-usuario/seu-projeto"),
          panel_action_button("github_connect", "Conectar remote", enabled = TRUE, class = "btn-primary", tooltip = "Configura a conex\u00E3o com o reposit\u00F3rio do GitHub")
        )
      }
    ),
    git_config_row_ui(
      title = "Sincroniza\u00E7\u00E3o",
      value = git_sync_status_text(diagnosis, sync_status = sync_status),
      complete = diagnosis$has_remote && diagnosis$has_commits,
      blocked = !diagnosis$has_remote || !diagnosis$has_commits || is.null(diagnosis$branch)
    )
  )
}

git_config_row_ui <- function(title, value, complete = FALSE, blocked = FALSE, details = NULL) {
  state <- if (isTRUE(complete)) {
    "complete"
  } else if (isTRUE(blocked)) {
    "blocked"
  } else {
    "pending"
  }

  icon <- if (isTRUE(complete)) {
    shiny::icon("check")
  } else if (isTRUE(blocked)) {
    shiny::icon("lock")
  } else {
    shiny::icon("circle")
  }

  row_header <- shiny::div(
    class = paste("tr-git-config-row-head", state),
    shiny::span(class = "tr-git-config-state", icon),
    shiny::span(class = "tr-git-config-name", title),
    shiny::span(class = "tr-git-config-value", value),
    if (!is.null(details)) shiny::span(class = "tr-git-config-action", "Editar")
  )

  if (is.null(details)) {
    return(shiny::div(class = "tr-git-config-row", row_header))
  }

  shiny::tags$details(
    class = "tr-git-config-row tr-git-config-details",
    shiny::tags$summary(row_header),
    shiny::div(class = "tr-git-config-detail-body", details)
  )
}

git_sync_status_text <- function(diagnosis, sync_status = diagnosis$sync_status) {
  if (!isTRUE(diagnosis$has_remote)) {
    return("Remote n\u00E3o configurado")
  }
  if (!isTRUE(diagnosis$has_commits)) {
    return("Nenhum commit ainda")
  }
  if (is.null(diagnosis$branch)) {
    return("Branch n\u00E3o identificada")
  }
  if (!isTRUE(sync_status$remote_branch_exists)) {
    return("Branch ainda n\u00E3o publicada no remote")
  }
  if (isTRUE(sync_status$ahead > 0L) && isTRUE(sync_status$behind > 0L)) {
    return(sprintf("%d \u00E0 frente / %d atr\u00E1s", sync_status$ahead, sync_status$behind))
  }
  if (isTRUE(sync_status$ahead > 0L)) {
    return(sprintf("%d commit(s) para enviar", sync_status$ahead))
  }
  if (isTRUE(sync_status$behind > 0L)) {
    return(sprintf("%d commit(s) para baixar", sync_status$behind))
  }
  if (!isTRUE(sync_status$has_upstream)) {
    return("Sem upstream local")
  }

  "Pronto para enviar"
}

git_status_badge_items <- function(diagnosis) {
  pending_changes <- diagnosis$status_counts$total %||% 0L
  branch_ok <- !is.null(diagnosis$branch)

  list(
    list(
      label = "Git",
      value = if (diagnosis$has_repo) "ativo" else "n\u00E3o iniciado",
      class = if (diagnosis$has_repo) "ok" else "error",
      title = if (diagnosis$has_repo) "Reposit\u00F3rio Git inicializado" else "Inicialize o Git local primeiro"
    ),
    list(
      label = "Commits",
      value = if (diagnosis$has_commits) "com hist\u00F3rico" else "sem commits",
      class = if (diagnosis$has_commits) "ok" else "warn",
      title = if (diagnosis$has_commits) "Hist\u00F3rico de commits existe" else "Crie o primeiro commit"
    ),
    list(
      label = "Remote",
      value = if (diagnosis$has_remote) diagnosis$remote_name %||% "origin" else "ausente",
      class = if (diagnosis$has_remote) "ok" else "warn",
      title = if (diagnosis$has_remote) "Remote conectado" else "Conecte um reposit\u00F3rio GitHub"
    ),
    list(
      label = "Branch",
      value = diagnosis$branch %||% "sem branch",
      class = if (branch_ok) "ok" else "warn",
      title = if (branch_ok) "Branch atual" else "Branch atual n\u00E3o identificada"
    ),
    list(
      label = "Pend\u00EAncias",
      value = as.character(pending_changes),
      class = if (pending_changes == 0L) "ok" else "warn",
      title = if (pending_changes == 0L) "Projeto limpo" else "H\u00E1 arquivos modificados"
    )
  )
}

disabled_reason_ui <- function(show, text) {
  if (!isTRUE(show)) {
    return(NULL)
  }

  shiny::div(
    class = "tr-disabled-reason",
    shiny::icon("lock"),
    shiny::span(text)
  )
}

git_notice_ui <- function(text) {
  shiny::div(class = "tr-git-notice", text)
}

changes_module_ui <- function(diagnosis) {
  if (!diagnosis$has_repo) {
    return(
      panel_section(
        "Aten\u00E7\u00E3o",
        shiny::div(
          style = "padding: 20px; text-align: center; color: #856404; background: #fff3cd; border-radius: 6px;",
          shiny::div(style = "display: flex; align-items: center; justify-content: center; margin-bottom: 12px;", shiny::icon("exclamation-triangle", style = "font-size: 28px;")),
          shiny::h4("Git n\u00E3o inicializado"),
          shiny::p("Esta funcionalidade exige que o Git esteja ativo neste projeto."),
          shiny::p("V\u00E1 para a aba ", shiny::strong("Git e GitHub"), " e clique em 'Inicializar Git'.")
        )
      )
    )
  }

  files <- changed_file_choices(diagnosis$status)

  if (length(files) == 0) {
    return(
      shiny::tagList(
        panel_section(
          "Arquivos com mudan\u00E7as",
          shiny::actionButton("changes_refresh", "Atualizar lista"),
          shiny::div(
            style = "padding: 40px 20px; text-align: center; color: #1a7f37; background: #dafbe1; border-radius: 6px; margin-top: 15px;",
            shiny::div(style = "display: flex; align-items: center; justify-content: center; margin-bottom: 12px;", shiny::icon("check-circle", style = "font-size: 32px; color: #34D399;")),
            shiny::h3("Seu projeto est\u00E1 limpo!"),
            shiny::p("Voc\u00EA n\u00E3o possui nenhuma mudan\u00E7a pendente no momento.")
          )
        )
      )
    )
  }

  shiny::tagList(
    panel_section(
      "O que mudou? (Diff)",
      shiny::actionButton("changes_refresh", "Atualizar lista", class = "btn-default btn-sm", style = "margin-bottom: 10px;"),
      shiny::selectInput("changes_file", "Selecione um arquivo para inspecionar", choices = files),
      shiny::selectInput(
        "changes_diff_context",
        "Modo de visualiza\u00E7\u00E3o",
        choices = c("Ver apenas os trechos alterados" = "changes", "Ver arquivo todo com as mudan\u00E7as destacadas" = "full"),
        selected = "changes"
      ),
      shiny::tags$div(class = "tr-tooltip", `data-tooltip` = "Veja as mudan\u00E7as linha por linha antes de salvar", shiny::actionButton("changes_diff", "Inspecionar Mudan\u00E7as", class = "btn-primary"))
    ),
    panel_section(
      "Salvar nova vers\u00E3o (Commit)",
      shiny::div(
        style = "background: #2E1065; border: 1px solid #6B21A8; border-left: 4px solid #9333EA; padding: 16px; margin-bottom: 24px; border-radius: 8px;",
        shiny::p("O commit funciona como um 'ponto de salvamento' (savepoint) do seu projeto. Voc\u00EA sempre poder\u00E1 voltar a ele se algo der errado no futuro.", style = "margin-bottom: 0; color: #EDE9FE; font-size: 14px;")
      ),
      shiny::selectInput("changes_files", "Arquivos a incluir nesta vers\u00E3o", choices = files, multiple = TRUE),
      shiny::textInput("changes_commit_message", "Mensagem curta descrevendo o que voc\u00EA fez", value = "", placeholder = "Ex: Adiciona an\u00E1lise descritiva, Corrige importa\u00E7\u00E3o dos dados"),
      shiny::actionButton("changes_commit_selected", "Salvar Vers\u00E3o (Commit)", class = "btn-success")
    ),
    panel_section(
      "Resumo",
      shiny::verbatimTextOutput("changes_summary")
    ),
    panel_section(
      "Diff visual",
      shiny::uiOutput("diff_view")
    ),
    panel_section(
      shiny::div(
        style = "display: flex; align-items: center; gap: 10px; margin-bottom: 0;",
        shiny::span(shiny::icon("exclamation-triangle"), style = "color: #DC2626; font-size: 18px;"),
        shiny::span("Zona de Perigo", style = "font-size: 16px; font-weight: 600; color: #EDEDED;")
      ),
      shiny::div(
        style = "border-left: 4px solid #DC2626; padding-left: 12px; margin-top: 12px; color: #FECACA; font-size: 13px; line-height: 1.5;",
        "A a\u00E7\u00E3o abaixo \u00E9 irrevers\u00EDvel. Use apenas quando quiser desfazer todas as altera\u00E7\u00F5es que ainda n\u00E3o foram salvas em um commit."
      ),
      shiny::div(style = "margin-top: 16px;",
        shiny::tags$div(class = "tr-tooltip", `data-tooltip` = "Desfaz as altera\u00E7\u00F5es n\u00E3o salvas nos arquivos selecionados", shiny::actionButton("changes_discard", "Descartar altera\u00E7\u00F5es selecionadas", class = "btn-danger"))
      )
    )
  )
}
format_module_ui <- function(diagnosis) {
  format_files <- project_format_file_choices(diagnosis$current_path)
  
  choices <- c("Nenhum script selecionado" = "", format_files)
  file_input <- shiny::selectInput("format_path", "Selecionar script (opcional)", choices = choices, width = "100%")

  shiny::tagList(
    shiny::div(
      style = "display: flex; gap: 8px; align-items: flex-end; margin-bottom: 15px;",
      shiny::div(style = "flex-grow: 1;", file_input),
      shiny::actionButton("format_browse", label = shiny::icon("folder-open"), class = "btn-default", style = "margin-bottom: 15px; height: 38px;", title = "Procurar arquivo no computador")
    ),
    shiny::div(
      style = "display: flex; flex-direction: column; gap: 10px;",
      shiny::tags$div(class = "tr-tooltip", `data-tooltip` = "Padroniza o estilo do arquivo selecionado", shiny::actionButton("format_active", "Formatar arquivo", class = "btn-primary", style = "width: 100%; font-weight: 600; font-size: 14px; height: 38px !important;")),
      shiny::tags$div(class = "tr-tooltip", `data-tooltip` = "Padroniza o estilo de todos os arquivos R do projeto", shiny::actionButton("format_project", "Formatar todo o projeto", class = "btn-default", style = "width: 100%; font-weight: 600; font-size: 14px; height: 38px !important;"))
    )
  )
}

criar_module_ui <- function(diagnosis) {
  shiny::div(
    class = "tr-project-layout",
    shiny::div(
      style = "display: flex; flex-direction: column; gap: 15px;",
      shiny::div(
        shiny::tags$label("Nome do arquivo", style = "font-weight: 600; font-size: 14px; margin-bottom: 6px; display: block; color: #EDEDED;"),
      shiny::textInput("file_create_name", label = NULL, placeholder = "ex: script.R, relatorio.qmd", value = "")
      ),
      shiny::selectInput(
        "file_create_type",
        "Tipo de arquivo",
        choices = c(
          "Script R" = "R",
          "RMarkdown" = "Rmd",
          "Quarto" = "qmd",
          "Markdown" = "md",
          "CSV" = "csv"
        ),
        selected = "R"
      ),
      shiny::selectInput(
        "file_create_destination",
        "Pasta de destino no projeto",
        choices = c(
          "Scripts R" = "scripts",
          "Relat\u00F3rios" = "reports",
          "Dados" = "data/raw",
          "Figuras" = "figs",
          "Raiz do projeto" = "."
        ),
        selected = "scripts"
      ),
      shiny::textAreaInput(
        "file_create_content",
        "Conte\u00FAdo inicial (opcional)",
        value = "",
        rows = 8,
        width = "100%"
      ),
      shiny::div(
        style = "margin-top: 20px;",
        shiny::actionButton("file_create", "Criar arquivo", class = "btn-primary", style = "width: 100%; font-weight: 600; font-size: 15px; height: 42px !important;")
      )
    ),
    shiny::div(
      style = "display: flex; flex-direction: column; gap: 16px; min-height: 0;",
      shiny::div(
        class = "tr-tree-card",
        shiny::h5("Sugest\u00F5es r\u00E1pidas:", style = "margin-top: 0; margin-bottom: 12px; font-weight: 600; color: #EDEDED;"),
        shiny::p("Use o tipo para carregar um template inicial edit\u00E1vel antes de criar o arquivo.", style = "font-size: 13px; color: #A1A1AA; line-height: 1.5; margin-bottom: 12px;"),
        shiny::tags$ul(
          style = "padding-left: 18px; margin: 0; color: #D4D4D8; font-size: 13px; line-height: 1.6;",
          shiny::tags$li("Scripts: coloque em scripts/."),
          shiny::tags$li("Relat\u00F3rios: prefira reports/ para .Rmd e .qmd."),
          shiny::tags$li("Dados tabulares simples: crie CSV direto em data/raw.")
        )
      ),
      project_files_explorer_card("Arquivos Atuais do Projeto")
    )
  )
}

project_files_explorer_card <- function(title = "Arquivos Atuais do Projeto:") {
  shiny::div(
    class = "tr-tree-card",
    shiny::h5(title, style = "margin-top: 0; margin-bottom: 12px; font-weight: 600; color: #EDEDED;"),
    shiny::uiOutput("recent_files_explorer")
  )
}

project_item_choices <- function(path) {
  if (is.null(path) || !nzchar(path) || !fs::dir_exists(path)) {
    return(character(0))
  }

  items <- list.files(
    path = path,
    all.files = FALSE,
    recursive = TRUE,
    include.dirs = TRUE,
    full.names = FALSE
  )

  sort(unique(items))
}

project_format_file_choices <- function(path) {
  if (is.null(path) || !nzchar(path) || !fs::dir_exists(path)) {
    return(character(0))
  }

  files <- list.files(path = path, pattern = "\\.(R|r|Rmd|rmd|qmd)$", recursive = TRUE, full.names = FALSE)
  sort(unique(files))
}

excluir_module_ui <- function(diagnosis) {
  all_items <- project_item_choices(diagnosis$current_path)
  choices <- c("Selecione..." = "", stats::setNames(all_items, all_items))

  shiny::div(
    class = "tr-project-layout",
    shiny::div(
      style = "display: flex; flex-direction: column; gap: 15px;",
      shiny::div(
        shiny::tags$label("Arquivo/Pasta a deletar", style = "font-weight: 600; font-size: 14px; margin-bottom: 6px; display: block; color: #EDEDED;"),
        shiny::div(
          style = "display: flex; gap: 10px; align-items: flex-end;",
          shiny::div(
            style = "flex-grow: 1;",
            shiny::selectInput("file_delete_path", label = NULL, choices = choices, selected = "", width = "100%")
          ),
          shiny::actionButton("file_delete_browse", label = shiny::icon("folder-open"), class = "btn-default", style = "margin-bottom: 15px; height: 38px;", title = "Procurar arquivo no computador")
        ),
        shiny::div(
          style = "padding: 12px; border-radius: 8px; background: #111827; border: 1px solid #1F2937; color: #D1D5DB; font-size: 13px; line-height: 1.5;",
          "Arquivos .Rproj, .git, .gitignore e .Rproj.user s\u00E3o protegidos. Itens versionados no Git mostram aviso antes da exclus\u00E3o."
        ),
        shiny::div(
          style = "margin-top: 20px;",
          shiny::actionButton("file_delete", "Deletar", class = "btn-danger", style = "width: 100%; font-weight: 600; font-size: 15px; height: 42px !important;")
        )
      )
    ),
    project_files_explorer_card("Estrutura atual do projeto")
  )
}

rename_module_ui <- function(diagnosis) {
  all_items <- project_item_choices(diagnosis$current_path)
  
  choices <- if (length(all_items) == 0) c("Nenhum item (busque na pasta)" = "") else all_items
  selected <- if (length(all_items) > 0) all_items[[1]] else ""
  item_input <- shiny::selectInput("rename_source", "Origem", choices = choices, selected = selected, width = "100%")
  
  shiny::tagList(
    shiny::div(
      style = "display: flex; gap: 8px; align-items: flex-end;",
      shiny::div(style = "flex-grow: 1;", item_input),
      shiny::actionButton("rename_browse", label = shiny::icon("folder-open"), class = "btn-default", style = "margin-bottom: 15px; height: 38px;", title = "Procurar no computador")
    )
    ,
    shiny::textInput("rename_target", "Novo nome (inclua extens\u00E3o)", value = selected, width = "100%"),
    shiny::actionButton("rename_execute", "Renomear", class = "btn-default", style = "width: 100%; font-weight: 600; height: 38px !important;")
  )
}

panel_section <- function(title, ...) {
  shiny::div(
    class = "tr-section",
    shiny::h4(title),
    ...
  )
}

overview_module_ui <- function(diagnosis) {
  shiny::tagList(
    shiny::div(
      class = "tr-overview-header",
      shiny::span(class = "tr-overview-title", "Hist\u00F3rico de Commits"),
      if (diagnosis$has_commits && !is.null(diagnosis$branch) && nzchar(diagnosis$branch)) {
        shiny::span(class = "tr-overview-branch", shiny::icon("code-branch"), " ", diagnosis$branch)
      }
    ),
    if (diagnosis$has_commits) {
      shiny::HTML(render_commit_timeline_html(diagnosis))
    } else {
      render_overview_empty_state_ui(diagnosis)
    }
  )
}

render_overview_empty_state_ui <- function(diagnosis) {
  steps <- list(
    list(
      done = isTRUE(diagnosis$has_repo),
      label = "Inicializar Git",
      hint = "Cria o registro de vers\u00F5es do projeto",
      nav = "git"
    ),
    list(
      done = isTRUE(diagnosis$identity$complete),
      label = "Configurar identidade",
      hint = "Seu nome aparece nos commits",
      nav = "git"
    ),
    list(
      done = isTRUE(diagnosis$has_commits),
      label = "Fazer o primeiro commit",
      hint = "Salva a vers\u00E3o inicial do projeto",
      nav = "git"
    ),
    list(
      done = isTRUE(diagnosis$has_remote),
      label = "Conectar ao GitHub",
      hint = "Backup na nuvem e compartilhamento",
      nav = "git"
    )
  )

  next_step_nav <- NULL
  items_html <- vapply(steps, function(s) {
    if (s$done) {
      icon_html <- "<span class='tr-steps-icon done'>&#10003;</span>"
      label_style <- "color: #52525B;"
      hint_style <- "color: #3F3F46;"
    } else {
      if (is.null(next_step_nav)) next_step_nav <<- s$nav
      icon_html <- "<span class='tr-steps-icon pending'></span>"
      label_style <- "color: #EDEDED;"
      hint_style <- "color: #A1A1AA;"
    }
    sprintf(
      "<div class='tr-steps-item'><div>%s</div><div><div style='font-size: 14px; font-weight: 500; %s'>%s</div><div style='font-size: 12px; margin-top: 2px; %s'>%s</div></div></div>",
      icon_html,
      label_style, htmltools::htmlEscape(s$label),
      hint_style, htmltools::htmlEscape(s$hint)
    )
  }, character(1))

  nav_onclick <- if (!is.null(next_step_nav)) {
    sprintf("Shiny.setInputValue('module', '%s', {priority: 'event'});", next_step_nav)
  } else ""

  shiny::div(
    class = "tr-overview-empty",
    shiny::div(
      class = "tr-steps-card",
      shiny::div(
        class = "tr-steps-heading",
        "Primeiros passos para versionar seu projeto"
      ),
      shiny::div(class = "tr-steps-list", shiny::HTML(paste(items_html, collapse = ""))),
      if (nzchar(nav_onclick)) {
        shiny::tags$button(
          "Ir para Git e GitHub \u2192",
          class = "btn btn-primary tr-steps-cta",
          onclick = nav_onclick
        )
      }
    )
  )
}

panel_action_button <- function(id, label, enabled = TRUE, class = "btn-default", tooltip = NULL) {
  button_elem <- if (isTRUE(enabled)) {
    shiny::actionButton(id, label, class = class)
  } else {
    shiny::tags$button(
      id = id,
      type = "button",
      class = paste("btn", class, "action-button"),
      disabled = "disabled",
      label
    )
  }

  if (!is.null(tooltip)) {
    shiny::tags$div(class = "tr-tooltip", `data-tooltip` = tooltip, button_elem)
  } else {
    button_elem
  }
}

panel_optional_path <- function(path) {
  path <- trimws(path %||% "")
  if (nzchar(path)) path else NULL
}

panel_icon_html <- function(name, color = NULL) {
  icon_tag <- shiny::icon(name)
  if (!is.null(color) && nzchar(color)) {
    icon_tag$attribs$style <- paste0("color: ", color, ";")
  }
  as.character(icon_tag)
}

panel_summary_items <- function(diagnosis) {
  list(
    list(
      label = "Git",
      value = if (diagnosis$has_repo) "Ativo" else "Desativado",
      class = if (diagnosis$has_repo) "ok" else "error",
      title = if (diagnosis$has_repo) "Reposit\u00F3rio Git inicializado" else "Voc\u00EA precisa inicializar o Git neste projeto"
    ),
    list(
      label = "",
      value = if (diagnosis$has_commits) "Hist\u00F3rico OK" else "0 Commits",
      class = if (diagnosis$has_commits) "ok" else "warn",
      title = if (diagnosis$has_commits) "Hist\u00F3rico de vers\u00F5es existe" else "Fa\u00E7a seu primeiro commit para come\u00E7ar a salvar vers\u00F5es"
    ),
    list(
      label = "GitHub",
      value = if (diagnosis$has_remote) diagnosis$remote_name else "N\u00E3o conectado",
      class = if (diagnosis$has_remote) "ok" else "warn",
      title = if (diagnosis$has_remote) "Projeto conectado ao GitHub" else "Conecte a um reposit\u00F3rio remoto para fazer backup na nuvem"
    ),
    list(
      label = "",
      value = if (diagnosis$status_counts$total == 0) "Tudo salvo" else paste(diagnosis$status_counts$total, "Modifica\u00E7\u00F5es"),
      class = if (diagnosis$status_counts$total == 0) "ok" else "warn",
      title = if (diagnosis$status_counts$total == 0) "Projeto limpo, nada a salvar" else "Voc\u00EA tem arquivos modificados que ainda n\u00E3o foram salvos no Git",
      interactive = diagnosis$status_counts$total > 0
    )
  )
}

project_context_warning_ui <- function(panel_path, active_rstudio_path = NULL) {
  panel_path <- trimws(as.character(panel_path %||% ""))
  active_rstudio_path <- trimws(as.character(active_rstudio_path %||% ""))

  if (!nzchar(panel_path) || !nzchar(active_rstudio_path) || identical(panel_path, active_rstudio_path)) {
    return(NULL)
  }

  shiny::div(
    class = "tr-context-warning",
    shiny::strong("Projeto do painel diferente do RStudio ativo."),
    shiny::span(
      sprintf(
        "Painel: %s | RStudio: %s. Use 'Usar projeto ativo no RStudio' se quiser sincronizar.",
        basename(panel_path),
        basename(active_rstudio_path)
      )
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

render_commit_timeline_html <- function(diagnosis) {
  if (!diagnosis$has_commits) {
    return("")
  }

  history <- repo_commit_history(diagnosis$path, max_commits = 15)
  if (nrow(history) == 0) return("")

  items <- lapply(seq_len(nrow(history)), function(i) {
    row <- history[i, ]
    hash_short <- substr(row$commit, 1, 7)
    time_str <- time_ago(row$time)
    date_str <- format(row$time, "%d/%m/%Y")

    # Truncate message to first line, max 80 chars
    msg_first <- strsplit(row$message, "\n", fixed = TRUE)[[1]][1]
    msg_first <- trimws(msg_first)
    if (nchar(msg_first) > 80) {
      msg_first <- paste0(substr(msg_first, 1, 80), "\u2026")
    }

    is_head <- i == 1
    head_badge <- if (is_head) " <span class='tr-timeline-head'>HEAD</span>" else ""

    paste0(
      "<div class='tr-timeline-item", if (is_head) " tr-timeline-item-head" else "", "'>",
      "<div class='tr-timeline-dot", if (is_head) " tr-timeline-dot-head" else "", "'></div>",
      "<div class='tr-timeline-meta'>",
      "<span class='tr-timeline-time'>", time_str, "</span>",
      "<span class='tr-timeline-date'>", date_str, "</span>",
      "</div>",
      "<div class='tr-timeline-content'>",
      "<div class='tr-timeline-message'>", htmltools::htmlEscape(msg_first), head_badge, "</div>",
      "<div class='tr-timeline-footer'>",
      "<span class='tr-timeline-author'>", htmltools::htmlEscape(row$author), "</span>",
      "<span class='tr-timeline-hash'>", hash_short, "</span>",
      "</div>",
      "</div>",
      "</div>"
    )
  })

  paste0(
    "<div class='tr-timeline'>",
    paste(items, collapse = "\n"),
    "</div>"
  )
}

# Fonte unica de modelos de projeto: metadados do card + arvore de previa.
# Usada pela galeria de "Criar", pela aba "Organizar" e pela previa de arvore.
project_template_catalog <- function() {
  list(
    analise_exploratoria = list(
      icon = "search",
      title = "An\u00E1lise Explorat\u00F3ria",
      desc = "Roteiros simples e an\u00E1lise r\u00E1pida de dados.",
      tree = c(
        "data/ (Pasta de dados)",
        "  raw/ (Dados brutos de entrada)",
        "  processed/ (Dados limpos para an\u00E1lise)",
        "reports/ (Relat\u00F3rios e manuscritos)",
        "  notas.qmd (Notas e documenta\u00E7\u00E3o)",
        "scripts/ (Scripts de an\u00E1lise)",
        "  01-exploracao.R (Script inicial)",
        "figs/ (Figuras e gr\u00E1ficos gerados)",
        "README.md (Documenta\u00E7\u00E3o do projeto)",
        ".gitignore (Configura\u00E7\u00E3o do Git)"
      )
    ),
    trabalho_disciplina = list(
      icon = "book",
      title = "Trabalho da Disciplina",
      desc = "Estrutura padr\u00E3o para tarefas e entregas acad\u00EAmicas.",
      tree = c(
        "data/ (Pasta de dados)",
        "  raw/ (Dados brutos de entrada)",
        "  processed/ (Dados limpos para an\u00E1lise)",
        "reports/ (Relat\u00F3rios e manuscritos)",
        "  relatorio-final.qmd (Relat\u00F3rio final)",
        "scripts/ (Scripts de an\u00E1lise)",
        "  01-preparacao.R (Limpeza dos dados)",
        "  02-analise.R (Modelagem e gr\u00E1ficos)",
        "figs/ (Figuras e gr\u00E1ficos gerados)",
        "README.md (Documenta\u00E7\u00E3o do projeto)",
        ".gitignore (Configura\u00E7\u00E3o do Git)"
      )
    ),
    iniciacao_cientifica = list(
      icon = "flask",
      title = "Inicia\u00E7\u00E3o Cient\u00EDfica",
      desc = "Para pesquisas com relat\u00F3rios parciais e modelagem.",
      tree = c(
        "data/ (Pasta de dados)",
        "  raw/ (Dados brutos)",
        "  processed/ (Dados limpos)",
        "reports/ (Relat\u00F3rios)",
        "  plano-de-trabalho.qmd (Planejamento)",
        "  relatorio-parcial.qmd (Andamento)",
        "scripts/ (Scripts)",
        "  01-limpeza.R (Tratamento inicial)",
        "  02-modelagem.R (An\u00E1lise principal)",
        "figs/ (Figuras geradas)",
        "README.md (Descri\u00E7\u00E3o da pesquisa)",
        ".gitignore (Configura\u00E7\u00E3o do Git)"
      )
    ),
    tcc = list(
      icon = "graduation-cap",
      title = "Trabalho de Conclus\u00E3o (TCC)",
      desc = "Monografia com pastas dedicadas para dados e resultados.",
      tree = c(
        "data/ (Pasta de dados)",
        "  raw/ (Dados originais)",
        "  processed/ (Dados finais)",
        "reports/ (Manuscritos)",
        "  tcc.qmd (Arquivo principal do TCC)",
        "scripts/ (Scripts de an\u00E1lise)",
        "  01-preparacao.R (Importa\u00E7\u00E3o)",
        "  02-resultados.R (Gera\u00E7\u00E3o de tabelas)",
        "figs/ (Figuras)",
        "README.md (Apresenta\u00E7\u00E3o do TCC)",
        ".gitignore (Configura\u00E7\u00E3o do Git)"
      )
    ),
    artigo_quarto = list(
      icon = "pencil-alt",
      title = "Artigo com Quarto",
      desc = "Arquivos prontos para escrita cient\u00EDfica com Quarto (.qmd).",
      tree = c(
        "reports/ (Manuscritos)",
        "  artigo.qmd (Artigo cient\u00EDfico)",
        "_quarto.yml (Configura\u00E7\u00E3o de publica\u00E7\u00E3o)",
        "refs.bib (Refer\u00EAncias bibliogr\u00E1ficas)",
        "README.md (Descri\u00E7\u00E3o)",
        ".gitignore (Configura\u00E7\u00E3o do Git)"
      )
    ),
    projeto_grupo = list(
      icon = "users",
      title = "Projeto em Grupo",
      desc = "Inclui guias de contribui\u00E7\u00E3o e scripts compartilhados.",
      tree = c(
        "reports/ (Relat\u00F3rios do grupo)",
        "  andamento.qmd (Acompanhamento)",
        "scripts/ (Scripts compartilhados)",
        "  00-setup.R (Instala\u00E7\u00E3o e carregamento de pacotes)",
        "CONTRIBUTING.md (Guia de colabora\u00E7\u00E3o)",
        "README.md (Manual do grupo)",
        ".gitignore (Configura\u00E7\u00E3o do Git)"
      )
    )
  )
}

# Galeria de cards de modelo, compartilhada por "Criar" e "Organizar".
project_template_cards_ui <- function(input_id, selected = "analise_exploratoria") {
  catalog <- project_template_catalog()
  shiny::div(
    class = "tr-template-selector",
    shiny::radioButtons(
      input_id,
      label = NULL,
      width = "100%",
      choiceNames = unname(lapply(catalog, function(tpl) {
        shiny::HTML(sprintf(
          "<div class='tr-template-card'><div class='tr-template-title'>%s %s</div><div class='tr-template-desc'>%s</div></div>",
          panel_icon_html(tpl$icon),
          htmltools::htmlEscape(tpl$title),
          htmltools::htmlEscape(tpl$desc)
        ))
      })),
      choiceValues = names(catalog),
      selected = selected
    )
  )
}

render_project_tree_html <- function(template, include_data) {
  catalog <- project_template_catalog()
  files_list <- catalog[[template]]$tree

  if (isFALSE(include_data)) {
    files_list <- c(files_list, "data/raw/README.md (Orienta\u00E7\u00E3o de dados)", "data/processed/README.md (Orienta\u00E7\u00E3o)")
  }
  
  lines <- vapply(files_list, function(line) {
    trimmed <- trimws(line)
    rel <- sub("[[:space:]]+\\(.*$", "", trimmed, perl = TRUE)
    icon_html <- if (grepl("/$", rel)) panel_icon_html("folder-open", "#3B82F6") else panel_icon_html("file-alt", "#10B981")
    line_esc <- htmltools::htmlEscape(line)
    line_esc <- gsub("\\(([^\\)]+)\\)", "<span style='color: #71717A; font-size: 11px;'>(\\1)</span>", line_esc)
    paste0("<div style='font-family: monospace; font-size: 12px; margin-bottom: 4px; line-height: 1.4; white-space: pre;'>", icon_html, " ", line_esc, "</div>")
  }, character(1))
  
  paste(lines, collapse = "")
}

# Calcula, de forma fiel ao project_organize(), o esqueleto que sera garantido
# e marca cada item como novo (sera criado) ou existente (sera preservado).
project_organize_planned_entries <- function(path, template, include_data) {
  project_path <- normalize_project_path(path)
  project_name <- basename(project_path)

  starter <- names(template_starter_files(template, project_name))
  files <- c("README.md", starter)
  if (isTRUE(include_data)) {
    files <- c(files, "data/raw/README.md", "data/processed/README.md")
  }

  dirs <- unique(c(base_project_directories(), dirname(files)))
  dirs <- dirs[nzchar(dirs) & dirs != "."]

  entries <- rbind(
    data.frame(rel = dirs, is_dir = TRUE, stringsAsFactors = FALSE),
    data.frame(rel = files, is_dir = FALSE, stringsAsFactors = FALSE)
  )
  entries <- entries[!duplicated(entries$rel), , drop = FALSE]
  entries <- entries[order(entries$rel), , drop = FALSE]

  entries$exists <- vapply(seq_len(nrow(entries)), function(i) {
    full <- fs::path(project_path, entries$rel[i])
    fs::file_exists(full) || fs::dir_exists(full)
  }, logical(1))

  entries
}

render_simple_tree_html <- function(rel_paths, muted = TRUE) {
  if (length(rel_paths) == 0) {
    return("<div style='color: #71717A; font-style: italic; font-size: 12px;'>Projeto vazio ou sem arquivos.</div>")
  }
  base_color <- if (muted) "#71717A" else "#A1A1AA"
  lines <- vapply(sort(rel_paths), function(rel) {
    parts <- strsplit(rel, "/", fixed = TRUE)[[1]]
    indent <- paste(rep("  ", length(parts) - 1), collapse = "")
    name <- parts[length(parts)]
    is_dir <- grepl("/$", rel) || tolower(fs::path_ext(name)) == ""
    icon <- if (is_dir) panel_icon_html("folder-open", "#3B82F6") else panel_icon_html("file-alt", "#10B981")
    paste0(
      "<div style='font-family: monospace; font-size: 12px; margin-bottom: 4px; line-height: 1.4; white-space: pre; color: ", base_color, ";'>",
      htmltools::htmlEscape(indent), icon, " ", htmltools::htmlEscape(name),
      "</div>"
    )
  }, character(1))
  paste(lines, collapse = "")
}

render_project_organize_diff_html <- function(path, template, include_data) {
  if (is.null(path) || !nzchar(path) || !fs::dir_exists(path)) {
    return("<div style='color: #EF4444; font-style: italic;'>N\u00E3o foi poss\u00EDvel localizar o projeto. Tente reabrir o painel.</div>")
  }

  current <- list_project_files_tree(path)
  entries <- project_organize_planned_entries(path, template, include_data)
  n_new <- sum(!entries$exists)

  after_lines <- vapply(seq_len(nrow(entries)), function(i) {
    rel <- entries$rel[i]
    is_new <- !entries$exists[i]
    parts <- strsplit(rel, "/", fixed = TRUE)[[1]]
    indent <- paste(rep("  ", length(parts) - 1), collapse = "")
    name <- parts[length(parts)]
    icon <- if (entries$is_dir[i]) panel_icon_html("folder-open", "#3B82F6") else panel_icon_html("file-alt", "#10B981")
    if (is_new) {
      tag <- " <span style='font-size: 9px; background: #064E3B; color: #6EE7B7; padding: 1px 5px; border-radius: 4px; margin-left: 6px; font-weight: 600; text-transform: uppercase;'>novo</span>"
      color <- "#6EE7B7"
    } else {
      tag <- ""
      color <- "#52525B"
    }
    paste0(
      "<div style='font-family: monospace; font-size: 12px; margin-bottom: 4px; line-height: 1.4; white-space: pre; color: ", color, ";'>",
      htmltools::htmlEscape(indent), icon, " ", htmltools::htmlEscape(name), tag,
      "</div>"
    )
  }, character(1))

  summary_txt <- if (n_new == 0) {
    "<span style='color: #6EE7B7;'>Tudo pronto \u2014 a estrutura deste modelo j\u00E1 existe.</span>"
  } else {
    if (n_new == 1L) {
      "<span style='color: #6EE7B7; font-weight: 600;'>1 item ser\u00E1 criado</span> <span style='color: #71717A;'>\u00B7 os demais j\u00E1 existem e ficam intactos.</span>"
    } else {
      sprintf("<span style='color: #6EE7B7; font-weight: 600;'>%d itens ser\u00E3o criados</span> <span style='color: #71717A;'>\u00B7 os demais j\u00E1 existem e ficam intactos.</span>", n_new)
    }
  }

  paste0(
    "<div class='tr-organize-cols'>",
    "<div class='tr-tree-card'>",
    "<h5 style='margin-top: 0; margin-bottom: 12px; font-weight: 600; color: #EDEDED;'>Seu projeto agora</h5>",
    render_simple_tree_html(current),
    "</div>",
    "<div class='tr-tree-card'>",
    "<h5 style='margin-top: 0; margin-bottom: 12px; font-weight: 600; color: #EDEDED;'>Ap\u00F3s organizar</h5>",
    paste(after_lines, collapse = ""),
    "</div>",
    "</div>",
    "<div style='margin-top: 12px; font-size: 12px; line-height: 1.5;'>", summary_txt, "</div>"
  )
}

list_project_files_tree <- function(path) {
  # Validate path
  if (is.null(path) || !nzchar(path) || !fs::dir_exists(path)) {
    return(character())
  }

  # Recursively list all files and directories
  all_paths <- tryCatch(
    fs::dir_ls(path, recurse = TRUE, all = FALSE),
    error = function(e) {
      warning("Erro ao listar arquivos do projeto: ", e$message, call. = FALSE)
      character()
    }
  )

  if (length(all_paths) == 0) {
    return(character())
  }

  # Make paths relative
  all_paths_rel <- as.character(fs::path_rel(all_paths, start = path))

  # Filter out system/ignored paths
  ignore_patterns <- c("^\\.git", "^\\.Rproj\\.user", "^\\.Rhistory", "^\\.DS_Store", "\\.tar\\.gz$", "^\\.antigravity")
  for (pat in ignore_patterns) {
    all_paths_rel <- all_paths_rel[!grepl(pat, all_paths_rel)]
  }

  # Sort alphabetically
  sort(all_paths_rel)
}

render_project_files_explorer_html <- function(path, diagnosis, selected = "") {
  # Validate path
  if (is.null(path) || !nzchar(path) || !fs::dir_exists(path)) {
    return("<div style='color: #EF4444; font-style: italic;'>N\u00E3o foi poss\u00EDvel localizar o projeto. Tente reabrir o painel.</div>")
  }

  files <- list_project_files_tree(path)
  if (length(files) == 0) {
    return("<div style='color: #71717A; font-style: italic;'>Projeto vazio ou sem arquivos.</div>")
  }
  
  status_tbl <- diagnosis$status
  selected <- trimws(selected %||% "")
  
  lines <- vapply(files, function(file_rel) {
    full_path <- fs::path(path, file_rel)
    
    # Determine status icon/color
    status_row <- if (!is.null(status_tbl) && nrow(status_tbl) > 0) {
      status_tbl[status_tbl$file == file_rel, , drop = FALSE]
    } else {
      data.frame()
    }
    
    status_indicator <- ""
    status_style <- "color: #EDEDED;"
    
    if (nrow(status_row) > 0) {
      status <- status_row$status[[1]]
      staged <- isTRUE(status_row$staged[[1]])
      
      if (staged) {
        status_indicator <- " <span style='font-size: 9px; background: #6B21A8; color: #D8B4FE; padding: 1px 4px; border-radius: 4px; margin-left: 6px; font-weight: 500; text-transform: uppercase;'>preparado</span>"
        status_style <- "color: #D8B4FE;"
      } else if (status == "modified") {
        status_indicator <- " <span style='font-size: 9px; background: #78350F; color: #F59E0B; padding: 1px 4px; border-radius: 4px; margin-left: 6px; font-weight: 500; text-transform: uppercase;'>modificado</span>"
        status_style <- "color: #F59E0B;"
      } else if (status == "new") {
        status_indicator <- " <span style='font-size: 9px; background: #7F1D1D; color: #EF4444; padding: 1px 4px; border-radius: 4px; margin-left: 6px; font-weight: 500; text-transform: uppercase;'>n\u00E3o rastreado</span>"
        status_style <- "color: #EF4444;"
      }
    } else {
      status_style <- "color: #A1A1AA;"
    }
    
    # Indent based on depth
    parts <- strsplit(file_rel, "/", fixed = TRUE)[[1]]
    indent <- paste(rep("    ", length(parts) - 1), collapse = "")
    name <- parts[length(parts)]
    
    is_dir_val <- fs::is_dir(full_path)
    
    icon <- if (is_dir_val) {
      panel_icon_html("folder-open", "#3B82F6")
    } else {
      ext <- tolower(fs::path_ext(name))
      if (ext %in% c("qmd", "rmd")) {
        panel_icon_html("file-text", "#8B5CF6")
      } else if (ext == "r") {
        panel_icon_html("file-code", "#9333EA")
      } else if (ext %in% c("csv", "xlsx", "rds", "data")) {
        panel_icon_html("table", "#F59E0B")
      } else {
        panel_icon_html("file-alt", "#71717A")
      }
    }
    
    if (is_dir_val) {
      status_style <- "color: #EDEDED; font-weight: 500;"
    }

    escaped_path <- htmltools::htmlEscape(file_rel)
    js_path <- gsub("\\\\", "\\\\\\\\", file_rel)
    js_path <- gsub("'", "\\\\'", js_path)
    selected_class <- if (identical(file_rel, selected)) " selected" else ""

    sprintf(
      "<button type='button' class='tr-tree-item%s' title='%s' onclick=\"Shiny.setInputValue('selected_project_item', '%s', {priority: 'event'});\" style='%s'><span class='tr-tree-indent'>%s</span>%s <span>%s</span>%s</button>",
      selected_class,
      escaped_path,
      js_path,
      status_style,
      indent,
      icon,
      htmltools::htmlEscape(name),
      status_indicator
    )
  }, character(1))

  paste(lines, collapse = "")
}

# Extensoes tratadas como binarias / dados, sem preview textual.
file_preview_binary_exts <- function() {
  c(
    "rds", "rda", "rdata", "xlsx", "xls", "feather", "parquet", "sav", "dta",
    "png", "jpg", "jpeg", "gif", "bmp", "ico", "svg", "pdf", "tiff",
    "zip", "tar", "gz", "7z", "rar", "woff", "woff2", "ttf", "otf", "eot",
    "doc", "docx", "ppt", "pptx", "bin", "o", "so", "dll", "exe"
  )
}

file_preview_lang_label <- function(ext) {
  switch(
    tolower(ext),
    r = "R", rmd = "R Markdown", qmd = "Quarto", md = "Markdown",
    csv = "CSV", txt = "Texto", json = "JSON", yml = "YAML", yaml = "YAML",
    py = "Python", sql = "SQL", html = "HTML", css = "CSS", js = "JavaScript",
    "Arquivo de texto"
  )
}

# Renderiza o conteudo do arquivo selecionado com numeracao de linhas.
render_file_content_preview_html <- function(path, selected = "") {
  selected <- trimws(selected %||% "")

  if (!nzchar(selected)) {
    return(paste0(
      "<div class='tr-preview-empty'>",
      "<div class='tr-preview-empty-icon'>", panel_icon_html("file-alt"), "</div>",
      "<div class='tr-preview-empty-title'>Nenhum arquivo selecionado</div>",
      "<div class='tr-preview-empty-hint'>Clique em um arquivo na lista \u00E0 esquerda para ver o conte\u00FAdo e as a\u00E7\u00F5es dispon\u00EDveis.</div>",
      "</div>"
    ))
  }

  full_path <- fs::path(path, selected)

  if (!fs::file_exists(full_path) && !fs::dir_exists(full_path)) {
    return("<div class='tr-preview-empty'><div class='tr-preview-empty-title'>Item n\u00E3o encontrado</div></div>")
  }

  if (fs::is_dir(full_path)) {
    n_items <- tryCatch(length(fs::dir_ls(full_path)), error = function(e) 0L)
    return(paste0(
      "<div class='tr-preview-empty'>",
      "<div class='tr-preview-empty-icon'>", panel_icon_html("folder-open"), "</div>",
      "<div class='tr-preview-empty-title'>", htmltools::htmlEscape(basename(selected)), "/</div>",
      "<div class='tr-preview-empty-hint'>Pasta com ", if (n_items == 1L) "1 item" else paste(n_items, "itens"), ". Selecione um arquivo para visualizar o conte\u00FAdo.</div>",
      "</div>"
    ))
  }

  ext <- tolower(fs::path_ext(selected))
  size <- tryCatch(as.numeric(fs::file_size(full_path)), error = function(e) NA_real_)

  if (ext %in% file_preview_binary_exts()) {
    return(paste0(
      "<div class='tr-preview-empty'>",
      "<div class='tr-preview-empty-icon'>", panel_icon_html("table"), "</div>",
      "<div class='tr-preview-empty-title'>", htmltools::htmlEscape(basename(selected)), "</div>",
      "<div class='tr-preview-empty-hint'>Arquivo bin\u00E1rio (", toupper(ext), "). Abra no RStudio para visualizar o conte\u00FAdo.</div>",
      "</div>"
    ))
  }

  if (!is.na(size) && size > 200000) {
    return(paste0(
      "<div class='tr-preview-empty'>",
      "<div class='tr-preview-empty-icon'>", panel_icon_html("file-alt"), "</div>",
      "<div class='tr-preview-empty-title'>", htmltools::htmlEscape(basename(selected)), "</div>",
      "<div class='tr-preview-empty-hint'>Arquivo muito grande (", round(size / 1024), " KB). Abra no RStudio para ver o conte\u00FAdo completo.</div>",
      "</div>"
    ))
  }

  lines <- tryCatch(
    readLines(full_path, warn = FALSE, encoding = "UTF-8"),
    error = function(e) NULL
  )

  if (is.null(lines)) {
    return("<div class='tr-preview-empty'><div class='tr-preview-empty-title'>N\u00E3o foi poss\u00EDvel ler este arquivo.</div></div>")
  }

  max_lines <- 600L
  truncated <- length(lines) > max_lines
  if (truncated) {
    lines <- lines[seq_len(max_lines)]
  }

  if (length(lines) == 0) {
    body_rows <- "<div class='tr-code-row'><span class='tr-code-gutter'>1</span><span class='tr-code-line'></span></div>"
  } else {
    body_rows <- paste(
      vapply(seq_along(lines), function(i) {
        sprintf(
          "<div class='tr-code-row'><span class='tr-code-gutter'>%d</span><span class='tr-code-line'>%s</span></div>",
          i,
          htmltools::htmlEscape(lines[[i]])
        )
      }, character(1)),
      collapse = ""
    )
  }

  trunc_note <- if (truncated) {
    paste0("<div class='tr-code-truncated'>Mostrando as primeiras ", max_lines, " linhas de ", length(lines), "+ \u2014 abra no RStudio para ver tudo.</div>")
  } else {
    ""
  }

  size_label <- if (!is.na(size)) paste0(round(size / 1024, 1), " KB") else ""

  paste0(
    "<div class='tr-code-preview'>",
    "<div class='tr-code-header'>",
    "<span class='tr-code-filename'>", htmltools::htmlEscape(basename(selected)), "</span>",
    "<span class='tr-code-meta'>", file_preview_lang_label(ext), if (nzchar(size_label)) paste0(" \u00B7 ", size_label) else "", "</span>",
    "</div>",
    "<div class='tr-code-body'>", body_rows, "</div>",
    trunc_note,
    "</div>"
  )
}

# Painel contextual de acoes para o arquivo selecionado (Shiny tags reais).
file_actions_panel_ui <- function(path, diagnosis, selected = "") {
  selected <- trimws(selected %||% "")

  if (!nzchar(selected)) {
    return(
      shiny::div(
        class = "tr-actions-empty",
        shiny::div(class = "tr-actions-empty-icon", shiny::icon("hand-pointer")),
        shiny::div(class = "tr-actions-empty-text", "Selecione um arquivo na lista para ver as a\u00E7\u00F5es dispon\u00EDveis.")
      )
    )
  }

  full_path <- fs::path(path, selected)
  is_dir_val <- fs::dir_exists(full_path) && !fs::file_exists(full_path)
  ext <- tolower(fs::path_ext(selected))
  is_formattable <- !is_dir_val && ext %in% c("r", "rmd", "qmd")

  # Status Git do arquivo
  status_tbl <- diagnosis$status
  status_label <- "Sem mudan\u00E7as"
  status_class <- "ok"
  has_changes <- FALSE
  if (!is.null(status_tbl) && nrow(status_tbl) > 0) {
    row <- status_tbl[status_tbl$file == selected, , drop = FALSE]
    if (nrow(row) > 0) {
      has_changes <- TRUE
      st <- row$status[[1]]
      staged <- isTRUE(row$staged[[1]])
      if (staged) {
        status_label <- "Preparado para commit"; status_class <- "ok"
      } else if (identical(st, "modified")) {
        status_label <- "Modificado"; status_class <- "warn"
      } else if (identical(st, "new")) {
        status_label <- "N\u00E3o rastreado"; status_class <- "error"
      } else if (identical(st, "deleted")) {
        status_label <- "Removido"; status_class <- "error"
      }
    }
  }

  shiny::div(
    class = "tr-actions-panel",
    shiny::div(
      class = "tr-actions-head",
      shiny::div(class = "tr-actions-filename", basename(selected)),
      shiny::div(class = "tr-actions-path", selected)
    ),

    if (diagnosis$has_repo && has_changes) {
      shiny::div(
        class = "tr-actions-group",
        shiny::div(class = "tr-actions-group-title", "Salvar vers\u00E3o (commit)"),
        shiny::textAreaInput("act_commit_message", label = NULL, value = "", placeholder = "O que voc\u00EA fez? Ex: Corrigi o c\u00E1lculo da m\u00E9dia, Adicionei gr\u00E1fico de distribui\u00E7\u00E3o", width = "100%", rows = 5),
        shiny::actionButton("act_commit", shiny::tagList(shiny::icon("floppy-disk"), " Commit deste arquivo"), class = "btn-primary", style = "width:100%; justify-content:center;")
      )
    }
  )
}

trackR_panel_css <- function() {
  "
  /* Design tokens for consistency */
  :root {
    --gap-xs: 8px;
    --gap-sm: 12px;
    --gap-md: 16px;
    --gap-lg: 24px;
    --gap-xl: 32px;
    --transition-fast: 0.15s ease;
    --transition-normal: 0.2s ease;
  }

  /* Antigravity / Codex Aesthetic Base (Dark Mode) */
  body {
    background-color: #0D0D0D;
  }
  body, .tr-shell {
    font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif;
    color: #EDEDED;
  }
  .tr-shell {
    display: flex;
    flex-direction: column;
    height: 100vh;
    background: #161616;
  }
  
  /* Layout Panels */
  .tr-sidebar {
    background: #0D0D0D;
    padding: var(--gap-sm) var(--gap-sm) var(--gap-sm) 4px;
    border-bottom: 1px solid #2D2D2D;
    flex-shrink: 0;
    overflow-x: auto;
  }
  .tr-sidebar .shiny-options-group {
    display: flex;
    flex-wrap: nowrap;
    gap: var(--gap-xs);
    align-items: center;
    width: max-content;
  }

  .tr-main {
    background: #161616;
    padding: var(--gap-lg) var(--gap-lg);
    overflow-y: auto;
    flex-grow: 1;
  }
  
  /* miniUI title bar overrides */
  .gadget-title {
    background-color: #161616 !important;
    border-bottom: 1px solid #2D2D2D !important;
    color: #EDEDED !important;
  }
  .gadget-title h1 {
    color: #EDEDED !important;
    font-size: 16px !important;
    font-weight: 600 !important;
  }
  .gadget-title .btn {
    background: transparent !important;
    border: none !important;
    color: #A1A1AA !important;
    box-shadow: none !important;
    font-weight: 500 !important;
  }
  .gadget-title .btn:hover {
    color: #EDEDED !important;
  }

  /* Form Elements (Inputs & Selects) */
  input[type='text'], select, .form-control {
    border: 1px solid #333333 !important;
    border-radius: 6px !important;
    padding: 8px 12px !important;
    font-size: 14px !important;
    color: #EDEDED !important;
    box-shadow: none !important;
    background-color: #000000 !important;
    transition: all 0.2s ease !important;
    height: auto !important;
    min-height: 38px;
  }
  input[type='text']:focus, select:focus, .form-control:focus {
    border-color: #666666 !important;
    outline: none !important;
    box-shadow: 0 0 0 1px #666666 !important;
  }

  /* Clean Buttons */
  .btn {
    display: inline-flex;
    align-items: center;
    justify-content: center;
    height: 38px !important;
    border-radius: 6px !important;
    font-weight: 500 !important;
    font-size: 14px !important;
    padding: 0 16px !important;
    transition: all 0.2s ease !important;
    text-shadow: none !important;
    background-image: none !important;
    box-shadow: 0 1px 2px rgba(0,0,0,0.5) !important;
  }
  .btn-sm {
    height: 24px !important;
    padding: 0 10px !important;
    font-size: 10px !important;
  }
  .btn-default {
    background-color: #262626 !important;
    border: 1px solid #333333 !important;
    color: #EDEDED !important;
  }
  .btn-default:hover {
    background-color: #333333 !important;
    border-color: #404040 !important;
  }
  .btn-primary {
    background-color: #2563EB !important;
    border: 1px solid #1D4ED8 !important;
    color: #FFFFFF !important;
  }
  .btn-primary:hover {
    background-color: #1D4ED8 !important;
    border-color: #1D4ED8 !important;
  }
  .btn-success {
    background-color: #2563EB !important; /* Blue */
    border: 1px solid #1D4ED8 !important;
    color: #FFFFFF !important;
  }
  .btn-success:hover {
    background-color: #1D4ED8 !important;
    border-color: #1D4ED8 !important;
  }
  .btn-danger {
    background-color: #991B1B !important; /* Muted Red */
    border: 1px solid #7F1D1D !important;
    color: #FFFFFF !important;
  }
  .btn-danger:hover {
    background-color: #B91C1C !important;
    border-color: #B91C1C !important;
  }
  .btn[disabled],
  .btn.disabled,
  .btn[disabled]:hover,
  .btn.disabled:hover {
    background-color: #262626 !important;
    border-color: #333333 !important;
    color: #71717A !important;
    cursor: not-allowed !important;
    opacity: 0.65 !important;
    box-shadow: none !important;
  }
  /* Checkboxes */
  .tr-checkbox-group {
    display: flex;
    flex-direction: column;
    gap: var(--gap-md);
    margin-top: var(--gap-md);
    margin-bottom: var(--gap-md);
  }
  .tr-checkbox-group .shiny-input-container {
    margin: 0 !important;
  }
  .tr-checkbox-group .form-group {
    margin: 0 !important;
  }
  .tr-checkbox-group .checkbox {
    margin: 0 !important;
    padding: 0 !important;
  }
  .tr-checkbox-group .checkbox label {
    display: flex;
    align-items: center;
    gap: 12px;
    margin: 0 !important;
    padding: 0 !important;
    color: #EDEDED;
    font-size: 14px;
    min-height: 20px;
    cursor: pointer;
  }
  .tr-checkbox-group .checkbox input[type='checkbox'] {
    margin: 0 !important;
    padding: 0 !important;
    position: static !important;
    width: 16px;
    height: 16px;
    cursor: pointer;
  }
  /* Navigation Sidebar */
  .tr-sidebar input[type=radio] {
    display: none !important;
  }
  .tr-sidebar .radio {
    margin: 0 !important;
    padding: 0 !important;
  }
  .tr-sidebar .radio label {
    display: inline-block !important;
    width: auto !important;
    margin: 0 !important;
    padding: 0 !important;
    cursor: pointer;
  }
  .tr-sidebar .radio label span {
    display: inline-block;
  }
  .tr-nav-item {
    display: flex;
    align-items: center;
    gap: var(--gap-xs);
    padding: 7px 9px;
    border-radius: 6px;
    font-weight: 500;
    color: #A1A1AA;
    font-size: 13px;
    transition: all var(--transition-fast);
    white-space: nowrap;
  }
  .tr-nav-item i {
    font-size: 14px;
    color: #71717A;
    width: 16px;
    text-align: center;
    transition: color 0.15s ease;
  }
  .tr-sidebar .radio label:hover .tr-nav-item {
    background: #1C1C1E;
    color: #EDEDED;
  }
  .tr-sidebar .radio label:hover .tr-nav-item i {
    color: #A1A1AA;
  }
  .tr-sidebar input[type=radio]:checked + span .tr-nav-item {
    background: #262626;
    color: #EDEDED;
    font-weight: 500;
  }
  .tr-sidebar input[type=radio]:checked + span .tr-nav-item i {
    color: #EDEDED;
  }
  .tr-nav-group {
    display: none;
  }

  /* Sticky Header */
  .tr-summary {
    padding-bottom: 6px;
    margin-bottom: 6px;
    background: rgba(22, 22, 22, 0.98);
    backdrop-filter: blur(8px);
    position: sticky;
    top: calc(var(--gap-lg) * -1);
    z-index: 10;
  }
  .tr-summary-main {
    display: flex;
    flex-direction: column;
    gap: 8px;
  }
  .tr-summary-header {
    display: flex;
    align-items: center;
    justify-content: space-between;
    gap: 16px;
    flex-wrap: nowrap;
  }
  .tr-summary-title-wrap {
    flex: 0 1 auto;
    min-width: 0;
  }
  .tr-path {
    color: #A1A1AA;
    font-size: 13px;
    word-break: break-all;
    margin-top: 4px;
  }
  .tr-summary-grid {
    display: flex;
    flex-wrap: wrap;
    gap: var(--gap-sm);
    margin-top: 0;
    justify-content: flex-end;
    align-content: flex-start;
    flex: 1 1 0;
    min-width: 0;
  }
  .tr-context-warning {
    margin-top: 10px;
    padding: 10px 12px;
    border: 1px solid #B45309;
    border-radius: 8px;
    background: rgba(217, 119, 6, 0.12);
    color: #FDE68A;
    display: flex;
    flex-direction: column;
    gap: 4px;
    font-size: 12px;
  }
  .tr-context-warning strong {
    color: #FCD34D;
    font-size: 12px;
  }
  .tr-pill {
    border-radius: 5px;
    border: 1px solid #2D2D2D;
    padding: 5px 10px;
    background: #1C1C1C;
    display: flex;
    align-items: center;
    gap: 6px;
    flex: 0 0 auto;
    box-shadow: 0 1px 2px rgba(0,0,0,0.3);
  }
  .tr-pill.interactive {
    cursor: pointer;
    transition: all var(--transition-fast);
  }
  .tr-pill.interactive:hover {
    transform: translateX(2px);
    border-color: #555555;
    background: #262626;
  }
  .tr-pill.ok {
    border-color: #2F9E75;
    background: rgba(20, 83, 65, 0.32);
  }
  .tr-pill.warn {
    border-color: #B45309;
    background: rgba(217, 119, 6, 0.15);
  }
  .tr-pill.error {
    border-color: #991B1B;
    background: rgba(220, 38, 38, 0.15);
  }
  .tr-pill.ok .tr-pill-value { color: #B7E4CE; }
  .tr-pill.warn .tr-pill-value { color: #FCD34D; }
  .tr-pill.error .tr-pill-value { color: #FCA5A5; }

  .tr-pill-label {
    font-size: 10px;
    color: #A1A1AA;
    font-weight: 500;
  }
  .tr-pill-value {
    font-weight: 600;
    font-size: 11px;
    color: #EDEDED;
  }
  .modal-content {
    background: #161616 !important;
    color: #EDEDED !important;
    border: 1px solid #2D2D2D !important;
    border-radius: 10px !important;
    box-shadow: 0 18px 48px rgba(0, 0, 0, 0.55) !important;
  }
  .modal-header {
    background: #1C1C1C !important;
    border-bottom: 1px solid #2D2D2D !important;
    color: #EDEDED !important;
  }
  .modal-header .modal-title {
    color: #EDEDED !important;
    font-weight: 600 !important;
  }
  .modal-header .close {
    color: #A1A1AA !important;
    opacity: 1 !important;
    text-shadow: none !important;
  }
  .modal-body {
    background: #161616 !important;
    color: #D4D4D8 !important;
  }
  .modal-body p,
  .modal-body label,
  .modal-body .checkbox,
  .modal-body .checkbox label {
    color: #D4D4D8 !important;
  }
  .modal-footer {
    background: #1C1C1C !important;
    border-top: 1px solid #2D2D2D !important;
  }
  .modal-backdrop.in {
    opacity: 0.72 !important;
  }

  /* Main Sections */
  .tr-section {
    border: 1px solid #2D2D2D;
    border-radius: 8px;
    padding: var(--gap-lg);
    margin-bottom: var(--gap-lg);
    background: #1C1C1C;
    box-shadow: 0 1px 2px rgba(0,0,0,0.5);
  }
  .tr-section h4 {
    font-size: 16px;
    font-weight: 600;
    color: #EDEDED;
    margin: 0 0 var(--gap-md) 0;
    letter-spacing: -0.01em;
  }
  .tr-section .form-group {
    margin-bottom: var(--gap-md);
  }
  .tr-git-dashboard {
    display: grid;
    grid-template-columns: minmax(320px, 0.95fr) minmax(420px, 1.35fr);
    gap: var(--gap-lg);
    align-items: start;
  }
  .tr-git-next-action {
    border: 1px solid #2D2D2D;
    border-radius: 8px;
    background: #111111;
    padding: 18px;
    min-width: 0;
  }
  .tr-git-next-label {
    color: #93C5FD;
    font-size: 11px;
    font-weight: 700;
    letter-spacing: 0.04em;
    margin-bottom: 8px;
    text-transform: uppercase;
  }
  .tr-git-next-action h5 {
    color: #EDEDED;
    font-size: 18px;
    font-weight: 700;
    margin: 0 0 8px 0;
  }
  .tr-git-next-action p {
    color: #A1A1AA;
    font-size: 13px;
    line-height: 1.5;
    margin: 0 0 16px 0;
  }
  .tr-remote-summary {
    display: flex;
    flex-direction: column;
    gap: 4px;
    margin-bottom: 12px;
    padding: 10px 12px;
    border: 1px solid #2D2D2D;
    border-radius: 8px;
    background: #111827;
  }
  .tr-remote-summary-meta {
    color: #A1A1AA;
    font-size: 12px;
    word-break: break-all;
  }
  .tr-git-next-action .form-group,
  .tr-git-config-detail-body .form-group {
    max-width: 520px;
  }
  .tr-git-form-grid {
    display: grid;
    grid-template-columns: repeat(2, minmax(0, 1fr));
    gap: 14px;
    max-width: 760px;
  }
  .tr-git-config {
    min-width: 0;
  }
  .tr-git-config-title {
    color: #EDEDED;
    font-size: 13px;
    font-weight: 700;
    margin-bottom: 10px;
    text-transform: uppercase;
    letter-spacing: 0.04em;
  }
  .tr-git-config-list {
    border: 1px solid #2D2D2D;
    border-radius: 8px;
    background: #111111;
    overflow: hidden;
  }
  .tr-git-config-row {
    border-bottom: 1px solid #2D2D2D;
  }
  .tr-git-config-row:last-child {
    border-bottom: 0;
  }
  .tr-git-config-row summary {
    list-style: none;
    cursor: pointer;
  }
  .tr-git-config-row summary::-webkit-details-marker {
    display: none;
  }
  .tr-git-config-row-head {
    display: grid;
    grid-template-columns: 28px minmax(120px, 0.45fr) minmax(0, 1fr) auto;
    gap: 12px;
    align-items: center;
    padding: 12px 14px;
    min-width: 0;
  }
  .tr-git-config-state {
    align-items: center;
    border: 1px solid #3F3F46;
    border-radius: 999px;
    color: #A1A1AA;
    display: inline-flex;
    font-size: 12px;
    font-weight: 700;
    height: 24px;
    justify-content: center;
    width: 24px;
  }
  .tr-git-config-row-head.complete .tr-git-config-state {
    background: rgba(52, 211, 153, 0.14);
    border-color: #10B981;
    color: #A7F3D0;
  }
  .tr-git-config-row-head.blocked .tr-git-config-state {
    background: rgba(153, 27, 27, 0.18);
    border-color: #991B1B;
    color: #FCA5A5;
  }
  .tr-git-config-name {
    color: #EDEDED;
    font-size: 13px;
    font-weight: 700;
  }
  .tr-git-config-value {
    color: #A1A1AA;
    font-size: 13px;
    min-width: 0;
    overflow: hidden;
    text-overflow: ellipsis;
    white-space: nowrap;
  }
  .tr-git-config-action {
    color: #93C5FD;
    font-size: 12px;
    font-weight: 700;
  }
  .tr-git-config-detail-body {
    border-top: 1px solid #2D2D2D;
    padding: 14px;
    background: #0D0D0D;
  }
  .tr-git-action-row {
    display: flex;
    flex-wrap: wrap;
    gap: 10px;
    align-items: center;
  }
  .tr-git-divider {
    border-color: #2D2D2D;
    margin: 14px 0;
  }
  .tr-disabled-reason {
    display: flex;
    align-items: center;
    gap: 8px;
    color: #A1A1AA;
    font-size: 12px;
    line-height: 1.4;
    margin-top: 8px;
  }
  .tr-git-notice {
    padding: 10px 12px;
    color: #FDE68A;
    background: rgba(120, 53, 15, 0.26);
    border: 1px solid #78350F;
    border-radius: 6px;
    margin-bottom: 12px;
    font-size: 12px;
    line-height: 1.4;
  }
  .tr-log {
    margin-top: 32px;
    border: 1px solid #2D2D2D;
    border-radius: 8px;
    background: #111111;
    overflow: visible;
  }
  .tr-log-summary {
    padding: var(--gap-sm) var(--gap-md);
    background: #1C1C1C;
    cursor: pointer;
    font-size: 13px;
    font-weight: 600;
    color: #A1A1AA;
    display: flex;
    justify-content: space-between;
    align-items: center;
    user-select: none;
    transition: background var(--transition-fast), color var(--transition-fast);
  }
  .tr-log-summary:hover {
    background: #262626;
    color: #EDEDED;
  }
  .tr-log[open] .tr-log-summary {
    border-bottom: 1px solid #2D2D2D;
    color: #EDEDED;
  }
  .tr-log-hint {
    font-size: 11px;
    font-weight: 400;
    color: #71717A;
  }
  .tr-log[open] .tr-log-hint {
    display: none;
  }
  .tr-log-content {
    padding: 16px;
    overflow: visible;
  }
  .tr-log pre {
    min-height: 150px;
    max-height: 300px;
    white-space: pre-wrap;
    word-break: break-word;
    background: #000000;
    border: 1px solid #2D2D2D;
    color: #A1A1AA;
    font-family: Menlo, Monaco, Consolas, 'Liberation Mono', monospace;
    font-size: 12px;
    padding: 12px;
    border-radius: 6px;
    margin-bottom: 12px;
  }
  .tr-diff {
    border: 1px solid #2D2D2D;
    border-radius: 8px;
    overflow: auto;
    background: #0B0F14;
    font-size: 12px;
    box-shadow: inset 0 1px 0 rgba(255,255,255,0.03);
  }
  .tr-diff-line {
    display: grid;
    grid-template-columns: 34px minmax(0, 1fr);
    min-height: 24px;
    border-bottom: 1px solid rgba(45, 45, 45, 0.7);
  }
  .tr-diff-line:last-child {
    border-bottom: 0;
  }
  .tr-diff-line code {
    display: block;
    padding: 4px 10px;
    color: #D4D4D8;
    background: transparent;
    white-space: pre;
    font-family: Menlo, Monaco, Consolas, 'Liberation Mono', monospace;
    line-height: 1.45;
  }
  .tr-diff-marker {
    padding: 4px 0;
    text-align: center;
    color: #71717A;
    user-select: none;
    font-family: Menlo, Monaco, Consolas, 'Liberation Mono', monospace;
    font-weight: 700;
  }
  .tr-diff-add {
    background: rgba(20, 83, 45, 0.42);
  }
  .tr-diff-add .tr-diff-marker {
    color: #A7F3D0;
    background: rgba(22, 101, 52, 0.6);
  }
  .tr-diff-add code {
    color: #D1FAE5;
  }
  .tr-diff-remove {
    background: rgba(127, 29, 29, 0.38);
  }
  .tr-diff-remove .tr-diff-marker {
    color: #FECACA;
    background: rgba(153, 27, 27, 0.55);
  }
  .tr-diff-remove code {
    color: #FEE2E2;
  }
  .tr-diff-hunk {
    background: rgba(30, 64, 175, 0.28);
  }
  .tr-diff-hunk .tr-diff-marker {
    color: #BFDBFE;
    background: rgba(30, 64, 175, 0.5);
  }
  .tr-diff-hunk code {
    color: #BFDBFE;
    font-weight: 700;
  }
  .tr-diff-meta {
    background: #111827;
  }
  .tr-diff-meta code {
    color: #9CA3AF;
    font-weight: 600;
  }
  .tr-diff-context {
    background: #0B0F14;
  }
  .tr-diff-empty {
    border: 1px dashed #374151;
    border-radius: 8px;
    padding: 14px;
    color: #A1A1AA;
    background: #111111;
  }
  /* Project Management Layout */
  .tr-project-layout {
    display: grid;
    grid-template-columns: 1fr 1.4fr;
    gap: 32px;
    align-items: start;
    margin-top: 10px;
    min-width: 0;
  }
  .tr-project-layout--3col {
    grid-template-columns: 1fr 1.4fr;
  }
  .tr-project-layout > * {
    min-width: 0;
    overflow-wrap: break-word;
  }
  @media (max-width: 650px) {
    .tr-project-layout {
      grid-template-columns: 1fr;
    }
  }
  
  /* Template Cards */
  .tr-template-selector {
    margin-bottom: 24px;
    width: 100%;
  }
  .tr-template-selector .shiny-options-group {
    display: grid;
    grid-template-columns: repeat(2, 1fr);
    gap: 12px;
    width: 100%;
  }
  .tr-template-selector input[type=radio] {
    display: none !important;
  }
  .tr-template-selector .radio {
    margin: 0 !important;
    padding: 0 !important;
  }
  .tr-template-selector .radio label {
    display: block !important;
    margin: 0 !important;
    padding: 0 !important;
    width: 100%;
    height: 100%;
    cursor: pointer;
  }
  .tr-template-selector .radio label span {
    display: block;
    width: 100%;
    height: 100%;
  }
  .tr-template-card {
    border: 1px solid #2D2D2D;
    background: #0D0D0D;
    padding: 12px;
    border-radius: 8px;
    transition: all 0.15s ease;
    height: 100%;
    width: 100%;
    box-sizing: border-box;
    display: flex;
    flex-direction: column;
    gap: 4px;
    user-select: none;
  }
  .tr-template-card:hover {
    border-color: #444444;
    background: #121212;
  }
  .tr-template-selector input[type=radio]:checked + span .tr-template-card {
    border-color: #EDEDED;
    background: #1C1C1E;
  }
  .tr-template-title {
    font-weight: 600;
    font-size: 13px;
    color: #EDEDED;
  }
  .tr-template-desc {
    font-size: 11px;
    color: #A1A1AA;
    line-height: 1.4;
  }
  
  /* Unified File Explorer */
  .tr-explorer {
    display: flex;
    flex-direction: column;
    gap: 12px;
    height: calc(100vh - 100px);
    min-height: 600px;
  }
  .tr-explorer-top {
    display: flex;
    flex-direction: row;
    gap: 12px;
    flex: 0 0 320px;
    min-height: 0;
    align-items: stretch;
  }
  .tr-explorer-tree {
    flex: 1 1 0;
    background: #0D0D0D;
    border: 1px solid #2D2D2D;
    border-radius: 8px;
    display: flex;
    flex-direction: column;
    min-width: 0;
    overflow: hidden;
  }
  .tr-explorer-actions {
    flex: 0 0 210px;
    background: #0D0D0D;
    border: 1px solid #2D2D2D;
    border-radius: 8px;
    display: flex;
    flex-direction: column;
    min-width: 0;
    overflow-y: auto;
    padding: 11px;
  }
  .tr-explorer-content {
    flex: 1 1 0;
    background: #0D0D0D;
    border: 1px solid #2D2D2D;
    border-radius: 8px;
    display: flex;
    flex-direction: column;
    min-height: 0;
    min-width: 0;
    overflow: hidden;
  }
  .tr-preview-tabs {
    display: flex;
    gap: 0;
    border-bottom: 1px solid #2D2D2D;
    flex-shrink: 0;
    background: #111111;
  }
  .tr-preview-tab {
    background: transparent;
    border: none;
    border-bottom: 2px solid transparent;
    color: #71717A;
    font-size: 12px;
    font-weight: 500;
    padding: 8px 16px;
    cursor: pointer;
    display: flex;
    align-items: center;
    gap: 6px;
    transition: color 0.15s, border-color 0.15s;
  }
  .tr-preview-tab:hover { color: #EDEDED; }
  .tr-preview-tab.active { color: #EDEDED; border-bottom-color: #6366F1; }
  .tr-preview-pane {
    display: none;
    flex: 1 1 0;
    min-height: 0;
    overflow: auto;
  }
  .tr-preview-pane.active { display: flex; flex-direction: column; }
  .tr-explorer-toolbar {
    display: flex;
    flex-wrap: wrap;
    gap: 3px;
    padding: 7px 8px;
    border-bottom: 1px solid #2D2D2D;
    flex-shrink: 0;
  }
  .tr-explorer-toolbar .btn {
    height: 26px !important;
    padding: 0 9px !important;
    font-size: 11px !important;
    white-space: nowrap;
  }
  .tr-explorer-tree-body {
    padding: 10px;
    overflow-y: auto;
    flex-grow: 1;
    min-height: 0;
  }

  /* Code preview (center column) */
  .tr-code-preview {
    display: flex;
    flex-direction: column;
    flex: 1 1 0;
    min-height: 0;
    min-width: 0;
  }
  .tr-code-header {
    display: flex;
    align-items: center;
    justify-content: flex-start;
    gap: 12px;
    padding: 10px 16px;
    border-bottom: 1px solid #2D2D2D;
    flex-shrink: 0;
  }
  .tr-code-header .btn {
    flex-shrink: 0;
    height: 32px !important;
    padding: 0 10px !important;
    font-size: 12px !important;
  }
  .tr-code-filename {
    font-weight: 500;
    font-size: 12px;
    color: #EDEDED;
    font-family: 'SFMono-Regular', Consolas, 'Liberation Mono', Menlo, monospace;
    flex-shrink: 0;
  }
  .tr-code-meta {
    font-size: 11px;
    color: #71717A;
    white-space: nowrap;
    margin-left: auto;
  }
  .tr-code-body {
    overflow: auto;
    padding: 8px 0;
    flex-grow: 1;
    width: 100%;
  }
  .tr-code-row {
    display: flex;
    font-family: 'SFMono-Regular', Consolas, 'Liberation Mono', Menlo, monospace;
    font-size: 12.5px;
    line-height: 1.55;
  }
  .tr-code-row:hover {
    background: #161616;
  }
  .tr-code-gutter {
    flex-shrink: 0;
    width: 44px;
    text-align: right;
    padding-right: 14px;
    color: #4B4B52;
    user-select: none;
  }
  .tr-code-line {
    white-space: pre;
    color: #D4D4D8;
    padding-right: 16px;
  }
  .tr-code-truncated {
    padding: 10px 16px;
    font-size: 12px;
    color: #A1A1AA;
    border-top: 1px solid #2D2D2D;
    background: #111111;
  }
  .tr-preview-empty {
    display: flex;
    flex-direction: column;
    align-items: center;
    justify-content: center;
    text-align: center;
    height: 100%;
    min-height: 320px;
    padding: 32px;
    gap: 8px;
  }
  .tr-preview-empty-icon {
    font-size: 38px;
    opacity: 0.5;
  }
  .tr-preview-empty-title {
    font-size: 15px;
    font-weight: 600;
    color: #D4D4D8;
  }
  .tr-preview-empty-hint {
    font-size: 13px;
    color: #71717A;
    max-width: 340px;
    line-height: 1.5;
  }

  /* Actions panel (right column) */
  .tr-actions-empty {
    display: flex;
    flex-direction: column;
    align-items: center;
    justify-content: center;
    text-align: center;
    gap: 12px;
    height: 100%;
    min-height: 280px;
    color: #71717A;
  }
  .tr-actions-empty-icon {
    font-size: 26px;
    opacity: 0.5;
  }
  .tr-actions-empty-text {
    font-size: 13px;
    max-width: 220px;
    line-height: 1.5;
  }
  .tr-actions-head {
    padding-bottom: 11px;
    border-bottom: 1px solid #2D2D2D;
    margin-bottom: 13px;
  }
  .tr-actions-filename {
    font-weight: 600;
    font-size: 12px;
    color: #EDEDED;
    word-break: break-word;
  }
  .tr-actions-path {
    font-size: 9px;
    color: #71717A;
    word-break: break-all;
    margin: 2px 0 8px 0;
  }
  .tr-actions-group {
    margin-bottom: 16px;
  }
  .tr-actions-group-title {
    font-size: 9px;
    font-weight: 600;
    text-transform: uppercase;
    letter-spacing: 0.5px;
    color: #71717A;
    margin-bottom: 8px;
  }
  .tr-actions-danger .tr-actions-group-title {
    color: #F87171;
  }
  .tr-inline-diff {
    border-top: 1px solid #2D2D2D;
    padding: 14px 16px 16px;
    overflow: auto;
    max-height: 520px;
    background: #09090B;
  }
  .tr-inline-diff::before {
    content: 'Diff deste arquivo';
    display: block;
    color: #A1A1AA;
    font-size: 11px;
    font-weight: 700;
    letter-spacing: 0.05em;
    margin-bottom: 10px;
    text-transform: uppercase;
  }

  /* Tree Card and Previews */
  .tr-tree-card {
    background: #0D0D0D;
    border: 1px solid #2D2D2D;
    padding: 16px;
    border-radius: 8px;
    overflow-y: auto;
    max-height: 500px;
    min-height: 300px;
    width: 100%;
  }
  .tr-tree-item {
    width: 100%;
    display: block;
    border: 0;
    border-radius: 6px;
    background: transparent;
    padding: 4px 6px;
    margin-bottom: 3px;
    color: #A1A1AA;
    font-family: monospace;
    font-size: 12px;
    line-height: 1.4;
    text-align: left;
    white-space: pre;
    cursor: pointer;
    transition: background var(--transition-fast), color var(--transition-fast);
  }
  .tr-tree-item:hover {
    background: #1C1C1E;
    color: #EDEDED;
  }
  .tr-tree-item.selected {
    background: rgba(59, 130, 246, 0.14);
    outline: 1px solid #1E40AF;
  }
  .tr-tree-indent {
    white-space: pre;
  }

  /* Organize diff (Atual -> Depois) */
  .tr-organize-cols {
    display: grid;
    grid-template-columns: 1fr 1fr;
    gap: 16px;
  }
  .tr-organize-cols .tr-tree-card {
    max-height: 420px;
  }
  @media (max-width: 720px) {
    .tr-organize-cols {
      grid-template-columns: 1fr;
    }
  }

  /* Recent Projects */
  .tr-recent-projects-grid {
    background: #0D0D0D;
    border: 1px solid #2D2D2D;
    padding: 16px;
    border-radius: 8px;
    max-height: 350px;
    overflow-y: auto;
    display: flex;
    flex-direction: column;
    gap: 8px;
  }
  .tr-recent-project-card {
    background: #161616;
    border: 1px solid #2D2D2D;
    padding: 10px 12px;
    border-radius: 6px;
    cursor: pointer;
    transition: all 0.15s ease;
    display: block;
    text-align: left;
  }
  .tr-recent-project-card:hover {
    border-color: #444444;
    background: #1C1C1E;
  }
  .tr-recent-name {
    font-weight: 600;
    font-size: 13px;
    color: #EDEDED;
  }
  .tr-recent-dir {
    font-size: 11px;
    color: #71717A;
    word-break: break-all;
    margin-top: 2px;
  }
  


  /* Bootstrap Nav Pills Overrides */
  .nav-pills {
    border-bottom: 1px solid #2D2D2D !important;
    margin-bottom: 20px !important;
    padding-bottom: 8px !important;
  }
  .nav-pills > li {
    margin-right: 4px !important;
  }
  .nav-pills > li > a {
    color: #A1A1AA !important;
    background-color: transparent !important;
    font-weight: 500 !important;
    font-size: 13px !important;
    border: 1px solid transparent !important;
    border-radius: 6px !important;
    padding: 6px 12px !important;
    transition: all 0.15s ease !important;
  }
  .nav-pills > li > a:hover {
    color: #EDEDED !important;
    background-color: #1C1C1E !important;
  }
  .nav-pills > li.active > a, 
  .nav-pills > li.active > a:hover, 
  .nav-pills > li.active > a:focus {
    color: #EDEDED !important;
    background-color: #262626 !important;
    border-color: #2D2D2D !important;
  }

  /* Timeline */
  /* Overview header */
  .tr-overview-header {
    display: flex;
    align-items: center;
    gap: 12px;
    margin-bottom: 20px;
  }
  .tr-overview-title {
    font-size: 17px;
    font-weight: 600;
    color: #EDEDED;
  }
  .tr-overview-branch {
    font-size: 12px;
    color: #93C5FD;
    background: rgba(59, 130, 246, 0.12);
    border: 1px solid rgba(59, 130, 246, 0.3);
    padding: 2px 8px;
    border-radius: 12px;
    font-family: Menlo, Monaco, Consolas, monospace;
  }

  /* Timeline */
  .tr-timeline {
    position: relative;
    padding-left: 20px;
    border-left: 2px solid #1E3A8A;
  }
  .tr-timeline-item {
    position: relative;
    margin-bottom: 14px;
    display: grid;
    grid-template-columns: 68px 1fr;
    gap: 12px;
    align-items: start;
  }
  .tr-timeline-item:last-child {
    margin-bottom: 0;
  }
  .tr-timeline-dot {
    position: absolute;
    left: -26px;
    top: 9px;
    width: 10px;
    height: 10px;
    border-radius: 50%;
    background: #1E3A8A;
    border: 2px solid #3B82F6;
    z-index: 1;
  }
  .tr-timeline-dot-head {
    background: #3B82F6;
    box-shadow: 0 0 6px rgba(59, 130, 246, 0.7);
  }
  .tr-timeline-meta {
    display: flex;
    flex-direction: column;
    align-items: flex-end;
    padding-top: 8px;
  }
  .tr-timeline-time {
    color: #A1A1AA;
    font-size: 11px;
    line-height: 1.4;
  }
  .tr-timeline-date {
    color: #52525B;
    font-size: 10px;
    line-height: 1.4;
  }
  .tr-timeline-content {
    background: #111827;
    border: 1px solid #1F2937;
    border-radius: 6px;
    padding: 10px 14px;
    transition: border-color 0.12s;
  }
  .tr-timeline-item-head .tr-timeline-content {
    background: #1E3A8A;
    border-color: #3B82F6;
  }
  .tr-timeline-content:hover {
    border-color: #3B82F6;
  }
  .tr-timeline-message {
    font-size: 13px;
    color: #EDEDED;
    font-weight: 500;
    line-height: 1.45;
    margin-bottom: 6px;
    word-break: break-word;
  }
  .tr-timeline-head {
    font-size: 9px;
    font-weight: 700;
    letter-spacing: 0.05em;
    text-transform: uppercase;
    color: #93C5FD;
    background: rgba(59, 130, 246, 0.2);
    padding: 1px 5px;
    border-radius: 4px;
    margin-left: 6px;
    vertical-align: middle;
  }
  .tr-timeline-footer {
    display: flex;
    align-items: center;
    gap: 10px;
  }
  .tr-timeline-author {
    font-size: 11px;
    color: #71717A;
  }
  .tr-timeline-hash {
    font-family: Menlo, Monaco, Consolas, monospace;
    font-size: 10px;
    color: #BFDBFE;
    background: rgba(59, 130, 246, 0.12);
    padding: 1px 5px;
    border-radius: 3px;
    display: inline-block;
  }

  /* Overview empty state / primeiros passos */
  .tr-overview-empty {
    padding: 8px 0;
  }
  .tr-steps-card {
    background: #111827;
    border: 1px solid #1F2937;
    border-radius: 8px;
    padding: 20px 24px;
    max-width: 440px;
  }
  .tr-steps-heading {
    font-size: 14px;
    font-weight: 600;
    color: #EDEDED;
    margin-bottom: 16px;
  }
  .tr-steps-list {
    display: flex;
    flex-direction: column;
    gap: 12px;
    margin-bottom: 20px;
  }
  .tr-steps-item {
    display: flex;
    align-items: flex-start;
    gap: 12px;
  }
  .tr-steps-icon {
    width: 20px;
    height: 20px;
    border-radius: 50%;
    flex-shrink: 0;
    margin-top: 1px;
    display: flex;
    align-items: center;
    justify-content: center;
    font-size: 11px;
    font-weight: 700;
  }
  .tr-steps-icon.done {
    background: #064E3B;
    color: #6EE7B7;
    border: 1px solid #065F46;
  }
  .tr-steps-icon.pending {
    background: transparent;
    border: 2px solid #374151;
  }
  .tr-steps-cta {
    width: 100%;
    font-weight: 600;
    font-size: 14px;
  }

  @media (max-width: 900px) {
    .tr-shell {
      grid-template-columns: 1fr;
      height: auto;
    }
    .tr-git-dashboard,
    .tr-git-form-grid {
      grid-template-columns: 1fr;
    }
    .tr-git-config-row-head {
      grid-template-columns: 28px minmax(0, 1fr) auto;
    }
    .tr-git-config-value {
      grid-column: 2 / -1;
      white-space: normal;
    }
    .tr-summary-grid {
      justify-content: flex-end;
      min-width: 0;
    }
  }

  /* Tooltips */
  .tr-tooltip {
    position: relative;
    display: inline-block;
  }
  .tr-tooltip[data-tooltip]::after {
    content: attr(data-tooltip);
    position: absolute;
    bottom: -40px;
    left: 50%;
    transform: translateX(-50%);
    background-color: #2D2D2D;
    color: #EDEDED;
    padding: 8px 12px;
    border-radius: 6px;
    font-size: 12px;
    white-space: nowrap;
    z-index: 1000;
    pointer-events: none;
    opacity: 0;
    transition: opacity 0.15s ease;
    box-shadow: 0 2px 8px rgba(0, 0, 0, 0.5);
    border: 1px solid #444444;
  }
  .tr-tooltip[data-tooltip]::before {
    content: '';
    position: absolute;
    bottom: -44px;
    left: 50%;
    transform: translateX(-50%);
    border: 6px solid transparent;
    border-top-color: #2D2D2D;
    z-index: 1001;
    opacity: 0;
    transition: opacity 0.15s ease;
  }
  .tr-tooltip[data-tooltip]:hover::after,
  .tr-tooltip[data-tooltip]:hover::before {
    opacity: 1;
  }
  "

}
