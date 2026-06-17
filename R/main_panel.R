#' Abre o painel principal do trackR
#'
#' Centraliza as acoes do pacote em uma interface Shiny com navegacao por
#' modulos.
#'
#' @param path Caminho do projeto.
#'
#' @return Invisivelmente, o caminho analisado.
#' @export
trackR <- function(path = active_project_path()) {
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
              shiny::HTML(paste("<div class='tr-nav-item'>", shiny::icon("heartbeat"), "Visão Geral</div>")),
              shiny::HTML(paste("<div class='tr-nav-item'>", shiny::icon("folder"), "Gerenciar Projeto</div>")),
              shiny::HTML(paste("<div class='tr-nav-item'>", shiny::icon("file-alt"), "Arquivos e Código</div>")),
              shiny::HTML(paste("<div class='tr-nav-item'>", shiny::icon("github"), "Git e GitHub</div>")),
              shiny::HTML(paste("<div class='tr-nav-item'>", shiny::icon("chart-bar"), "Relatórios</div>"))
            ),
            choiceValues = c("overview", "project", "files", "git", "reports"),
            selected = default_module
          )
        ),
        shiny::div(
          class = "tr-main",
          shiny::uiOutput("project_summary"),
          shiny::uiOutput("module_ui"),
          shiny::tags$details(
            class = "tr-log",
            shiny::tags$summary(
              class = "tr-log-summary",
              shiny::tags$span(shiny::icon("terminal"), " Terminal de Execução"),
              shiny::tags$span(class = "tr-log-hint", "Clique para expandir")
            ),
            shiny::div(
              class = "tr-log-content",
              shiny::verbatimTextOutput("action_log"),
              shiny::div(
                style = "display: flex; gap: 10px;",
                shiny::actionButton("refresh_all", "Atualizar estado", class = "btn-default btn-sm"),
                shiny::actionButton("refresh_project_path", "Sincronizar com RStudio", class = "btn-default btn-sm", icon = shiny::icon("sync"))
              )
            )
          )
        )
      )
    )
  )
}

trackR_panel_server <- function(project_path, initial_diagnosis = NULL) {
  force(project_path)

  function(input, output, session) {
    project_choices <- shiny::reactiveVal(named_project_choices(default_projects_directory()))
    
    shiny::observeEvent(input$project_base_dir, {
      path <- trimws(input$project_base_dir %||% "")
      if (nzchar(path) && fs::dir_exists(path)) {
        project_choices(named_project_choices(path))
      }
    })

    if (is.null(initial_diagnosis)) {
      initial_diagnosis <- build_git_diagnosis(project_path)
    }
    diagnosis_state <- shiny::reactiveVal(initial_diagnosis)
    values <- shiny::reactiveValues(
      log = paste(diagnosis_lines(initial_diagnosis), collapse = "\n"),
      diff_html = format_diff_for_panel_html(character()),
      pending_delete = NULL
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
      stats <- if (identical(input$module %||% "overview", "git")) {
        git_status_badge_items(diagnosis)
      } else {
        panel_summary_items(diagnosis)
      }
      
      project_name <- if (diagnosis$is_rstudio_project) basename(diagnosis$rproj_path) else basename(diagnosis$current_path)

      shiny::div(
        class = "tr-summary",
        shiny::div(
          class = "tr-summary-main",
          shiny::div(
            class = "tr-summary-header",
            shiny::div(
              style = "margin-bottom: 4px;",
              shiny::strong(project_name, style = "font-size: 22px; color: #60A5FA; letter-spacing: -0.5px;")
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
          shiny::div(class = "tr-path", style = "font-size: 12px; color: #71717A;", diagnosis$current_path)
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
        reports = reports_module_ui(d()),
        overview_module_ui(d())
      )
    })

    output$action_log <- shiny::renderText(values$log)

    output$diff_view <- shiny::renderUI({
      shiny::HTML(values$diff_html)
    })

    output$project_structure_preview <- shiny::renderUI({
      shiny::HTML(render_project_tree_html(input$project_template %||% "analise_exploratoria", isTRUE(input$project_include_data)))
    })
    
    output$project_structure_preview_organize <- shiny::renderUI({
      shiny::HTML(render_project_tree_html(input$project_structure_template %||% "analise_exploratoria", isTRUE(input$project_structure_include_data)))
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
          "<span style='color: #10B981; margin-right: 8px;'>✓</span>"
        } else {
          "<span style='color: #EF4444; margin-right: 8px;'>✗</span>"
        }
        label <- if (exists) {
          paste0("<span style='color: #EDEDED;'>", f, "/</span> <span style='color: #71717A; font-size: 11px;'>(Já existe)</span>")
        } else {
          paste0("<span style='color: #A1A1AA;'>", f, "/</span> <span style='color: #71717A; font-size: 11px;'>(Será criada)</span>")
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
      shiny::HTML(render_project_files_explorer_html(project_path, d(), input$selected_project_item %||% ""))
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
      shiny::showNotification("Navegando para Arquivos e Código → Versionar", type = "message", duration = 2)
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$refresh_all, {
      diagnosis <- refresh_panel_state()
      set_log(paste(diagnosis_lines(diagnosis), collapse = "\n"))
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$refresh_project_path, {
      new_path <- active_project_path()
      if (new_path != project_path) {
        project_path <<- new_path
      }
      run_panel_action(
        list(ok = TRUE, output = paste("Painel sincronizado com a pasta atual:", project_path)),
        refresh = TRUE
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

    shiny::observeEvent(input$project_create, {
      project_name <- trimws(input$project_name %||% "")
      if (!nzchar(project_name)) {
        shiny::showNotification("Informe um nome para o projeto.", type = "error")
        return()
      }

      target_path <- fs::path(normalize_project_path(input$project_base_dir), project_name)
      extra_files <- split_extra_file_lines(input$project_extra_files)

      run_panel_action(
        project_create(
          path = target_path,
          template = input$project_template,
          include_data = isTRUE(input$project_include_data),
          initialize_git = isTRUE(input$project_initialize_git),
          open = isTRUE(input$project_open_after_create),
          extra_files = extra_files
        ),
        refresh = FALSE
      )
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$git_save_identity, {
      name <- trimws(input$git_user_name %||% "")
      email <- trimws(input$git_user_email %||% "")
      if (!nzchar(name) || !nzchar(email)) {
        shiny::showNotification("Preencha nome e email.", type = "error")
        return()
      }
      run_panel_action(git_set_identity(name, email))
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$project_open, {
      selected <- input$project_to_open %||% ""
      if (!nzchar(selected)) {
        shiny::showNotification("Nenhum projeto foi informado.", type = "error")
        return()
      }

      run_panel_action(project_open(selected), refresh = FALSE)
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$project_structure, {
      run_panel_action(
        project_organize(
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
        file_import(
          source = input$file_source,
          destination = input$file_destination,
          path = project_path,
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
      selected <- trimws(input$selected_project_item %||% "")
      if (!nzchar(selected)) {
        return()
      }

      selected_full_path <- fs::path(project_path, selected)
      all_items <- project_item_choices(project_path)

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
          format_files <- project_format_file_choices(project_path)
          shiny::updateSelectInput(
            session,
            "format_path",
            choices = unique(c("Nenhum script (busque na pasta)" = "", stats::setNames(format_files, format_files), stats::setNames(selected, selected))),
            selected = selected
          )
        }
      }
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$file_create, {
      run_panel_action(
        file_create(
          filename = input$file_create_name,
          type = input$file_create_type,
          destination = input$file_create_destination,
          path = project_path,
          content = input$file_create_content,
          open_in_rstudio = TRUE
        )
      )
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$file_delete_browse, {
      selected <- choose_project_file()
      if (!is.null(selected) && nzchar(selected)) {
        rel_path <- tryCatch(relative_project_path(selected, project_path), error = function(e) selected)
        choices <- project_item_choices(project_path)
        shiny::updateSelectInput(
          session,
          "file_delete_path",
          choices = unique(c("Selecione..." = "", stats::setNames(choices, choices), stats::setNames(rel_path, rel_path))),
          selected = rel_path
        )
      }
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$file_delete, {
      info <- file_delete_info(input$file_delete_path, path = project_path)
      if (!isTRUE(info$ok)) {
        set_log(info$message)
        shiny::showNotification(info$message, type = "error")
        return()
      }

      values$pending_delete <- info$relative_path

      tracked_warning <- if (isTRUE(info$was_tracked)) {
        shiny::div(
          style = "margin-top: 12px; padding: 12px; border-radius: 8px; background: #3B1D1F; color: #FECACA; border: 1px solid #7F1D1D;",
          "Este item está rastreado no Git. A exclusão vai aparecer como remoção nas mudanças do projeto."
        )
      } else {
        NULL
      }

      shiny::showModal(
        shiny::modalDialog(
          title = "Confirmar exclusão",
          shiny::p(sprintf("Você quer deletar %s?", info$label)),
          shiny::p("Esta ação remove o item do disco local."),
          tracked_warning,
          if (isTRUE(info$was_tracked)) {
            shiny::checkboxInput("file_delete_remove_from_git", "Preparar a remoção no Git também", value = TRUE)
          },
          easyClose = TRUE,
          footer = shiny::tagList(
            shiny::modalButton("Cancelar"),
            shiny::actionButton("confirm_file_delete", "Confirmar exclusão", class = "btn-danger")
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
          path = project_path,
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
      run_panel_action(git_init(project_path))
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$gitignore_write, {
      run_panel_action(
        git_ignore(
          project_path,
          include_data = !isTRUE(input$gitignore_ignore_data)
        )
      )
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$git_status, {
      run_panel_action(git_status(project_path))
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$git_commit, {
      run_panel_action(
        git_commit_all(
          message = input$commit_message %||% "Primeiro commit",
          path = project_path
        )
      )
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$changes_refresh, {
      diagnosis <- refresh_panel_state()
      values$diff_html <- format_diff_for_panel_html(character())
      shiny::showNotification("Lista de mudanças atualizada.", type = "message", duration = 3)
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$changes_diff, {
      result <- git_diff(
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
      run_panel_action(git_stage(input$changes_files, path = project_path))
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$changes_unstage, {
      run_panel_action(git_unstage(input$changes_files, path = project_path))
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$changes_discard, {
      shiny::showModal(
        shiny::modalDialog(
          title = "Cuidado: Descarte Definitivo",
          "Tem certeza de que deseja descartar as mudanças locais nos arquivos selecionados? Esta ação não pode ser desfeita.",
          footer = shiny::tagList(
            shiny::modalButton("Cancelar"),
            shiny::actionButton("confirm_discard_action", "Confirmar Descarte", class = "btn-danger")
          )
        )
      )
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$confirm_discard_action, {
      shiny::removeModal()
      run_panel_action(git_discard(input$changes_files, path = project_path))
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$changes_commit_selected, {
      selected <- normalize_git_file_selection(input$changes_files)
      if (length(selected) > 0) {
        stage_output <- capture_lines(git_stage(selected, path = project_path))
        commit_output <- capture_lines(git_commit(input$changes_commit_message, path = project_path))
        set_log(paste(c(stage_output, commit_output), collapse = "\n"))
        refresh_panel_state()
      } else {
        run_panel_action(git_commit(input$changes_commit_message, path = project_path))
      }
    }, ignoreInit = TRUE)

    # Observers of removed buttons git_stage_commit_files and git_commit_staged were deleted.

    shiny::observeEvent(input$github_connect, {
      run_panel_action(
        github_connect(
          remote_url = input$github_remote_url,
          path = project_path,
          replace = isTRUE(input$github_replace_remote)
        )
      )
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$github_auth, {
      run_panel_action(github_check(path = project_path))
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$github_push, {
      run_panel_action(git_push(path = project_path))
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$github_sync, {
      run_panel_action(git_sync(path = project_path))
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$report_browse, {
      file <- tryCatch(rstudioapi::selectFile(caption = "Selecionar Relatório", filter = "Quarto/RMarkdown (*.qmd *.Rmd *.rmd)", existing = TRUE), error = function(e) NULL)
      if (!is.null(file) && nzchar(file)) {
        rel_path <- tryCatch(relative_project_path(file, project_path), error = function(e) file)
        all_items <- list.files(path = project_path, pattern = "\\.(qmd|Rmd|rmd)$", recursive = TRUE, full.names = FALSE)
        shiny::updateSelectInput(session, "report_path", choices = unique(c(rel_path, all_items)), selected = rel_path)
      }
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$report_preview, {
      run_panel_action(
        report_preview(
          path = panel_optional_path(input$report_path),
          style = isTRUE(input$report_style)
        ),
        refresh = FALSE
      )
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$report_live_preview, {
      run_panel_action(
        report_live_preview(
          path = panel_optional_path(input$report_path),
          style = isTRUE(input$report_style)
        ),
        refresh = FALSE
      )
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
        rel_path <- tryCatch(relative_project_path(file, project_path), error = function(e) file)
        all_items <- list.files(path = project_path, all.files = FALSE, recursive = TRUE, include.dirs = TRUE, full.names = FALSE)
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
      run_panel_action(code_format_all(project_path), refresh = FALSE)
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$rename_execute, {
      source <- panel_optional_path(input$rename_source)
      target <- panel_optional_path(input$rename_target)
      if (!is.null(source) && nzchar(source) && !is.null(target) && nzchar(target)) {
        run_panel_action(file_rename(source, target, path = project_path))
      } else {
        run_panel_action(list(ok = FALSE, output = "Selecione o arquivo/pasta e preencha o novo nome."), refresh = FALSE)
      }
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
  panel_section(
    "Gerenciar Projeto",
    shiny::tabsetPanel(
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
              shiny::tags$summary("Opções Avançadas", style = "margin-bottom: 8px; cursor: pointer; color: #A1A1AA; font-weight: 600; outline: none;"),
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
            shiny::div(
              class = "tr-template-selector",
              shiny::radioButtons(
                "project_template",
                label = NULL,
                width = "100%",
                choiceNames = list(
                  shiny::HTML("<div class='tr-template-card'><div class='tr-template-title'>🔍 Análise Exploratória</div><div class='tr-template-desc'>Roteiros simples e análise rápida de dados.</div></div>"),
                  shiny::HTML("<div class='tr-template-card'><div class='tr-template-title'>📚 Trabalho da Disciplina</div><div class='tr-template-desc'>Estrutura padrão para tarefas e entregas acadêmicas.</div></div>"),
                  shiny::HTML("<div class='tr-template-card'><div class='tr-template-title'>🧪 Iniciação Científica</div><div class='tr-template-desc'>Para pesquisas com relatórios parciais e modelagem.</div></div>"),
                  shiny::HTML("<div class='tr-template-card'><div class='tr-template-title'>🎓 Trabalho de Conclusão (TCC)</div><div class='tr-template-desc'>Monografia com pastas dedicadas para dados e resultados.</div></div>"),
                  shiny::HTML("<div class='tr-template-card'><div class='tr-template-title'>📝 Artigo com Quarto</div><div class='tr-template-desc'>Arquivos prontos para escrita científica com Quarto (.qmd).</div></div>"),
                  shiny::HTML("<div class='tr-template-card'><div class='tr-template-title'>👥 Projeto em Grupo</div><div class='tr-template-desc'>Inclui guias de contribuição e scripts compartilhados.</div></div>")
                ),
                choiceValues = c(
                  "analise_exploratoria",
                  "trabalho_disciplina",
                  "iniciacao_cientifica",
                  "tcc",
                  "artigo_quarto",
                  "projeto_grupo"
                ),
                selected = "analise_exploratoria"
              )
            ),
            shiny::div(
              class = "tr-tree-card",
              shiny::h5("Prévia da Estrutura a ser Criada:", style = "margin-top: 0; margin-bottom: 12px; font-weight: 600; color: #EDEDED;"),
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
      )
    )
  )
}

files_module_ui <- function(diagnosis) {
  panel_section(
    "Arquivos e Código",
    shiny::tabsetPanel(
      type = "pills",
      shiny::tabPanel(
        "Importar",
        shiny::br(),
        shiny::div(
          class = "tr-project-layout",
          shiny::div(
            style = "display: flex; flex-direction: column; gap: 15px;",
            shiny::div(
              shiny::tags$label("Arquivo de origem", style = "font-weight: 600; font-size: 14px; margin-bottom: 6px; display: block; color: #EDEDED;"),
              shiny::div(
                style = "display: flex; gap: 10px; align-items: stretch;",
                shiny::div(
                  style = "flex-grow: 1;",
                  shiny::textInput("file_source", label = NULL, value = "", width = "100%")
                ),
                shiny::actionButton("file_choose_source", "Escolher...")
              )
            ),
            shiny::selectInput(
              "file_destination",
              "Pasta de destino no projeto",
              choices = c(
                "Dados originais (data/raw)" = "data/raw",
                "Dados tratados (data/processed)" = "data/processed",
                "Relatórios Quarto (reports)" = "reports",
                "Scripts R (scripts)" = "scripts",
                "Figuras/Gráficos (figs)" = "figs"
              ),
              selected = "data/raw"
            ),
            shiny::div(
              class = "tr-checkbox-group",
              shiny::checkboxInput("file_move", "Mover em vez de copiar arquivo", value = FALSE),
              shiny::checkboxInput("file_overwrite", "Substituir arquivo existente", value = FALSE),
              shiny::checkboxInput("file_add_to_git", "Adicionar ao controle de versão (Git)", value = TRUE)
            ),
            shiny::div(
              style = "margin-top: 20px;",
              shiny::actionButton("file_import", "Importar arquivo", class = "btn-primary", style = "width: 100%; font-weight: 600; font-size: 15px; height: 42px !important;")
            )
          ),
          shiny::div(
            shiny::div(
              class = "tr-tree-card",
              shiny::h5("Arquivos Atuais do Projeto:", style = "margin-top: 0; margin-bottom: 12px; font-weight: 600; color: #EDEDED;"),
              shiny::uiOutput("recent_files_explorer")
            )
          )
        )
      ),
      shiny::tabPanel(
        "Criar",
        shiny::br(),
        criar_module_ui(diagnosis)
      ),
      shiny::tabPanel(
        "Gerenciar",
        shiny::br(),
        shiny::tabsetPanel(
          type = "pills",
          shiny::tabPanel(
            "Formatar",
            shiny::br(),
            panel_section(
              "Formatar Código",
              shiny::p("Organiza a indentação e espaçamentos automaticamente.", style = "font-size: 13px; color: #A1A1AA; margin-bottom: 15px; line-height: 1.4;"),
              format_module_ui(diagnosis)
            )
          ),
          shiny::tabPanel(
            "Renomear",
            shiny::br(),
            panel_section(
              "Renomear Arquivos/Pastas",
              shiny::p("Mude o nome ou o local de pastas e arquivos.", style = "font-size: 13px; color: #A1A1AA; margin-bottom: 15px; line-height: 1.4;"),
              rename_module_ui(diagnosis)
            )
          ),
          shiny::tabPanel(
            "Organizar",
            shiny::br(),
            panel_section(
              "Organizar Estrutura",
              shiny::p("Cria ou completa a estrutura padrão de pastas e arquivos do projeto.", style = "font-size: 13px; color: #A1A1AA; margin-bottom: 15px; line-height: 1.4;"),
              organize_module_ui()
            )
          ),
          shiny::tabPanel(
            "Excluir",
            shiny::br(),
            panel_section(
              "Deletar Arquivos/Pastas",
              shiny::p("Remove um arquivo ou pasta do projeto.", style = "font-size: 13px; color: #A1A1AA; margin-bottom: 15px; line-height: 1.4;"),
              excluir_module_ui(diagnosis)
            )
          )
        )
      ),
      shiny::tabPanel(
        "Versionar",
        shiny::br(),
        shiny::div(
          style = "max-width: 800px;",
          changes_module_ui(diagnosis)
        )
      )
    )
  )
}

git_module_ui <- function(diagnosis) {
  remote_info <- if (diagnosis$has_repo) remote_by_name(diagnosis$current_path) else NULL
  remote_url <- if (!is.null(remote_info)) remote_info$url[[1]] else ""
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
      class = "tr-git-layout",
      shiny::div(
          style = "display: flex; flex-direction: column; gap: 16px;",
          shiny::div(
            class = "tr-git-subsection",
            git_step_title("1", "Identidade", complete = isTRUE(diagnosis$identity$complete), description = "Seu nome aparecerá no histórico de versões"),
            if (isTRUE(diagnosis$identity$complete)) {
              shiny::div(
                class = "tr-git-success-note",
                shiny::span("Identidade atual"),
                shiny::strong(sprintf("%s <%s>", diagnosis$identity$name, diagnosis$identity$email))
              )
            },
            shiny::textInput("git_user_name", "Seu Nome Completo", value = diagnosis$identity$name %||% ""),
            shiny::textInput("git_user_email", "Seu Email Acadêmico/Profissional", value = diagnosis$identity$email %||% ""),
            panel_action_button("git_save_identity", "Salvar ou editar identidade", class = "btn-primary", enabled = TRUE)
          ),
          shiny::div(
            class = "tr-git-subsection",
            git_step_title("2", "Git local", complete = diagnosis$has_repo, blocked = !diagnosis$git_installed, description = "Inicialize Git para criar versões do seu projeto"),
            if (diagnosis$has_repo) {
              shiny::div(
                class = "tr-git-success-note",
                shiny::span("Branch ativa"),
                shiny::strong(diagnosis$branch %||% "sem branch")
              )
            } else {
              shiny::tagList(
                panel_action_button("git_init", "Inicializar Git", enabled = diagnosis$git_installed, class = "btn-success"),
                shiny::div(class = "tr-checkbox-group", shiny::checkboxInput("gitignore_ignore_data", "Ignorar data/raw/ e data/processed/", value = TRUE)),
                shiny::actionButton("gitignore_write", "Criar ou atualizar .gitignore", class = "btn-primary")
              )
            },
            disabled_reason_ui(!diagnosis$git_installed, "Git não encontrado no computador.")
          )
        ),
      shiny::div(
          style = "display: flex; flex-direction: column; gap: 16px;",
          shiny::div(
            class = "tr-git-subsection",
            git_step_title(
              "3",
              "Salvar versão local",
              complete = diagnosis$has_commits,
              blocked = !diagnosis$has_repo || !isTRUE(diagnosis$identity$complete),
              description = "Crie um 'ponto de salvamento' do seu projeto"
            ),
            shiny::textInput("commit_message", "Mensagem do commit", value = "Primeiro commit"),
            panel_action_button(
              "git_commit",
              "Preparar tudo e fazer commit",
              enabled = commit_enabled,
              class = "btn-success"
            ),
            disabled_reason_ui(!diagnosis$has_repo, "Inicialize o Git antes de criar commits."),
            disabled_reason_ui(diagnosis$has_repo && !isTRUE(diagnosis$identity$complete), "Configure nome e email antes de criar commits."),
            disabled_reason_ui(diagnosis$has_repo && isTRUE(diagnosis$identity$complete) && pending_changes == 0, "Não há mudanças pendentes para commitar.")
          ),
          shiny::div(
            class = "tr-git-subsection",
            git_step_title("4", "Conectar GitHub", complete = diagnosis$has_remote, blocked = !diagnosis$has_repo, description = "Crie um backup das versões na nuvem (opcional no início)"),
            if (!diagnosis$has_repo) {
              git_notice_ui("Ative o Git local antes de conectar um repositório remoto.")
            },
            if (has_remote_url) {
              shiny::div(
                class = "tr-git-current-remote",
                shiny::span("Remote atual"),
                shiny::code(remote_url)
              )
            },
            shiny::textInput("github_remote_url", "URL do repositório GitHub", value = remote_url),
            if (has_remote_url) {
              shiny::checkboxInput("github_replace_remote", "Trocar URL conectada", value = FALSE)
            },
            panel_action_button("github_connect", if (has_remote_url) "Salvar remote" else "Conectar remote", enabled = diagnosis$has_repo, class = "btn-primary"),
            disabled_reason_ui(!diagnosis$has_repo, "Inicialize o Git antes de conectar o GitHub.")
          ),
          shiny::div(
            class = "tr-git-subsection",
            git_step_title(
              "5",
              "Enviar",
              complete = diagnosis$has_remote && diagnosis$has_commits,
              blocked = !diagnosis$has_remote || !diagnosis$has_commits || is.null(diagnosis$branch),
              description = "Compartilhe as versões com colegas e professores"
            ),
            shiny::div(
              class = "tr-git-action-row",
              panel_action_button(
                "github_push",
                "Push",
                enabled = push_enabled,
                class = "btn-success"
              ),
              panel_action_button(
                "github_sync",
                "Pull + Push",
                enabled = push_enabled,
                class = "btn-default"
              )
            ),
            disabled_reason_ui(!diagnosis$has_remote, "Conecte um repositório GitHub antes de enviar."),
            disabled_reason_ui(diagnosis$has_remote && !diagnosis$has_commits, "Crie pelo menos uma versão (commit) antes de enviar."),
            disabled_reason_ui(diagnosis$has_remote && diagnosis$has_commits && is.null(diagnosis$branch), "Não foi possível identificar a versão de trabalho atual.")
          )
        )
      )
    )
}

git_status_badge_items <- function(diagnosis) {
  pending_changes <- diagnosis$status_counts$total %||% 0L
  branch_ok <- !is.null(diagnosis$branch)

  list(
    list(
      label = "Git",
      value = if (diagnosis$has_repo) "ativo" else "não iniciado",
      class = if (diagnosis$has_repo) "ok" else "error",
      title = if (diagnosis$has_repo) "Repositório Git inicializado" else "Inicialize o Git local primeiro"
    ),
    list(
      label = "Commits",
      value = if (diagnosis$has_commits) "existem" else "nenhum",
      class = if (diagnosis$has_commits) "ok" else "warn",
      title = if (diagnosis$has_commits) "Histórico de commits existe" else "Crie o primeiro commit"
    ),
    list(
      label = "Remote",
      value = if (diagnosis$has_remote) diagnosis$remote_name %||% "origin" else "ausente",
      class = if (diagnosis$has_remote) "ok" else "warn",
      title = if (diagnosis$has_remote) "Remote conectado" else "Conecte um repositório GitHub"
    ),
    list(
      label = "Branch",
      value = diagnosis$branch %||% "sem branch",
      class = if (branch_ok) "ok" else "warn",
      title = if (branch_ok) "Branch atual" else "Branch atual não identificada"
    ),
    list(
      label = "Pendências",
      value = as.character(pending_changes),
      class = if (pending_changes == 0L) "ok" else "warn",
      title = if (pending_changes == 0L) "Projeto limpo" else "Há arquivos modificados"
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

git_step_title <- function(number, title, complete = FALSE, blocked = FALSE, description = NULL) {
  state_class <- if (isTRUE(complete)) {
    "complete"
  } else if (isTRUE(blocked)) {
    "blocked"
  } else {
    "pending"
  }
  badge <- if (isTRUE(complete)) {
    "\u2713"
  } else if (isTRUE(blocked)) {
    shiny::icon("lock")
  } else {
    number
  }

  shiny::tagList(
    shiny::div(
      class = paste("tr-git-step-title", state_class),
      shiny::span(class = "tr-git-step-number", badge),
      shiny::span(title)
    ),
    if (!is.null(description) && nzchar(description)) {
      shiny::div(
        class = "tr-step-description",
        description
      )
    }
  )
}

changes_module_ui <- function(diagnosis) {
  if (!diagnosis$has_repo) {
    return(
      panel_section(
        "Atenção",
        shiny::div(
          style = "padding: 20px; text-align: center; color: #856404; background: #fff3cd; border-radius: 6px;",
          shiny::div(style = "display: flex; align-items: center; justify-content: center; margin-bottom: 12px;", shiny::icon("exclamation-triangle", style = "font-size: 28px;")),
          shiny::h4("Git não inicializado"),
          shiny::p("O Controle Fino de Mudanças exige que o Git esteja ativo neste projeto."),
          shiny::p("Por favor, vá para a aba ", shiny::strong("Git e GitHub"), " e inicialize o repositório.")
        )
      )
    )
  }

  files <- changed_file_choices(diagnosis$status)

  if (length(files) == 0) {
    return(
      shiny::tagList(
        panel_section(
          "Arquivos com mudancas",
          shiny::actionButton("changes_refresh", "Atualizar lista"),
          shiny::div(
            style = "padding: 40px 20px; text-align: center; color: #1a7f37; background: #dafbe1; border-radius: 6px; margin-top: 15px;",
            shiny::div(style = "display: flex; align-items: center; justify-content: center; margin-bottom: 12px;", shiny::icon("check-circle", style = "font-size: 32px; color: #34D399;")),
            shiny::h3("Seu projeto está limpo!"),
            shiny::p("Você não possui nenhuma mudança pendente no momento.")
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
        "Modo de visualização",
        choices = c("Ver apenas os trechos alterados" = "changes", "Ver arquivo todo com as mudanças destacadas" = "full"),
        selected = "changes"
      ),
      shiny::actionButton("changes_diff", "Inspecionar Mudanças", class = "btn-primary")
    ),
    panel_section(
      "Salvar nova versão (Commit)",
      shiny::div(
        style = "background: #022C22; border: 1px solid #064E3B; border-left: 4px solid #10B981; padding: 16px; margin-bottom: 24px; border-radius: 8px;",
        shiny::p("O commit funciona como um 'ponto de salvamento' (savepoint) do seu projeto. Você sempre poderá voltar a ele se algo der errado no futuro.", style = "margin-bottom: 0; color: #D1FAE5; font-size: 14px;")
      ),
      shiny::selectInput("changes_files", "Arquivos a incluir nesta versão", choices = files, multiple = TRUE),
      shiny::textInput("changes_commit_message", "Mensagem curta descrevendo o que você fez", value = "Atualiza análise"),
      shiny::actionButton("changes_commit_selected", "Salvar Versão (Commit)", class = "btn-success")
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
        "A ação abaixo é irreversível. Use apenas se cometeu erro e quer descartar tudo que editou."
      ),
      shiny::div(style = "margin-top: 16px;",
        shiny::actionButton("changes_discard", "Descartar alterações (Apagar para sempre)", class = "btn-danger")
      )
    )
  )
}



reports_module_ui <- function(diagnosis) {
  report_files <- if (!is.null(diagnosis$current_path) && nzchar(diagnosis$current_path)) {
    list.files(path = diagnosis$current_path, pattern = "\\.(qmd|Rmd|rmd)$", recursive = TRUE, full.names = FALSE)
  } else {
    character(0)
  }
  
  choices <- c("Nenhum relatório (busque na pasta 📁)" = "", report_files)
  report_input <- shiny::selectInput("report_path", "Selecione o Relatório", choices = choices, width = "100%")

  panel_section(
    "Visualizar Relatório (Preview)",
    shiny::p("Gere o preview dos seus relatórios Quarto/RMarkdown diretamente pelo painel.", style = "font-size: 13px; color: #A1A1AA; margin-bottom: 15px; line-height: 1.4;"),
    shiny::div(
      style = "display: flex; gap: 8px; align-items: flex-end; margin-bottom: 15px;",
      shiny::div(style = "flex-grow: 1;", report_input),
      shiny::actionButton("report_browse", "📁", class = "btn-default", style = "margin-bottom: 15px; height: 38px;", title = "Procurar relatório no computador")
    ),
    shiny::div(
      style = "display: flex; gap: 10px;",
      panel_action_button("report_preview", "Gerar Preview", class = "btn-primary", enabled = TRUE),
      panel_action_button("report_live_preview", "Live Preview", class = "btn-default", enabled = TRUE)
    )
  )
}

format_module_ui <- function(diagnosis) {
  format_files <- project_format_file_choices(diagnosis$current_path)
  
  choices <- c("Nenhum script (busque na pasta 📁)" = "", format_files)
  file_input <- shiny::selectInput("format_path", "Selecionar script (opcional)", choices = choices, width = "100%")

  shiny::tagList(
    shiny::div(
      style = "display: flex; gap: 8px; align-items: flex-end; margin-bottom: 15px;",
      shiny::div(style = "flex-grow: 1;", file_input),
      shiny::actionButton("format_browse", "📁", class = "btn-default", style = "margin-bottom: 15px; height: 38px;", title = "Procurar arquivo no computador")
    ),
    shiny::div(
      style = "display: flex; flex-direction: column; gap: 10px;",
      shiny::actionButton("format_active", "Formatar arquivo", class = "btn-primary", style = "width: 100%; font-weight: 600; font-size: 14px; height: 38px !important;"),
      shiny::actionButton("format_project", "Formatar todo o projeto", class = "btn-default", style = "width: 100%; font-weight: 600; font-size: 14px; height: 38px !important;")
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
          "Relatórios" = "reports",
          "Dados" = "data/raw",
          "Figuras" = "figs",
          "Raiz do projeto" = "."
        ),
        selected = "scripts"
      ),
      shiny::textAreaInput(
        "file_create_content",
        "Conteúdo inicial (opcional)",
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
      style = "display: flex; flex-direction: column; gap: 16px;",
      shiny::div(
        class = "tr-tree-card",
        shiny::h5("Sugestões rápidas:", style = "margin-top: 0; margin-bottom: 12px; font-weight: 600; color: #EDEDED;"),
        shiny::p("Use o tipo para carregar um template inicial editável antes de criar o arquivo.", style = "font-size: 13px; color: #A1A1AA; line-height: 1.5; margin-bottom: 12px;"),
        shiny::tags$ul(
          style = "padding-left: 18px; margin: 0; color: #D4D4D8; font-size: 13px; line-height: 1.6;",
          shiny::tags$li("Scripts: coloque em scripts/."),
          shiny::tags$li("Relatórios: prefira reports/ para .Rmd e .qmd."),
          shiny::tags$li("Dados tabulares simples: crie CSV direto em data/raw.")
        )
      ),
      project_files_explorer_card("Estrutura atual do projeto")
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

organize_module_ui <- function() {
  shiny::div(
    class = "tr-project-layout",
    shiny::div(
      style = "display: flex; flex-direction: column; gap: 15px;",
      shiny::radioButtons(
        "project_structure_template",
        "Modelo de estrutura",
        choices = c(
          "Análise Exploratória" = "analise_exploratoria",
          "Trabalho da Disciplina" = "trabalho_disciplina",
          "Iniciação Científica" = "iniciacao_cientifica",
          "TCC" = "tcc",
          "Artigo com Quarto" = "artigo_quarto",
          "Projeto em Grupo" = "projeto_grupo"
        ),
        selected = "analise_exploratoria"
      ),
      shiny::div(
        class = "tr-checkbox-group",
        shiny::checkboxInput("project_structure_include_data", "Incluir pastas data/raw e data/processed", value = TRUE)
      ),
      shiny::actionButton("project_structure", "Organizar estrutura", class = "btn-primary", style = "width: 100%; font-weight: 600; font-size: 15px; height: 42px !important;"),
      shiny::uiOutput("project_organize_status")
    ),
    shiny::div(
      class = "tr-tree-card",
      shiny::h5("Prévia da estrutura", style = "margin-top: 0; margin-bottom: 12px; font-weight: 600; color: #EDEDED;"),
      shiny::uiOutput("project_structure_preview_organize")
    )
  )
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
          shiny::actionButton("file_delete_browse", "📁", class = "btn-default", style = "margin-bottom: 15px; height: 38px;", title = "Procurar arquivo no computador")
        ),
        shiny::div(
          style = "padding: 12px; border-radius: 8px; background: #111827; border: 1px solid #1F2937; color: #D1D5DB; font-size: 13px; line-height: 1.5;",
          "Arquivos .Rproj, .git, .gitignore e .Rproj.user são protegidos. Itens versionados no Git mostram aviso antes da exclusão."
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
  
  choices <- if (length(all_items) == 0) c("Nenhum item (busque na pasta 📁)" = "") else all_items
  selected <- if (length(all_items) > 0) all_items[[1]] else ""
  item_input <- shiny::selectInput("rename_source", "Origem", choices = choices, selected = selected, width = "100%")
  
  shiny::tagList(
    shiny::div(
      style = "display: flex; gap: 8px; align-items: flex-end;",
      shiny::div(style = "flex-grow: 1;", item_input),
      shiny::actionButton("rename_browse", "📁", class = "btn-default", style = "margin-bottom: 15px; height: 38px;", title = "Procurar no computador")
    )
    ,
    shiny::textInput("rename_target", "Novo nome (inclua extensão)", value = selected, width = "100%"),
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
  next_step <- next_step_message(diagnosis)

  shiny::tagList(
    shiny::div(
      style = "padding: 24px; border-radius: 8px; background: #172554; border: 1px solid #1E3A8A; border-left: 4px solid #3B82F6; display: flex; flex-direction: column; gap: 12px; box-shadow: 0 1px 2px rgba(0,0,0,0.5);",
      shiny::h4(shiny::icon("lightbulb"), " Ação Recomendada", style = "margin: 0; color: #DBEAFE; font-weight: 600; font-size: 16px;"),
      shiny::p(next_step, style = "margin: 0; color: #DBEAFE; font-size: 15px;")
    ),

    if (diagnosis$has_commits) shiny::tagList(
      shiny::h3("Histórico de Commits", style = "margin-top: 32px; margin-bottom: 16px; font-weight: 600; color: #EDEDED;"),
      shiny::HTML(render_commit_timeline_html(diagnosis))
    )
  )
}

panel_action_button <- function(id, label, enabled = TRUE, class = "btn-default") {
  if (isTRUE(enabled)) {
    return(shiny::actionButton(id, label, class = class))
  }

  shiny::tags$button(
    id = id,
    type = "button",
    class = paste("btn", class, "action-button"),
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
      label = "Git",
      value = if (diagnosis$has_repo) "Ativo" else "Desativado",
      class = if (diagnosis$has_repo) "ok" else "error",
      title = if (diagnosis$has_repo) "Repositório Git inicializado" else "Você precisa inicializar o Git neste projeto"
    ),
    list(
      label = "",
      value = if (diagnosis$has_commits) "Histórico OK" else "0 Commits",
      class = if (diagnosis$has_commits) "ok" else "warn",
      title = if (diagnosis$has_commits) "Histórico de versões existe" else "Faça seu primeiro commit para começar a salvar versões"
    ),
    list(
      label = "GitHub",
      value = if (diagnosis$has_remote) diagnosis$remote_name else "Não conectado",
      class = if (diagnosis$has_remote) "ok" else "warn",
      title = if (diagnosis$has_remote) "Projeto conectado ao GitHub" else "Conecte a um repositório remoto para fazer backup na nuvem"
    ),
    list(
      label = "",
      value = if (diagnosis$status_counts$total == 0) "Tudo salvo" else paste(diagnosis$status_counts$total, "Modificações"),
      class = if (diagnosis$status_counts$total == 0) "ok" else "warn",
      title = if (diagnosis$status_counts$total == 0) "Projeto limpo, nada a salvar" else "Você tem arquivos modificados que ainda não foram salvos no Git",
      interactive = diagnosis$status_counts$total > 0
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

  history <- repo_commit_history(diagnosis$path, max_commits = 10)
  if (nrow(history) == 0) return("")
  
  items <- lapply(seq_len(nrow(history)), function(i) {
    row <- history[i, ]
    hash_short <- substr(row$commit, 1, 7)
    time_str <- time_ago(row$time)
    
    paste0(
      "<div class='tr-timeline-item'>",
      "<div class='tr-timeline-dot'></div>",
      "<div class='tr-timeline-content'>",
      "<div class='tr-timeline-header'>",
      "<span class='tr-timeline-author'>", htmltools::htmlEscape(row$author), "</span>",
      "<span class='tr-timeline-time'>", time_str, "</span>",
      "</div>",
      "<div class='tr-timeline-message'>", htmltools::htmlEscape(row$message), "</div>",
      "<div class='tr-timeline-hash'>", hash_short, "</div>",
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

render_project_tree_html <- function(template, include_data) {
  files_list <- list(
    analise_exploratoria = c(
      "📁 data/ (Pasta de dados)",
      "  📁 raw/ (Dados brutos de entrada)",
      "  📁 processed/ (Dados limpos para análise)",
      "📁 reports/ (Relatórios e manuscritos)",
      "  📄 notas.qmd (Notas e documentação)",
      "📁 scripts/ (Scripts de análise)",
      "  📄 01-exploracao.R (Script inicial)",
      "📁 figs/ (Figuras e gráficos gerados)",
      "📄 README.md (Documentação do projeto)",
      "📄 .gitignore (Configuração do Git)"
    ),
    trabalho_disciplina = c(
      "📁 data/ (Pasta de dados)",
      "  📁 raw/ (Dados brutos de entrada)",
      "  📁 processed/ (Dados limpos para análise)",
      "📁 reports/ (Relatórios e manuscritos)",
      "  📄 relatorio-final.qmd (Relatório final)",
      "📁 scripts/ (Scripts de análise)",
      "  📄 01-preparacao.R (Limpeza dos dados)",
      "  📄 02-analise.R (Modelagem e gráficos)",
      "📁 figs/ (Figuras e gráficos gerados)",
      "📄 README.md (Documentação do projeto)",
      "📄 .gitignore (Configuração do Git)"
    ),
    iniciacao_cientifica = c(
      "📁 data/ (Pasta de dados)",
      "  📁 raw/ (Dados brutos)",
      "  📁 processed/ (Dados limpos)",
      "📁 reports/ (Relatórios)",
      "  📄 plano-de-trabalho.qmd (Planejamento)",
      "  📄 relatorio-parcial.qmd (Andamento)",
      "📁 scripts/ (Scripts)",
      "  📄 01-limpeza.R (Tratamento inicial)",
      "  📄 02-modelagem.R (Análise principal)",
      "📁 figs/ (Figuras geradas)",
      "📄 README.md (Descrição da pesquisa)",
      "📄 .gitignore (Configuração do Git)"
    ),
    tcc = c(
      "📁 data/ (Pasta de dados)",
      "  📁 raw/ (Dados originais)",
      "  📁 processed/ (Dados finais)",
      "📁 reports/ (Manuscritos)",
      "  📄 tcc.qmd (Arquivo principal do TCC)",
      "📁 scripts/ (Scripts de análise)",
      "  📄 01-preparacao.R (Importação)",
      "  📄 02-resultados.R (Geração de tabelas)",
      "📁 figs/ (Figuras)",
      "📄 README.md (Apresentação do TCC)",
      "📄 .gitignore (Configuração do Git)"
    ),
    artigo_quarto = c(
      "📁 reports/ (Manuscritos)",
      "  📄 artigo.qmd (Artigo científico)",
      "📄 _quarto.yml (Configuração de publicação)",
      "📄 refs.bib (Referências bibliográficas)",
      "📄 README.md (Descrição)",
      "📄 .gitignore (Configuração do Git)"
    ),
    projeto_grupo = c(
      "📁 reports/ (Relatórios do grupo)",
      "  📄 andamento.qmd (Acompanhamento)",
      "📁 scripts/ (Scripts compartilhados)",
      "  📄 00-setup.R (Instalação e carregamento de pacotes)",
      "📄 CONTRIBUTING.md (Guia de colaboração)",
      "📄 README.md (Manual do grupo)",
      "📄 .gitignore (Configuração do Git)"
    )
  )[[template]]
  
  if (isFALSE(include_data)) {
    files_list <- c(files_list, "📄 data/raw/README.md (Orientação de dados)", "📄 data/processed/README.md (Orientação)")
  }
  
  lines <- vapply(files_list, function(line) {
    line_esc <- htmltools::htmlEscape(line)
    line_esc <- gsub("📁", "<span style='color: #3B82F6;'>📁</span>", line_esc)
    line_esc <- gsub("📄", "<span style='color: #10B981;'>📄</span>", line_esc)
    line_esc <- gsub("\\(([^\\)]+)\\)", "<span style='color: #71717A; font-size: 11px;'>(\\1)</span>", line_esc)
    paste0("<div style='font-family: monospace; font-size: 12px; margin-bottom: 4px; line-height: 1.4; white-space: pre;'>", line_esc, "</div>")
  }, character(1))
  
  paste(lines, collapse = "")
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
    return("<div style='color: #EF4444; font-style: italic;'>Caminho do projeto inválido ou não existe.</div>")
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
        status_indicator <- " <span style='font-size: 9px; background: #064E3B; color: #10B981; padding: 1px 4px; border-radius: 4px; margin-left: 6px; font-weight: 500; text-transform: uppercase;'>preparado</span>"
        status_style <- "color: #10B981;"
      } else if (status == "modified") {
        status_indicator <- " <span style='font-size: 9px; background: #78350F; color: #F59E0B; padding: 1px 4px; border-radius: 4px; margin-left: 6px; font-weight: 500; text-transform: uppercase;'>modificado</span>"
        status_style <- "color: #F59E0B;"
      } else if (status == "new") {
        status_indicator <- " <span style='font-size: 9px; background: #7F1D1D; color: #EF4444; padding: 1px 4px; border-radius: 4px; margin-left: 6px; font-weight: 500; text-transform: uppercase;'>não rastreado</span>"
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
      "<span style='color: #3B82F6;'>📁</span>"
    } else {
      ext <- tolower(fs::path_ext(name))
      if (ext %in% c("qmd", "rmd")) {
        "<span style='color: #8B5CF6;'>📊</span>"
      } else if (ext == "r") {
        "<span style='color: #10B981;'>📄</span>"
      } else if (ext %in% c("csv", "xlsx", "rds", "data")) {
        "<span style='color: #F59E0B;'>📊</span>"
      } else {
        "<span style='color: #71717A;'>📄</span>"
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

trackR_panel_css <- function() {
  "
  @import url('https://fonts.googleapis.com/css2?family=Inter:wght@400;500;600&display=swap');

  /* Antigravity / Codex Aesthetic Base (Dark Mode) */
  body {
    background-color: #0D0D0D;
  }
  body, .tr-shell {
    font-family: 'Inter', -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif;
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
    padding: 9px 10px 9px 4px;
    border-bottom: 1px solid #2D2D2D;
    flex-shrink: 0;
    overflow-x: auto;
  }
  .tr-sidebar .shiny-options-group {
    display: flex;
    flex-wrap: nowrap;
    gap: 3px;
    align-items: center;
    width: max-content;
  }
  
  .tr-main {
    background: #161616;
    padding: 20px 24px;
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
    height: 30px !important;
    padding: 0 12px !important;
    font-size: 12px !important;
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
    background-color: #EDEDED !important;
    border: 1px solid #EDEDED !important;
    color: #000000 !important;
  }
  .btn-primary:hover {
    background-color: #CCCCCC !important;
    border-color: #CCCCCC !important;
  }
  .btn-success {
    background-color: #166534 !important; /* Muted Green */
    border: 1px solid #14532D !important;
    color: #FFFFFF !important;
  }
  .btn-success:hover {
    background-color: #15803D !important;
    border-color: #15803D !important;
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
    gap: 16px;
    margin-top: 16px;
    margin-bottom: 16px;
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
    gap: 8px;
    padding: 7px 9px;
    border-radius: 6px;
    font-weight: 500;
    color: #A1A1AA;
    font-size: 13px;
    transition: all 0.15s ease;
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
    border-bottom: 1px solid #2D2D2D;
    padding-bottom: 16px;
    margin-bottom: 24px;
    background: rgba(22, 22, 22, 0.98);
    backdrop-filter: blur(8px);
    position: sticky;
    top: -24px;
    z-index: 10;
  }
  .tr-summary-main {
    display: flex;
    flex-direction: column;
    gap: 8px;
  }
  .tr-summary-header {
    display: flex;
    align-items: flex-start;
    justify-content: space-between;
    gap: 16px;
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
    gap: 12px;
    margin-top: 0;
    justify-content: flex-end;
  }
  .tr-pill {
    border-radius: 6px;
    border: 1px solid #2D2D2D;
    padding: 6px 12px;
    background: #1C1C1C;
    display: flex;
    align-items: center;
    gap: 8px;
    box-shadow: 0 1px 2px rgba(0,0,0,0.3);
  }
  .tr-pill.interactive {
    cursor: pointer;
    transition: all 0.15s ease;
  }
  .tr-pill.interactive:hover {
    transform: translateX(2px);
    border-color: #555555;
    background: #262626;
  }
  .tr-pill.ok {
    border-color: #059669;
    background: rgba(5, 150, 105, 0.15);
  }
  .tr-pill.warn {
    border-color: #B45309;
    background: rgba(217, 119, 6, 0.15);
  }
  .tr-pill.error {
    border-color: #991B1B;
    background: rgba(220, 38, 38, 0.15);
  }
  .tr-pill.ok .tr-pill-value { color: #34D399; }
  .tr-pill.warn .tr-pill-value { color: #FCD34D; }
  .tr-pill.error .tr-pill-value { color: #FCA5A5; }
  
  .tr-pill-label {
    font-size: 12px;
    color: #A1A1AA;
    font-weight: 500;
  }
  .tr-pill-value {
    font-weight: 600;
    font-size: 13px;
    color: #EDEDED;
  }

  /* Main Sections */
  .tr-section {
    border: 1px solid #2D2D2D;
    border-radius: 8px;
    padding: 24px;
    margin-bottom: 24px;
    background: #1C1C1C;
    box-shadow: 0 1px 2px rgba(0,0,0,0.5);
  }
  .tr-section h4 {
    font-size: 16px;
    font-weight: 600;
    color: #EDEDED;
    margin: 0 0 16px 0;
    letter-spacing: -0.01em;
  }
  .tr-section .form-group {
    margin-bottom: 16px;
  }
  .tr-git-current-remote code {
    color: #EDEDED;
    background: transparent;
    padding: 0;
    white-space: nowrap;
    overflow: hidden;
    text-overflow: ellipsis;
  }
  .tr-git-layout {
    display: grid;
    grid-template-columns: minmax(0, 1fr) minmax(0, 1fr);
    gap: 18px;
    align-items: start;
  }
  .tr-git-subsection {
    border: 1px solid #2D2D2D;
    border-radius: 8px;
    background: #161616;
    padding: 16px;
  }
  .tr-git-subtitle {
    margin: 0 0 12px 0;
    color: #EDEDED;
    font-size: 14px;
    font-weight: 700;
  }
  .tr-git-step-title {
    display: flex;
    align-items: center;
    gap: 10px;
    margin-bottom: 12px;
    color: #EDEDED;
    font-size: 14px;
    font-weight: 700;
  }
  .tr-git-step-number {
    display: inline-flex;
    align-items: center;
    justify-content: center;
    width: 24px;
    height: 24px;
    border-radius: 999px;
    background: #262626;
    border: 1px solid #3F3F46;
    color: #EDEDED;
    font-size: 12px;
    font-weight: 700;
    flex: 0 0 auto;
  }
  .tr-step-description {
    font-size: 11px;
    color: #71717A;
    margin-top: 6px;
    margin-left: 0;
    line-height: 1.4;
    font-weight: 400;
  }
  .tr-git-step-title.complete .tr-git-step-number {
    background: #064E3B;
    border-color: #10B981;
    color: #D1FAE5;
  }
  .tr-git-step-title.blocked .tr-git-step-number {
    background: #2D2D2D;
    border-color: #3F3F46;
    color: #A1A1AA;
  }
  .tr-git-success-note {
    display: grid;
    grid-template-columns: auto minmax(0, 1fr);
    gap: 10px;
    align-items: center;
    border: 1px solid #064E3B;
    border-radius: 6px;
    background: rgba(6, 78, 59, 0.24);
    color: #A7F3D0;
    padding: 8px 10px;
    margin-bottom: 12px;
    font-size: 12px;
  }
  .tr-git-success-note strong {
    color: #D1FAE5;
    overflow: hidden;
    text-overflow: ellipsis;
    white-space: nowrap;
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
  .tr-git-current-remote {
    display: grid;
    grid-template-columns: auto minmax(0, 1fr);
    gap: 10px;
    align-items: center;
    border: 1px solid #2D2D2D;
    border-radius: 6px;
    background: #0D0D0D;
    color: #A1A1AA;
    font-size: 12px;
    padding: 8px 10px;
    margin-bottom: 12px;
  }
  .tr-log {
    margin-top: 32px;
    border: 1px solid #2D2D2D;
    border-radius: 8px;
    background: #111111;
    overflow: hidden;
  }
  .tr-log-summary {
    padding: 12px 16px;
    background: #1C1C1C;
    cursor: pointer;
    font-size: 13px;
    font-weight: 600;
    color: #A1A1AA;
    display: flex;
    justify-content: space-between;
    align-items: center;
    user-select: none;
    transition: background 0.15s ease, color 0.15s ease;
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
    border: 1px solid #d8dee4;
    border-radius: 6px;
    overflow: auto;
    background: #f6f8fa;
    font-size: 12px;
  }
  .tr-diff-line {
    display: grid;
    grid-template-columns: 28px minmax(0, 1fr);
    min-height: 22px;
    border-bottom: 1px solid rgba(216, 222, 228, 0.65);
  }
  .tr-diff-line:last-child {
    border-bottom: 0;
  }
  .tr-diff-line code {
    display: block;
    padding: 3px 8px;
    color: #24292f;
    background: transparent;
    white-space: pre;
    font-family: Menlo, Monaco, Consolas, 'Liberation Mono', monospace;
  }
  .tr-diff-marker {
    padding: 3px 0;
    text-align: center;
    color: #57606a;
    user-select: none;
    font-family: Menlo, Monaco, Consolas, 'Liberation Mono', monospace;
  }
  .tr-diff-add {
    background: #dafbe1;
  }
  .tr-diff-add .tr-diff-marker {
    color: #1a7f37;
    background: #aceebb;
  }
  .tr-diff-remove {
    background: #ffebe9;
  }
  .tr-diff-remove .tr-diff-marker {
    color: #cf222e;
    background: #ffcecb;
  }
  .tr-diff-hunk {
    background: #ddf4ff;
  }
  .tr-diff-hunk .tr-diff-marker {
    color: #0969da;
    background: #b6e3ff;
  }
  .tr-diff-meta {
    background: #f6f8fa;
  }
  .tr-diff-meta code {
    color: #57606a;
    font-weight: 600;
  }
  .tr-diff-context {
    background: #fff;
  }
  .tr-diff-empty {
    border: 1px dashed #d8dee4;
    border-radius: 6px;
    padding: 14px;
    color: #57606a;
    background: #f6f8fa;
  }
  /* Project Management Layout */
  .tr-project-layout {
    display: grid;
    grid-template-columns: 1.1fr 1.3fr;
    gap: 32px;
    align-items: start;
    margin-top: 10px;
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
  
  /* Tree Card and Previews */
  .tr-tree-card {
    background: #0D0D0D;
    border: 1px solid #2D2D2D;
    padding: 16px;
    border-radius: 8px;
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
  .tr-timeline {
    position: relative;
    padding-left: 16px;
    border-left: 2px solid #2D2D2D;
    margin-bottom: 24px;
  }
  .tr-timeline-item {
    position: relative;
    margin-bottom: 24px;
    padding-left: 16px;
  }
  .tr-timeline-item:last-child {
    margin-bottom: 0;
  }
  .tr-timeline-dot {
    position: absolute;
    left: -23px;
    top: 6px;
    width: 12px;
    height: 12px;
    border-radius: 50%;
    background: #1C1C1E;
    border: 2px solid #3B82F6;
    z-index: 1;
  }
  .tr-timeline-content {
    background: #1C1C1E;
    border: 1px solid #2D2D2D;
    border-radius: 6px;
    padding: 12px 16px;
    transition: background-color 0.15s;
  }
  .tr-timeline-content:hover {
    background: #262626;
  }
  .tr-timeline-header {
    display: flex;
    justify-content: space-between;
    align-items: baseline;
    margin-bottom: 8px;
    font-size: 13px;
  }
  .tr-timeline-author {
    font-weight: 600;
    color: #EDEDED;
  }
  .tr-timeline-time {
    color: #A1A1AA;
    font-size: 12px;
  }
  .tr-timeline-message {
    font-size: 14px;
    color: #D4D4D8;
    margin-bottom: 10px;
    white-space: pre-wrap;
    line-height: 1.4;
  }
  .tr-timeline-hash {
    font-family: Menlo, Monaco, Consolas, monospace;
    font-size: 11px;
    color: #60A5FA;
    background: rgba(96, 165, 250, 0.1);
    padding: 3px 6px;
    border-radius: 4px;
    display: inline-block;
  }

  @media (max-width: 900px) {
    .tr-shell {
      grid-template-columns: 1fr;
      height: auto;
    }
    .tr-git-layout {
      grid-template-columns: 1fr;
    }
    .tr-summary-header {
      flex-direction: column;
      align-items: flex-start;
      gap: 10px;
    }
    .tr-summary-grid {
      justify-content: flex-start;
    }
  }
  "
}
