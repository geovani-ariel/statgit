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
    git4stats_panel_ui(project_path, default_module),
    server = git4stats_panel_server(project_path, initial_diagnosis),
    viewer = shiny::paneViewer(minHeight = "maximize")
  )

  invisible(project_path)
}

git4stats_panel_ui <- function(project_path, default_module = "project") {
  miniUI::miniPage(
    shiny::tags$head(
      shiny::tags$style(shiny::HTML(git4stats_panel_css())),
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
        class = "g4s-shell",
        shiny::div(
          class = "g4s-sidebar",
          shiny::radioButtons(
            "module",
            label = NULL,
            choiceNames = list(
              shiny::HTML(paste("<div class='g4s-nav-item'>", shiny::icon("heartbeat"), "Visão Geral</div>")),
              shiny::HTML(paste("<div class='g4s-nav-item'>", shiny::icon("folder"), "Gerenciar Projeto</div>")),
              shiny::HTML(paste("<div class='g4s-nav-item'>", shiny::icon("file-alt"), "Arquivos e Código</div>")),
              shiny::HTML(paste("<div class='g4s-nav-item'>", shiny::icon("github"), "Git e GitHub</div>")),
              shiny::HTML(paste("<div class='g4s-nav-item'>", shiny::icon("chart-bar"), "Relatórios</div>"))
            ),
            choiceValues = c("overview", "project", "files", "git", "reports"),
            selected = default_module
          )
        ),
        shiny::div(
          class = "g4s-main",
          shiny::uiOutput("project_summary"),
          shiny::uiOutput("module_ui"),
          shiny::tags$details(
            class = "g4s-log",
            shiny::tags$summary(
              class = "g4s-log-summary",
              shiny::tags$span(shiny::icon("terminal"), " Terminal de Execução"),
              shiny::tags$span(class = "g4s-log-hint", "Clique para expandir")
            ),
            shiny::div(
              class = "g4s-log-content",
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

git4stats_panel_server <- function(project_path, initial_diagnosis = NULL) {
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
      diff_html = format_diff_for_panel_html(character())
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
      
      project_name <- if (diagnosis$is_rstudio_project) basename(diagnosis$rproj_path) else basename(diagnosis$current_path)

      shiny::div(
        class = "g4s-summary",
        shiny::div(
          class = "g4s-summary-main",
          shiny::div(
            style = "margin-bottom: 4px;",
            shiny::strong(project_name, style = "font-size: 22px; color: #60A5FA; letter-spacing: -0.5px;")
          ),
          shiny::div(class = "g4s-path", style = "font-size: 12px; color: #71717A;", diagnosis$current_path)
        ),
        shiny::div(
          class = "g4s-summary-grid",
          lapply(stats, function(item) {
            shiny::div(
              class = paste("g4s-pill", item$class),
              title = item$title,
              if (nzchar(item$label)) shiny::span(class = "g4s-pill-label", item$label),
              shiny::span(class = "g4s-pill-value", item$value)
            )
          })
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
          class = "g4s-recent-project-card",
          onclick = sprintf("Shiny.setInputValue('project_to_open', '%s', {priority: 'event'});", js_path),
          shiny::div(class = "g4s-recent-name", proj_name),
          shiny::div(class = "g4s-recent-dir", proj_dir)
        )
      })
      
      shiny::div(
        class = "g4s-recent-projects-grid",
        shiny::h5("Projetos detectados na pasta base:", style = "margin-top: 0; margin-bottom: 12px; font-weight: 600; color: #EDEDED;"),
        items
      )
    })



    output$recent_files_explorer <- shiny::renderUI({
      shiny::HTML(render_project_files_explorer_html(project_path, d()))
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
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$git_save_identity, {
      name <- trimws(input$git_user_name %||% "")
      email <- trimws(input$git_user_email %||% "")
      if (!nzchar(name) || !nzchar(email)) {
        shiny::showNotification("Preencha nome e email.", type = "error")
        return()
      }
      run_panel_action(setup_git_identity(name, email))
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$project_open, {
      selected <- input$project_to_open %||% ""
      if (!nzchar(selected)) {
        shiny::showNotification("Nenhum projeto foi informado.", type = "error")
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
      shiny::showNotification("Lista de mudanças atualizada.", type = "message", duration = 3)
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
      run_panel_action(discard_file_changes(input$changes_files, path = project_path))
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

    # Observers of removed buttons git_stage_commit_files and git_commit_staged were deleted.

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

    shiny::observeEvent(input$format_browse, {
      file <- tryCatch(rstudioapi::selectFile(caption = "Selecionar script para formatar", filter = "R/Quarto files (*.R *.Rmd *.qmd)", existing = TRUE), error = function(e) NULL)
      if (!is.null(file) && nzchar(file)) {
        run_panel_action(format_active_file(file), refresh = FALSE)
      }
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$rename_browse, {
      file <- tryCatch(rstudioapi::selectFile(caption = "Selecionar arquivo/pasta para renomear", existing = TRUE), error = function(e) NULL)
      if (!is.null(file) && nzchar(file)) {
        rel_path <- tryCatch(relative_project_path(file, project_path), error = function(e) file)
        all_items <- list.files(path = project_path, all.files = FALSE, recursive = TRUE, include.dirs = TRUE, full.names = FALSE)
        shiny::updateSelectInput(session, "rename_source", choices = unique(c(rel_path, all_items)), selected = rel_path)
      }
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

    shiny::observeEvent(input$rename_execute, {
      source <- panel_optional_path(input$rename_source)
      target <- panel_optional_path(input$rename_target)
      if (!is.null(source) && nzchar(source) && !is.null(target) && nzchar(target)) {
        run_panel_action(rename_project_item(source, target, path = project_path))
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
          class = "g4s-project-layout",
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
              class = "g4s-checkbox-group",
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
              class = "g4s-template-selector",
              shiny::radioButtons(
                "project_template",
                label = NULL,
                width = "100%",
                choiceNames = list(
                  shiny::HTML("<div class='g4s-template-card'><div class='g4s-template-title'>🔍 Análise Exploratória</div><div class='g4s-template-desc'>Roteiros simples e análise rápida de dados.</div></div>"),
                  shiny::HTML("<div class='g4s-template-card'><div class='g4s-template-title'>📚 Trabalho da Disciplina</div><div class='g4s-template-desc'>Estrutura padrão para tarefas e entregas acadêmicas.</div></div>"),
                  shiny::HTML("<div class='g4s-template-card'><div class='g4s-template-title'>🧪 Iniciação Científica</div><div class='g4s-template-desc'>Para pesquisas com relatórios parciais e modelagem.</div></div>"),
                  shiny::HTML("<div class='g4s-template-card'><div class='g4s-template-title'>🎓 Trabalho de Conclusão (TCC)</div><div class='g4s-template-desc'>Monografia com pastas dedicadas para dados e resultados.</div></div>"),
                  shiny::HTML("<div class='g4s-template-card'><div class='g4s-template-title'>📝 Artigo com Quarto</div><div class='g4s-template-desc'>Arquivos prontos para escrita científica com Quarto (.qmd).</div></div>"),
                  shiny::HTML("<div class='g4s-template-card'><div class='g4s-template-title'>👥 Projeto em Grupo</div><div class='g4s-template-desc'>Inclui guias de contribuição e scripts compartilhados.</div></div>")
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
              class = "g4s-tree-card",
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
          class = "g4s-project-layout",
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
          class = "g4s-project-layout",
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
              class = "g4s-checkbox-group",
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
              class = "g4s-tree-card",
              shiny::h5("Arquivos Atuais do Projeto:", style = "margin-top: 0; margin-bottom: 12px; font-weight: 600; color: #EDEDED;"),
              shiny::uiOutput("recent_files_explorer")
            )
          )
        )
      ),
      shiny::tabPanel(
        "Estruturar",
        shiny::br(),
        shiny::div(
          style = "display: flex; flex-direction: column; gap: 32px;",
          
          shiny::div(
            style = "display: grid; grid-template-columns: 1fr 1fr; gap: 24px;",
            panel_section(
              "Formatar Código",
              shiny::p("Organiza a indentação e espaçamentos automaticamente.", style = "font-size: 13px; color: #A1A1AA; margin-bottom: 15px; line-height: 1.4;"),
              format_module_ui(diagnosis)
            ),
            panel_section(
              "Renomear Arquivos/Pastas",
              shiny::p("Mude o nome ou o local de pastas e arquivos.", style = "font-size: 13px; color: #A1A1AA; margin-bottom: 15px; line-height: 1.4;"),
              rename_module_ui(diagnosis)
            )
          ),
          
          shiny::hr(style = "border-color: #2D2D2D; margin: 0;"),
          
          panel_section(
            "Organizar Estrutura",
            shiny::p("Cria uma estrutura padronizada de pastas no seu projeto atual.", style = "font-size: 13px; color: #A1A1AA; margin-bottom: 20px; line-height: 1.4;"),
            
            shiny::div(
              style = "display: flex; flex-direction: column; gap: 24px;",
              
              shiny::div(
                class = "g4s-template-selector",
                shiny::radioButtons(
                  "project_structure_template",
                  label = "Selecione o Modelo para organizar",
                  width = "100%",
                  choiceNames = list(
                    shiny::HTML("<div class='g4s-template-card'><div class='g4s-template-title'>🔍 Análise Exploratória</div><div class='g4s-template-desc'>Roteiros simples e análise rápida de dados.</div></div>"),
                    shiny::HTML("<div class='g4s-template-card'><div class='g4s-template-title'>📚 Trabalho da Disciplina</div><div class='g4s-template-desc'>Estrutura padrão para tarefas e entregas acadêmicas.</div></div>"),
                    shiny::HTML("<div class='g4s-template-card'><div class='g4s-template-title'>🧪 Iniciação Científica</div><div class='g4s-template-desc'>Para pesquisas com relatórios parciais e modelagem.</div></div>"),
                    shiny::HTML("<div class='g4s-template-card'><div class='g4s-template-title'>🎓 Trabalho de Conclusão (TCC)</div><div class='g4s-template-desc'>Monografia com pastas dedicadas para dados e resultados.</div></div>"),
                    shiny::HTML("<div class='g4s-template-card'><div class='g4s-template-title'>📝 Artigo com Quarto</div><div class='g4s-template-desc'>Arquivos prontos para escrita científica com Quarto (.qmd).</div></div>"),
                    shiny::HTML("<div class='g4s-template-card'><div class='g4s-template-title'>👥 Projeto em Grupo</div><div class='g4s-template-desc'>Inclui guias de contribuição e scripts compartilhados.</div></div>")
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
                style = "display: grid; grid-template-columns: 1fr 300px; gap: 24px; align-items: start;",
                
                shiny::div(
                  class = "g4s-tree-card",
                  shiny::h5("Estrutura Resultante Recomendada:", style = "margin-top: 0; margin-bottom: 12px; font-weight: 600; color: #EDEDED;"),
                  shiny::uiOutput("project_structure_preview_organize")
                ),
                
                shiny::div(
                  style = "display: flex; flex-direction: column; gap: 15px; padding: 16px; border: 1px solid #2D2D2D; border-radius: 8px; background: #1C1C1E;",
                  shiny::div(
                    class = "g4s-checkbox-group",
                    shiny::checkboxInput("project_structure_include_data", "Versionar a pasta data/", value = TRUE)
                  ),
                  shiny::uiOutput("project_organize_status"),
                  shiny::actionButton("project_structure", "Criar estrutura no projeto atual", class = "btn-primary", style = "width: 100%; font-weight: 600; font-size: 15px; height: 42px !important;")
                )
              )
            )
          )
        )
      ),
      shiny::tabPanel(
        "Controle Fino (Diffs)",
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
  github_ui <- if (!diagnosis$has_repo) {
    panel_section(
      "Nuvem (GitHub)",
      shiny::div(
        style = "padding: 20px; text-align: center; color: #856404; background: #fff3cd; border-radius: 6px;",
        shiny::h4("⚠️ Git não inicializado"),
        shiny::p("Para enviar seu projeto ao GitHub, você precisa inicializar o Git localmente primeiro.")
      )
    )
  } else {
    shiny::tagList(
      panel_section(
        "Conectar com a Nuvem (Remote)",
        shiny::div(
          style = "background: #172554; border: 1px solid #1E3A8A; border-left: 4px solid #3B82F6; padding: 16px; margin-bottom: 24px; border-radius: 8px;",
          shiny::p("Conectar ao GitHub permite que você salve um backup online seguro do seu histórico.", style = "margin-bottom: 0; color: #DBEAFE; font-size: 14px;")
        ),
        shiny::textInput("github_remote_url", "URL do repositório no GitHub (ex: https://github.com/user/repo.git)", value = ""),
        shiny::checkboxInput("github_replace_remote", "Substituir URL caso já exista uma conexão anterior", value = FALSE),
        panel_action_button("github_connect", "Conectar ao GitHub", enabled = diagnosis$has_repo, class = "btn-primary")
      ),
      panel_section(
        "Sincronizar Histórico (Push / Pull)",
        shiny::p("Se a conexão estiver correta, você já pode enviar suas versões salvas para a nuvem (Push).", style = "font-size: 13px; color: #57606a; margin-bottom: 10px;"),
        panel_action_button("github_auth", "Verificar permissões de Acesso", enabled = diagnosis$has_remote, class = "btn-default"),
        panel_action_button(
          "github_push",
          "Enviar Histórico (Push)",
          enabled = diagnosis$has_remote && diagnosis$has_commits && !is.null(diagnosis$branch),
          class = "btn-success"
        )
      )
    )
  }

  shiny::div(
    class = "g4s-project-layout",
    shiny::div(
      style = "display: flex; flex-direction: column; gap: 15px;",
      panel_section(
        "Identidade (Quem é você?)",
        shiny::p("O Git precisa saber seu nome e email para carimbar as mudanças no histórico.", style = "font-size: 13px; color: #A1A1AA; margin-bottom: 10px;"),
        shiny::textInput("git_user_name", "Seu Nome Completo", value = diagnosis$identity$name %||% ""),
        shiny::textInput("git_user_email", "Seu Email Acadêmico/Profissional", value = diagnosis$identity$email %||% ""),
        panel_action_button("git_save_identity", "Salvar Identidade Global", class = "btn-primary", enabled = !isTRUE(diagnosis$identity$complete))
      ),
      panel_section(
        "Ativar Git Local",
        shiny::p("Inicializa o repositório oculto do Git (.git) para começar a gravar o histórico deste projeto.", style = "font-size: 13px; color: #A1A1AA; margin-bottom: 10px;"),
        panel_action_button("git_init", "Inicializar Git", enabled = diagnosis$git_installed && !diagnosis$has_repo, class = "btn-success")
      ),
      panel_section(
        "Segurança de Dados (.gitignore)",
        shiny::p("Dados brutos pesados ou confidenciais não devem ir para o histórico do Git nem para o GitHub.", style = "font-size: 13px; color: #A1A1AA; margin-bottom: 10px;"),
        shiny::checkboxInput("gitignore_ignore_data", "Garantir que data/raw/ e data/processed/ sejam ignorados", value = TRUE),
        shiny::actionButton("gitignore_write", "Gerar ou atualizar .gitignore", class = "btn-primary")
      )
    ),
    shiny::div(
      style = "display: flex; flex-direction: column; gap: 15px;",
      github_ui
    )
  )
}

changes_module_ui <- function(diagnosis) {
  if (!diagnosis$has_repo) {
    return(
      panel_section(
        "Atenção",
        shiny::div(
          style = "padding: 20px; text-align: center; color: #856404; background: #fff3cd; border-radius: 6px;",
          shiny::h4("⚠️ Git não inicializado"),
          shiny::p("O Controle Fino de Mudanças exige que o Git esteja ativo neste projeto."),
          shiny::p("Por favor, vá para a aba ", shiny::strong("Setup & Ações Rápidas"), " e inicialize o repositório.")
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
            shiny::h3("🎉 Seu projeto está limpo!"),
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
      "Zona de Perigo",
      shiny::actionButton("changes_discard", "Descartar alterações (Apagar para sempre)", class = "btn-danger")
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
  format_files <- if (!is.null(diagnosis$current_path) && nzchar(diagnosis$current_path)) {
    list.files(path = diagnosis$current_path, pattern = "\\.(R|r|Rmd|rmd|qmd)$", recursive = TRUE, full.names = FALSE)
  } else {
    character(0)
  }
  
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

rename_module_ui <- function(diagnosis) {
  all_items <- if (!is.null(diagnosis$current_path) && nzchar(diagnosis$current_path)) {
    list.files(path = diagnosis$current_path, all.files = FALSE, recursive = TRUE, include.dirs = TRUE, full.names = FALSE)
  } else {
    character(0)
  }
  
  choices <- if (length(all_items) == 0) c("Nenhum item (busque na pasta 📁)" = "") else all_items
  item_input <- shiny::selectInput("rename_source", "Selecionar Arquivo/Pasta", choices = choices, width = "100%")
  
  shiny::tagList(
    shiny::div(
      style = "display: flex; gap: 8px; align-items: flex-end;",
      shiny::div(style = "flex-grow: 1;", item_input),
      shiny::actionButton("rename_browse", "📁", class = "btn-default", style = "margin-bottom: 15px; height: 38px;", title = "Procurar no computador")
    ),
    shiny::textInput("rename_target", "Novo nome (inclua extensão)", value = "", width = "100%"),
    shiny::actionButton("rename_execute", "Renomear", class = "btn-default", style = "width: 100%; font-weight: 600; height: 38px !important;")
  )
}

panel_section <- function(title, ...) {
  shiny::div(
    class = "g4s-section",
    shiny::h4(title),
    ...
  )
}

overview_module_ui <- function(diagnosis) {
  next_step <- next_step_message(diagnosis)
  
  shiny::tagList(
    shiny::h3("Saúde do Projeto", style = "margin-top: 0; margin-bottom: 20px; font-weight: 600; color: #EDEDED;"),
    
    shiny::div(
      style = "display: grid; grid-template-columns: repeat(auto-fit, minmax(200px, 1fr)); gap: 20px; margin-bottom: 32px;",
      
      shiny::div(
        style = "padding: 16px; border-radius: 8px; border: 1px solid #2D2D2D; background: #1C1C1C; box-shadow: 0 1px 2px rgba(0,0,0,0.3);",
        shiny::h4("Controle de Versão (Git)", style = "margin-top: 0; font-size: 13px; color: #A1A1AA; text-transform: uppercase; letter-spacing: 0.5px;"),
        if (diagnosis$has_repo) shiny::tags$span(shiny::icon("check-circle", style = "color: #10B981;"), " Ativo", style="font-weight: 600; color: #EDEDED;") else shiny::tags$span(shiny::icon("times-circle", style = "color: #EF4444;"), " Não inicializado", style="font-weight: 600; color: #EDEDED;")
      ),
      
      shiny::div(
        style = "padding: 16px; border-radius: 8px; border: 1px solid #2D2D2D; background: #1C1C1C; box-shadow: 0 1px 2px rgba(0,0,0,0.3);",
        shiny::h4("Histórico (Commits)", style = "margin-top: 0; font-size: 13px; color: #A1A1AA; text-transform: uppercase; letter-spacing: 0.5px;"),
        if (diagnosis$has_commits) shiny::tags$span(shiny::icon("check-circle", style = "color: #10B981;"), " Salvo", style="font-weight: 600; color: #EDEDED;") else shiny::tags$span(shiny::icon("exclamation-triangle", style = "color: #F59E0B;"), " Sem histórico", style="font-weight: 600; color: #EDEDED;")
      ),
      
      shiny::div(
        style = "padding: 16px; border-radius: 8px; border: 1px solid #2D2D2D; background: #1C1C1C; box-shadow: 0 1px 2px rgba(0,0,0,0.3);",
        shiny::h4("Nuvem (GitHub)", style = "margin-top: 0; font-size: 13px; color: #A1A1AA; text-transform: uppercase; letter-spacing: 0.5px;"),
        if (diagnosis$has_remote) shiny::tags$span(shiny::icon("check-circle", style = "color: #10B981;"), " Conectado", style="font-weight: 600; color: #EDEDED;") else shiny::tags$span(shiny::icon("info-circle", style = "color: #A1A1AA;"), " Apenas local", style="font-weight: 600; color: #EDEDED;")
      )
    ),
    
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
      title = if (diagnosis$status_counts$total == 0) "Projeto limpo, nada a salvar" else "Você tem arquivos modificados que ainda não foram salvos no Git"
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
  
  history <- repo_commit_history(diagnosis$current_path, max_commits = 10)
  if (nrow(history) == 0) return("")
  
  items <- lapply(seq_len(nrow(history)), function(i) {
    row <- history[i, ]
    hash_short <- substr(row$commit, 1, 7)
    time_str <- time_ago(row$time)
    
    paste0(
      "<div class='g4s-timeline-item'>",
      "<div class='g4s-timeline-dot'></div>",
      "<div class='g4s-timeline-content'>",
      "<div class='g4s-timeline-header'>",
      "<span class='g4s-timeline-author'>", htmltools::htmlEscape(row$author), "</span>",
      "<span class='g4s-timeline-time'>", time_str, "</span>",
      "</div>",
      "<div class='g4s-timeline-message'>", htmltools::htmlEscape(row$message), "</div>",
      "<div class='g4s-timeline-hash'>", hash_short, "</div>",
      "</div>",
      "</div>"
    )
  })
  
  paste0(
    "<div class='g4s-timeline'>",
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
  # Recursively list all files and directories
  all_paths <- tryCatch(fs::dir_ls(path, recurse = TRUE), error = function(e) character())
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

render_project_files_explorer_html <- function(path, diagnosis) {
  files <- list_project_files_tree(path)
  if (length(files) == 0) {
    return("<div style='color: #71717A; font-style: italic;'>Projeto vazio ou sem arquivos.</div>")
  }
  
  status_tbl <- diagnosis$status
  
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
    
    sprintf("<div style='font-family: monospace; font-size: 12px; margin-bottom: 5px; line-height: 1.4; white-space: pre; %s'>%s%s %s%s</div>",
            status_style, indent, icon, htmltools::htmlEscape(name), status_indicator)
  }, character(1))
  
  paste(lines, collapse = "")
}

git4stats_panel_css <- function() {
  "
  @import url('https://fonts.googleapis.com/css2?family=Inter:wght@400;500;600&display=swap');

  /* Antigravity / Codex Aesthetic Base (Dark Mode) */
  body {
    background-color: #0D0D0D;
  }
  body, .g4s-shell {
    font-family: 'Inter', -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif;
    color: #EDEDED;
  }
  .g4s-shell {
    display: flex;
    flex-direction: column;
    height: 100vh;
    background: #161616;
  }
  
  /* Layout Panels */
  .g4s-sidebar {
    background: #0D0D0D;
    padding: 10px 16px;
    border-bottom: 1px solid #2D2D2D;
    flex-shrink: 0;
    overflow-x: auto;
  }
  .g4s-sidebar .shiny-options-group {
    display: flex;
    flex-wrap: nowrap;
    gap: 8px;
    align-items: center;
    width: max-content;
  }
  
  .g4s-main {
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
  /* Checkboxes */
  .g4s-checkbox-group {
    display: flex;
    flex-direction: column;
    gap: 16px;
    margin-top: 16px;
    margin-bottom: 16px;
  }
  .g4s-checkbox-group .shiny-input-container {
    margin: 0 !important;
  }
  .g4s-checkbox-group .form-group {
    margin: 0 !important;
  }
  .g4s-checkbox-group .checkbox {
    margin: 0 !important;
    padding: 0 !important;
  }
  .g4s-checkbox-group .checkbox label {
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
  .g4s-checkbox-group .checkbox input[type='checkbox'] {
    margin: 0 !important;
    padding: 0 !important;
    position: static !important;
    width: 16px;
    height: 16px;
    cursor: pointer;
  }

  /* Navigation Sidebar */
  .g4s-sidebar input[type=radio] {
    display: none !important;
  }
  .g4s-sidebar .radio {
    margin: 0 !important;
    padding: 0 !important;
  }
  .g4s-sidebar .radio label {
    display: inline-block !important;
    width: auto !important;
    margin: 0 !important;
    padding: 0 !important;
    cursor: pointer;
  }
  .g4s-sidebar .radio label span {
    display: inline-block;
  }
  .g4s-nav-item {
    display: flex;
    align-items: center;
    gap: 10px;
    padding: 8px 12px;
    border-radius: 6px;
    font-weight: 500;
    color: #A1A1AA;
    font-size: 13px;
    transition: all 0.15s ease;
    white-space: nowrap;
  }
  .g4s-nav-item i {
    font-size: 14px;
    color: #71717A;
    width: 16px;
    text-align: center;
    transition: color 0.15s ease;
  }
  .g4s-sidebar .radio label:hover .g4s-nav-item {
    background: #1C1C1E;
    color: #EDEDED;
  }
  .g4s-sidebar .radio label:hover .g4s-nav-item i {
    color: #A1A1AA;
  }
  .g4s-sidebar input[type=radio]:checked + span .g4s-nav-item {
    background: #262626;
    color: #EDEDED;
    font-weight: 500;
  }
  .g4s-sidebar input[type=radio]:checked + span .g4s-nav-item i {
    color: #EDEDED;
  }
  .g4s-nav-group {
    display: none;
  }

  /* Sticky Header */
  .g4s-summary {
    border-bottom: 1px solid #2D2D2D;
    padding-bottom: 16px;
    margin-bottom: 24px;
    background: rgba(22, 22, 22, 0.98);
    backdrop-filter: blur(8px);
    position: sticky;
    top: -24px;
    z-index: 10;
  }
  .g4s-path {
    color: #A1A1AA;
    font-size: 13px;
    word-break: break-all;
    margin-top: 4px;
  }
  .g4s-summary-grid {
    display: flex;
    flex-wrap: wrap;
    gap: 12px;
    margin-top: 16px;
  }
  .g4s-pill {
    border-radius: 6px;
    border: 1px solid #2D2D2D;
    padding: 6px 12px;
    background: #1C1C1C;
    display: flex;
    align-items: center;
    gap: 8px;
    box-shadow: 0 1px 2px rgba(0,0,0,0.3);
  }
  .g4s-pill.ok {
    border-color: #059669;
    background: rgba(5, 150, 105, 0.1);
  }
  .g4s-pill.warn {
    border-color: #D97706;
    background: rgba(217, 119, 6, 0.1);
  }
  .g4s-pill.error {
    border-color: #DC2626;
    background: rgba(220, 38, 38, 0.1);
  }
  .g4s-pill.ok .g4s-pill-value { color: #34D399; }
  .g4s-pill.warn .g4s-pill-value { color: #FBBF24; }
  .g4s-pill.error .g4s-pill-value { color: #F87171; }
  
  .g4s-pill-label {
    font-size: 12px;
    color: #A1A1AA;
    font-weight: 500;
  }
  .g4s-pill-value {
    font-weight: 600;
    font-size: 13px;
    color: #EDEDED;
  }

  /* Main Sections */
  .g4s-section {
    border: 1px solid #2D2D2D;
    border-radius: 8px;
    padding: 24px;
    margin-bottom: 24px;
    background: #1C1C1C;
    box-shadow: 0 1px 2px rgba(0,0,0,0.5);
  }
  .g4s-section h4 {
    font-size: 16px;
    font-weight: 600;
    color: #EDEDED;
    margin: 0 0 16px 0;
    letter-spacing: -0.01em;
  }
  .g4s-section .form-group {
    margin-bottom: 16px;
  }
  .g4s-log {
    margin-top: 32px;
    border: 1px solid #2D2D2D;
    border-radius: 8px;
    background: #111111;
    overflow: hidden;
  }
  .g4s-log-summary {
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
  .g4s-log-summary:hover {
    background: #262626;
    color: #EDEDED;
  }
  .g4s-log[open] .g4s-log-summary {
    border-bottom: 1px solid #2D2D2D;
    color: #EDEDED;
  }
  .g4s-log-hint {
    font-size: 11px;
    font-weight: 400;
    color: #71717A;
  }
  .g4s-log[open] .g4s-log-hint {
    display: none;
  }
  .g4s-log-content {
    padding: 16px;
  }
  .g4s-log pre {
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
  /* Project Management Layout */
  .g4s-project-layout {
    display: grid;
    grid-template-columns: 1.1fr 1.3fr;
    gap: 32px;
    align-items: start;
    margin-top: 10px;
  }
  @media (max-width: 650px) {
    .g4s-project-layout {
      grid-template-columns: 1fr;
    }
  }
  
  /* Template Cards */
  .g4s-template-selector {
    margin-bottom: 24px;
    width: 100%;
  }
  .g4s-template-selector .shiny-options-group {
    display: grid;
    grid-template-columns: repeat(2, 1fr);
    gap: 12px;
    width: 100%;
  }
  .g4s-template-selector input[type=radio] {
    display: none !important;
  }
  .g4s-template-selector .radio {
    margin: 0 !important;
    padding: 0 !important;
  }
  .g4s-template-selector .radio label {
    display: block !important;
    margin: 0 !important;
    padding: 0 !important;
    width: 100%;
    height: 100%;
    cursor: pointer;
  }
  .g4s-template-selector .radio label span {
    display: block;
    width: 100%;
    height: 100%;
  }
  .g4s-template-card {
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
  .g4s-template-card:hover {
    border-color: #444444;
    background: #121212;
  }
  .g4s-template-selector input[type=radio]:checked + span .g4s-template-card {
    border-color: #EDEDED;
    background: #1C1C1E;
  }
  .g4s-template-title {
    font-weight: 600;
    font-size: 13px;
    color: #EDEDED;
  }
  .g4s-template-desc {
    font-size: 11px;
    color: #A1A1AA;
    line-height: 1.4;
  }
  
  /* Tree Card and Previews */
  .g4s-tree-card {
    background: #0D0D0D;
    border: 1px solid #2D2D2D;
    padding: 16px;
    border-radius: 8px;
  }
  
  /* Recent Projects */
  .g4s-recent-projects-grid {
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
  .g4s-recent-project-card {
    background: #161616;
    border: 1px solid #2D2D2D;
    padding: 10px 12px;
    border-radius: 6px;
    cursor: pointer;
    transition: all 0.15s ease;
    display: block;
    text-align: left;
  }
  .g4s-recent-project-card:hover {
    border-color: #444444;
    background: #1C1C1E;
  }
  .g4s-recent-name {
    font-weight: 600;
    font-size: 13px;
    color: #EDEDED;
  }
  .g4s-recent-dir {
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
  .g4s-timeline {
    position: relative;
    padding-left: 16px;
    border-left: 2px solid #2D2D2D;
    margin-bottom: 24px;
  }
  .g4s-timeline-item {
    position: relative;
    margin-bottom: 24px;
    padding-left: 16px;
  }
  .g4s-timeline-item:last-child {
    margin-bottom: 0;
  }
  .g4s-timeline-dot {
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
  .g4s-timeline-content {
    background: #1C1C1E;
    border: 1px solid #2D2D2D;
    border-radius: 6px;
    padding: 12px 16px;
    transition: background-color 0.15s;
  }
  .g4s-timeline-content:hover {
    background: #262626;
  }
  .g4s-timeline-header {
    display: flex;
    justify-content: space-between;
    align-items: baseline;
    margin-bottom: 8px;
    font-size: 13px;
  }
  .g4s-timeline-author {
    font-weight: 600;
    color: #EDEDED;
  }
  .g4s-timeline-time {
    color: #A1A1AA;
    font-size: 12px;
  }
  .g4s-timeline-message {
    font-size: 14px;
    color: #D4D4D8;
    margin-bottom: 10px;
    white-space: pre-wrap;
    line-height: 1.4;
  }
  .g4s-timeline-hash {
    font-family: Menlo, Monaco, Consolas, monospace;
    font-size: 11px;
    color: #60A5FA;
    background: rgba(96, 165, 250, 0.1);
    padding: 3px 6px;
    border-radius: 4px;
    display: inline-block;
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
