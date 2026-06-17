test_that("git4stats_panel_server inicializa com diagnostico e log", {
  skip_if_not_installed("shiny")

  project <- withr::local_tempdir()
  server <- git4stats_panel_server(project)

  expect_no_error(
    shiny::testServer(server, {
      expect_true(is.character(values$log))
      expect_match(values$log, "Diagnóstico Git do projeto")
      expect_true(is.list(panel_summary_items(d())))
    })
  )
})

test_that("painel atualiza diagnostico pela acao de Git", {
  skip_if_not_installed("shiny")

  project <- withr::local_tempdir()
  server <- git4stats_panel_server(project)

  shiny::testServer(server, {
    session$setInputs(git_diagnose = 1)

    expect_match(values$log, "Diagnóstico Git do projeto")
  })
})

test_that("painel cria projeto usando inputs da UI", {
  skip_if_not_installed("shiny")

  project <- withr::local_tempdir()
  created <- NULL
  server <- git4stats_panel_server(project)

  testthat::local_mocked_bindings(
    create_stats_project = function(path, template, include_data, initialize_git, open, extra_files) {
      created <<- list(
        path = as.character(path),
        template = template,
        include_data = include_data,
        initialize_git = initialize_git,
        open = open,
        extra_files = extra_files
      )
      invisible(list(ok = TRUE))
    },
    named_project_choices = function(root) stats::setNames(character(), character())
  )

  shiny::testServer(server, {
    session$setInputs(
      project_base_dir = project,
      project_name = "teste-painel",
      project_template = "tcc",
      project_include_data = FALSE,
      project_initialize_git = TRUE,
      project_open_after_create = FALSE,
      project_extra_files = "scripts/extra.R",
      project_search_root = project
    )
    session$setInputs(project_create = 1)

    expect_equal(
      as.character(normalize_project_path(created$path)),
      as.character(normalize_project_path(file.path(project, "teste-painel")))
    )
    expect_equal(created$template, "tcc")
    expect_false(created$include_data)
    expect_true(created$initialize_git)
    expect_false(created$open)
    expect_equal(created$extra_files, "scripts/extra.R")
  })
})

test_that("painel chama preview e formatacao pelos motores existentes", {
  skip_if_not_installed("shiny")

  project <- withr::local_tempdir()
  calls <- list()
  server <- git4stats_panel_server(project)

  testthat::local_mocked_bindings(
    preview_knit = function(path = NULL, style = FALSE) {
      calls$preview <<- list(path = path, style = style)
      invisible(list(ok = TRUE))
    },
    live_preview_knit = function(path = NULL, style = FALSE) {
      calls$live_preview <<- list(path = path, style = style)
      invisible(list(ok = TRUE))
    },
    format_active_file = function(path = NULL) {
      calls$format_active <<- list(path = path)
      invisible(list(ok = TRUE))
    },
    format_project_files = function(path = ".") {
      calls$format_project <<- list(path = path)
      invisible(list(ok = TRUE))
    }
  )

  shiny::testServer(server, {
    session$setInputs(report_path = "reports/a.Rmd", report_style = TRUE)
    session$setInputs(report_preview = 1)
    session$setInputs(report_live_preview = 1)
    session$setInputs(format_path = "scripts/a.R")
    session$setInputs(format_active = 1)
    session$setInputs(format_project = 1)

    expect_equal(calls$preview$path, "reports/a.Rmd")
    expect_true(calls$preview$style)
    expect_equal(calls$live_preview$path, "reports/a.Rmd")
    expect_true(calls$live_preview$style)
    expect_equal(calls$format_active$path, "scripts/a.R")
    expect_equal(
      as.character(normalize_project_path(calls$format_project$path)),
      as.character(normalize_project_path(project))
    )
  })
})

test_that("painel importa arquivo e mostra diff", {
  skip_if_not_installed("shiny")

  project <- withr::local_tempdir()
  calls <- list()
  server <- git4stats_panel_server(project)

  testthat::local_mocked_bindings(
    import_project_file = function(source, destination, path, move, add_to_git, overwrite) {
      calls$import <<- list(
        source = source,
        destination = destination,
        path = path,
        move = move,
        add_to_git = add_to_git,
        overwrite = overwrite
      )
      invisible(list(ok = TRUE))
    },
    git_diff_file = function(file, path = ".", staged = FALSE, context = "changes") {
      calls$diff <<- list(file = file, path = path, staged = staged, context = context)
      invisible(list(ok = TRUE, diff = c("- antigo", "+ novo")))
    }
  )

  shiny::testServer(server, {
    session$setInputs(
      file_source = "/tmp/dados.csv",
      file_destination = "data/raw",
      file_move = FALSE,
      file_add_to_git = TRUE,
      file_overwrite = FALSE
    )
    session$setInputs(file_import = 1)
    session$setInputs(changes_file = "analise.R", changes_staged = FALSE, changes_diff_context = "full")
    session$setInputs(changes_diff = 1)

    expect_equal(calls$import$source, "/tmp/dados.csv")
    expect_equal(calls$import$destination, "data/raw")
    expect_true(calls$import$add_to_git)
    expect_equal(calls$diff$file, "analise.R")
    expect_equal(calls$diff$context, "full")
    expect_match(values$log, "\\+ novo")
    expect_match(values$diff_html, "g4s-diff-remove", fixed = TRUE)
    expect_match(values$diff_html, "g4s-diff-add", fixed = TRUE)
  })
})

test_that("painel prepara, desprepara, descarta e commita selecionados", {
  skip_if_not_installed("shiny")

  project <- withr::local_tempdir()
  calls <- list()
  server <- git4stats_panel_server(project)

  testthat::local_mocked_bindings(
    stage_files = function(files, path = ".") {
      calls$stage <<- list(files = files, path = path)
      invisible(list(ok = TRUE))
    },
    unstage_files = function(files, path = ".") {
      calls$unstage <<- list(files = files, path = path)
      invisible(list(ok = TRUE))
    },
    discard_file_changes = function(files, path = ".") {
      calls$discard <<- list(files = files, path = path)
      invisible(list(ok = TRUE))
    },
    commit_staged_files = function(message = "Atualiza projeto", path = ".") {
      calls$commit <<- list(message = message, path = path)
      invisible(list(ok = TRUE, committed = TRUE))
    }
  )

  shiny::testServer(server, {
    session$setInputs(changes_files = c("analise.R", "dados.csv"))
    session$setInputs(changes_stage = 1)
    session$setInputs(changes_unstage = 1)
    session$setInputs(changes_discard = 1)

    expect_equal(calls$stage$files, c("analise.R", "dados.csv"))
    expect_equal(calls$unstage$files, c("analise.R", "dados.csv"))
    expect_null(calls$discard)

    session$setInputs(confirm_discard_action = 1)
    session$setInputs(changes_commit_message = "Salva analise")
    session$setInputs(changes_commit_selected = 1)

    expect_equal(calls$discard$files, c("analise.R", "dados.csv"))
    expect_equal(calls$commit$message, "Salva analise")
  })
})
