test_that("painel aciona criacao de arquivo com os dados da UI", {
  skip_if_not_installed("shiny")

  project <- withr::local_tempdir()
  calls <- list()
  server <- trackR_panel_server(project)

  testthat::local_mocked_bindings(
    file_create = function(filename, type = "R", destination = ".", path = ".", content = NULL, open_in_rstudio = TRUE) {
      calls$create <<- list(
        filename = filename,
        type = type,
        destination = destination,
        path = path,
        content = content,
        open_in_rstudio = open_in_rstudio
      )
      invisible(list(ok = TRUE))
    }
  )

  shiny::testServer(server, {
    session$setInputs(
      file_create_name = "analise.qmd",
      file_create_type = "qmd",
      file_create_destination = "reports",
      file_create_content = "---\ntitle: \"Teste\"\n---\n",
      file_create_open = FALSE
    )
    session$setInputs(file_create = 1)

    expect_equal(calls$create$filename, "analise.qmd")
    expect_equal(calls$create$type, "qmd")
    expect_equal(calls$create$destination, "reports")
    expect_equal(calls$create$content, "---\ntitle: \"Teste\"\n---\n")
    expect_false(calls$create$open_in_rstudio)
    expect_equal(
      as.character(normalize_project_path(calls$create$path)),
      as.character(normalize_project_path(project))
    )
  })
})

test_that("painel confirma exclusao antes de deletar", {
  skip_if_not_installed("shiny")

  project <- withr::local_tempdir()
  calls <- list()
  server <- trackR_panel_server(project)

  testthat::local_mocked_bindings(
    file_delete_info = function(path_to_delete, path = ".") {
      list(
        ok = TRUE,
        relative_path = path_to_delete,
        item_kind = "file",
        item_type_label = "Arquivo",
        label = sprintf("Arquivo '%s'", path_to_delete),
        was_tracked = TRUE
      )
    },
    file_delete = function(path_to_delete, path = ".") {
      calls$delete <<- list(path_to_delete = path_to_delete, path = path)
      invisible(list(ok = TRUE))
    }
  )

  shiny::testServer(server, {
    session$setInputs(file_delete_path = "scripts/velho.R")
    session$setInputs(file_delete = 1)

    expect_equal(values$pending_delete, "scripts/velho.R")
    expect_null(calls$delete)

    session$setInputs(confirm_file_delete = 1)

    expect_equal(calls$delete$path_to_delete, "scripts/velho.R")
    expect_equal(
      as.character(normalize_project_path(calls$delete$path)),
      as.character(normalize_project_path(project))
    )
    expect_null(values$pending_delete)
  })
})

test_that("abas criar e deletar exibem a estrutura atual do projeto", {
  skip_if_not_installed("shiny")

  project <- withr::local_tempdir()
  writeLines("x <- 1", file.path(project, "scripts.R"))
  diagnosis <- build_git_diagnosis(project)

  criar_html <- htmltools::renderTags(criar_module_ui(diagnosis))$html
  excluir_html <- htmltools::renderTags(excluir_module_ui(diagnosis))$html

  expect_match(criar_html, "Estrutura atual do projeto", fixed = TRUE)
  expect_match(excluir_html, "Estrutura atual do projeto", fixed = TRUE)
  expect_match(criar_html, "recent_files_explorer", fixed = TRUE)
  expect_match(excluir_html, "recent_files_explorer", fixed = TRUE)
})
