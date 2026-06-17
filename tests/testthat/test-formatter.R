test_that("find_styleable_files encontra apenas arquivos suportados", {
  project <- withr::local_tempdir()
  dir.create(file.path(project, "R"), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(project, "reports"), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(project, "renv"), recursive = TRUE, showWarnings = FALSE)
  writeLines("x<-1", file.path(project, "R", "analise.R"))
  writeLines("---", file.path(project, "reports", "relatorio.qmd"))
  writeLines("options()", file.path(project, ".Rprofile"))
  writeLines("nao", file.path(project, "README.md"))
  writeLines("ignorar", file.path(project, "renv", "activate.R"))

  files <- find_styleable_files(project)

  expect_equal(
    basename(files),
    c(".Rprofile", "analise.R", "relatorio.qmd")
  )
})

test_that("format_active_file roda styler e atualiza editor ativo", {
  project <- withr::local_tempdir()
  input <- as.character(normalize_project_path(file.path(project, "analise.R")))
  writeLines("x <- 1", input)
  saved_ids <- character()
  refreshed <- list()
  styled <- character()

  testthat::local_mocked_bindings(
    ensure_suggested_package = function(...) TRUE,
    source_editor_context = function() list(path = input, id = "doc-1"),
    save_source_document = function(id = NULL) {
      saved_ids <<- c(saved_ids, id %||% "")
      TRUE
    },
    run_styler_file = function(path) {
      styled <<- c(styled, as.character(path))
      invisible(path)
    },
    refresh_source_document_from_disk = function(path, id) {
      refreshed <<- list(path = as.character(path), id = id)
      TRUE
    }
  )

  result <- format_active_file(input)

  expect_true(result$ok)
  expect_equal(styled, input)
  expect_equal(saved_ids, c("doc-1"))
  expect_equal(refreshed$id, "doc-1")
  expect_equal(refreshed$path, input)
})

test_that("format_project_files formata todos os arquivos suportados", {
  project <- withr::local_tempdir()
  dir.create(file.path(project, "R"), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(project, "reports"), recursive = TRUE, showWarnings = FALSE)
  writeLines("x<-1", file.path(project, "R", "a.R"))
  writeLines("y<-2", file.path(project, "R", "b.R"))
  writeLines("---", file.path(project, "reports", "relatorio.Rmd"))
  styled <- character()

  testthat::local_mocked_bindings(
    ensure_suggested_package = function(...) TRUE,
    run_styler_file = function(path) {
      styled <<- c(styled, basename(path))
      invisible(path)
    },
    source_editor_context = function() NULL
  )

  result <- format_project_files(project)

  expect_true(result$ok)
  expect_equal(sort(styled), c("a.R", "b.R", "relatorio.Rmd"))
  expect_length(result$styled_files, 3)
})
