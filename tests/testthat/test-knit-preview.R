test_that("preview_knit usa o arquivo informado e abre o HTML gerado", {
  project <- withr::local_tempdir()
  input <- as.character(normalize_project_path(file.path(project, "relatorio.Rmd")))
  output <- file.path(project, "relatorio.html")
  writeLines("---\ntitle: Teste\n---", input)

  opened_path <- NULL

  testthat::local_mocked_bindings(
    render_preview_document = function(path) {
      expect_equal(as.character(path), input)
      output
    },
    open_preview_output = function(path) {
      opened_path <<- path
      invisible(path)
    }
  )

  result <- preview_knit(input)

  expect_true(result$ok)
  expect_equal(as.character(result$input), input)
  expect_equal(result$output, output)
  expect_equal(opened_path, output)
})

test_that("preview_knit usa o documento ativo quando path nao e informado", {
  project <- withr::local_tempdir()
  input <- as.character(normalize_project_path(file.path(project, "relatorio.qmd")))
  output <- file.path(project, "relatorio.html")
  writeLines("---\ntitle: Teste\n---", input)

  testthat::local_mocked_bindings(
    active_source_document_path = function() input,
    render_preview_document = function(path) {
      expect_equal(as.character(path), input)
      output
    },
    open_preview_output = function(path) invisible(path)
  )

  result <- preview_knit()

  expect_equal(as.character(result$input), input)
  expect_equal(result$output, output)
})

test_that("resolve_preview_document_path rejeita extensoes nao suportadas", {
  project <- withr::local_tempdir()
  input <- file.path(project, "relatorio.R")
  writeLines("x <- 1", input)

  expect_error(
    resolve_preview_document_path(input),
    "suporta arquivos .Rmd, .Rmarkdown e .qmd"
  )
})

test_that("preview_knit pode formatar antes de renderizar", {
  project <- withr::local_tempdir()
  input <- as.character(normalize_project_path(file.path(project, "relatorio.Rmd")))
  output <- file.path(project, "relatorio.html")
  writeLines("---\ntitle: Teste\n---", input)
  formatted <- character()

  testthat::local_mocked_bindings(
    format_active_file = function(path = NULL) {
      formatted <<- c(formatted, as.character(path))
      invisible(list(ok = TRUE, path = path))
    },
    render_preview_document = function(path) {
      expect_equal(as.character(path), input)
      output
    },
    open_preview_output = function(path) invisible(path)
  )

  result <- preview_knit(input, style = TRUE)

  expect_equal(formatted, input)
  expect_equal(result$output, output)
})

test_that("live_preview_knit usa Quarto para arquivos qmd", {
  project <- withr::local_tempdir()
  input <- as.character(normalize_project_path(file.path(project, "relatorio.qmd")))
  writeLines("---\ntitle: Teste\n---", input)
  called <- NULL

  testthat::local_mocked_bindings(
    start_quarto_live_preview = function(path) {
      called <<- as.character(path)
      invisible(list(ok = TRUE, mode = "quarto", input = path))
    }
  )

  result <- live_preview_knit(input)

  expect_equal(called, input)
  expect_equal(result$mode, "quarto")
})

test_that("live_preview_knit usa gadget para arquivos Rmd", {
  project <- withr::local_tempdir()
  input <- as.character(normalize_project_path(file.path(project, "relatorio.Rmd")))
  writeLines("---\ntitle: Teste\n---", input)
  called <- NULL

  testthat::local_mocked_bindings(
    start_rmarkdown_live_preview = function(path, style = FALSE, interval_ms = 1500) {
      called <<- list(path = as.character(path), style = style, interval_ms = interval_ms)
      invisible(list(ok = TRUE, mode = "rmarkdown", input = path))
    }
  )

  result <- live_preview_knit(input, style = TRUE, interval_ms = 900)

  expect_equal(called$path, input)
  expect_true(called$style)
  expect_equal(called$interval_ms, 900)
  expect_equal(result$mode, "rmarkdown")
})

test_that("start_quarto_live_preview chama o comando em background", {
  input <- "/tmp/relatorio.qmd"
  launched <- NULL

  testthat::local_mocked_bindings(
    quarto_command = function() "/usr/local/bin/quarto",
    run_background_command = function(command, args, wd = NULL) {
      launched <<- list(command = command, args = args, wd = wd)
      invisible(NULL)
    }
  )

  result <- start_quarto_live_preview(input)

  expect_equal(launched$command, "/usr/local/bin/quarto")
  expect_equal(launched$args, c("preview", input))
  expect_equal(launched$wd, dirname(input))
  expect_equal(result$mode, "quarto")
})
