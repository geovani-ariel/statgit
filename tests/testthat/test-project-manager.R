test_that("project_find lista arquivos .Rproj encontrados", {
  root <- withr::local_tempdir()
  dir.create(file.path(root, "a"), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(root, "b"), recursive = TRUE, showWarnings = FALSE)
  writeLines("Version: 1.0", file.path(root, "a", "a.Rproj"))
  writeLines("Version: 1.0", file.path(root, "b", "b.Rproj"))

  projects <- project_find(root)

  expect_equal(nrow(projects), 2)
  expect_equal(projects$name, c("a", "b"))
})

test_that("project_open retorna o caminho quando RStudio nao esta disponivel", {
  root <- withr::local_tempdir()
  project_file <- file.path(root, "analise.Rproj")
  writeLines("Version: 1.0", project_file)

  testthat::local_mocked_bindings(
    rstudio_available = function() FALSE
  )

  result <- project_open(project_file)

  expect_true(result$ok)
  expect_false(result$opened)
  expect_equal(as.character(result$path), as.character(normalize_project_path(project_file)))
})

test_that("project_open usa openProject quando RStudio esta disponivel", {
  root <- withr::local_tempdir()
  project_file <- file.path(root, "analise.Rproj")
  writeLines("Version: 1.0", project_file)
  opened <- NULL

  testthat::local_mocked_bindings(
    rstudio_available = function() TRUE,
    rstudio_open_project = function(path) {
      opened <<- path
      invisible(TRUE)
    }
  )

  result <- project_open(project_file)

  expect_true(result$opened)
  expect_equal(as.character(opened), as.character(normalize_project_path(project_file)))
})
