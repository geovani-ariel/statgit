test_that("import_project_file copia arquivo para pasta do projeto", {
  project <- withr::local_tempdir()
  source <- file.path(withr::local_tempdir(), "dados.csv")
  writeLines("x,y\n1,2", source)

  result <- import_project_file(source, destination = "data/raw", path = project)

  expect_true(result$ok)
  expect_false(result$moved)
  expect_false(result$git_added)
  expect_equal(result$relative_path, "data/raw/dados.csv")
  expect_true(file.exists(file.path(project, "data/raw/dados.csv")))
  expect_true(file.exists(source))
})

test_that("import_project_file rejeita destino fora do projeto", {
  project <- withr::local_tempdir()
  source <- file.path(withr::local_tempdir(), "dados.csv")
  writeLines("x,y\n1,2", source)

  expect_error(
    import_project_file(source, destination = "../fora", path = project),
    "destino relativa dentro do projeto"
  )
})

test_that("import_project_file pode preparar arquivo no Git", {
  with_isolated_git_identity({
    project <- withr::local_tempdir()
    init_git_project(project)
    source <- file.path(withr::local_tempdir(), "dados.csv")
    writeLines("x,y\n1,2", source)

    result <- import_project_file(
      source,
      destination = "data/raw",
      path = project,
      add_to_git = TRUE
    )

    status <- repo_status_table(project)

    expect_true(result$git_added)
    expect_equal(result$relative_path, "data/raw/dados.csv")
    expect_true(any(status$staged))
  })
})
