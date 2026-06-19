test_that("file_import copia arquivo para pasta do projeto", {
  project <- withr::local_tempdir()
  source <- file.path(withr::local_tempdir(), "dados.csv")
  writeLines("x,y\n1,2", source)

  result <- file_import(source, destination = "data/raw", path = project)

  expect_true(result$ok)
  expect_false(result$moved)
  expect_false(result$git_added)
  expect_equal(result$relative_path, "data/raw/dados.csv")
  expect_true(file.exists(file.path(project, "data/raw/dados.csv")))
  expect_true(file.exists(source))
})

test_that("file_import rejeita destino fora do projeto", {
  project <- withr::local_tempdir()
  source <- file.path(withr::local_tempdir(), "dados.csv")
  writeLines("x,y\n1,2", source)

  expect_error(
    file_import(source, destination = "../fora", path = project),
    "destino relativa dentro do projeto"
  )
})

test_that("file_import pode preparar arquivo no Git", {
  with_isolated_git_identity({
    project <- withr::local_tempdir()
    git_init(project)
    source <- file.path(withr::local_tempdir(), "dados.csv")
    writeLines("x,y\n1,2", source)

    result <- file_import(
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

test_that("file_create cria arquivo com template customizavel", {
  project <- withr::local_tempdir()

  result <- file_create(
    filename = "analise.R",
    type = "R",
    destination = "scripts",
    path = project,
    content = "x <- 1"
  )

  created_path <- file.path(project, "scripts", "analise.R")

  expect_true(result$ok)
  expect_equal(result$relative_path, "scripts/analise.R")
  expect_true(file.exists(created_path))
  expect_equal(readLines(created_path, warn = FALSE), "x <- 1")
})

test_that("file_create adiciona extensao quando nome nao informa", {
  project <- withr::local_tempdir()

  result <- file_create(
    filename = "analise",
    type = "qmd",
    destination = "reports",
    path = project,
    content = "",
    open_in_rstudio = FALSE
  )

  expect_true(result$ok)
  expect_equal(result$relative_path, "reports/analise.qmd")
  created_path <- file.path(project, "reports", "analise.qmd")
  expect_true(file.exists(created_path))
  expect_equal(unname(file.info(created_path)$size), 0)
})

test_that("file_create usa template padrao quando conteudo nao e informado", {
  project <- withr::local_tempdir()

  result <- file_create(
    filename = "relatorio.qmd",
    type = "qmd",
    destination = "reports",
    path = project,
    open_in_rstudio = FALSE
  )

  created_path <- file.path(project, "reports", "relatorio.qmd")
  created_lines <- readLines(created_path, warn = FALSE)

  expect_true(result$ok)
  expect_equal(result$relative_path, "reports/relatorio.qmd")
  expect_true(any(grepl('title: "Relatório"', created_lines, fixed = TRUE)))
  expect_true(any(grepl("^format: html$", created_lines)))
})

test_that("file_delete remove arquivo dentro do projeto", {
  project <- withr::local_tempdir()
  dir.create(file.path(project, "scripts"), recursive = TRUE)
  target <- file.path(project, "scripts", "velho.R")
  writeLines("old <- TRUE", target)

  result <- file_delete("scripts/velho.R", path = project)

  expect_true(result$ok)
  expect_equal(result$item_type, "file")
  expect_false(file.exists(target))
})

test_that("file_delete protege arquivos criticos e caminhos fora do projeto", {
  project <- withr::local_tempdir()
  protected <- file.path(project, "meu-projeto.Rproj")
  writeLines("Version: 1.0", protected)

  protected_result <- file_delete("meu-projeto.Rproj", path = project)
  outside_result <- file_delete("../fora.txt", path = project)

  expect_false(protected_result$ok)
  expect_equal(protected_result$reason, "protected_file")
  expect_false(outside_result$ok)
  expect_equal(outside_result$reason, "outside_project")
})

test_that("file_delete identifica item rastreado no Git mesmo sem mudancas pendentes", {
  with_isolated_git_identity({
    project <- withr::local_tempdir()
    git_init(project)
    dir.create(file.path(project, "scripts"), recursive = TRUE)
    target <- file.path(project, "scripts", "analise.R")
    writeLines("x <- 1", target)
    git_stage("scripts/analise.R", path = project)
    git_commit("Adiciona arquivo", path = project)

    result <- file_delete("scripts/analise.R", path = project)

    expect_true(result$ok)
    expect_true(result$was_tracked)
    expect_false(file.exists(target))
  })
})

test_that("file_delete pode preparar remocao de arquivo rastreado no Git", {
  with_isolated_git_identity({
    project <- withr::local_tempdir()
    git_init(project)
    dir.create(file.path(project, "scripts"), recursive = TRUE)
    target <- file.path(project, "scripts", "analise.R")
    writeLines("x <- 1", target)
    git_stage("scripts/analise.R", path = project)
    git_commit("Adiciona arquivo", path = project)

    result <- file_delete("scripts/analise.R", path = project, remove_from_git = TRUE)
    status <- repo_status_table(project)

    expect_true(result$ok)
    expect_true(result$was_tracked)
    expect_true(result$git_removed)
    expect_false(file.exists(target))
    expect_true(any(status$file == "scripts/analise.R" & status$status == "deleted" & status$staged))
  })
})
