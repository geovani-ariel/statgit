test_that("project_organize cria a estrutura básica sem sobrescrever README existente", {
  project <- withr::local_tempdir()
  readme_path <- file.path(project, "README.md")
  writeLines("README existente", readme_path)

  result <- project_organize(project, include_data = TRUE)

  expect_true(all(dir.exists(file.path(project, c(
    "R", "scripts", "data", "data/raw", "data/processed", "reports", "figs"
  )))))
  expect_equal(readLines(readme_path, warn = FALSE), "README existente")
  expect_false("README.md" %in% result$created_files)
})

test_that("project_organize cria READMEs de dados quando include_data é FALSE", {
  project <- withr::local_tempdir()

  project_organize(project, include_data = FALSE)

  expect_true(file.exists(file.path(project, "data/raw/README.md")))
  expect_true(file.exists(file.path(project, "data/processed/README.md")))
})

test_that("project_organize cria arquivos do template artigo_quarto", {
  project <- withr::local_tempdir()

  result <- project_organize(project, template = "artigo_quarto", include_data = TRUE)

  expect_equal(result$template, "artigo_quarto")
  expect_true(file.exists(file.path(project, "_quarto.yml")))
  expect_true(file.exists(file.path(project, "reports/artigo.qmd")))
  expect_true(file.exists(file.path(project, "refs.bib")))
})

test_that("project_create cria .Rproj, .gitignore e arquivos extras", {
  project <- file.path(withr::local_tempdir(), "meu-estudo")

  result <- project_create(
    path = project,
    template = "projeto_grupo",
    include_data = FALSE,
    initialize_git = FALSE,
    open = FALSE,
    extra_files = c("notes/reuniao.md", "scripts/99-checklist.R")
  )

  expect_equal(result$template, "projeto_grupo")
  expect_true(file.exists(file.path(project, "meu-estudo.Rproj")))
  expect_true(file.exists(file.path(project, ".gitignore")))
  expect_true(file.exists(file.path(project, "CONTRIBUTING.md")))
  expect_true(file.exists(file.path(project, "notes/reuniao.md")))
  expect_true(file.exists(file.path(project, "scripts/99-checklist.R")))
  expect_true(file.exists(file.path(project, "data/raw/README.md")))
  expect_true(file.exists(file.path(project, "data/processed/README.md")))
})

test_that("project_create rejeita arquivos extras fora do projeto", {
  project <- file.path(withr::local_tempdir(), "meu-estudo")

  expect_error(
    project_create(project, extra_files = "../fora.txt"),
    "caminhos relativos dentro do projeto"
  )
})
