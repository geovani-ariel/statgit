test_that("use_stats_project cria a estrutura básica sem sobrescrever README existente", {
  project <- withr::local_tempdir()
  readme_path <- file.path(project, "README.md")
  writeLines("README existente", readme_path)

  result <- use_stats_project(project, include_data = TRUE)

  expect_true(all(dir.exists(file.path(project, c(
    "R", "scripts", "data", "data/raw", "data/processed", "reports", "figs"
  )))))
  expect_equal(readLines(readme_path, warn = FALSE), "README existente")
  expect_false("README.md" %in% result$created_files)
})

test_that("use_stats_project cria READMEs de dados quando include_data é FALSE", {
  project <- withr::local_tempdir()

  use_stats_project(project, include_data = FALSE)

  expect_true(file.exists(file.path(project, "data/raw/README.md")))
  expect_true(file.exists(file.path(project, "data/processed/README.md")))
})

test_that("use_stats_project cria arquivos do template artigo_quarto", {
  project <- withr::local_tempdir()

  result <- use_stats_project(project, template = "artigo_quarto", include_data = TRUE)

  expect_equal(result$template, "artigo_quarto")
  expect_true(file.exists(file.path(project, "_quarto.yml")))
  expect_true(file.exists(file.path(project, "reports/artigo.qmd")))
  expect_true(file.exists(file.path(project, "refs.bib")))
})

test_that("create_stats_project cria .Rproj, .gitignore e arquivos extras", {
  project <- file.path(withr::local_tempdir(), "meu-estudo")

  result <- create_stats_project(
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

test_that("create_stats_project rejeita arquivos extras fora do projeto", {
  project <- file.path(withr::local_tempdir(), "meu-estudo")

  expect_error(
    create_stats_project(project, extra_files = "../fora.txt"),
    "caminhos relativos dentro do projeto"
  )
})
