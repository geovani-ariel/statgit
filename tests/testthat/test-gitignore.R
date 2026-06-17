test_that("create_r_gitignore não duplica linhas e ignora dados quando pedido", {
  project <- withr::local_tempdir()

  create_r_gitignore(project, include_data = FALSE)
  create_r_gitignore(project, include_data = FALSE)

  lines <- readLines(file.path(project, ".gitignore"), warn = FALSE)

  expect_true("data/raw/*" %in% lines)
  expect_true("data/processed/*" %in% lines)
  expect_equal(sum(lines == ".Rhistory"), 1)
  expect_equal(sum(lines == "data/raw/*"), 1)
})

test_that("create_r_gitignore sugere decisão quando include_data é NULL", {
  project <- withr::local_tempdir()

  result <- create_r_gitignore(project, include_data = NULL)
  lines <- readLines(result$path, warn = FALSE)

  expect_false("data/raw/*" %in% lines)
  expect_false("data/processed/*" %in% lines)
})

test_that("create_r_gitignore completa um arquivo parcial sem repetir conteudo", {
  project <- withr::local_tempdir()
  writeLines(c(".Rhistory", ".RData"), file.path(project, ".gitignore"))

  create_r_gitignore(project, include_data = FALSE)
  lines <- readLines(file.path(project, ".gitignore"), warn = FALSE)

  expect_equal(sum(lines == ".Rhistory"), 1)
  expect_equal(sum(lines == ".RData"), 1)
  expect_true(".Rproj.user/" %in% lines)
  expect_true("data/raw/*" %in% lines)
})
