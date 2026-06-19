test_that("git_ignore não duplica linhas e ignora dados quando pedido", {
  project <- withr::local_tempdir()

  git_ignore(project, include_data = FALSE)
  git_ignore(project, include_data = FALSE)

  lines <- readLines(file.path(project, ".gitignore"), warn = FALSE)

  expect_true("data/raw/*" %in% lines)
  expect_true("data/processed/*" %in% lines)
  expect_equal(sum(lines == ".Rhistory"), 1)
  expect_equal(sum(lines == "data/raw/*"), 1)
})

test_that("git_ignore sugere decisão quando include_data é NULL", {
  project <- withr::local_tempdir()

  result <- git_ignore(project, include_data = NULL)
  lines <- readLines(result$path, warn = FALSE)

  expect_false("data/raw/*" %in% lines)
  expect_false("data/processed/*" %in% lines)
})

test_that("git_ignore completa um arquivo parcial sem repetir conteudo", {
  project <- withr::local_tempdir()
  writeLines(c(".Rhistory", ".RData"), file.path(project, ".gitignore"))

  git_ignore(project, include_data = FALSE)
  lines <- readLines(file.path(project, ".gitignore"), warn = FALSE)

  expect_equal(sum(lines == ".Rhistory"), 1)
  expect_equal(sum(lines == ".RData"), 1)
  expect_true(".Rproj.user/" %in% lines)
  expect_true("data/raw/*" %in% lines)
})
