test_that("git_changed_files lista arquivos alterados", {
  with_isolated_git_identity({
    project <- withr::local_tempdir()
    init_git_project(project)
    writeLines("v1", file.path(project, "analise.R"))
    first_commit(path = project)
    writeLines("v2", file.path(project, "analise.R"))

    changes <- git_changed_files(project)

    expect_equal(changes$file, "analise.R")
    expect_equal(changes$status, "modified")
  })
})

test_that("git_diff_file retorna diff de arquivo modificado", {
  with_isolated_git_identity({
    project <- withr::local_tempdir()
    init_git_project(project)
    writeLines("x <- 1", file.path(project, "analise.R"))
    first_commit(path = project)
    writeLines("x <- 2", file.path(project, "analise.R"))

    result <- git_diff_file("analise.R", path = project)

    expect_true(result$ok)
    expect_true(any(grepl("-x <- 1", result$diff, fixed = TRUE)))
    expect_true(any(grepl("+x <- 2", result$diff, fixed = TRUE)))
  })
})

test_that("format_diff_for_panel_html destaca e escapa linhas", {
  html <- format_diff_for_panel_html(c(
    "diff --git a/analise.R b/analise.R",
    "@@ -1 +1 @@",
    "-x < 1",
    "+x <- 2",
    " contexto"
  ))

  expect_match(html, "g4s-diff-meta", fixed = TRUE)
  expect_match(html, "g4s-diff-hunk", fixed = TRUE)
  expect_match(html, "g4s-diff-remove", fixed = TRUE)
  expect_match(html, "g4s-diff-add", fixed = TRUE)
  expect_match(html, "g4s-diff-context", fixed = TRUE)
  expect_match(html, "-x &lt; 1", fixed = TRUE)
})

test_that("format_diff_for_panel_html mostra estado vazio", {
  html <- format_diff_for_panel_html(character())

  expect_match(html, "g4s-diff-empty", fixed = TRUE)
  expect_match(html, "Nenhuma mudanca", fixed = TRUE)
})
