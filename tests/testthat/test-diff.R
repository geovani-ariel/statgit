test_that("git_changed lista arquivos alterados", {
  with_isolated_git_identity({
    project <- withr::local_tempdir()
    git_init(project)
    writeLines("v1", file.path(project, "analise.R"))
    git_commit_all(path = project)
    writeLines("v2", file.path(project, "analise.R"))

    changes <- git_changed(project)

    expect_equal(changes$file, "analise.R")
    expect_equal(changes$status, "modified")
  })
})

test_that("git_diff retorna diff de arquivo modificado", {
  with_isolated_git_identity({
    project <- withr::local_tempdir()
    git_init(project)
    writeLines("x <- 1", file.path(project, "analise.R"))
    git_commit_all(path = project)
    writeLines("x <- 2", file.path(project, "analise.R"))

    result <- git_diff("analise.R", path = project)

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

  expect_match(html, "tr-diff-meta", fixed = TRUE)
  expect_match(html, "tr-diff-hunk", fixed = TRUE)
  expect_match(html, "tr-diff-remove", fixed = TRUE)
  expect_match(html, "tr-diff-add", fixed = TRUE)
  expect_match(html, "tr-diff-context", fixed = TRUE)
  expect_match(html, "-x &lt; 1", fixed = TRUE)
})

test_that("format_diff_for_panel_html mostra estado vazio", {
  html <- format_diff_for_panel_html(character())

  expect_match(html, "tr-preview-empty", fixed = TRUE)
  expect_match(html, "No changes in this file", fixed = TRUE)
})
