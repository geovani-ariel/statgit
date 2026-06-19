test_that("github_connect adiciona um remote GitHub", {
  with_isolated_git_identity({
    project <- withr::local_tempdir()
    git_init(project, branch = "main")

    result <- github_connect(
      remote_url = "https://github.com/example/repo.git",
      path = project
    )
    diagnosis <- git_check(project)

    expect_true(result$ok)
    expect_true(result$changed)
    expect_true(diagnosis$has_remote)
    expect_equal(diagnosis$remote_name, "origin")
    expect_true(diagnosis$remote_is_github)
  })
})

test_that("github_check informa ausencia de remote", {
  with_isolated_git_identity({
    project <- withr::local_tempdir()
    git_init(project, branch = "main")

    result <- github_check(project)

    expect_false(result$ok)
    expect_equal(result$reason, "remote_missing")
  })
})

test_that("git_push falha sem commit ou sem remote", {
  with_isolated_git_identity({
    project <- withr::local_tempdir()
    git_init(project, branch = "main")

    no_commit <- git_push(project)
    expect_false(no_commit$ok)
    expect_equal(no_commit$reason, "no_commits")

    writeLines("conteudo", file.path(project, "analise.R"))
    git_commit_all(path = project)

    no_remote <- git_push(project)
    expect_false(no_remote$ok)
    expect_equal(no_remote$reason, "remote_missing")
  })
})
