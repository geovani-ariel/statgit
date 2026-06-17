test_that("connect_github_repo adiciona um remote GitHub", {
  with_isolated_git_identity({
    project <- withr::local_tempdir()
    init_git_project(project, branch = "main")

    result <- connect_github_repo(
      remote_url = "https://github.com/example/repo.git",
      path = project
    )
    diagnosis <- check_git_setup(project)

    expect_true(result$ok)
    expect_true(result$changed)
    expect_true(diagnosis$has_remote)
    expect_equal(diagnosis$remote_name, "origin")
    expect_true(diagnosis$remote_is_github)
  })
})

test_that("check_github_auth informa ausencia de remote", {
  with_isolated_git_identity({
    project <- withr::local_tempdir()
    init_git_project(project, branch = "main")

    result <- check_github_auth(project)

    expect_false(result$ok)
    expect_equal(result$reason, "remote_missing")
  })
})

test_that("push_first_time falha sem commit ou sem remote", {
  with_isolated_git_identity({
    project <- withr::local_tempdir()
    init_git_project(project, branch = "main")

    no_commit <- push_first_time(project)
    expect_false(no_commit$ok)
    expect_equal(no_commit$reason, "no_commits")

    writeLines("conteudo", file.path(project, "analise.R"))
    first_commit(path = project)

    no_remote <- push_first_time(project)
    expect_false(no_remote$ok)
    expect_equal(no_remote$reason, "remote_missing")
  })
})
