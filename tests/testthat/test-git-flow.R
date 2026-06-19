test_that("git_init inicializa um repositório com branch main", {
  with_isolated_git_identity({
    project <- withr::local_tempdir()

    result <- git_init(project, branch = "main")
    diagnosis <- git_check(project)

    expect_true(result$ok)
    expect_true(isTRUE(diagnosis$has_repo))
    expect_equal(diagnosis$branch, "main")
    expect_false(diagnosis$has_commits)
  })
})

test_that("git_init nao altera projeto que ja e Git", {
  with_isolated_git_identity({
    project <- withr::local_tempdir()
    git_init(project, branch = "main")

    result <- git_init(project, branch = "main")

    expect_true(result$ok)
    expect_false(result$created)
  })
})

test_that("git_commit_all cria um commit quando há arquivos no projeto", {
  with_isolated_git_identity({
    project <- withr::local_tempdir()
    git_init(project, branch = "main")
    writeLines("conteúdo", file.path(project, "analise.R"))

    result <- git_commit_all(path = project)
    diagnosis <- git_check(project)

    expect_true(result$ok)
    expect_true(result$committed)
    expect_true(diagnosis$has_commits)
    expect_equal(diagnosis$status_counts$total, 0)
  })
})

test_that("git_commit_all informa quando nao ha arquivos novos", {
  with_isolated_git_identity({
    project <- withr::local_tempdir()
    git_init(project, branch = "main")

    result <- git_commit_all(path = project)

    expect_true(result$ok)
    expect_false(result$committed)
    expect_equal(result$reason, "nothing_to_commit")
  })
})

test_that("git_commit_all falha sem identidade Git configurada", {
  with_isolated_git_home({
    project <- withr::local_tempdir()
    git_init(project, branch = "main")
    writeLines("conteudo", file.path(project, "analise.R"))

    result <- git_commit_all(path = project)

    expect_false(result$ok)
    expect_equal(result$reason, "missing_identity")
  })
})
