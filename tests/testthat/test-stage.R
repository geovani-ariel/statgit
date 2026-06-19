test_that("git_stage e git_unstage controlam preparacao", {
  with_isolated_git_identity({
    project <- withr::local_tempdir()
    git_init(project)
    writeLines("x <- 1", file.path(project, "analise.R"))

    stage_result <- git_stage("analise.R", path = project)
    status <- repo_status_table(project)

    expect_true(stage_result$ok)
    expect_true(any(status$file == "analise.R" & status$staged))

    unstage_result <- git_unstage("analise.R", path = project)
    status <- repo_status_table(project)

    expect_true(unstage_result$ok)
    expect_false(any(status$file == "analise.R" & status$staged))
  })
})

test_that("git_commit salva apenas arquivos preparados", {
  with_isolated_git_identity({
    project <- withr::local_tempdir()
    git_init(project)
    writeLines("x <- 1", file.path(project, "analise.R"))
    writeLines("y <- 1", file.path(project, "rascunho.R"))

    git_stage("analise.R", path = project)
    result <- git_commit("Adiciona analise", path = project)
    status <- repo_status_table(project)

    expect_true(result$ok)
    expect_true(result$committed)
    expect_true(any(status$file == "rascunho.R"))
    expect_false(any(status$file == "analise.R"))
  })
})

test_that("git_discard descarta modificacao rastreada", {
  with_isolated_git_identity({
    project <- withr::local_tempdir()
    git_init(project)
    writeLines("x <- 1", file.path(project, "analise.R"))
    git_commit_all(path = project)
    writeLines("x <- 2", file.path(project, "analise.R"))

    result <- git_discard("analise.R", path = project)

    expect_true(result$ok)
    expect_equal(readLines(file.path(project, "analise.R")), "x <- 1")
    expect_equal(nrow(repo_status_table(project)), 0)
  })
})
