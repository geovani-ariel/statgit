test_that("stage_files e unstage_files controlam preparacao", {
  with_isolated_git_identity({
    project <- withr::local_tempdir()
    init_git_project(project)
    writeLines("x <- 1", file.path(project, "analise.R"))

    stage_result <- stage_files("analise.R", path = project)
    status <- repo_status_table(project)

    expect_true(stage_result$ok)
    expect_true(any(status$file == "analise.R" & status$staged))

    unstage_result <- unstage_files("analise.R", path = project)
    status <- repo_status_table(project)

    expect_true(unstage_result$ok)
    expect_false(any(status$file == "analise.R" & status$staged))
  })
})

test_that("commit_staged_files salva apenas arquivos preparados", {
  with_isolated_git_identity({
    project <- withr::local_tempdir()
    init_git_project(project)
    writeLines("x <- 1", file.path(project, "analise.R"))
    writeLines("y <- 1", file.path(project, "rascunho.R"))

    stage_files("analise.R", path = project)
    result <- commit_staged_files("Adiciona analise", path = project)
    status <- repo_status_table(project)

    expect_true(result$ok)
    expect_true(result$committed)
    expect_true(any(status$file == "rascunho.R"))
    expect_false(any(status$file == "analise.R"))
  })
})

test_that("discard_file_changes descarta modificacao rastreada", {
  with_isolated_git_identity({
    project <- withr::local_tempdir()
    init_git_project(project)
    writeLines("x <- 1", file.path(project, "analise.R"))
    first_commit(path = project)
    writeLines("x <- 2", file.path(project, "analise.R"))

    result <- discard_file_changes("analise.R", path = project)

    expect_true(result$ok)
    expect_equal(readLines(file.path(project, "analise.R")), "x <- 1")
    expect_equal(nrow(repo_status_table(project)), 0)
  })
})
