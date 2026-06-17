test_that("check_git_setup informa ausência de repositório Git", {
  with_isolated_git_identity({
    project <- withr::local_tempdir()
    file.create(file.path(project, "analise.R"))

    diagnosis <- check_git_setup(project)

    expect_true(diagnosis$git_installed)
    expect_false(diagnosis$has_repo)
    expect_false(diagnosis$has_commits)
    expect_equal(diagnosis$status_counts$total, 0)
  })
})

test_that("check_git_setup detecta remote e repositorio acima da subpasta", {
  with_isolated_git_identity({
    project <- withr::local_tempdir()
    init_git_project(project, branch = "main")
    run_git(c("remote", "add", "origin", "https://github.com/example/repo.git"), path = project)
    nested <- file.path(project, "scripts")
    dir.create(nested)

    diagnosis <- check_git_setup(nested)

    expect_true(diagnosis$has_repo)
    expect_false(diagnosis$is_repo_root)
    expect_true(diagnosis$has_remote)
    expect_equal(diagnosis$remote_name, "origin")
    expect_true(diagnosis$remote_is_github)
  })
})

test_that("git_status_pretty descreve arquivos novos e modificados", {
  with_isolated_git_identity({
    project <- withr::local_tempdir()
    init_git_project(project, branch = "main")
    writeLines("versão 1", file.path(project, "analise.R"))
    first_commit(path = project)
    writeLines("versão 2", file.path(project, "analise.R"))
    writeLines("novo arquivo", file.path(project, "relatorio.qmd"))

    output <- capture.output(git_status_pretty(project))

    expect_true(any(grepl("Arquivos novos ainda fora do histórico", output, fixed = TRUE)))
    expect_true(any(grepl("relatorio.qmd", output, fixed = TRUE)))
    expect_true(any(grepl("Arquivos modificados desde o último commit", output, fixed = TRUE)))
    expect_true(any(grepl("analise.R", output, fixed = TRUE)))
  })
})

test_that("git_status_pretty descreve staged e removidos", {
  with_isolated_git_identity({
    project <- withr::local_tempdir()
    init_git_project(project, branch = "main")
    writeLines("v1", file.path(project, "analise.R"))
    first_commit(path = project)
    unlink(file.path(project, "analise.R"))
    writeLines("novo", file.path(project, "README.md"))
    gert::git_add("README.md", repo = project)

    output <- capture.output(git_status_pretty(project))

    expect_true(any(grepl("Arquivos removidos desde o último commit", output, fixed = TRUE)))
    expect_true(any(grepl("Arquivos prontos para commit (staged)", output, fixed = TRUE)))
  })
})
