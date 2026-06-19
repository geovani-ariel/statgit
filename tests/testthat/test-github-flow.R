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

test_that("github_check aceita remote acessível mesmo sem refs publicadas", {
  testthat::local_mocked_bindings(
    git_installed = function() TRUE,
    is_git_repo = function(path = ".") TRUE,
    normalize_project_path = function(path = ".") path,
    remote_by_name = function(path = ".", remote = "origin") {
      list(name = "origin", url = "git@github.com:example/repo.git", is_github = TRUE)
    },
    run_git = function(args, path = NULL) {
      list(status = 0L, output = character())
    }
  )

  result <- github_check("fake-project")

  expect_true(result$ok)
  expect_equal(result$remote, "origin")
})

test_that("github_disconnect remove um remote existente", {
  with_isolated_git_identity({
    project <- withr::local_tempdir()
    git_init(project, branch = "main")

    github_connect(
      remote_url = "https://github.com/example/repo.git",
      path = project
    )

    result <- github_disconnect(project)
    diagnosis <- git_check(project)

    expect_true(result$ok)
    expect_true(result$changed)
    expect_false(diagnosis$has_remote)
    expect_null(diagnosis$remote_name)
    expect_null(diagnosis$remote_url)
  })
})

test_that("github_connect permite nome de remote diferente de origin", {
  with_isolated_git_identity({
    project <- withr::local_tempdir()
    git_init(project, branch = "main")

    result <- github_connect(
      remote_url = "git@github.com:example/repo.git",
      path = project,
      remote = "upstream"
    )

    upstream <- remote_by_name(project, remote = "upstream")

    expect_true(result$ok)
    expect_equal(result$remote, "upstream")
    expect_equal(upstream$name[[1]], "upstream")
    expect_equal(upstream$url[[1]], "git@github.com:example/repo.git")
    expect_equal(remote_protocol_label(upstream$url[[1]]), "SSH")
  })
})

test_that("github_connect atualiza URL existente quando replace = TRUE", {
  with_isolated_git_identity({
    project <- withr::local_tempdir()
    git_init(project, branch = "main")

    github_connect(
      remote_url = "https://github.com/example/old.git",
      path = project,
      remote = "origin"
    )

    result <- github_connect(
      remote_url = "https://github.com/example/new.git",
      path = project,
      remote = "origin",
      replace = TRUE
    )

    origin <- remote_by_name(project, remote = "origin")

    expect_true(result$ok)
    expect_true(result$changed)
    expect_equal(origin$url[[1]], "https://github.com/example/new.git")
  })
})

test_that("build_git_diagnosis prioriza o remote do upstream quando houver varios", {
  testthat::local_mocked_bindings(
    git_installed = function() TRUE,
    git_identity = function(path = ".") list(name = "Ada", email = "ada@example.com", scope = "global", complete = TRUE),
    git_global_config = function(key) if (grepl("name$", key)) "Ada" else "ada@example.com",
    project_context = function(path = ".") list(
      current_path = normalize_project_path(path),
      repo_path = normalize_project_path(path),
      repo_ref = normalize_project_path(path),
      is_repo_root = TRUE,
      rproj_files = character(),
      rproj_path = NULL
    ),
    repo_remote_info = function(path = ".") data.frame(
      name = c("origin", "upstream"),
      url = c("https://github.com/example/origin.git", "git@github.com:example/upstream.git"),
      is_github = c(TRUE, TRUE),
      stringsAsFactors = FALSE
    ),
    repo_upstream_branch = function(path = ".") "upstream/main",
    repo_has_commits = function(path = ".") TRUE,
    repo_current_branch = function(path = ".") "main",
    repo_sync_status = function(path = ".", remote = NULL, branch = NULL) list(
      has_upstream = TRUE,
      upstream_branch = "upstream/main",
      remote_branch_exists = TRUE,
      ahead = 0L,
      behind = 0L,
      can_compare = TRUE,
      remote_used = remote
    ),
    repo_status_table = function(path = ".") empty_status_table(),
    status_counts = function(status) list(staged = 0L, new = 0L, modified = 0L, deleted = 0L, conflicted = 0L, total = 0L)
  )

  diagnosis <- build_git_diagnosis("fake-project")

  expect_true(diagnosis$has_remote)
  expect_equal(diagnosis$remote_name, "upstream")
  expect_equal(diagnosis$remote_url, "git@github.com:example/upstream.git")
  expect_true(diagnosis$remote_is_github)
  expect_equal(diagnosis$sync_status$remote_used, "upstream")
})

test_that("build_git_diagnosis prioriza origin sem upstream configurado", {
  testthat::local_mocked_bindings(
    git_installed = function() TRUE,
    git_identity = function(path = ".") list(name = "Ada", email = "ada@example.com", scope = "global", complete = TRUE),
    git_global_config = function(key) if (grepl("name$", key)) "Ada" else "ada@example.com",
    project_context = function(path = ".") list(
      current_path = normalize_project_path(path),
      repo_path = normalize_project_path(path),
      repo_ref = normalize_project_path(path),
      is_repo_root = TRUE,
      rproj_files = character(),
      rproj_path = NULL
    ),
    repo_remote_info = function(path = ".") data.frame(
      name = c("backup", "origin"),
      url = c("https://example.com/backup.git", "https://github.com/example/origin.git"),
      is_github = c(FALSE, TRUE),
      stringsAsFactors = FALSE
    ),
    repo_upstream_branch = function(path = ".") NULL,
    repo_has_commits = function(path = ".") TRUE,
    repo_current_branch = function(path = ".") "main",
    repo_sync_status = function(path = ".", remote = NULL, branch = NULL) list(
      has_upstream = FALSE,
      upstream_branch = NULL,
      remote_branch_exists = TRUE,
      ahead = 0L,
      behind = 0L,
      can_compare = FALSE,
      remote_used = remote
    ),
    repo_status_table = function(path = ".") empty_status_table(),
    status_counts = function(status) list(staged = 0L, new = 0L, modified = 0L, deleted = 0L, conflicted = 0L, total = 0L)
  )

  diagnosis <- build_git_diagnosis("fake-project")

  expect_equal(diagnosis$remote_name, "origin")
  expect_equal(diagnosis$remote_url, "https://github.com/example/origin.git")
  expect_equal(diagnosis$sync_status$remote_used, "origin")
})

test_that("build_git_diagnosis troca para origin quando upstream e removido", {
  testthat::local_mocked_bindings(
    git_installed = function() TRUE,
    git_identity = function(path = ".") list(name = "Ada", email = "ada@example.com", scope = "global", complete = TRUE),
    git_global_config = function(key) if (grepl("name$", key)) "Ada" else "ada@example.com",
    project_context = function(path = ".") list(
      current_path = normalize_project_path(path),
      repo_path = normalize_project_path(path),
      repo_ref = normalize_project_path(path),
      is_repo_root = TRUE,
      rproj_files = character(),
      rproj_path = NULL
    ),
    repo_remote_info = function(path = ".") data.frame(
      name = "origin",
      url = "https://github.com/example/origin.git",
      is_github = TRUE,
      stringsAsFactors = FALSE
    ),
    repo_upstream_branch = function(path = ".") "upstream/main",
    repo_has_commits = function(path = ".") TRUE,
    repo_current_branch = function(path = ".") "main",
    repo_sync_status = function(path = ".", remote = NULL, branch = NULL) list(
      has_upstream = FALSE,
      upstream_branch = NULL,
      remote_branch_exists = TRUE,
      ahead = 0L,
      behind = 0L,
      can_compare = FALSE,
      remote_used = remote
    ),
    repo_status_table = function(path = ".") empty_status_table(),
    status_counts = function(status) list(staged = 0L, new = 0L, modified = 0L, deleted = 0L, conflicted = 0L, total = 0L)
  )

  diagnosis <- build_git_diagnosis("fake-project")

  expect_equal(diagnosis$remote_name, "origin")
  expect_equal(diagnosis$remote_url, "https://github.com/example/origin.git")
  expect_equal(diagnosis$sync_status$remote_used, "origin")
})

test_that("github_repo_browse_url converte remotes GitHub em URL navegável", {
  expect_equal(
    github_repo_browse_url("git@github.com:example/repo.git"),
    "https://github.com/example/repo"
  )
  expect_equal(
    github_repo_browse_url("ssh://git@github.com/example/repo.git"),
    "https://github.com/example/repo"
  )
  expect_equal(
    github_repo_browse_url("https://github.com/example/repo.git"),
    "https://github.com/example/repo"
  )
  expect_null(github_repo_browse_url("https://gitlab.com/example/repo.git"))
})

test_that("github_open_repo usa a URL web do remote configurado", {
  with_isolated_git_identity({
    project <- withr::local_tempdir()
    git_init(project, branch = "main")
    github_connect(
      remote_url = "git@github.com:example/repo.git",
      path = project
    )

    opened <- NULL
    testthat::local_mocked_bindings(
      browseURL = function(url, ...) {
        opened <<- url
        invisible(TRUE)
      },
      .package = "utils"
    )

    result <- github_open_repo(project)

    expect_true(result$ok)
    expect_equal(opened, "https://github.com/example/repo")
    expect_equal(result$browse_url, "https://github.com/example/repo")
  })
})

test_that("git_fetch busca atualizações do remote sem alterar a branch local", {
  with_isolated_git_identity({
    remote_repo <- file.path(withr::local_tempdir(), "origin.git")
    system2("git", c("init", "--bare", remote_repo))

    project <- withr::local_tempdir()
    git_init(project, branch = "main")
    writeLines("v1", file.path(project, "analise.R"))
    git_commit_all(path = project)
    run_git(c("remote", "add", "origin", remote_repo), path = project)
    git_push(project, remote = "origin")

    collaborator <- file.path(withr::local_tempdir(), "collaborator")
    system2("git", c("clone", remote_repo, collaborator))
    writeLines("v2", file.path(collaborator, "analise.R"))
    run_git(c("add", "analise.R"), path = collaborator)
    run_git(c("commit", "-m", "Atualiza remoto"), path = collaborator)
    run_git(c("push", "origin", "main"), path = collaborator)

    result <- git_fetch(project, remote = "origin")
    sync_status <- repo_sync_status(project, remote = "origin", branch = "main")

    expect_true(result$ok)
    expect_equal(sync_status$behind, 1L)
    expect_equal(sync_status$ahead, 0L)
  })
})

test_that("git_pull sinaliza conflito de rebase quando remoto e local divergem na mesma linha", {
  with_isolated_git_identity({
    remote_repo <- file.path(withr::local_tempdir(), "origin.git")
    system2("git", c("init", "--bare", remote_repo))

    seed <- withr::local_tempdir()
    git_init(seed, branch = "main")
    writeLines("base", file.path(seed, "analise.R"))
    git_commit_all(path = seed)
    run_git(c("remote", "add", "origin", remote_repo), path = seed)
    git_push(seed, remote = "origin")

    project <- file.path(withr::local_tempdir(), "project")
    collaborator <- file.path(withr::local_tempdir(), "collaborator")
    system2("git", c("clone", remote_repo, project))
    system2("git", c("clone", remote_repo, collaborator))

    writeLines("mudanca-remota", file.path(collaborator, "analise.R"))
    run_git(c("add", "analise.R"), path = collaborator)
    run_git(c("commit", "-m", "Atualiza remoto"), path = collaborator)
    run_git(c("push", "origin", "main"), path = collaborator)

    writeLines("mudanca-local", file.path(project, "analise.R"))
    run_git(c("add", "analise.R"), path = project)
    run_git(c("commit", "-m", "Atualiza local"), path = project)

    result <- git_pull(project, remote = "origin", branch = "main")

    expect_false(result$ok)
    expect_equal(result$reason, "pull_conflict")
    expect_true(any(grepl("CONFLICT|could not apply", result$output, ignore.case = TRUE)))

    run_git(c("rebase", "--abort"), path = project)
  })
})

test_that("git_sync sinaliza rebase em andamento quando pull entra em conflito", {
  with_isolated_git_identity({
    remote_repo <- file.path(withr::local_tempdir(), "origin.git")
    system2("git", c("init", "--bare", remote_repo))

    seed <- withr::local_tempdir()
    git_init(seed, branch = "main")
    writeLines("base", file.path(seed, "analise.R"))
    git_commit_all(path = seed)
    run_git(c("remote", "add", "origin", remote_repo), path = seed)
    git_push(seed, remote = "origin")

    project <- file.path(withr::local_tempdir(), "project")
    collaborator <- file.path(withr::local_tempdir(), "collaborator")
    system2("git", c("clone", remote_repo, project))
    system2("git", c("clone", remote_repo, collaborator))

    writeLines("mudanca-remota", file.path(collaborator, "analise.R"))
    run_git(c("add", "analise.R"), path = collaborator)
    run_git(c("commit", "-m", "Atualiza remoto"), path = collaborator)
    run_git(c("push", "origin", "main"), path = collaborator)

    writeLines("mudanca-local", file.path(project, "analise.R"))
    run_git(c("add", "analise.R"), path = project)
    run_git(c("commit", "-m", "Atualiza local"), path = project)

    result <- git_sync(project, remote = "origin", branch = "main")

    expect_false(result$ok)
    expect_equal(result$reason, "pull_conflict")
    expect_true(result$rebase_in_progress)
    expect_match(result$next_step, "git rebase --continue", fixed = TRUE)

    run_git(c("rebase", "--abort"), path = project)
  })
})

test_that("git_clone_repo clona repositório para a pasta de destino", {
  with_isolated_git_identity({
    source_repo <- withr::local_tempdir()
    git_init(source_repo, branch = "main")
    writeLines("Version: 1.0", file.path(source_repo, "estudo.Rproj"))
    writeLines("x <- 1", file.path(source_repo, "analise.R"))
    git_commit_all(path = source_repo)

    destination_root <- withr::local_tempdir()
    result <- git_clone_repo(source_repo, path = destination_root, directory = "clone-estudo", open = FALSE)

    expect_true(result$ok)
    expect_true(fs::dir_exists(result$path))
    expect_true(fs::file_exists(file.path(result$path, "estudo.Rproj")))
    expect_true(is_git_repo(result$path))
  })
})

test_that("git_clone_repo informa ausencia de .Rproj unico ao abrir clone", {
  with_isolated_git_identity({
    source_repo <- withr::local_tempdir()
    git_init(source_repo, branch = "main")
    writeLines("x <- 1", file.path(source_repo, "analise.R"))
    git_commit_all(path = source_repo)

    destination_root <- withr::local_tempdir()
    result <- git_clone_repo(source_repo, path = destination_root, directory = "clone-sem-rproj", open = TRUE)

    expect_true(result$ok)
    expect_false(result$open$ok)
    expect_false(result$open$opened)
    expect_equal(result$open$reason, "missing_rproj")
    expect_equal(as.character(normalize_project_path(result$open$path)), as.character(normalize_project_path(result$path)))
  })
})

test_that("git_clone_repo informa multiplos .Rproj ao abrir clone", {
  with_isolated_git_identity({
    source_repo <- withr::local_tempdir()
    git_init(source_repo, branch = "main")
    writeLines("Version: 1.0", file.path(source_repo, "a.Rproj"))
    writeLines("Version: 1.0", file.path(source_repo, "b.Rproj"))
    writeLines("x <- 1", file.path(source_repo, "analise.R"))
    git_commit_all(path = source_repo)

    destination_root <- withr::local_tempdir()
    result <- git_clone_repo(source_repo, path = destination_root, directory = "clone-varios-rproj", open = TRUE)

    expect_true(result$ok)
    expect_false(result$open$ok)
    expect_false(result$open$opened)
    expect_equal(result$open$reason, "multiple_rproj")
    expect_length(result$open$rproj_files, 2L)
    expect_equal(as.character(normalize_project_path(result$open$path)), as.character(normalize_project_path(result$path)))
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

test_that("disconnect seguido de connect com outro nome troca o remote preferido", {
  with_isolated_git_identity({
    project <- withr::local_tempdir()
    git_init(project, branch = "main")

    github_connect(
      remote_url = "https://github.com/example/origin.git",
      path = project,
      remote = "origin"
    )
    github_disconnect(project, remote = "origin")

    result <- github_connect(
      remote_url = "git@github.com:example/upstream.git",
      path = project,
      remote = "upstream"
    )
    diagnosis <- build_git_diagnosis(project)

    expect_true(result$ok)
    expect_equal(diagnosis$remote_name, "upstream")
    expect_equal(diagnosis$remote_url, "git@github.com:example/upstream.git")
  })
})

test_that("github_connect informa remote existente sem replace quando URL difere", {
  with_isolated_git_identity({
    project <- withr::local_tempdir()
    git_init(project, branch = "main")

    github_connect(
      remote_url = "https://github.com/example/origin.git",
      path = project,
      remote = "origin"
    )

    result <- github_connect(
      remote_url = "https://github.com/example/outro.git",
      path = project,
      remote = "origin",
      replace = FALSE
    )

    expect_true(result$ok)
    expect_false(result$changed)
    expect_equal(result$reason, "remote_exists")
    expect_equal(result$remote_url, "https://github.com/example/origin.git")
    expect_equal(result$requested_remote_url, "https://github.com/example/outro.git")
  })
})

test_that("repo_sync_status detecta branch atras da remota e git_push bloqueia", {
  with_isolated_git_identity({
    remote_repo <- file.path(withr::local_tempdir(), "origin.git")
    system2("git", c("init", "--bare", remote_repo))

    project <- withr::local_tempdir()
    git_init(project, branch = "main")
    writeLines("v1", file.path(project, "analise.R"))
    git_commit_all(path = project)
    run_git(c("remote", "add", "origin", remote_repo), path = project)
    git_push(project, remote = "origin")

    collaborator <- file.path(withr::local_tempdir(), "collaborator")
    system2("git", c("clone", remote_repo, collaborator))
    writeLines("v2", file.path(collaborator, "analise.R"))
    run_git(c("add", "analise.R"), path = collaborator)
    run_git(c("commit", "-m", "Atualiza remoto"), path = collaborator)
    run_git(c("push", "origin", "main"), path = collaborator)

    run_git(c("fetch", "origin"), path = project)

    sync_status <- repo_sync_status(project, remote = "origin", branch = "main")
    blocked_push <- git_push(project, remote = "origin", branch = "main")

    expect_true(sync_status$has_upstream)
    expect_true(sync_status$remote_branch_exists)
    expect_equal(sync_status$behind, 1L)
    expect_equal(sync_status$ahead, 0L)
    expect_false(blocked_push$ok)
    expect_equal(blocked_push$reason, "behind_remote")
  })
})
