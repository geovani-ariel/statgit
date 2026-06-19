test_that("statgit_panel_server inicializa com diagnostico e log", {
  skip_if_not_installed("shiny")

  project <- withr::local_tempdir()
  server <- statgit_panel_server(project)

  expect_no_error(
    shiny::testServer(server, {
      expect_true(is.character(values$log))
      expect_match(values$log, "Diagnóstico Git do projeto")
      expect_true(is.list(panel_summary_items(d())))
    })
  )
})

test_that("painel atualiza diagnostico pela acao de Git", {
  skip_if_not_installed("shiny")

  project <- withr::local_tempdir()
  server <- statgit_panel_server(project)

  shiny::testServer(server, {
    session$setInputs(git_diagnose = 1)

    expect_match(values$log, "Diagnóstico Git do projeto")
  })
})

test_that("painel cria projeto usando inputs da UI", {
  skip_if_not_installed("shiny")

  project <- withr::local_tempdir()
  created <- NULL
  server <- statgit_panel_server(project)

  testthat::local_mocked_bindings(
    project_create = function(path, template, include_data, initialize_git, open, extra_files) {
      created <<- list(
        path = as.character(path),
        template = template,
        include_data = include_data,
        initialize_git = initialize_git,
        open = open,
        extra_files = extra_files
      )
      invisible(list(ok = TRUE))
    },
    named_project_choices = function(root) stats::setNames(character(), character())
  )

  shiny::testServer(server, {
    session$setInputs(
      project_base_dir = project,
      project_name = "teste-painel",
      project_template = "tcc",
      project_include_data = FALSE,
      project_initialize_git = TRUE,
      project_open_after_create = FALSE,
      project_extra_files = "scripts/extra.R",
      project_search_root = project
    )
    session$setInputs(project_create = 1)

    expect_equal(
      as.character(normalize_project_path(created$path)),
      as.character(normalize_project_path(file.path(project, "teste-painel")))
    )
    expect_equal(created$template, "tcc")
    expect_false(created$include_data)
    expect_true(created$initialize_git)
    expect_false(created$open)
    expect_equal(created$extra_files, "scripts/extra.R")
  })
})

test_that("painel clona projeto usando inputs da UI", {
  skip_if_not_installed("shiny")

  project <- withr::local_tempdir()
  cloned <- NULL
  server <- statgit_panel_server(project)

  testthat::local_mocked_bindings(
    git_clone_repo = function(remote_url, path, directory, open) {
      cloned <<- list(
        remote_url = remote_url,
        path = path,
        directory = directory,
        open = open
      )
      invisible(list(ok = TRUE))
    }
  )

  shiny::testServer(server, {
    session$setInputs(
      project_clone_url = "https://github.com/example/repo.git",
      project_clone_base_dir = project,
      project_clone_dir = "repo-local",
      project_clone_open_after = FALSE
    )
    session$setInputs(project_clone = 1)

    expect_equal(cloned$remote_url, "https://github.com/example/repo.git")
    expect_equal(as.character(normalize_project_path(cloned$path)), as.character(normalize_project_path(project)))
    expect_equal(cloned$directory, "repo-local")
    expect_false(cloned$open)
  })
})

test_that("painel mostra proxima acao ao clonar projeto com multiplos .Rproj", {
  skip_if_not_installed("shiny")

  project <- withr::local_tempdir()
  clone_path <- fs::path(project, "repo-clonado")
  fs::dir_create(clone_path)
  notification <- NULL

  diagnosis_original <- structure(list(
    current_path = project,
    repo_path = project,
    is_repo_root = TRUE,
    rproj_path = fs::path(project, "original.Rproj"),
    git_installed = TRUE,
    user_name = "Nome",
    user_email = "email@example.com",
    identity = list(name = "Nome", email = "email@example.com", scope = "global", complete = TRUE),
    is_rstudio_project = TRUE,
    has_repo = TRUE,
    has_commits = TRUE,
    branch = "main",
    has_remote = TRUE,
    remote_name = "origin",
    remote_url = "https://github.com/exemplo/original.git",
    remote_is_github = TRUE,
    sync_status = list(has_upstream = TRUE, upstream_branch = "origin/main", remote_branch_exists = TRUE, ahead = 0L, behind = 0L, can_compare = TRUE),
    status = empty_status_table(),
    status_counts = list(staged = 0L, new = 0L, modified = 0L, deleted = 0L, conflicted = 0L, total = 0L)
  ), class = "trackr_diagnosis")

  diagnosis_clone <- diagnosis_original
  diagnosis_clone$current_path <- clone_path
  diagnosis_clone$repo_path <- clone_path
  diagnosis_clone$is_rstudio_project <- FALSE
  diagnosis_clone$rproj_path <- NULL
  diagnosis_clone$remote_url <- "https://github.com/exemplo/clonado.git"

  server <- statgit_panel_server(project, initial_diagnosis = diagnosis_original)

  testthat::local_mocked_bindings(
    git_clone_repo = function(remote_url, path, directory, open) {
      invisible(list(
        ok = TRUE,
        remote_url = remote_url,
        path = clone_path,
        open = list(
          ok = FALSE,
          opened = FALSE,
          reason = "multiple_rproj",
          path = clone_path,
          rproj_files = c(fs::path(clone_path, "a.Rproj"), fs::path(clone_path, "b.Rproj"))
        )
      ))
    },
    build_git_diagnosis = function(path = ".") {
      normalized <- normalize_project_path(path)
      if (identical(normalized, normalize_project_path(project))) {
        return(diagnosis_original)
      }
      if (identical(normalized, normalize_project_path(clone_path))) {
        return(diagnosis_clone)
      }
      stop("Projeto inesperado no teste.", call. = FALSE)
    },
    named_project_choices = function(root) stats::setNames(character(), character())
  )
  testthat::local_mocked_bindings(
    showNotification = function(ui, type = "default", duration = NULL, id = NULL, closeButton = TRUE, session = shiny::getDefaultReactiveDomain()) {
      notification <<- list(message = as.character(ui), type = type)
      invisible("notification-id")
    },
    .package = "shiny"
  )

  shiny::testServer(server, {
    session$setInputs(
      project_clone_url = "https://github.com/example/repo.git",
      project_clone_base_dir = project,
      project_clone_dir = "repo-clonado",
      project_clone_open_after = TRUE
    )
    session$setInputs(project_clone = 1)

    expect_equal(as.character(d()$current_path), as.character(normalize_project_path(clone_path)))
    expect_match(notification$message, "mais de um .Rproj", fixed = TRUE)
    expect_equal(notification$type, "warning")
  })
})

test_that("painel sincroniza clone mesmo sem .Rproj unico", {
  skip_if_not_installed("shiny")

  project <- withr::local_tempdir()
  clone_path <- fs::path(project, "repo-clonado")
  fs::dir_create(clone_path)

  diagnosis_original <- structure(list(
    current_path = project,
    repo_path = project,
    is_repo_root = TRUE,
    rproj_path = fs::path(project, "original.Rproj"),
    git_installed = TRUE,
    user_name = "Nome",
    user_email = "email@example.com",
    identity = list(name = "Nome", email = "email@example.com", scope = "global", complete = TRUE),
    is_rstudio_project = TRUE,
    has_repo = TRUE,
    has_commits = TRUE,
    branch = "main",
    has_remote = TRUE,
    remote_name = "origin",
    remote_url = "https://github.com/exemplo/original.git",
    remote_is_github = TRUE,
    sync_status = list(has_upstream = TRUE, upstream_branch = "origin/main", remote_branch_exists = TRUE, ahead = 0L, behind = 0L, can_compare = TRUE),
    status = empty_status_table(),
    status_counts = list(staged = 0L, new = 0L, modified = 0L, deleted = 0L, conflicted = 0L, total = 0L)
  ), class = "trackr_diagnosis")

  diagnosis_clone <- diagnosis_original
  diagnosis_clone$current_path <- clone_path
  diagnosis_clone$repo_path <- clone_path
  diagnosis_clone$is_rstudio_project <- FALSE
  diagnosis_clone$rproj_path <- NULL
  diagnosis_clone$remote_url <- "https://github.com/exemplo/clonado.git"

  server <- statgit_panel_server(project, initial_diagnosis = diagnosis_original)

  testthat::local_mocked_bindings(
    git_clone_repo = function(remote_url, path, directory, open) {
      invisible(list(
        ok = TRUE,
        remote_url = remote_url,
        path = clone_path,
        open = list(ok = FALSE, opened = FALSE, reason = "missing_rproj", path = clone_path)
      ))
    },
    build_git_diagnosis = function(path = ".") {
      normalized <- normalize_project_path(path)
      if (identical(normalized, normalize_project_path(project))) {
        return(diagnosis_original)
      }
      if (identical(normalized, normalize_project_path(clone_path))) {
        return(diagnosis_clone)
      }
      stop("Projeto inesperado no teste.", call. = FALSE)
    },
    named_project_choices = function(root) stats::setNames(character(), character())
  )

  shiny::testServer(server, {
    session$setInputs(
      project_clone_url = "https://github.com/example/repo.git",
      project_clone_base_dir = project,
      project_clone_dir = "repo-clonado",
      project_clone_open_after = TRUE
    )
    session$setInputs(project_clone = 1)

    expect_equal(as.character(d()$current_path), as.character(normalize_project_path(clone_path)))
    expect_false(d()$is_rstudio_project)
  })
})

test_that("painel sincroniza o diagnostico ao abrir outro projeto", {
  skip_if_not_installed("shiny")

  project <- withr::local_tempdir()
  old_project <- fs::path(project, "projeto-antigo")
  new_project <- fs::path(project, "projeto-novo")
  fs::dir_create(old_project)
  fs::dir_create(new_project)
  writeLines("Version: 1.0", fs::path(old_project, "projeto-antigo.Rproj"))
  writeLines("Version: 1.0", fs::path(new_project, "projeto-novo.Rproj"))

  diagnosis_old <- structure(list(
    current_path = old_project,
    repo_path = old_project,
    is_repo_root = TRUE,
    rproj_path = fs::path(old_project, "projeto-antigo.Rproj"),
    git_installed = TRUE,
    user_name = "Nome",
    user_email = "email@example.com",
    identity = list(name = "Nome", email = "email@example.com", scope = "global", complete = TRUE),
    is_rstudio_project = TRUE,
    has_repo = TRUE,
    has_commits = TRUE,
    branch = "main",
    has_remote = TRUE,
    remote_name = "origin",
    remote_url = "https://github.com/exemplo/antigo.git",
    remote_is_github = TRUE,
    sync_status = list(has_upstream = TRUE, upstream_branch = "origin/main", remote_branch_exists = TRUE, ahead = 0L, behind = 0L, can_compare = TRUE),
    status = empty_status_table(),
    status_counts = list(staged = 0L, new = 0L, modified = 0L, deleted = 0L, conflicted = 0L, total = 0L)
  ), class = "trackr_diagnosis")

  diagnosis_new <- diagnosis_old
  diagnosis_new$current_path <- new_project
  diagnosis_new$repo_path <- new_project
  diagnosis_new$rproj_path <- fs::path(new_project, "projeto-novo.Rproj")
  diagnosis_new$has_remote <- FALSE
  diagnosis_new$remote_name <- NULL
  diagnosis_new$remote_url <- NULL
  diagnosis_new$remote_is_github <- FALSE
  diagnosis_new$sync_status <- list(has_upstream = FALSE, upstream_branch = NULL, remote_branch_exists = FALSE, ahead = 0L, behind = 0L, can_compare = FALSE)

  server <- statgit_panel_server(old_project, initial_diagnosis = diagnosis_old)

  testthat::local_mocked_bindings(
    project_open = function(path) invisible(list(ok = TRUE, opened = FALSE, path = path)),
    build_git_diagnosis = function(path = ".") {
      normalized <- normalize_project_path(path)
      if (identical(normalized, normalize_project_path(old_project))) {
        return(diagnosis_old)
      }
      if (identical(normalized, normalize_project_path(new_project))) {
        return(diagnosis_new)
      }
      stop("Projeto inesperado no teste.", call. = FALSE)
    },
    named_project_choices = function(root) stats::setNames(character(), character())
  )

  shiny::testServer(server, {
    session$setInputs(project_to_open = fs::path(new_project, "projeto-novo.Rproj"))
    session$setInputs(project_open = 1)

    expect_equal(as.character(d()$current_path), as.character(normalize_project_path(new_project)))
    expect_false(d()$has_remote)
    expect_match(values$log, "projeto-novo", fixed = TRUE)
  })
})

test_that("acoes git usam o projeto interno atualizado apos troca", {
  skip_if_not_installed("shiny")

  project <- withr::local_tempdir()
  old_project <- fs::path(project, "projeto-antigo")
  new_project <- fs::path(project, "projeto-novo")
  fs::dir_create(old_project)
  fs::dir_create(new_project)
  writeLines("Version: 1.0", fs::path(old_project, "projeto-antigo.Rproj"))
  writeLines("Version: 1.0", fs::path(new_project, "projeto-novo.Rproj"))

  diagnosis_old <- structure(list(
    current_path = old_project,
    repo_path = old_project,
    is_repo_root = TRUE,
    rproj_path = fs::path(old_project, "projeto-antigo.Rproj"),
    git_installed = TRUE,
    user_name = "Nome",
    user_email = "email@example.com",
    identity = list(name = "Nome", email = "email@example.com", scope = "global", complete = TRUE),
    is_rstudio_project = TRUE,
    has_repo = TRUE,
    has_commits = TRUE,
    branch = "main",
    has_remote = TRUE,
    remote_name = "origin",
    remote_url = "https://github.com/exemplo/antigo.git",
    remote_is_github = TRUE,
    sync_status = list(has_upstream = TRUE, upstream_branch = "origin/main", remote_branch_exists = TRUE, ahead = 0L, behind = 0L, can_compare = TRUE),
    status = empty_status_table(),
    status_counts = list(staged = 0L, new = 0L, modified = 0L, deleted = 0L, conflicted = 0L, total = 0L)
  ), class = "trackr_diagnosis")

  diagnosis_new <- diagnosis_old
  diagnosis_new$current_path <- new_project
  diagnosis_new$repo_path <- new_project
  diagnosis_new$rproj_path <- fs::path(new_project, "projeto-novo.Rproj")
  diagnosis_new$remote_url <- "https://github.com/exemplo/novo.git"

  fetched <- NULL
  server <- statgit_panel_server(old_project, initial_diagnosis = diagnosis_old)

  testthat::local_mocked_bindings(
    project_open = function(path) invisible(list(ok = TRUE, opened = FALSE, path = path)),
    build_git_diagnosis = function(path = ".") {
      normalized <- normalize_project_path(path)
      if (identical(normalized, normalize_project_path(old_project))) {
        return(diagnosis_old)
      }
      if (identical(normalized, normalize_project_path(new_project))) {
        return(diagnosis_new)
      }
      stop("Projeto inesperado no teste.", call. = FALSE)
    },
    git_fetch = function(path = ".", remote = "origin") {
      fetched <<- list(path = path, remote = remote)
      invisible(list(ok = TRUE))
    },
    named_project_choices = function(root) stats::setNames(character(), character())
  )

  shiny::testServer(server, {
    session$setInputs(project_to_open = fs::path(new_project, "projeto-novo.Rproj"))
    session$setInputs(project_open = 1)
    session$setInputs(github_remote_name = "origin")
    session$setInputs(github_fetch = 1)

    expect_equal(as.character(normalize_project_path(fetched$path)), as.character(normalize_project_path(new_project)))
    expect_equal(fetched$remote, "origin")
  })
})

test_that("acoes git usam o remote preferido do projeto novo apos troca", {
  skip_if_not_installed("shiny")

  project <- withr::local_tempdir()
  old_project <- fs::path(project, "projeto-antigo")
  new_project <- fs::path(project, "projeto-novo")
  fs::dir_create(old_project)
  fs::dir_create(new_project)
  writeLines("Version: 1.0", fs::path(old_project, "projeto-antigo.Rproj"))
  writeLines("Version: 1.0", fs::path(new_project, "projeto-novo.Rproj"))

  diagnosis_old <- structure(list(
    current_path = old_project,
    repo_path = old_project,
    is_repo_root = TRUE,
    rproj_path = fs::path(old_project, "projeto-antigo.Rproj"),
    git_installed = TRUE,
    user_name = "Nome",
    user_email = "email@example.com",
    identity = list(name = "Nome", email = "email@example.com", scope = "global", complete = TRUE),
    is_rstudio_project = TRUE,
    has_repo = TRUE,
    has_commits = TRUE,
    branch = "main",
    has_remote = TRUE,
    remote_name = "origin",
    remote_url = "https://github.com/exemplo/antigo.git",
    remote_is_github = TRUE,
    sync_status = list(has_upstream = TRUE, upstream_branch = "origin/main", remote_branch_exists = TRUE, ahead = 0L, behind = 0L, can_compare = TRUE),
    status = empty_status_table(),
    status_counts = list(staged = 0L, new = 0L, modified = 0L, deleted = 0L, conflicted = 0L, total = 0L)
  ), class = "trackr_diagnosis")

  diagnosis_new <- diagnosis_old
  diagnosis_new$current_path <- new_project
  diagnosis_new$repo_path <- new_project
  diagnosis_new$rproj_path <- fs::path(new_project, "projeto-novo.Rproj")
  diagnosis_new$remote_name <- "upstream"
  diagnosis_new$remote_url <- "git@github.com:exemplo/novo.git"
  diagnosis_new$sync_status <- list(has_upstream = TRUE, upstream_branch = "upstream/main", remote_branch_exists = TRUE, ahead = 0L, behind = 0L, can_compare = TRUE)

  fetched <- NULL
  server <- statgit_panel_server(old_project, initial_diagnosis = diagnosis_old)

  testthat::local_mocked_bindings(
    project_open = function(path) invisible(list(ok = TRUE, opened = FALSE, path = path)),
    build_git_diagnosis = function(path = ".") {
      normalized <- normalize_project_path(path)
      if (identical(normalized, normalize_project_path(old_project))) {
        return(diagnosis_old)
      }
      if (identical(normalized, normalize_project_path(new_project))) {
        return(diagnosis_new)
      }
      stop("Projeto inesperado no teste.", call. = FALSE)
    },
    git_fetch = function(path = ".", remote = "origin") {
      fetched <<- list(path = path, remote = remote)
      invisible(list(ok = TRUE))
    },
    named_project_choices = function(root) stats::setNames(character(), character())
  )

  shiny::testServer(server, {
    session$setInputs(project_to_open = fs::path(new_project, "projeto-novo.Rproj"))
    session$setInputs(project_open = 1)
    session$setInputs(github_fetch = 1)

    expect_equal(as.character(normalize_project_path(fetched$path)), as.character(normalize_project_path(new_project)))
    expect_equal(fetched$remote, "upstream")
  })
})

test_that("troca de projeto atualiza o remote do painel e evita estado antigo", {
  skip_if_not_installed("shiny")

  project <- withr::local_tempdir()
  old_project <- fs::path(project, "projeto-antigo")
  new_project <- fs::path(project, "projeto-novo")
  fs::dir_create(old_project)
  fs::dir_create(new_project)
  writeLines("Version: 1.0", fs::path(old_project, "projeto-antigo.Rproj"))
  writeLines("Version: 1.0", fs::path(new_project, "projeto-novo.Rproj"))

  diagnosis_old <- structure(list(
    current_path = old_project,
    repo_path = old_project,
    is_repo_root = TRUE,
    rproj_path = fs::path(old_project, "projeto-antigo.Rproj"),
    git_installed = TRUE,
    user_name = "Nome",
    user_email = "email@example.com",
    identity = list(name = "Nome", email = "email@example.com", scope = "global", complete = TRUE),
    is_rstudio_project = TRUE,
    has_repo = TRUE,
    has_commits = TRUE,
    branch = "main",
    has_remote = TRUE,
    remote_name = "origin",
    remote_url = "https://github.com/exemplo/antigo.git",
    remote_is_github = TRUE,
    sync_status = list(has_upstream = TRUE, upstream_branch = "origin/main", remote_branch_exists = TRUE, ahead = 0L, behind = 0L, can_compare = TRUE),
    status = empty_status_table(),
    status_counts = list(staged = 0L, new = 0L, modified = 0L, deleted = 0L, conflicted = 0L, total = 0L)
  ), class = "trackr_diagnosis")

  diagnosis_new <- diagnosis_old
  diagnosis_new$current_path <- new_project
  diagnosis_new$repo_path <- new_project
  diagnosis_new$rproj_path <- fs::path(new_project, "projeto-novo.Rproj")
  diagnosis_new$remote_name <- "upstream"
  diagnosis_new$remote_url <- "git@github.com:exemplo/novo.git"
  diagnosis_new$sync_status <- list(has_upstream = TRUE, upstream_branch = "upstream/main", remote_branch_exists = TRUE, ahead = 0L, behind = 0L, can_compare = TRUE)

  fetched <- NULL
  server <- statgit_panel_server(old_project, initial_diagnosis = diagnosis_old)

  testthat::local_mocked_bindings(
    project_open = function(path) invisible(list(ok = TRUE, opened = FALSE, path = path)),
    build_git_diagnosis = function(path = ".") {
      normalized <- normalize_project_path(path)
      if (identical(normalized, normalize_project_path(old_project))) {
        return(diagnosis_old)
      }
      if (identical(normalized, normalize_project_path(new_project))) {
        return(diagnosis_new)
      }
      stop("Projeto inesperado no teste.", call. = FALSE)
    },
    git_fetch = function(path = ".", remote = "origin") {
      fetched <<- list(path = path, remote = remote)
      invisible(list(ok = TRUE))
    },
    named_project_choices = function(root) stats::setNames(character(), character())
  )

  shiny::testServer(server, {
    session$setInputs(github_remote_name = "origin")
    session$setInputs(project_to_open = fs::path(new_project, "projeto-novo.Rproj"))
    session$setInputs(project_open = 1)
    session$setInputs(github_fetch = 1)

    expect_equal(as.character(normalize_project_path(fetched$path)), as.character(normalize_project_path(new_project)))
    expect_equal(fetched$remote, "upstream")
  })
})

test_that("painel so sincroniza com projeto ativo do RStudio quando solicitado", {
  skip_if_not_installed("shiny")

  project <- withr::local_tempdir()
  panel_project <- fs::path(project, "painel")
  active_project <- fs::path(project, "ativo")
  fs::dir_create(panel_project)
  fs::dir_create(active_project)
  writeLines("Version: 1.0", fs::path(panel_project, "painel.Rproj"))
  writeLines("Version: 1.0", fs::path(active_project, "ativo.Rproj"))

  diagnosis_panel <- structure(list(
    current_path = panel_project,
    repo_path = panel_project,
    is_repo_root = TRUE,
    rproj_path = fs::path(panel_project, "painel.Rproj"),
    git_installed = TRUE,
    user_name = "Nome",
    user_email = "email@example.com",
    identity = list(name = "Nome", email = "email@example.com", scope = "global", complete = TRUE),
    is_rstudio_project = TRUE,
    has_repo = TRUE,
    has_commits = TRUE,
    branch = "main",
    has_remote = TRUE,
    remote_name = "origin",
    remote_url = "https://github.com/exemplo/painel.git",
    remote_is_github = TRUE,
    sync_status = list(has_upstream = TRUE, upstream_branch = "origin/main", remote_branch_exists = TRUE, ahead = 0L, behind = 0L, can_compare = TRUE),
    status = empty_status_table(),
    status_counts = list(staged = 0L, new = 0L, modified = 0L, deleted = 0L, conflicted = 0L, total = 0L)
  ), class = "trackr_diagnosis")

  diagnosis_active <- diagnosis_panel
  diagnosis_active$current_path <- active_project
  diagnosis_active$repo_path <- active_project
  diagnosis_active$rproj_path <- fs::path(active_project, "ativo.Rproj")
  diagnosis_active$remote_url <- "https://github.com/exemplo/ativo.git"

  server <- statgit_panel_server(panel_project, initial_diagnosis = diagnosis_panel)

  testthat::local_mocked_bindings(
    active_project_path = function(default = ".") active_project,
    build_git_diagnosis = function(path = ".") {
      normalized <- normalize_project_path(path)
      if (identical(normalized, normalize_project_path(panel_project))) {
        return(diagnosis_panel)
      }
      if (identical(normalized, normalize_project_path(active_project))) {
        return(diagnosis_active)
      }
      stop("Projeto inesperado no teste.", call. = FALSE)
    },
    named_project_choices = function(root) stats::setNames(character(), character())
  )

  shiny::testServer(server, {
    expect_equal(as.character(d()$current_path), as.character(normalize_project_path(panel_project)))

    session$setInputs(refresh_all = 1)
    expect_equal(as.character(d()$current_path), as.character(normalize_project_path(panel_project)))

    session$setInputs(refresh_project_path = 1)
    expect_equal(as.character(d()$current_path), as.character(normalize_project_path(active_project)))
    expect_match(values$log, "Painel sincronizado com a pasta atual", fixed = TRUE)
  })
})

test_that("painel sincroniza para pasta sem Git e bloqueia acoes dependentes de repo", {
  skip_if_not_installed("shiny")

  project <- withr::local_tempdir()
  panel_project <- fs::path(project, "painel")
  plain_folder <- fs::path(project, "pasta-solta")
  fs::dir_create(panel_project)
  fs::dir_create(plain_folder)
  writeLines("Version: 1.0", fs::path(panel_project, "painel.Rproj"))

  diagnosis_panel <- structure(list(
    current_path = panel_project,
    repo_path = panel_project,
    is_repo_root = TRUE,
    rproj_path = fs::path(panel_project, "painel.Rproj"),
    git_installed = TRUE,
    user_name = "Nome",
    user_email = "email@example.com",
    identity = list(name = "Nome", email = "email@example.com", scope = "global", complete = TRUE),
    is_rstudio_project = TRUE,
    has_repo = TRUE,
    has_commits = TRUE,
    branch = "main",
    has_remote = TRUE,
    remote_name = "origin",
    remote_url = "https://github.com/exemplo/painel.git",
    remote_is_github = TRUE,
    sync_status = list(has_upstream = TRUE, upstream_branch = "origin/main", remote_branch_exists = TRUE, ahead = 0L, behind = 0L, can_compare = TRUE),
    status = empty_status_table(),
    status_counts = list(staged = 0L, new = 0L, modified = 0L, deleted = 0L, conflicted = 0L, total = 0L)
  ), class = "trackr_diagnosis")

  diagnosis_plain <- diagnosis_panel
  diagnosis_plain$current_path <- plain_folder
  diagnosis_plain$repo_path <- NULL
  diagnosis_plain$is_repo_root <- FALSE
  diagnosis_plain$rproj_path <- NULL
  diagnosis_plain$is_rstudio_project <- FALSE
  diagnosis_plain$has_repo <- FALSE
  diagnosis_plain$has_commits <- FALSE
  diagnosis_plain$branch <- NULL
  diagnosis_plain$has_remote <- FALSE
  diagnosis_plain$remote_name <- NULL
  diagnosis_plain$remote_url <- NULL
  diagnosis_plain$remote_is_github <- FALSE
  diagnosis_plain$sync_status <- list(has_upstream = FALSE, upstream_branch = NULL, remote_branch_exists = FALSE, ahead = 0L, behind = 0L, can_compare = FALSE)

  server <- statgit_panel_server(panel_project, initial_diagnosis = diagnosis_panel)

  testthat::local_mocked_bindings(
    project_open = function(path) invisible(list(ok = TRUE, opened = FALSE, path = path)),
    build_git_diagnosis = function(path = ".") {
      normalized <- normalize_project_path(path)
      if (identical(normalized, normalize_project_path(panel_project))) {
        return(diagnosis_panel)
      }
      if (identical(normalized, normalize_project_path(plain_folder))) {
        return(diagnosis_plain)
      }
      stop("Projeto inesperado no teste.", call. = FALSE)
    },
    named_project_choices = function(root) stats::setNames(character(), character())
  )

  shiny::testServer(server, {
    session$setInputs(project_to_open = plain_folder)
    session$setInputs(project_open = 1)

    expect_equal(as.character(d()$current_path), as.character(normalize_project_path(plain_folder)))
    expect_false(d()$has_repo)
    expect_match(values$log, "pasta-solta", fixed = TRUE)
  })
})

test_that("painel chama formatacao pelos motores existentes", {
  skip_if_not_installed("shiny")

  project <- withr::local_tempdir()
  calls <- list()
  server <- statgit_panel_server(project)

  testthat::local_mocked_bindings(
    code_format = function(path = NULL) {
      calls$format_active <<- list(path = path)
      invisible(list(ok = TRUE))
    },
    code_format_all = function(path = ".") {
      calls$format_project <<- list(path = path)
      invisible(list(ok = TRUE))
    }
  )

  shiny::testServer(server, {
    session$setInputs(format_path = "scripts/a.R")
    session$setInputs(format_active = 1)
    session$setInputs(format_project = 1)

    expect_equal(calls$format_active$path, "scripts/a.R")
    expect_equal(
      as.character(normalize_project_path(calls$format_project$path)),
      as.character(normalize_project_path(project))
    )
  })
})

test_that("painel importa arquivo e mostra diff", {
  skip_if_not_installed("shiny")

  project <- withr::local_tempdir()
  calls <- list()
  server <- statgit_panel_server(project)

  testthat::local_mocked_bindings(
    file_import = function(source, destination, path, move, add_to_git, overwrite) {
      calls$import <<- list(
        source = source,
        destination = destination,
        path = path,
        move = move,
        add_to_git = add_to_git,
        overwrite = overwrite
      )
      invisible(list(ok = TRUE))
    },
    git_diff = function(file, path = ".", staged = FALSE, context = "changes") {
      calls$diff <<- list(file = file, path = path, staged = staged, context = context)
      invisible(list(ok = TRUE, diff = c("- antigo", "+ novo")))
    }
  )

  shiny::testServer(server, {
    session$setInputs(
      file_source = "/tmp/dados.csv",
      file_destination = "data/raw",
      file_move = FALSE,
      file_add_to_git = TRUE,
      file_overwrite = FALSE
    )
    session$setInputs(file_import = 1)
    session$setInputs(changes_file = "analise.R", changes_staged = FALSE, changes_diff_context = "full")
    session$setInputs(changes_diff = 1)

    expect_equal(calls$import$source, "/tmp/dados.csv")
    expect_equal(calls$import$destination, "data/raw")
    expect_true(calls$import$add_to_git)
    expect_equal(calls$diff$file, "analise.R")
    expect_equal(calls$diff$context, "full")
    expect_match(values$log, "\\+ novo")
    expect_match(values$diff_html, "tr-diff-remove", fixed = TRUE)
    expect_match(values$diff_html, "tr-diff-add", fixed = TRUE)
  })
})

test_that("painel prepara, desprepara, descarta e commita selecionados", {
  skip_if_not_installed("shiny")

  project <- withr::local_tempdir()
  calls <- list()
  server <- statgit_panel_server(project)

  testthat::local_mocked_bindings(
    git_stage = function(files, path = ".") {
      calls$stage <<- list(files = files, path = path)
      invisible(list(ok = TRUE))
    },
    git_unstage = function(files, path = ".") {
      calls$unstage <<- list(files = files, path = path)
      invisible(list(ok = TRUE))
    },
    git_discard = function(files, path = ".") {
      calls$discard <<- list(files = files, path = path)
      invisible(list(ok = TRUE))
    },
    git_commit = function(message = "Atualiza projeto", path = ".") {
      calls$commit <<- list(message = message, path = path)
      invisible(list(ok = TRUE, committed = TRUE))
    }
  )

  shiny::testServer(server, {
    session$setInputs(changes_files = c("analise.R", "dados.csv"))
    session$setInputs(changes_stage = 1)
    session$setInputs(changes_unstage = 1)
    session$setInputs(changes_discard = 1)

    expect_equal(calls$stage$files, c("analise.R", "dados.csv"))
    expect_equal(calls$unstage$files, c("analise.R", "dados.csv"))
    expect_null(calls$discard)

    session$setInputs(confirm_discard_action = 1)
    session$setInputs(changes_commit_message = "Salva analise")
    session$setInputs(changes_commit_selected = 1)

    expect_equal(calls$discard$files, c("analise.R", "dados.csv"))
    expect_equal(calls$commit$message, "Salva analise")
  })
})
