test_that("painel aciona criacao de arquivo com os dados da UI", {
  skip_if_not_installed("shiny")

  project <- withr::local_tempdir()
  calls <- list()
  server <- statgit_panel_server(project)

  testthat::local_mocked_bindings(
    file_create = function(filename, type = "R", destination = ".", path = ".", content = NULL, open_in_rstudio = TRUE) {
      calls$create <<- list(
        filename = filename,
        type = type,
        destination = destination,
        path = path,
        content = content,
        open_in_rstudio = open_in_rstudio
      )
      invisible(list(ok = TRUE))
    }
  )

  shiny::testServer(server, {
    session$setInputs(
      file_create_name = "analise.qmd",
      file_create_type = "qmd",
      file_create_destination = "reports",
      file_create_content = "---\ntitle: \"Teste\"\n---\n",
      file_create_open = FALSE
    )
    session$setInputs(file_create = 1)

    expect_equal(calls$create$filename, "analise.qmd")
    expect_equal(calls$create$type, "qmd")
    expect_equal(calls$create$destination, "reports")
    expect_equal(calls$create$content, "---\ntitle: \"Teste\"\n---\n")
    expect_true(calls$create$open_in_rstudio)
    expect_equal(
      as.character(normalize_project_path(calls$create$path)),
      as.character(normalize_project_path(project))
    )
  })
})

test_that("painel principal inclui estrutura de navegacao", {
  skip_if_not_installed("shiny")

  server <- statgit_panel_server(withr::local_tempdir())

  shiny::testServer(server, {
    html <- htmltools::renderTags(output$module_nav)$html

    expect_match(html, "tr-nav-item", fixed = TRUE)
    expect_match(html, "Visão Geral|Overview")
  })
})

test_that("painel confirma exclusao antes de deletar", {
  skip_if_not_installed("shiny")

  project <- withr::local_tempdir()
  calls <- list()
  server <- statgit_panel_server(project)

  testthat::local_mocked_bindings(
    file_delete_info = function(path_to_delete, path = ".") {
      list(
        ok = TRUE,
        relative_path = path_to_delete,
        item_kind = "file",
        item_type_label = "Arquivo",
        label = sprintf("Arquivo '%s'", path_to_delete),
        was_tracked = TRUE
      )
    },
    file_delete = function(path_to_delete, path = ".", remove_from_git = FALSE) {
      calls$delete <<- list(
        path_to_delete = path_to_delete,
        path = path,
        remove_from_git = remove_from_git
      )
      invisible(list(ok = TRUE))
    }
  )

  shiny::testServer(server, {
    session$setInputs(file_delete_path = "scripts/velho.R")
    session$setInputs(file_delete = 1)

    expect_equal(values$pending_delete, "scripts/velho.R")
    expect_null(calls$delete)

    session$setInputs(file_delete_remove_from_git = TRUE)
    session$setInputs(confirm_file_delete = 1)

    expect_equal(calls$delete$path_to_delete, "scripts/velho.R")
    expect_true(calls$delete$remove_from_git)
    expect_equal(
      as.character(normalize_project_path(calls$delete$path)),
      as.character(normalize_project_path(project))
    )
    expect_null(values$pending_delete)
  })
})

test_that("abas criar e deletar exibem a estrutura atual do projeto", {
  skip_if_not_installed("shiny")

  project <- withr::local_tempdir()
  writeLines("x <- 1", file.path(project, "scripts.R"))
  diagnosis <- build_git_diagnosis(project)

  criar_html <- htmltools::renderTags(criar_module_ui(diagnosis))$html
  excluir_html <- htmltools::renderTags(excluir_module_ui(diagnosis))$html

  expect_match(criar_html, "Arquivos Atuais do Projeto", fixed = TRUE)
  expect_match(excluir_html, "Estrutura atual do projeto", fixed = TRUE)
  expect_match(criar_html, "recent_files_explorer", fixed = TRUE)
  expect_match(excluir_html, "recent_files_explorer", fixed = TRUE)
})

test_that("arvore de arquivos renderiza itens clicaveis", {
  skip_if_not_installed("shiny")

  project <- withr::local_tempdir()
  dir.create(file.path(project, "scripts"), recursive = TRUE)
  writeLines("x <- 1", file.path(project, "scripts", "analise.R"))
  diagnosis <- build_git_diagnosis(project)
  diagnosis$current_path <- normalize_project_path(project)

  html <- render_project_files_explorer_html(project, diagnosis)

  expect_match(html, "tr-tree-item", fixed = TRUE)
  expect_match(html, "Shiny.setInputValue('selected_project_item'", fixed = TRUE)
  expect_match(html, "scripts/analise.R", fixed = TRUE)
})

test_that("rename_module_ui preenche novo nome com origem inicial", {
  skip_if_not_installed("shiny")

  project <- withr::local_tempdir()
  dir.create(file.path(project, "scripts"), recursive = TRUE)
  writeLines("x <- 1", file.path(project, "scripts", "analise.R"))
  diagnosis <- build_git_diagnosis(project)
  diagnosis$current_path <- normalize_project_path(project)

  html <- htmltools::renderTags(rename_module_ui(diagnosis))$html

  expect_match(html, "Origem", fixed = TRUE)
  expect_match(html, "scripts/analise.R", fixed = TRUE)
})

test_that("modulos exibem os rótulos em português", {
  skip_if_not_installed("shiny")

  project <- withr::local_tempdir()
  diagnosis <- build_git_diagnosis(project)

  criar_html <- htmltools::renderTags(criar_module_ui(diagnosis))$html
  rename_html <- htmltools::renderTags(rename_module_ui(diagnosis))$html

  expect_match(criar_html, "Nome do arquivo", fixed = TRUE)
  expect_match(rename_html, "Origem", fixed = TRUE)
})

test_that("git_module_ui mostra estado atual e motivos de bloqueio", {
  skip_if_not_installed("shiny")

  diagnosis <- list(
    current_path = normalize_project_path(withr::local_tempdir()),
    has_repo = TRUE,
    git_installed = TRUE,
    identity = list(name = "Ada", email = "ada@example.com", complete = TRUE),
    has_commits = FALSE,
    has_remote = FALSE,
    remote_name = NULL,
    remote_url = NULL,
    branch = "main",
    sync_status = list(has_upstream = FALSE, upstream_branch = NULL, remote_branch_exists = FALSE, ahead = 0L, behind = 0L, can_compare = FALSE),
    status_counts = list(total = 0L)
  )

  html <- htmltools::renderTags(git_module_ui(diagnosis))$html

  expect_match(html, "Git e GitHub", fixed = TRUE)
  expect_false(grepl("Diagnóstico", html, fixed = TRUE))
  expect_match(html, "Próxima ação", fixed = TRUE)
  expect_match(html, "Conectar GitHub", fixed = TRUE)
  expect_match(html, "Configuração", fixed = TRUE)
  expect_match(html, "Identidade", fixed = TRUE)
  expect_match(html, "Ada &lt;ada@example.com&gt;", fixed = TRUE)
  expect_match(html, "Git local", fixed = TRUE)
  expect_match(html, "Branch main", fixed = TRUE)
  expect_match(html, "main", fixed = TRUE)
  expect_false(grepl("Inicializar Git", html, fixed = TRUE))
  expect_false(grepl("Criar ou atualizar .gitignore", html, fixed = TRUE))
  expect_match(html, "tr-git-config-row-head complete", fixed = TRUE)
  expect_match(html, "Remote não configurado", fixed = TRUE)
  expect_match(html, "Nome do remote", fixed = TRUE)
  expect_match(html, "Conectar remote", fixed = TRUE)
  expect_false(grepl("Trocar URL conectada", html, fixed = TRUE))
  expect_false(grepl("Testar acesso ao GitHub", html, fixed = TRUE))
})

test_that("badges superiores da aba git nao incluem identidade", {
  diagnosis <- list(
    current_path = normalize_project_path(withr::local_tempdir()),
    has_repo = TRUE,
    git_installed = TRUE,
    identity = list(name = "Ada", email = "ada@example.com", complete = TRUE),
    has_commits = TRUE,
    has_remote = TRUE,
    remote_name = "origin",
    remote_url = "https://github.com/user/repo.git",
    branch = "main",
    sync_status = list(has_upstream = TRUE, upstream_branch = "origin/main", remote_branch_exists = TRUE, ahead = 0L, behind = 0L, can_compare = TRUE),
    status_counts = list(total = 2L)
  )

  labels <- vapply(git_status_badge_items(diagnosis), `[[`, character(1), "label")

  expect_equal(labels, c("Git", "Commits", "Remote", "Branch", "Pendências"))
})

test_that("git_module_ui preenche remote atual quando existir", {
  skip_if_not_installed("shiny")

  diagnosis <- list(
    current_path = normalize_project_path(withr::local_tempdir()),
    has_repo = TRUE,
    git_installed = TRUE,
    identity = list(name = "Ada", email = "ada@example.com", complete = TRUE),
    has_commits = TRUE,
    has_remote = TRUE,
    remote_name = "origin",
    remote_url = "https://github.com/user/repo.git",
    branch = "main",
    sync_status = list(has_upstream = TRUE, upstream_branch = "origin/main", remote_branch_exists = TRUE, ahead = 2L, behind = 0L, can_compare = TRUE),
    status_counts = list(total = 0L)
  )

  html <- htmltools::renderTags(git_module_ui(diagnosis))$html

  expect_match(html, "Próxima ação", fixed = TRUE)
  expect_match(html, "Sincronizar com GitHub", fixed = TRUE)
  expect_match(html, "2 commit(s) pendente(s) para enviar", fixed = TRUE)
  expect_match(html, "Remote atual: origin", fixed = TRUE)
  expect_match(html, "HTTPS", fixed = TRUE)
  expect_match(html, "https://github.com/user/repo.git", fixed = TRUE)
  expect_match(html, "Abrir repositório", fixed = TRUE)
  expect_match(html, "Fetch", fixed = TRUE)
  expect_match(html, "Nome do remote", fixed = TRUE)
  expect_match(html, "Reconectar / atualizar remote", fixed = TRUE)
  expect_match(html, "Desconectar remote", fixed = TRUE)
  expect_match(html, "Trocar URL conectada", fixed = TRUE)
  expect_match(html, "Pull + Push", fixed = TRUE)
})

test_that("git_module_ui mostra bloqueios com explicacao", {
  skip_if_not_installed("shiny")

  diagnosis <- list(
    current_path = normalize_project_path(withr::local_tempdir()),
    has_repo = FALSE,
    git_installed = TRUE,
    identity = list(name = "", email = "", complete = FALSE),
    has_commits = FALSE,
    has_remote = FALSE,
    remote_name = NULL,
    remote_url = NULL,
    branch = NULL,
    sync_status = list(has_upstream = FALSE, upstream_branch = NULL, remote_branch_exists = FALSE, ahead = 0L, behind = 0L, can_compare = FALSE),
    status_counts = list(total = 0L)
  )

  html <- htmltools::renderTags(git_module_ui(diagnosis))$html

  expect_match(html, "Configurar identidade", fixed = TRUE)
  expect_match(html, "tr-git-config-row-head blocked", fixed = TRUE)
  expect_match(html, "Remote não configurado", fixed = TRUE)
  expect_match(html, "pending", fixed = TRUE)
})

test_that("git_module_ui prioriza pull quando branch local esta atras", {
  skip_if_not_installed("shiny")

  diagnosis <- list(
    current_path = normalize_project_path(withr::local_tempdir()),
    has_repo = TRUE,
    git_installed = TRUE,
    identity = list(name = "Ada", email = "ada@example.com", complete = TRUE),
    has_commits = TRUE,
    has_remote = TRUE,
    remote_name = "origin",
    remote_url = "https://github.com/user/repo.git",
    branch = "main",
    sync_status = list(has_upstream = TRUE, upstream_branch = "origin/main", remote_branch_exists = TRUE, ahead = 0L, behind = 3L, can_compare = TRUE),
    status_counts = list(total = 0L)
  )

  html <- htmltools::renderTags(git_module_ui(diagnosis))$html

  expect_match(html, "Atualizar branch local", fixed = TRUE)
  expect_match(html, "3 commit(s) atrás do remote", fixed = TRUE)
  expect_match(html, "Pull", fixed = TRUE)
})

test_that("git_module_ui usa o remote do diagnostico mesmo fora de origin", {
  skip_if_not_installed("shiny")

  diagnosis <- list(
    current_path = normalize_project_path(withr::local_tempdir()),
    has_repo = TRUE,
    git_installed = TRUE,
    identity = list(name = "Ada", email = "ada@example.com", complete = TRUE),
    has_commits = TRUE,
    has_remote = TRUE,
    remote_name = "upstream",
    remote_url = "git@github.com:user/repo.git",
    branch = "main",
    sync_status = list(has_upstream = TRUE, upstream_branch = "upstream/main", remote_branch_exists = TRUE, ahead = 1L, behind = 0L, can_compare = TRUE),
    status_counts = list(total = 0L)
  )

  html <- htmltools::renderTags(git_module_ui(diagnosis))$html

  expect_match(html, "Remote atual: upstream", fixed = TRUE)
  expect_match(html, "git@github.com:user/repo.git", fixed = TRUE)
  expect_match(html, "SSH", fixed = TRUE)
  expect_match(html, "upstream (SSH)", fixed = TRUE)
})

test_that("aviso de contexto aparece quando painel e RStudio ativo divergem", {
  warning_ui <- project_context_warning_ui(
    panel_path = "/tmp/projeto-painel",
    active_rstudio_path = "/tmp/projeto-rstudio"
  )

  html <- htmltools::renderTags(warning_ui)$html

  expect_match(html, "Projeto do painel diferente do RStudio ativo.", fixed = TRUE)
  expect_match(html, "projeto-painel", fixed = TRUE)
  expect_match(html, "projeto-rstudio", fixed = TRUE)
  expect_match(html, "Usar projeto ativo no RStudio", fixed = TRUE)
})

test_that("aviso de contexto nao aparece quando os projetos coincidem", {
  expect_null(project_context_warning_ui(
    panel_path = "/tmp/projeto",
    active_rstudio_path = "/tmp/projeto"
  ))
  expect_null(project_context_warning_ui(
    panel_path = "/tmp/projeto",
    active_rstudio_path = NULL
  ))
})
