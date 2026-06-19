test_that("git_wizard_server inicializa sem contexto reativo ativo", {
  skip_if_not_installed("shiny")

  project <- withr::local_tempdir()
  server <- git_wizard_server(project)

  expect_no_error(
    shiny::testServer(server, {
      expect_true(is.character(values$diagnosis))
      expect_true(length(values$diagnosis) == 1)
      expect_match(values$diagnosis, "Diagnóstico Git do projeto")
    })
  )
})
