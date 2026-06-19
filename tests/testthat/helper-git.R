skip_if_not(nzchar(Sys.which("git")))

with_isolated_git_identity <- function(code) {
  home <- withr::local_tempdir()
  config_home <- file.path(home, ".config")
  dir.create(config_home, recursive = TRUE, showWarnings = FALSE)

  withr::local_envvar(c(
    HOME = home,
    USERPROFILE = home,
    XDG_CONFIG_HOME = config_home
  ))

  system2("git", c("config", "--global", "user.name", "Teste statgit"))
  system2("git", c("config", "--global", "user.email", "teste@example.com"))

  force(code)
}

with_isolated_git_home <- function(code) {
  home <- withr::local_tempdir()
  config_home <- file.path(home, ".config")
  dir.create(config_home, recursive = TRUE, showWarnings = FALSE)

  withr::local_envvar(c(
    HOME = home,
    USERPROFILE = home,
    XDG_CONFIG_HOME = config_home
  ))

  force(code)
}

set_bare_repo_head <- function(path, branch = "main") {
  status <- system2(
    "git",
    c("--git-dir", path, "symbolic-ref", "HEAD", sprintf("refs/heads/%s", branch))
  )

  testthat::expect_equal(status, 0L)
}
