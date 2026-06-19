git_installed <- function() {
  nzchar(Sys.which("git"))
}

run_git <- function(args, path = NULL) {
  git_bin <- Sys.which("git")

  if (!nzchar(git_bin)) {
    return(list(status = 127L, output = "Git n\u00e3o encontrado no computador."))
  }

  full_args <- args
  if (!is.null(path)) {
    full_args <- c("-C", normalize_project_path(path), full_args)
  }

  output <- tryCatch(
    suppressWarnings(system2(git_bin, shQuote(full_args), stdout = TRUE, stderr = TRUE)),
    error = function(e) structure(conditionMessage(e), status = 1L)
  )

  list(
    status = as.integer(attr(output, "status") %||% 0L),
    output = unname(output)
  )
}

git_global_config <- function(key) {
  result <- run_git(c("config", "--global", "--get", key))

  if (result$status != 0L || length(result$output) == 0) {
    return(NULL)
  }

  value <- trimws(result$output[[1]])
  if (!nzchar(value)) {
    return(NULL)
  }

  value
}

git_local_config <- function(key, path = ".") {
  repo <- git_repo_root(path)
  if (is.null(repo)) {
    return(NULL)
  }

  result <- run_git(c("config", "--local", "--get", key), path = repo)

  if (result$status != 0L || length(result$output) == 0) {
    return(NULL)
  }

  value <- trimws(result$output[[1]])
  if (!nzchar(value)) {
    return(NULL)
  }

  value
}

git_identity <- function(path = ".") {
  local_name <- git_local_config("user.name", path)
  local_email <- git_local_config("user.email", path)
  global_name <- git_global_config("user.name")
  global_email <- git_global_config("user.email")

  if (!is.null(local_name) && !is.null(local_email)) {
    return(list(
      name = local_name,
      email = local_email,
      scope = "local",
      complete = TRUE
    ))
  }

  if (!is.null(global_name) && !is.null(global_email)) {
    return(list(
      name = global_name,
      email = global_email,
      scope = "global",
      complete = TRUE
    ))
  }

  list(
    name = local_name %||% global_name,
    email = local_email %||% global_email,
    scope = if (!is.null(local_name) || !is.null(local_email)) "local" else "global",
    complete = FALSE
  )
}

git_repo_root <- function(path = ".") {
  project_path <- normalize_project_path(path)

  tryCatch(
    normalize_project_path(gert::git_find(project_path)),
    error = function(e) NULL
  )
}

is_git_repo <- function(path = ".") {
  !is.null(git_repo_root(path))
}

repo_has_commits <- function(path = ".") {
  repo <- git_repo_root(path)
  if (is.null(repo)) {
    return(FALSE)
  }

  result <- run_git(c("rev-parse", "--verify", "HEAD"), path = repo)
  result$status == 0L
}

repo_current_branch <- function(path = ".") {
  repo <- git_repo_root(path) %||% normalize_project_path(path)
  result <- run_git(c("branch", "--show-current"), path = repo)

  if (result$status != 0L || length(result$output) == 0) {
    return(NULL)
  }

  branch <- trimws(result$output[[1]])
  if (!nzchar(branch)) {
    return(NULL)
  }

  branch
}

repo_upstream_branch <- function(path = ".") {
  repo <- git_repo_root(path)
  if (is.null(repo) || !repo_has_commits(repo)) {
    return(NULL)
  }

  result <- run_git(c("rev-parse", "--abbrev-ref", "--symbolic-full-name", "@{upstream}"), path = repo)
  if (result$status != 0L || length(result$output) == 0) {
    return(NULL)
  }

  branch <- trimws(result$output[[1]])
  if (!nzchar(branch)) {
    return(NULL)
  }

  branch
}

repo_has_remote <- function(path = ".") {
  repo <- git_repo_root(path)
  if (is.null(repo)) {
    return(FALSE)
  }

  tryCatch(nrow(gert::git_remote_list(repo = repo)) > 0, error = function(e) FALSE)
}

repo_remote_info <- function(path = ".") {
  repo <- git_repo_root(path)
  if (is.null(repo)) {
    return(data.frame(
      name = character(),
      url = character(),
      is_github = logical(),
      stringsAsFactors = FALSE
    ))
  }

  remotes <- tryCatch(
    as.data.frame(gert::git_remote_list(repo = repo)),
    error = function(e) data.frame(name = character(), url = character(), stringsAsFactors = FALSE)
  )

  if (nrow(remotes) == 0) {
    return(data.frame(
      name = character(),
      url = character(),
      is_github = logical(),
      stringsAsFactors = FALSE
    ))
  }

  remotes$is_github <- vapply(remotes$url, looks_like_github_url, logical(1))
  remotes
}

preferred_remote_name <- function(path = ".", remotes = NULL) {
  remotes <- remotes %||% repo_remote_info(path)

  if (nrow(remotes) == 0) {
    return(NULL)
  }

  upstream_branch <- repo_upstream_branch(path)
  if (!is.null(upstream_branch) && nzchar(upstream_branch)) {
    remote_candidates <- remotes$name[startsWith(upstream_branch, paste0(remotes$name, "/"))]
    if (length(remote_candidates) > 0) {
      return(remote_candidates[[which.max(nchar(remote_candidates))]])
    }
  }

  origin_idx <- which(remotes$name == "origin")[1]
  if (!is.na(origin_idx)) {
    return(remotes$name[[origin_idx]])
  }

  remotes$name[[1]]
}

remote_by_name <- function(path = ".", remote = "origin") {
  remotes <- repo_remote_info(path)
  if (nrow(remotes) == 0) {
    return(NULL)
  }

  idx <- which(remotes$name == remote)[1]
  if (is.na(idx)) {
    return(NULL)
  }

  as.list(remotes[idx, , drop = FALSE])
}

remote_branch_exists <- function(path = ".", remote = "origin", branch = NULL) {
  repo <- git_repo_root(path)
  branch <- trimws(branch %||% "")

  if (is.null(repo) || !nzchar(branch)) {
    return(FALSE)
  }

  result <- run_git(c("ls-remote", "--exit-code", "--heads", remote, branch), path = repo)
  result$status == 0L
}

repo_sync_status <- function(path = ".", remote = NULL, branch = NULL) {
  repo <- git_repo_root(path)
  if (is.null(repo) || !repo_has_commits(repo)) {
    return(list(
      has_upstream = FALSE,
      upstream_branch = NULL,
      remote_branch_exists = FALSE,
      ahead = 0L,
      behind = 0L,
      can_compare = FALSE
    ))
  }

  current_branch <- branch %||% repo_current_branch(repo)
  upstream_branch <- repo_upstream_branch(repo)
  remote_name <- normalize_remote_name(remote)
  remote_exists <- !is.null(remote_by_name(repo, remote = remote_name))
  branch_exists <- if (remote_exists) remote_branch_exists(repo, remote = remote_name, branch = current_branch) else FALSE

  if (is.null(upstream_branch)) {
    return(list(
      has_upstream = FALSE,
      upstream_branch = NULL,
      remote_branch_exists = branch_exists,
      ahead = 0L,
      behind = 0L,
      can_compare = FALSE
    ))
  }

  result <- run_git(c("rev-list", "--left-right", "--count", paste0(upstream_branch, "...HEAD")), path = repo)
  if (result$status != 0L || length(result$output) == 0) {
    return(list(
      has_upstream = TRUE,
      upstream_branch = upstream_branch,
      remote_branch_exists = branch_exists,
      ahead = 0L,
      behind = 0L,
      can_compare = FALSE
    ))
  }

  counts <- strsplit(trimws(result$output[[1]]), "[[:space:]]+")[[1]]
  behind <- suppressWarnings(as.integer(counts[[1]] %||% "0"))
  ahead <- suppressWarnings(as.integer(counts[[2]] %||% "0"))

  list(
    has_upstream = TRUE,
    upstream_branch = upstream_branch,
    remote_branch_exists = branch_exists,
    ahead = ahead %||% 0L,
    behind = behind %||% 0L,
    can_compare = TRUE
  )
}

looks_like_github_url <- function(url) {
  if (is.null(url) || !nzchar(url)) {
    return(FALSE)
  }

  grepl("github\\.com[:/]", url, ignore.case = TRUE)
}

normalize_remote_name <- function(remote, default = "origin") {
  value <- trimws(remote %||% "")
  if (!nzchar(value)) {
    return(default)
  }

  value
}

remote_protocol_label <- function(url) {
  if (is.null(url) || !nzchar(url)) {
    return("desconhecido")
  }

  if (grepl("^git@", url, ignore.case = TRUE) || grepl("^ssh://", url, ignore.case = TRUE)) {
    return("SSH")
  }

  if (grepl("^https?://", url, ignore.case = TRUE)) {
    return("HTTPS")
  }

  "outro"
}

repo_status_table <- function(path = ".") {
  repo <- git_repo_root(path)
  if (is.null(repo)) {
    return(empty_status_table())
  }

  tryCatch(
    as.data.frame(gert::git_status(repo = repo)),
    error = function(e) empty_status_table()
  )
}

empty_status_table <- function() {
  data.frame(
    file = character(),
    status = character(),
    staged = logical(),
    stringsAsFactors = FALSE
  )
}

status_counts <- function(status_tbl) {
  if (nrow(status_tbl) == 0) {
    return(list(
      staged = 0L,
      new = 0L,
      modified = 0L,
      deleted = 0L,
      conflicted = 0L,
      total = 0L
    ))
  }

  list(
    staged = sum(status_tbl$staged, na.rm = TRUE),
    new = sum(status_tbl$status == "new" & !status_tbl$staged, na.rm = TRUE),
    modified = sum(status_tbl$status == "modified" & !status_tbl$staged, na.rm = TRUE),
    deleted = sum(status_tbl$status == "deleted" & !status_tbl$staged, na.rm = TRUE),
    conflicted = sum(status_tbl$status == "conflicted", na.rm = TRUE),
    total = nrow(status_tbl)
  )
}

status_breakdown <- function(status_tbl) {
  if (nrow(status_tbl) == 0) {
    return(list(
      new = character(),
      modified = character(),
      deleted = character(),
      staged = character(),
      conflicted = character()
    ))
  }

  list(
    new = status_tbl$file[status_tbl$status == "new" & !status_tbl$staged],
    modified = status_tbl$file[status_tbl$status == "modified" & !status_tbl$staged],
    deleted = status_tbl$file[status_tbl$status == "deleted" & !status_tbl$staged],
    staged = status_tbl$file[status_tbl$staged],
    conflicted = status_tbl$file[status_tbl$status == "conflicted"]
  )
}

repo_commit_history <- function(path = ".", max_commits = 10) {
  repo <- git_repo_root(path)
  if (is.null(repo) || !repo_has_commits(path)) {
    return(data.frame(
      commit = character(),
      author = character(),
      time = .POSIXct(numeric()),
      message = character(),
      stringsAsFactors = FALSE
    ))
  }
  
  tryCatch({
    log <- as.data.frame(gert::git_log(max = max_commits, repo = repo))
    log$message <- trimws(log$message)
    log$author <- gsub(" <.*>$", "", log$author)
    log[, c("commit", "author", "time", "message")]
  }, error = function(e) {
    data.frame(
      commit = character(),
      author = character(),
      time = .POSIXct(numeric()),
      message = character(),
      stringsAsFactors = FALSE
    )
  })
}
