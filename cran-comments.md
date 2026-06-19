## Submission

This is a new submission of the package 'statgit' (version 0.1.0).

## Test environments

* local macOS 15.5, R 4.5.1 (primary check environment)
* GitHub Actions (via `.github/workflows/R-CMD-check.yaml`):
  * ubuntu-latest, R release
  * ubuntu-latest, R devel
  * ubuntu-latest, R oldrel-1
  * windows-latest, R release
  * macos-latest, R release

## R CMD check results

Local `R CMD check --as-cran` results:

0 errors | 0 warnings | 0 notes

## Expected CRAN notes

### 1. New submission

Expected for a first submission. No previous version on CRAN.

### 2. Repository URL

The package URL and BugReports entries point to
<https://github.com/geovani-ariel/statgit> and
<https://github.com/geovani-ariel/statgit/issues>.
The repository should be public at submission time so CRAN can validate the
links.

## Interactive-only behavior

The package is designed for interactive use in RStudio. It interacts with Git,
RStudio, Shiny/miniUI gadgets, Pandoc, and the Quarto CLI only after explicit
user action in an interactive session. No examples launch gadgets, open
browsers, or access the network during `R CMD check`.

Code that relies on suggested packages (shiny, miniUI, htmltools, styler,
rmarkdown) is guarded with `requireNamespace()`. RStudio-specific calls are
guarded with `rstudioapi::isAvailable()`.

System requirements are declared in the DESCRIPTION `SystemRequirements` field
as Git; Pandoc for 'R Markdown' preview; and optional Quarto CLI for '.qmd'
preview.

## Downstream dependencies

There are currently no downstream dependencies (new package).
