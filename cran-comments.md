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

0 errors | 0 warnings | 3 notes (all acceptable; see below)

### Notes

#### 1. New submission

Expected for a first submission. No previous version on CRAN.

#### 2. Found the following (possibly) invalid URLs

The GitHub repository <https://github.com/geovani-ariel/statgit> does not yet
exist publicly (it will be created before the package goes live on CRAN). The
URL is correct and will resolve once the repository is made public.

#### 3. Title field not in title case / Language is pt-BR

The Title field is written in Portuguese (DESCRIPTION contains `Language: pt-BR`).
Portuguese grammar uses lowercase for prepositions and articles, so
"Controle de Versão e Gerenciamento de Projetos de Estatística no RStudio"
is correctly cased. The automated English title-case suggestion does not apply
to Portuguese text.

#### 4. Pandoc/tidy not available locally (environmental, not a package issue)

Two sub-notes are environmental:

* "README.md or NEWS.md cannot be checked without pandoc" — pandoc is not in
  the local PATH during check; the files are valid Markdown.
* "Skipping checking HTML validation: tidy doesn't look like recent enough
  HTML Tidy" — local tidy binary is outdated; the generated HTML is valid.
  Both checks pass on win-builder and GitHub Actions where pandoc/tidy are
  available.

## Interactive-only behavior

The package interacts with Git, RStudio, Shiny/miniUI gadgets, Pandoc, and
the Quarto CLI. All such functionality is only triggered by explicit user
action in an interactive session. No examples launch gadgets, open browsers,
or access the network during `R CMD check`.

Code that relies on suggested packages (shiny, miniUI, htmltools, styler,
rmarkdown) is guarded with `requireNamespace()`. RStudio-specific calls are
guarded with `rstudioapi::isAvailable()`.

System requirements (Git, Pandoc, optional Quarto) are declared in the
DESCRIPTION `SystemRequirements` field.

## Downstream dependencies

There are currently no downstream dependencies (new package).
