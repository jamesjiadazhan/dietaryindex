#' @keywords internal
.onAttach <- function(libname, pkgname) {
  packageStartupMessage("Loaded dietaryindex")
  packageStartupMessage("Thank you for using dietaryindex!")
  packageStartupMessage("Tutorials: https://github.com/jamesjiadazhan/dietaryindex")
  packageStartupMessage("Questions, issues: Follow the prompts at https://github.com/jamesjiadazhan/dietaryindex/blob/main/.github/ISSUE_TEMPLATE/bug_report.md")
  packageStartupMessage("Cite us:  citation('dietaryindex')")
}
