#' @keywords internal
.onAttach <- function(libname, pkgname) {
    packageStartupMessage("Loaded dietaryindex")
    packageStartupMessage("Thank you for using dietaryindex!")
    packageStartupMessage("Tutorials: https://github.com/jamesjiadazhan/dietaryindex")
    packageStartupMessage("Questions, issues: Follow the prompts at https://github.com/jamesjiadazhan/dietaryindex/blob/main/.github/ISSUE_TEMPLATE/bug_report.md")
    packageStartupMessage("Cite us:  citation('dietaryindex') or
Jiada James Zhan, Rebecca A Hodge, Anne Dunlop, et al. Dietaryindex: A User-Friendly and Versatile R Package for Standardizing Dietary Pattern Analysis in Epidemiological and Clinical Studies. bioRxiv. Published online August 07, 2023:2023.08.07.548466. doi:10.1101/2023.08.07.548466")
}
