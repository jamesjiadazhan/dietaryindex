#' @keywords internal
.onAttach <- function(libname, pkgname) {
    packageStartupMessage("Loaded dietaryindex")
    packageStartupMessage("Thank you for using dietaryindex!")
    packageStartupMessage("Tutorials: https://github.com/jamesjiadazhan/dietaryindex")
    packageStartupMessage("Dietary index calculations vary by the research question of the analysis. Currently, dietaryindex only supports simple scoring algorithm methods for all dietary indexes. Please use dietaryindex responsibly according to your research question.")
    packageStartupMessage("Cite us:  citation('dietaryindex') or
Jiada James Zhan, Rebecca A Hodge, Anne Dunlop, et al. Dietaryindex: A User-Friendly and Versatile R Package for Standardizing Dietary Pattern Analysis in Epidemiological and Clinical Studies. bioRxiv. Published online August 09, 2023:2023.08.09.548466. doi:10.1101/2023.08.07.548466")
}
