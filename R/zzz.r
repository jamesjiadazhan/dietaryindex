#' @keywords internal
.onAttach <- function(libname, pkgname) {
    packageStartupMessage("Loaded dietaryindex")
    packageStartupMessage("Thank you for using dietaryindex!")
    packageStartupMessage("Tutorials: https://github.com/jamesjiadazhan/dietaryindex")
    packageStartupMessage("Dietary index calculations vary by the research question of the analysis. Currently, dietaryindex only supports simple scoring algorithm methods for all dietary indexes. Please use dietaryindex responsibly according to your research question. For more information, please refer to this resource: https://epi.grants.cancer.gov/hei/tools.html.")
    packageStartupMessage("Cite us:  citation('dietaryindex') or
Zhan JJ, Hodge RA, Dunlop AL, et al. Dietaryindex: a user-friendly and versatile R package for standardizing dietary pattern analysis in epidemiological and clinical studies. Am J Clin Nutr. Published online August 23, 2024. doi:10.1016/j.ajcnut.2024.08.021")
}
