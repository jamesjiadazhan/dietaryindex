#' MED_DHQ3
#'
#' Calculate the MED for the DHQ3 data within 1 step
#' @import dplyr
#' @import readr
#' @import haven
#' @param DATA_PATH The file path for the data. The data is Total Daily Results file, ending with results.csv
#' @return The MED and its component scores
#' @examples
#' data("DHQ3_exp")
#' MED_DHQ3(DHQ3_exp)
#' @export

MED_DHQ3 = function(DATA_PATH) {
    if (is.character(DATA_PATH) == TRUE) {
        COHORT = read_csv(DATA_PATH)
    } else {
        COHORT = DATA_PATH
    }

    if ("Food ID" %in% colnames(COHORT)) {
        stop("Please use population-level data for this function. Population-level data should be like results.csv")
    }

    # Match participant response food frequency to the standard food frequency response code

    COHORT = COHORT %>%
        dplyr::mutate(
            RESPONDENTID = `Respondent ID`,
            FRT_FRTJ_SERV = `Total fruit (cups)`,
            VEG_SERV = `Dark-green vegetable (cups)` + (`Total red/orange vegetable (cups)` + `Other starchy vegetable (cups)` + `Other vegetable (cups)`) / 0.5,
            WGRAIN_SERV = `Whole grain (oz)`,
            LEGUMES_SERV = `Soy products protein foods (oz)` + `Legumes protein foods (oz)`,
            NUTS_SERV = `Nuts and seeds protein foods (oz)`,
            FISH_SERV = `Seafood (oz)`,
            REDPROC_MEAT_SERV = (`Cured meat protein foods (oz)` / 1.5) + ((`Meat from beef, pork, veal, lamb, and game protein foods (oz)` + `Meat from organ meat protein foods (oz)`) / 4),
            MONSATFAT_SERV = case_when(
                `Total saturated fatty acids (g)` == 0 ~ 0,
                TRUE ~ `Total monounsaturated fatty acids (g)` / `Total saturated fatty acids (g)`
            ),
            ALCOHOL_SERV = `Alcohol (g)`
        )

    ## Create variables and functions needed for MED
    median_healthy = function(actual) {
        median_score = median(actual)
        case_when(
            actual < median_score ~ 0,
            actual >= median_score ~ 1
        )
    }

    median_unhealthy = function(actual) {
        median_score = median(actual)
        case_when(
            actual < median_score ~ 1,
            actual >= median_score ~ 0
        )
    }

    COHORT %>%
        dplyr::mutate(
            MED_FRT = median_healthy(FRT_FRTJ_SERV),
            MED_VEG = median_healthy(VEG_SERV),
            MED_WGRAIN = median_healthy(WGRAIN_SERV),
            MED_LEGUMES = median_healthy(LEGUMES_SERV),
            MED_NUTS = median_healthy(NUTS_SERV),
            MED_FISH = median_healthy(FISH_SERV),
            MED_REDPROC_MEAT = median_unhealthy(REDPROC_MEAT_SERV),
            MED_MONSATFAT = median_healthy(MONSATFAT_SERV),
            MED_ALCOHOL = case_when(
                ALCOHOL_SERV <= 25 & ALCOHOL_SERV >= 10 ~ 1,
                TRUE ~ 0
            ),
            MED_ALL = MED_FRT + MED_VEG + MED_WGRAIN + MED_LEGUMES + MED_NUTS + MED_FISH + MED_REDPROC_MEAT + MED_MONSATFAT + MED_ALCOHOL,
            MED_NOETOH = MED_FRT + MED_VEG + MED_WGRAIN + MED_LEGUMES + MED_NUTS + MED_FISH + MED_REDPROC_MEAT + MED_MONSATFAT
        ) %>%
        dplyr::select(
            RESPONDENTID, MED_ALL, MED_NOETOH, MED_FRT, MED_VEG, MED_WGRAIN, MED_LEGUMES, MED_NUTS,
            MED_FISH, MED_REDPROC_MEAT, MED_MONSATFAT, MED_ALCOHOL
        )
}
