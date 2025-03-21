#' AHEIP_DHQ3
#'
#' Calculate the AHEIP (alternative healthy eating index - pregnancy) for the DHQ3 data within 1 step
#' @import dplyr
#' @import readr
#' @import haven
#' @param DATA_PATH The file path for the data. The data is Detailed analysis file, ending with detail.csv
#' @return The AHEIP and its component scores
#' @examples
#' data("DHQ3_exp")
#' AHEIP_DHQ3(DHQ3_exp)
#' @export

AHEIP_DHQ3 = function(DATA_PATH) {
    if (is.character(DATA_PATH) == TRUE) {
        COHORT = read_csv(DATA_PATH)
    } else {
        COHORT = DATA_PATH
    }

    if ("Food ID" %in% colnames(COHORT)) {
        stop("Please use population-level data for this function. Population-level data should be like results.csv")
    }

    COHORT = COHORT %>%
        dplyr::mutate(
            KCAL = `Energy (kcal)`,
            VEG_SERV = `Dark-green vegetable (cups)` + (`Total red/orange vegetable (cups)` + `Other starchy vegetable (cups)` + `Other vegetable (cups)`) / 0.5,
            FRT_SERV = `Total fruit (cups)` - `Juice fruit (cups)`,
            # if the WHITERED_RT_SERV  is not available because the denominator is 0, set it to 4 (maximum value)
            WHITERED_RT_SERV = case_when(
                ((`Cured meat protein foods (oz)` / 1.5) + ((`Meat from beef, pork, veal, lamb, and game protein foods (oz)` + `Meat from organ meat protein foods (oz)`) / 4)) == 0 ~ 4,
                TRUE ~ ((`Poultry protein foods (oz)` + `Seafood high in omega-3 protein foods (oz)` + `Seafood low in omega-3 protein foods (oz)`) / 4) / ((`Cured meat protein foods (oz)` / 1.5) + ((`Meat from beef, pork, veal, lamb, and game protein foods (oz)` + `Meat from organ meat protein foods (oz)`) / 4))
            ),
            FIBER_SERV = `Dietary fiber (g)`,
            TRANS_SERV = ((`*Total trans fatty acitds (g)` * 9) / KCAL) * 100,
            NUTSLEG_SERV = `Nuts, seeds, soy, and legumes (oz)`,
            # if the POLYSAT_RT_SERV is not available because the denominator is 0, set it to 1 (maximum value)
            POLYSAT_RT_SERV = case_when(
                `Total saturated fatty acids (g)` == 0 ~ 1,
                TRUE ~ `*Polyunsaturated to saturated fatty acid ratio`
            ),
            CALCIUM_SERV = `Calcium (mg)`,
            FOLATE_SERV = `Total folate (mcg)`,
            IRON_SERV = `Iron (mg)`
        )

    # calculate the AHEIP using the precalculated SERV_DATA for AHEIP
    AHEIP_result = AHEIP(
        SERV_DATA = COHORT, 
        RESPONDENTID = COHORT$`Respondent ID`, 
        VEG_SERV_AHEIP = COHORT$VEG_SERV, 
        FRT_SERV_AHEIP = COHORT$FRT_SERV, 
        WHITERED_RT_SERV_AHEIP = COHORT$WHITERED_RT_SERV, 
        FIBER_SERV_AHEIP = COHORT$FIBER_SERV, 
        TRANS_SERV_AHEIP = COHORT$TRANS_SERV, 
        POLYSAT_RT_SERV_AHEIP = COHORT$POLYSAT_RT_SERV, 
        CALCIUM_SERV_AHEIP = COHORT$CALCIUM_SERV, 
        FOLATE_SERV_AHEIP = COHORT$FOLATE_SERV, 
        IRON_SERV_AHEIP = COHORT$IRON_SERV
    )

    return(AHEIP_result)
}
