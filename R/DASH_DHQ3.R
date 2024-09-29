#' DASH_DHQ3
#'
#' Calculate the DASH for the DHQ3 data within 1 step
#' @import dplyr
#' @import readr
#' @import haven
#' @param DATA_PATH The file path for the data. The data is Detailed analysis file, ending with detail.csv
#' @return The DASH and its component scores
#' @examples
#' data("DHQ3_exp_detailed")
#' DASH_DHQ3(DHQ3_exp_detailed)
#' @export

DASH_DHQ3 = function(DATA_PATH) {
    if (is.character(DATA_PATH) == TRUE) {
        COHORT = read_csv(DATA_PATH)
    } else {
        COHORT = DATA_PATH
    }

    if (!("Food ID" %in% colnames(COHORT))) {
        stop("Please use individual-level data for this function. Individual-level data should be like detail.csv")
    }

    SKIM_MILK = c(5.4, 6.4, 10.4)
    LOWF_CHEESE = c(50.2)
    LOWF_CREAM = c(64.2, 75.2, 80.1)
    COFFEE_TEA = c(16.1, 17.1, 64.1, 64.2, 1081.1, 1123.1, 1123.2, 1130.2, 1130.5)
    DRINK = c(10.6, 10.9, 11.1, 11.2, 1140.1, 1140.2, 1144.1, 1150.1, 1152.1)
    SSB = c(COFFEE_TEA, DRINK)

    COHORT = COHORT %>%
        dplyr::mutate(
            SKIM_MILK_SERV = case_when(
                `Food ID` %in% SKIM_MILK ~ `Milk (cups)`,
                TRUE ~ 0
            ),
            LOWF_CHEESECREAM_SERV = case_when(
                `Food ID` %in% LOWF_CHEESE ~ `Total dairy (cups)` * 4,
                `Food ID` %in% LOWF_CREAM ~ `Total dairy (cups)` * 2,
                TRUE ~ 0
            ),
            ADDED_SUGAR_SSB_SERV = case_when(
                `Food ID` %in% SSB ~ `*Added sugars (g)`,
                TRUE ~ 0
            )
        ) %>%
        dplyr::group_by(`Respondent ID`) %>%
        dplyr::summarize(
            KCAL = sum(`Energy (kcal)`),
            FRT_FRTJ_SERV = sum(`Total fruit (cups)`),
            VEG_SERV = sum(`Dark-green vegetable (cups)` + (`Total red/orange vegetable (cups)` + `Other starchy vegetable (cups)` + `Other vegetable (cups)`) / 0.5),
            NUTSLEG_SERV = sum(`Nuts, seeds, soy, and legumes (oz)`),
            WGRAIN_SERV = sum(`Whole grain (oz)`),
            LOWF_DAIRY_SERV = sum(SKIM_MILK_SERV + LOWF_CHEESECREAM_SERV + `Yogurt (cups)`),
            SODIUM_SERV = sum(`Sodium (mg)` / (KCAL / 2000)),
            REDPROC_MEAT_SERV = sum((`Cured meat protein foods (oz)` / 1.5) + ((`Meat from beef, pork, veal, lamb, and game protein foods (oz)` + `Meat from organ meat protein foods (oz)`) / 4)),
            SSB_FRTJ_SERV = sum((ADDED_SUGAR_SSB_SERV / 26))
        )


    ## Create variables needed for DASH calculation
    ## Create variables and functions needed for DASH calculation
    quintile_healthy = function(actual) {
        quintile = quantile(actual, probs = seq(0, 1, by = 0.2))
        case_when(
            actual <= quintile[6] & actual >= quintile[5] ~ 5,
            actual < quintile[5] & actual >= quintile[4] ~ 4,
            actual < quintile[4] & actual >= quintile[3] ~ 3,
            actual < quintile[3] & actual >= quintile[2] ~ 2,
            actual < quintile[2] & actual >= quintile[1] ~ 1
        )
    }

    quintile_unhealthy = function(actual) {
        quintile = quantile(actual, probs = seq(0, 1, by = 0.2))
        case_when(
            actual <= quintile[6] & actual >= quintile[5] ~ 1,
            actual < quintile[5] & actual >= quintile[4] ~ 2,
            actual < quintile[4] & actual >= quintile[3] ~ 3,
            actual < quintile[3] & actual >= quintile[2] ~ 4,
            actual < quintile[2] & actual >= quintile[1] ~ 5
        )
    }

    message("Reminder: this DASH index uses quintiles to rank participants' food/drink serving sizes and then calculate DASH component scores, which may generate results that are specific to your study population but not comparable to other populations.")


    ## DASH calculation
    COHORT %>%
        dplyr::mutate(
            DASH_FRT = quintile_healthy(FRT_FRTJ_SERV),
            DASH_VEG = quintile_healthy(VEG_SERV),
            DASH_NUTSLEG = quintile_healthy(NUTSLEG_SERV),
            DASH_WGRAIN = quintile_healthy(WGRAIN_SERV),
            DASH_LOWF_DAIRY = quintile_healthy(LOWF_DAIRY_SERV),
            DASH_SODIUM = quintile_unhealthy(SODIUM_SERV),
            DASH_REDPROC_MEAT = quintile_unhealthy(REDPROC_MEAT_SERV),
            DASH_SSB_FRTJ = quintile_unhealthy(SSB_FRTJ_SERV),
            DASH_ALL = DASH_FRT + DASH_VEG + DASH_NUTSLEG + DASH_WGRAIN + DASH_LOWF_DAIRY +
                DASH_SODIUM + DASH_REDPROC_MEAT + DASH_SSB_FRTJ
        ) %>%
        dplyr::select(
            `Respondent ID`, DASH_ALL, DASH_FRT, DASH_VEG, DASH_NUTSLEG, DASH_WGRAIN, DASH_LOWF_DAIRY,
            DASH_SODIUM, DASH_REDPROC_MEAT, DASH_SSB_FRTJ
        )
}
