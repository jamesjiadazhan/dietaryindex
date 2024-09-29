#' DASH Calculation
#'
#' Calculate the DASH dietary index, Dietary Approaches to Stop Hypertension, using given the serving sizes of foods and nutrients consumed per 1 day
#' @import dplyr
#' @import readr
#' @import haven
#' @param SERV_DATA The raw data file that includes all the serving sizes of foods and nutrients
#' @param RESPONDENTID The unique participant ID for each participant
#' @param TOTALKCAL_DASH The total energy intake, unit = kcal/day
#' @param FRT_FRTJ_SERV_DASH The serving size of fruits and 100\% fruit juice, unit=servings/day (0.5 c of berries; 1 cup=236.59 g; 1 med fruit (1 cup = 236.59 g); 1 cup fruit juice
#' @param VEG_SERV_DASH The serving size of All vegetable except potatoes and legume, unit=servings/day (0.5 c of vege; 1 cup of green leafy (1 cup = 236.59 g)
#' @param NUTSLEG_SERV_DASH The serving size of Nuts, legumes, and vegetable protein (e.g., tofu), unit=servings/day = 1 srv=1oz (28.35 g) of nuts or 1 TBLSP peanut butter (15 mL), 1 cup legume = 4 oz
#' @param WGRAIN_SERV_DASH The serving size of whole grains, unit=1oz
#' @param LOWF_DAIRY_SERV_DASH The serving size of low fat dairy, including 2\% or less fat milk + yogurt + low-fat ice cream and frozen yogurt + low-fat cheese, unit=servings/day = 1 glass milk + 1 cup yogurt + 1/2 cup ice cream/frozen yogurt + 1 slice cheese
#' @param SODIUM_SERV_DASH The serving size of sodium, unit=mg/day per 2000 kcal
#' @param REDPROC_MEAT_SERV_DASH The serving size of red and processed meats, including Beef, pork, lamb, goat, veal, sausages, bacon, salami, ham, hot dog, deli meat, unit=servings/day; 1 srv= 4 oz. unprocessed meat; 1.5 oz. processed meat (1 oz. = 28.35 g)
#' @param SSB_FRTJ_SERV_DASH The serving size of sugar-sweetened beverages and non-100\% fruit juice, unit=servings/day = 1 ser= 8oz (1 oz. = 28.35 g)
#' @return The DASH index/score
#' @examples
#' data("DASH_VALIDATION")
#' DASH(DASH_VALIDATION, RESPONDENTID = DASH_VALIDATION$id, TOTALKCAL_DASH = DASH_VALIDATION$kcal, FRT_FRTJ_SERV_DASH = DASH_VALIDATION$fruit, VEG_SERV_DASH = DASH_VALIDATION$vegetable, NUTSLEG_SERV_DASH = DASH_VALIDATION$nut_legume, WGRAIN_SERV_DASH = DASH_VALIDATION$whole_grain, LOWF_DAIRY_SERV_DASH = DASH_VALIDATION$low_fat_dairy, SODIUM_SERV_DASH = DASH_VALIDATION$sodium, REDPROC_MEAT_SERV_DASH = DASH_VALIDATION$red_processed_meat, SSB_FRTJ_SERV_DASH = DASH_VALIDATION$ssb)
#' @export

# Score calculation for DASH
DASH = function(SERV_DATA, RESPONDENTID, TOTALKCAL_DASH, FRT_FRTJ_SERV_DASH, VEG_SERV_DASH, NUTSLEG_SERV_DASH, WGRAIN_SERV_DASH, LOWF_DAIRY_SERV_DASH,
                SODIUM_SERV_DASH, REDPROC_MEAT_SERV_DASH, SSB_FRTJ_SERV_DASH) {
    ## Create variables and functions needed for DASH calculation
    quintile_healthy = function(actual) {
        quintile = quantile(actual, probs = seq(0, 1, by = 0.2), na.rm = TRUE)
        case_when(
            actual <= quintile[6] & actual >= quintile[5] ~ 5,
            actual < quintile[5] & actual >= quintile[4] ~ 4,
            actual < quintile[4] & actual >= quintile[3] ~ 3,
            actual < quintile[3] & actual >= quintile[2] ~ 2,
            actual < quintile[2] & actual >= quintile[1] ~ 1
        )
    }

    quintile_unhealthy = function(actual) {
        quintile = quantile(actual, probs = seq(0, 1, by = 0.2), na.rm = TRUE)
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
    SERV_DATA %>%
        dplyr::mutate(
            RESPONDENTID = RESPONDENTID,
            DASH_FRT = quintile_healthy(FRT_FRTJ_SERV_DASH),
            DASH_VEG = quintile_healthy(VEG_SERV_DASH),
            DASH_NUTSLEG = quintile_healthy(NUTSLEG_SERV_DASH),
            DASH_WGRAIN = quintile_healthy(WGRAIN_SERV_DASH),
            DASH_LOWF_DAIRY = quintile_healthy(LOWF_DAIRY_SERV_DASH),
            DASH_SODIUM = quintile_unhealthy(SODIUM_SERV_DASH / (TOTALKCAL_DASH / 2000)),
            DASH_REDPROC_MEAT = quintile_unhealthy(REDPROC_MEAT_SERV_DASH),
            DASH_SSB_FRTJ = quintile_unhealthy(SSB_FRTJ_SERV_DASH),
            DASH_ALL = DASH_FRT + DASH_VEG + DASH_NUTSLEG + DASH_WGRAIN + DASH_LOWF_DAIRY +
                DASH_SODIUM + DASH_REDPROC_MEAT + DASH_SSB_FRTJ
        ) %>%
        dplyr::select(
            RESPONDENTID, DASH_ALL, DASH_FRT, DASH_VEG, DASH_NUTSLEG, DASH_WGRAIN, DASH_LOWF_DAIRY,
            DASH_SODIUM, DASH_REDPROC_MEAT, DASH_SSB_FRTJ
        )
}
