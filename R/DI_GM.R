#' DI_GM Calculation
#'
#' Calculate the Dietary Index for Gut Microbiota (DI-GM), using given the serving sizes of foods and nutrients consumed per 1 day
#' @import dplyr
#' @import readr
#' @import haven
#' @param SERV_DATA The raw data file that includes all the serving sizes of foods and nutrients
#' @param RESPONDENTID The unique participant ID for each participant
#' @param GENDER The gender of the participant
#' @param AVOCADO The serving size of Avocado. Unit does not matter here since the scoring is based on the median intake of the food in the population. However, it is important to keep the same unit for all indivudal food consumption data.
#' @param BROCCOLI The serving size of Broccoli. Unit does not matter here as indicated previously.
#' @param CHICKPEA The serving size of Chickpea. Unit does not matter here as indicated previously.
#' @param COFFEE The serving size of Coffee. Unit does not matter here as indicated previously.
#' @param CRANBERRY The serving size of Cranberry. Unit does not matter here as indicated previously.
#' @param FERMENTED_DAIRY The serving size of Fermented Dairy. Unit does not matter here as indicated previously.
#' @param FIBER The serving size of Fiber. Unit does not matter here as indicated previously.
#' @param GREEN_TEA The serving size of Green Tea. Unit does not matter here as indicated previously.
#' @param SOYBEAN The serving size of Soybean. Unit does not matter here as indicated previously.
#' @param WHOLE_GRAIN The serving size of Whole Grain. Unit does not matter here as indicated previously.
#' @param TOTAL_FAT_PERCENTAGE The total fat calorie percentage of the total calories consumed. Unit is percent calories.
#' @param PROCESSED_MEAT The serving size of Processed Meat. Unit does not matter here as indicated previously.
#' @param RED_MEAT The serving size of Red Meat. Unit does not matter here as indicated previously.
#' @param REFINED_GRAIN The serving size of Refined Grain. Unit does not matter here as indicated previously.
#' @return The DI_GM index/score
#' @examples
#' data("DI_GM_VALIDATION")
#' DI_GM(SERV_DATA = DI_GM_VALIDATION, RESPONDENTID = DI_GM_VALIDATION$id, GENDER = DI_GM_VALIDATION$gender, AVOCADO = DI_GM_VALIDATION$avocado, BROCCOLI = DI_GM_VALIDATION$broccoli, CHICKPEA = DI_GM_VALIDATION$chickpea, COFFEE = DI_GM_VALIDATION$coffee, CRANBERRY = DI_GM_VALIDATION$cranberry, FERMENTED_DAIRY = DI_GM_VALIDATION$fermented_dairy, FIBER = DI_GM_VALIDATION$fiber, GREEN_TEA = DI_GM_VALIDATION$green_tea, SOYBEAN = DI_GM_VALIDATION$soybean, WHOLE_GRAIN = DI_GM_VALIDATION$whole_grain, TOTAL_FAT_PERCENTAGE = DI_GM_VALIDATION$total_fat_percentage, PROCESSED_MEAT = DI_GM_VALIDATION$processed_meat, RED_MEAT = DI_GM_VALIDATION$red_meat, REFINED_GRAIN = DI_GM_VALIDATION$refined_grain)
#' @export

# Score calculation for DI_GM
DI_GM = function(SERV_DATA, RESPONDENTID, GENDER, AVOCADO, BROCCOLI, CHICKPEA, COFFEE, CRANBERRY, FERMENTED_DAIRY, FIBER, GREEN_TEA, SOYBEAN, WHOLE_GRAIN, TOTAL_FAT_PERCENTAGE, PROCESSED_MEAT, RED_MEAT, REFINED_GRAIN) {
  
    ## Create variables and functions needed for DI_GM
    # this is for healthy foods
    median_healthy = function(actual) {
        median_score = median(actual, na.rm = TRUE)
        case_when(
            actual < median_score | actual == 0 ~ 0,
            actual >= median_score ~ 1
        )
    }

    # this is for unhealthy foods
    median_unhealthy = function(actual) {
        median_score = median(actual, na.rm = TRUE)
        case_when(
            actual < median_score | actual == 0 ~ 1,
            actual >= median_score ~ 0
        )
    }

    message("Reminder: this DI_GM index uses sex-specific medians to rank participants' food/drink serving sizes and then calculate DI_GM component scores, which may generate results that are specific to your study population but not comparable to other populations.")

    DI_GM_SCORE = SERV_DATA %>%
        dplyr::mutate(
            GENDER = GENDER,
            RESPONDENTID = RESPONDENTID,
            AVOCADO_SERV = AVOCADO,
            BROCCOLI_SERV = BROCCOLI,
            CHICKPEA_SERV = CHICKPEA,
            COFFEE_SERV = COFFEE,
            CRANBERRY_SERV = CRANBERRY,
            FERMENTED_DAIRY_SERV = FERMENTED_DAIRY,
            FIBER_SERV = FIBER,
            GREEN_TEA_SERV = GREEN_TEA,
            SOYBEAN_SERV = SOYBEAN,
            WHOLE_GRAIN_SERV = WHOLE_GRAIN,
            TOTAL_FAT_PERCENTAGE_SERV = TOTAL_FAT_PERCENTAGE,
            PROCESSED_MEAT_SERV = PROCESSED_MEAT,
            RED_MEAT_SERV = RED_MEAT,
            REFINED_GRAIN_SERV = REFINED_GRAIN
        ) %>%
        dplyr::group_by(GENDER) %>%
        dplyr::mutate(
            DI_GM_AVOCADO = median_healthy(AVOCADO_SERV),
            DI_GM_BROCCOLI = median_healthy(BROCCOLI_SERV),
            DI_GM_CHICKPEA = median_healthy(CHICKPEA_SERV),
            DI_GM_COFFEE = median_healthy(COFFEE_SERV),
            DI_GM_CRANBERRY = median_healthy(CRANBERRY_SERV),
            DI_GM_FERMENTED_DAIRY = median_healthy(FERMENTED_DAIRY_SERV),
            DI_GM_FIBER = median_healthy(FIBER_SERV),
            DI_GM_GREEN_TEA = median_healthy(GREEN_TEA_SERV),
            DI_GM_SOYBEAN = median_healthy(SOYBEAN_SERV),
            DI_GM_WHOLE_GRAIN = median_healthy(WHOLE_GRAIN_SERV),
            # for total fat percent kcal inake, 0 if consumption at or above 40% energy from fat, else 1
            DI_GM_TOTAL_FAT_PERCENTAGE = case_when(
                TOTAL_FAT_PERCENTAGE_SERV >= 40 ~ 0,
                TOTAL_FAT_PERCENTAGE_SERV < 40 ~ 1
            ),
            DI_GM_PROCESSED_MEAT = median_unhealthy(PROCESSED_MEAT_SERV),
            DI_GM_RED_MEAT = median_unhealthy(RED_MEAT_SERV),
            DI_GM_REFINED_GRAIN = median_unhealthy(REFINED_GRAIN_SERV),
            DI_GM_TOTAL = DI_GM_AVOCADO + DI_GM_BROCCOLI + DI_GM_CHICKPEA + DI_GM_COFFEE + DI_GM_CRANBERRY + DI_GM_FERMENTED_DAIRY + DI_GM_FIBER + DI_GM_GREEN_TEA + DI_GM_SOYBEAN + DI_GM_WHOLE_GRAIN + DI_GM_TOTAL_FAT_PERCENTAGE + DI_GM_PROCESSED_MEAT + DI_GM_RED_MEAT + DI_GM_REFINED_GRAIN
        ) %>%
        dplyr::select(
            RESPONDENTID, GENDER, DI_GM_TOTAL, DI_GM_AVOCADO, DI_GM_BROCCOLI, DI_GM_CHICKPEA, DI_GM_COFFEE, DI_GM_CRANBERRY, DI_GM_FERMENTED_DAIRY, DI_GM_FIBER, DI_GM_GREEN_TEA, DI_GM_SOYBEAN, DI_GM_WHOLE_GRAIN, DI_GM_TOTAL_FAT_PERCENTAGE, DI_GM_PROCESSED_MEAT, DI_GM_RED_MEAT, DI_GM_REFINED_GRAIN) %>%
        dplyr::ungroup()

    return(DI_GM_SCORE)
}
