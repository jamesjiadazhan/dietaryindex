#' DASHI
#'
#' Calculate the DASHI dietary index (nutrient-based), Dietary Approaches to Stop Hypertension, using given the nutrients consumed per 1 day. All nutrients will be divided by (total energy/2000 kcal) to adjust energy intake.
#' @import dplyr
#' @import readr
#' @import haven
#' @param SERV_DATA The raw data file that includes all the serving sizes of foods and nutrients
#' @param RESPONDENTID The unique participant ID for each participant
#' @param TOTALKCAL_DASHI The total energy intake, unit = kcal/day
#' @param TOTAL_FAT_DASHI The total fat intake, unit = g/day
#' @param SAT_FAT_DASHI The saturated fat intake, unit = g/day
#' @param PROTEIN_DASHI The protein intake, unit = g/day
#' @param CHOLESTEROL_DASHI The cholesterol intake, unit = mg/day
#' @param FIBER_DASHI The fiber intake, unit = g/day
#' @param POTASSIUM_DASHI The potassium intake, unit = mg/day
#' @param MAGNESIUM_DASHI The magnesium intake, unit = mg/day
#' @param CALCIUM_DASHI The calcium intake, unit = mg/day
#' @param SODIUM_DASHI The sodium intake, unit = mg/day
#' @return The DASHI index/score
#' @examples
#' data("DASH_trial")
#' DASHI(SERV_DATA = DASH_trial, RESPONDENTID = DASH_trial$Diet_Type, TOTALKCAL_DASHI = DASH_trial$Kcal, TOTAL_FAT_DASHI = DASH_trial$Totalfat_Percent, SAT_FAT_DASHI = DASH_trial$Satfat_Percent, PROTEIN_DASHI = DASH_trial$Protein_Percent, CHOLESTEROL_DASHI = DASH_trial$Cholesterol, FIBER_DASHI = DASH_trial$Fiber, POTASSIUM_DASHI = DASH_trial$Potassium, MAGNESIUM_DASHI = DASH_trial$Magnesium, CALCIUM_DASHI = DASH_trial$Calcium, SODIUM_DASHI = DASH_trial$Sodium)
#' @export

# Score calculation for DASHI
DASHI = function(SERV_DATA, RESPONDENTID, TOTALKCAL_DASHI, TOTAL_FAT_DASHI, SAT_FAT_DASHI, PROTEIN_DASHI, CHOLESTEROL_DASHI, FIBER_DASHI, POTASSIUM_DASHI, MAGNESIUM_DASHI, CALCIUM_DASHI, SODIUM_DASHI) {
    ## Create variables and functions needed for DASHI calculation
    DASHI_MIN = 0
    DASHI_MAX = 1

    DASHI_MIN_TOTAL_FAT = 37
    DASHI_MAX_TOTAL_FAT = 27
    DASHI_MIN_SAT_FAT = 16
    DASHI_MAX_SAT_FAT = 6
    DASHI_MIN_PROTEIN = 15
    DASHI_MAX_PROTEIN = 18
    DASHI_MIN_CHOLESTEROL = 285.7
    DASHI_MAX_CHOLESTEROL = 142.8
    DASHI_MIN_FIBER = 8.6
    DASHI_MAX_FIBER = 29.5
    DASHI_MIN_POTASSIUM = 1619
    DASHI_MAX_POTASSIUM = 4476
    DASHI_MIN_MAGNESIUM = 157
    DASHI_MAX_MAGNESIUM = 476
    DASHI_MIN_CALCIUM = 429
    DASHI_MAX_CALCIUM = 1181
    DASHI_MIN_SODIUM = 2857
    DASHI_MAX_SODIUM = 2286

    DASHI_HEALTHY = function(actual, min, max) {
        case_when(
            actual >= max ~ DASHI_MAX,
            actual <= min ~ DASHI_MIN,
            TRUE ~ DASHI_MIN + (actual - min) * DASHI_MAX / (max - min)
        )
    }

    DASHI_UNHEALTHY = function(actual, min, max) {
        case_when(
            actual >= min ~ DASHI_MIN,
            actual <= max ~ DASHI_MAX,
            TRUE ~ DASHI_MIN + (actual - min) * DASHI_MAX / (max - min)
        )
    }

    ## DASHI calculation
    SERV_DATA %>%
        dplyr::mutate(
            RESPONDENTID = RESPONDENTID,
            TOTALKCAL_DASHI = TOTALKCAL_DASHI,
            TOTAL_FAT_DASHI = TOTAL_FAT_DASHI,
            SAT_FAT_DASHI = SAT_FAT_DASHI,
            PROTEIN_DASHI = PROTEIN_DASHI,
            CHOLESTEROL_DASHI = CHOLESTEROL_DASHI,
            FIBER_DASHI = FIBER_DASHI,
            POTASSIUM_DASHI = POTASSIUM_DASHI,
            MAGNESIUM_DASHI = MAGNESIUM_DASHI,
            CALCIUM_DASHI = CALCIUM_DASHI,
            SODIUM_DASHI = SODIUM_DASHI,
            DASHI_TOTAL_FAT = DASHI_UNHEALTHY(TOTAL_FAT_DASHI, DASHI_MIN_TOTAL_FAT, DASHI_MAX_TOTAL_FAT),
            DASHI_SAT_FAT = DASHI_UNHEALTHY(SAT_FAT_DASHI, DASHI_MIN_SAT_FAT, DASHI_MAX_SAT_FAT),
            DASHI_CHOLESTEROL = DASHI_UNHEALTHY(CHOLESTEROL_DASHI, DASHI_MIN_CHOLESTEROL, DASHI_MAX_CHOLESTEROL),
            DASHI_SODIUM = DASHI_UNHEALTHY(SODIUM_DASHI, DASHI_MIN_SODIUM, DASHI_MAX_SODIUM),
            DASHI_PROTEIN = DASHI_HEALTHY(PROTEIN_DASHI, DASHI_MIN_PROTEIN, DASHI_MAX_PROTEIN),
            DASHI_FIBER = DASHI_HEALTHY(FIBER_DASHI, DASHI_MIN_FIBER, DASHI_MAX_FIBER),
            DASHI_POTASSIUM = DASHI_HEALTHY(POTASSIUM_DASHI, DASHI_MIN_POTASSIUM, DASHI_MAX_POTASSIUM),
            DASHI_MAGNESIUM = DASHI_HEALTHY(MAGNESIUM_DASHI, DASHI_MIN_MAGNESIUM, DASHI_MAX_MAGNESIUM),
            DASHI_CALCIUM = DASHI_HEALTHY(CALCIUM_DASHI, DASHI_MIN_CALCIUM, DASHI_MAX_CALCIUM),
            DASHI_ALL = DASHI_TOTAL_FAT + DASHI_SAT_FAT + DASHI_CHOLESTEROL + DASHI_SODIUM +
                DASHI_PROTEIN + DASHI_FIBER + DASHI_POTASSIUM + DASHI_MAGNESIUM + DASHI_CALCIUM
        ) %>%
        dplyr::select(RESPONDENTID, DASHI_ALL, TOTALKCAL_DASHI, DASHI_TOTAL_FAT, DASHI_SAT_FAT, DASHI_PROTEIN, DASHI_CHOLESTEROL, DASHI_FIBER, DASHI_POTASSIUM, DASHI_MAGNESIUM, DASHI_CALCIUM, DASHI_SODIUM)
}
