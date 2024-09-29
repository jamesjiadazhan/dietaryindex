#' DASHI_NHANES_FPED
#'
#' Calculate the DASHI (nutrient-based) for the NHANES_FPED data (after 2005) within 1 step
#' @import dplyr
#' @import readr
#' @import haven
#' @param NUTRIENT_PATH The file path for the NUTRIENT data. The file name should be like: DR1TOT_J.XPT
#' @param NUTRIENT_PATH2 The file path for the NUTRIENT2 data. The file name should be like: DR2TOT_J.XPT
#' @return The DASHI and its component scores and serving sizes
#' @examples
#' data("NHANES_20172018")
#' DASHI_NHANES_FPED(NUTRIENT_PATH = NHANES_20172018$NUTRIENT, NUTRIENT_PATH2 = NHANES_20172018$NUTRIENT2)
#' @export


DASHI_NHANES_FPED = function(NUTRIENT_PATH = NULL, NUTRIENT_PATH2 = NULL) {
    # stop if the input data is not provided for any day
    if (is.null(NUTRIENT_PATH) & is.null(NUTRIENT_PATH2)) {
        stop("Please provide the file path for the NUTRIENT data, day 1 or day 2 or day 1 and day 2.")
    }

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

    # if only day 1 data is provided
    if (!is.null(NUTRIENT_PATH)) {
        if (is.character(NUTRIENT_PATH) == TRUE) {
            NUTRIENT = read_xpt(NUTRIENT_PATH)
        } else {
            NUTRIENT = NUTRIENT_PATH
        }

        if ("DR1ILINE" %in% colnames(NUTRIENT)) {
            stop("Please use the population-level data for the first day data. The file name should contain: TOT")
        }

        # Select only the high quality data
        COHORT = NUTRIENT %>%
            filter(DR1DRSTZ == 1) %>%
            arrange(SEQN)

        # Specify variables for DASHI seving size
        COHORT = COHORT %>%
            mutate(
                TOTAL_FAT_DASHI = (DR1TTFAT * 9 / DR1TKCAL) * 100,
                SAT_FAT_DASHI = (DR1TSFAT * 9 / DR1TKCAL) * 100,
                PROTEIN_DASHI = (DR1TPROT * 4 / DR1TKCAL) * 100,
                CHOLESTEROL_DASHI = DR1TCHOL / (DR1TKCAL / 2000),
                FIBER_DASHI = DR1TFIBE / (DR1TKCAL / 2000),
                POTASSIUM_DASHI = DR1TPOTA / (DR1TKCAL / 2000),
                MAGNESIUM_DASHI = DR1TMAGN / (DR1TKCAL / 2000),
                CALCIUM_DASHI = DR1TCALC / (DR1TKCAL / 2000),
                SODIUM_DASHI = DR1TSODI / (DR1TKCAL / 2000)
            )

        ## DASHI calculation
        COHORT = COHORT %>%
            dplyr::mutate(
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
            dplyr::select(
                SEQN, DASHI_ALL, DASHI_TOTAL_FAT, DASHI_SAT_FAT, DASHI_CHOLESTEROL, DASHI_SODIUM,
                DASHI_PROTEIN, DASHI_FIBER, DASHI_POTASSIUM, DASHI_MAGNESIUM, DASHI_CALCIUM
            )
    }

    # if only day 2 data is provided
    if (!is.null(NUTRIENT_PATH2)) {
        if (is.character(NUTRIENT_PATH2) == TRUE) {
            NUTRIENT2 = read_xpt(NUTRIENT_PATH2)
        } else {
            NUTRIENT2 = NUTRIENT_PATH2
        }

        if ("DR2ILINE" %in% colnames(NUTRIENT2)) {
            stop("Please use the population-level data for the second day data. The file name should contain: TOT")
        }

        COHORT2 = NUTRIENT2 %>%
            filter(DR2DRSTZ == 1) %>%
            arrange(SEQN)

        # Specify variables for DASHI seving size
        COHORT2 = COHORT2 %>%
            mutate(
                TOTAL_FAT_DASHI = (DR2TTFAT * 9 / DR2TKCAL) * 100,
                SAT_FAT_DASHI = (DR2TSFAT * 9 / DR2TKCAL) * 100,
                PROTEIN_DASHI = (DR2TPROT * 4 / DR2TKCAL) * 100,
                CHOLESTEROL_DASHI = DR2TCHOL / (DR2TKCAL / 2000),
                FIBER_DASHI = DR2TFIBE / (DR2TKCAL / 2000),
                POTASSIUM_DASHI = DR2TPOTA / (DR2TKCAL / 2000),
                MAGNESIUM_DASHI = DR2TMAGN / (DR2TKCAL / 2000),
                CALCIUM_DASHI = DR2TCALC / (DR2TKCAL / 2000),
                SODIUM_DASHI = DR2TSODI / (DR2TKCAL / 2000)
            )

        ## DASHI calculation
        COHORT2 = COHORT2 %>%
            dplyr::mutate(
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
            dplyr::select(
                SEQN, DASHI_ALL, DASHI_TOTAL_FAT, DASHI_SAT_FAT, DASHI_CHOLESTEROL, DASHI_SODIUM,
                DASHI_PROTEIN, DASHI_FIBER, DASHI_POTASSIUM, DASHI_MAGNESIUM, DASHI_CALCIUM
            )
    }

    # if only day 1 data is provided
    if (!is.null(NUTRIENT_PATH) & is.null(NUTRIENT_PATH2)) {
        return(COHORT)
    }

    # if only day 2 data is provided
    if (is.null(NUTRIENT_PATH) & !is.null(NUTRIENT_PATH2)) {
        return(COHORT2)
    }

    # merge two days data if they both exist by creating columns with the same name but taking average of the original column
    if (!is.null(NUTRIENT_PATH) & !is.null(NUTRIENT_PATH2)) {
        COHORT12 <- inner_join(COHORT, COHORT2, by = "SEQN") %>%
            dplyr::mutate(
                DASHI_ALL = (DASHI_ALL.x + DASHI_ALL.y) / 2,
                DASHI_TOTAL_FAT = (DASHI_TOTAL_FAT.x + DASHI_TOTAL_FAT.y) / 2,
                DASHI_SAT_FAT = (DASHI_SAT_FAT.x + DASHI_SAT_FAT.y) / 2,
                DASHI_CHOLESTEROL = (DASHI_CHOLESTEROL.x + DASHI_CHOLESTEROL.y) / 2,
                DASHI_SODIUM = (DASHI_SODIUM.x + DASHI_SODIUM.y) / 2,
                DASHI_PROTEIN = (DASHI_PROTEIN.x + DASHI_PROTEIN.y) / 2,
                DASHI_FIBER = (DASHI_FIBER.x + DASHI_FIBER.y) / 2,
                DASHI_POTASSIUM = (DASHI_POTASSIUM.x + DASHI_POTASSIUM.y) / 2,
                DASHI_MAGNESIUM = (DASHI_MAGNESIUM.x + DASHI_MAGNESIUM.y) / 2,
                DASHI_CALCIUM = (DASHI_CALCIUM.x + DASHI_CALCIUM.y) / 2
            ) %>%
            dplyr::select(
                SEQN, DASHI_ALL, DASHI_TOTAL_FAT, DASHI_SAT_FAT, DASHI_CHOLESTEROL, DASHI_SODIUM,
                DASHI_PROTEIN, DASHI_FIBER, DASHI_POTASSIUM, DASHI_MAGNESIUM, DASHI_CALCIUM
            )
        return(COHORT12)
    }
}
