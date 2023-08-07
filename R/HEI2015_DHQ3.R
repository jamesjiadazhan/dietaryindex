#' HEI2015_DHQ3
#'
#' Calculate the HEI2015 for the DHQ3 data within 1 step
#' @import dplyr
#' @import readr
#' @import haven
#' @param DATA_PATH The file path for the data. The data is Total Daily Results file, ending with results.csv
#' @return The HEI2015 and its component scores
#' @examples
#' data("DHQ3_exp")
#' HEI2015_DHQ3(DHQ3_exp)
#' @export

HEI2015_DHQ3 = function(DATA_PATH) {
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
            TOTALKCAL = `Energy (kcal)`,
            TOTALFRT_SERV = `Total fruit (cups)` / (TOTALKCAL / 1000),
            FRT_SERV = (`Total fruit (cups)` - `Juice fruit (cups)`) / (TOTALKCAL / 1000),
            VEG_SERV = (`Total vegetable (cups)` + `Legumes vegetable (cups)`) / (TOTALKCAL / 1000),
            GREENNBEAN_SERV = (`Dark-green vegetable (cups)` + `Legumes vegetable (cups)`) / (TOTALKCAL / 1000),
            TOTALPRO_SERV = (`Total meat, poultry, seafood protein foods (oz)` + `Eggs protein foods (oz)` + `Nuts and seeds protein foods (oz)` + `Soy products protein foods (oz)` + `Legumes protein foods (oz)`) / (TOTALKCAL / 1000),
            SEAPLANTPRO_SERV = (`Seafood (oz)` + `Nuts and seeds protein foods (oz)` + `Soy products protein foods (oz)` + `Legumes protein foods (oz)`) / (TOTALKCAL / 1000),
            WHOLEGRAIN_SERV = (`Whole grain (oz)`) / (TOTALKCAL / 1000),
            DAIRY_SERV = (`Total dairy (cups)`) / (TOTALKCAL / 1000),
            FATTYACID_SERV = (`Total monounsaturated fatty acids (g)` + `Total polyunsaturated fatty acids (g)`) / `Total saturated fatty acids (g)`,
            REFINEDGRAIN_SERV = (`Refined grain (oz)`) / (TOTALKCAL / 1000),
            SODIUM_SERV = (`Sodium (mg)` / 1000) / (TOTALKCAL / 1000),
            ADDEDSUGAR_SERV = ((`Added sugars (tsp)` * 4 * 4) / (TOTALKCAL)) * 100,
            SATFAT_SERV = ((`Total saturated fatty acids (g)` * 9) / (TOTALKCAL)) * 100
        )


    ## Create variables needed for HEI2015 calculation
    HEI2015_MIN = 0
    HEI2015_MAX1 = 5
    HEI2015_MAX2 = 10

    HEI2015_MIN_TOTALFRT_SERV = 0
    HEI2015_MAX_TOTALFRT_SERV = 0.8
    HEI2015_MIN_FRT_SERV = 0
    HEI2015_MAX_FRT_SERV = 0.4
    HEI2015_MIN_VEG_SERV = 0
    HEI2015_MAX_VEG_SERV = 1.1
    HEI2015_MIN_GREENNBEAN_SERV = 0
    HEI2015_MAX_GREENNBEAN_SERV = 0.2
    HEI2015_MIN_TOTALPRO_SERV = 0
    HEI2015_MAX_TOTALPRO_SERV = 2.5
    HEI2015_MIN_SEAPLANTPRO_SERV = 0
    HEI2015_MAX_SEAPLANTPRO_SERV = 0.8
    HEI2015_MIN_WHOLEGRAIN_SERV = 0
    HEI2015_MAX_WHOLEGRAIN_SERV = 1.5
    HEI2015_MIN_DAIRY_SERV = 0
    HEI2015_MAX_DAIRY_SERV = 1.3
    HEI2015_MIN_FATTYACID_SERV = 1.2
    HEI2015_MAX_FATTYACID_SERV = 2.5

    HEI2015_MIN_REFINEDGRAIN_SERV = 4.3
    HEI2015_MAX_REFINEDGRAIN_SERV = 1.8
    HEI2015_MIN_SODIUM_SERV = 2.0
    HEI2015_MAX_SODIUM_SERV = 1.1
    HEI2015_MIN_ADDEDSUGAR_SERV = 26
    HEI2015_MAX_ADDEDSUGAR_SERV = 6.5
    HEI2015_MIN_SATFAT_SERV = 16
    HEI2015_MAX_SATFAT_SERV = 8

    HEI2015_HEALTHY1 = function(actual, min, max) {
        case_when(
            actual >= max ~ HEI2015_MAX1,
            actual <= min ~ HEI2015_MIN,
            TRUE ~ HEI2015_MIN + (actual - min) * HEI2015_MAX1 / (max - min)
        )
    }

    HEI2015_HEALTHY2 = function(actual, min, max) {
        case_when(
            actual >= max ~ HEI2015_MAX2,
            actual <= min ~ HEI2015_MIN,
            TRUE ~ HEI2015_MIN + (actual - min) * HEI2015_MAX2 / (max - min)
        )
    }

    HEI2015_UNHEALTHY = function(actual, min, max) {
        case_when(
            actual >= min ~ HEI2015_MIN,
            actual <= max ~ HEI2015_MAX2,
            TRUE ~ HEI2015_MIN + (actual - min) * HEI2015_MAX2 / (max - min)
        )
    }

    COHORT = COHORT %>%
        dplyr::mutate(
            HEI2015_TOTALFRT = HEI2015_HEALTHY1(TOTALFRT_SERV, HEI2015_MIN_TOTALFRT_SERV, HEI2015_MAX_TOTALFRT_SERV),
            HEI2015_FRT = HEI2015_HEALTHY1(FRT_SERV, HEI2015_MIN_FRT_SERV, HEI2015_MAX_FRT_SERV),
            HEI2015_VEG = HEI2015_HEALTHY1(VEG_SERV, HEI2015_MIN_VEG_SERV, HEI2015_MAX_VEG_SERV),
            HEI2015_GREENNBEAN = HEI2015_HEALTHY1(GREENNBEAN_SERV, HEI2015_MIN_GREENNBEAN_SERV, HEI2015_MAX_GREENNBEAN_SERV),
            HEI2015_TOTALPRO = HEI2015_HEALTHY1(TOTALPRO_SERV, HEI2015_MIN_TOTALPRO_SERV, HEI2015_MAX_TOTALPRO_SERV),
            HEI2015_SEAPLANTPRO = HEI2015_HEALTHY1(SEAPLANTPRO_SERV, HEI2015_MIN_SEAPLANTPRO_SERV, HEI2015_MAX_SEAPLANTPRO_SERV),
            HEI2015_WHOLEGRAIN = HEI2015_HEALTHY2(WHOLEGRAIN_SERV, HEI2015_MIN_WHOLEGRAIN_SERV, HEI2015_MAX_WHOLEGRAIN_SERV),
            HEI2015_DAIRY = HEI2015_HEALTHY2(DAIRY_SERV, HEI2015_MIN_DAIRY_SERV, HEI2015_MAX_DAIRY_SERV),
            HEI2015_FATTYACID = HEI2015_HEALTHY2(FATTYACID_SERV, HEI2015_MIN_FATTYACID_SERV, HEI2015_MAX_FATTYACID_SERV),
            HEI2015_REFINEDGRAIN = HEI2015_UNHEALTHY(REFINEDGRAIN_SERV, HEI2015_MIN_REFINEDGRAIN_SERV, HEI2015_MAX_REFINEDGRAIN_SERV),
            HEI2015_SODIUM = HEI2015_UNHEALTHY(SODIUM_SERV, HEI2015_MIN_SODIUM_SERV, HEI2015_MAX_SODIUM_SERV),
            HEI2015_ADDEDSUGAR = HEI2015_UNHEALTHY(ADDEDSUGAR_SERV, HEI2015_MIN_ADDEDSUGAR_SERV, HEI2015_MAX_ADDEDSUGAR_SERV),
            HEI2015_SATFAT = HEI2015_UNHEALTHY(SATFAT_SERV, HEI2015_MIN_SATFAT_SERV, HEI2015_MAX_SATFAT_SERV),
            HEI2015_ALL = HEI2015_TOTALFRT + HEI2015_FRT + HEI2015_VEG + HEI2015_GREENNBEAN +
                HEI2015_TOTALPRO + HEI2015_SEAPLANTPRO + HEI2015_WHOLEGRAIN + HEI2015_DAIRY +
                HEI2015_FATTYACID + HEI2015_REFINEDGRAIN + HEI2015_SODIUM + HEI2015_ADDEDSUGAR +
                HEI2015_SATFAT
        )

    for (i in 1:length(COHORT$TOTALKCAL)) {
        if (COHORT$TOTALKCAL[i] == 0) {
            COHORT$HEI2015_TOTALFRT[i] = 0
            COHORT$HEI2015_FRT[i] = 0
            COHORT$HEI2015_VEG[i] = 0
            COHORT$HEI2015_GREENNBEAN[i] = 0
            COHORT$HEI2015_TOTALPRO[i] = 0
            COHORT$HEI2015_SEAPLANTPRO[i] = 0
            COHORT$HEI2015_WHOLEGRAIN[i] = 0
            COHORT$HEI2015_DAIRY[i] = 0
            COHORT$HEI2015_FATTYACID[i] = 0
            COHORT$HEI2015_REFINEDGRAIN[i] = 0
            COHORT$HEI2015_ADDEDSUGAR[i] = 0
            COHORT$HEI2015_ALL[i] = 0
        }
    }

    COHORT %>%
        dplyr::select(
            `Respondent ID`, TOTALKCAL, HEI2015_ALL, HEI2015_TOTALFRT, HEI2015_FRT, HEI2015_VEG, HEI2015_GREENNBEAN,
            HEI2015_TOTALPRO, HEI2015_SEAPLANTPRO, HEI2015_WHOLEGRAIN, HEI2015_DAIRY,
            HEI2015_FATTYACID, HEI2015_REFINEDGRAIN, HEI2015_SODIUM, HEI2015_ADDEDSUGAR,
            HEI2015_SATFAT
        )
}
