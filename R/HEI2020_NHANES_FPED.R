#' HEI2020_NHANES_FPED
#'
#' Calculate the HEI2020 for the NHANES_FPED data (after 2005) within 1 step for day 1, day 2, or day 1 and 2 combined, including HEI2020 and HEI-Toddlers-2020
#' @import dplyr
#' @import readr
#' @import haven
#' @param FPED_PATH The file path for the FPED data. The file name should be like: fped_dr1tot_1112.sas7bdat
#' @param NUTRIENT_PATH The file path for the NUTRIENT data. The file name should be like: DR1TOT_J.XPT
#' @param DEMO_PATH The file path for the DEMOGRAPHIC data. The file name should be like: DEMO_J.XPT
#' @param FPED_PATH2 The file path for the FPED2 data. The file name should be like: fped_DR2tot_1112.sas7bdat
#' @param NUTRIENT_PATH2 The file path for the NUTRIENT2 data. The file name should be like: DR2TOT_J.XPT
#' @return The HEI2020 and its component scores and serving sizes
#' @examples
#' data("NHANES_20172018")
#' HEI2020_NHANES_FPED(FPED_PATH = NHANES_20172018$FPED, NUTRIENT_PATH = NHANES_20172018$NUTRIENT, DEMO_PATH = NHANES_20172018$DEMO, FPED_PATH2 = NHANES_20172018$FPED2, NUTRIENT_PATH2 = NHANES_20172018$NUTRIENT2)
#' @export

HEI2020_NHANES_FPED = function(FPED_PATH = NULL, NUTRIENT_PATH = NULL, DEMO_PATH, FPED_PATH2 = NULL, NUTRIENT_PATH2 = NULL) {
    ## Create variables needed for HEI2020 calculation
    HEI2020_MIN = 0
    HEI2020_MAX1 = 5
    HEI2020_MAX2 = 10

    HEI2020_MIN_TOTALFRT_SERV = 0
    HEI2020_MAX_TOTALFRT_SERV = 0.8
    HEI2020_MIN_FRT_SERV = 0
    HEI2020_MAX_FRT_SERV = 0.4
    HEI2020_MIN_VEG_SERV = 0
    HEI2020_MAX_VEG_SERV = 1.1
    HEI2020_MIN_GREENNBEAN_SERV = 0
    HEI2020_MAX_GREENNBEAN_SERV = 0.2
    HEI2020_MIN_TOTALPRO_SERV = 0
    HEI2020_MAX_TOTALPRO_SERV = 2.5
    HEI2020_MIN_SEAPLANTPRO_SERV = 0
    HEI2020_MAX_SEAPLANTPRO_SERV = 0.8
    HEI2020_MIN_WHOLEGRAIN_SERV = 0
    HEI2020_MAX_WHOLEGRAIN_SERV = 1.5
    HEI2020_MIN_DAIRY_SERV = 0
    HEI2020_MAX_DAIRY_SERV = 1.3
    HEI2020_MIN_FATTYACID_SERV = 1.2
    HEI2020_MAX_FATTYACID_SERV = 2.5

    HEI2020_MIN_REFINEDGRAIN_SERV = 4.3
    HEI2020_MAX_REFINEDGRAIN_SERV = 1.8
    HEI2020_MIN_SODIUM_SERV = 2.0
    HEI2020_MAX_SODIUM_SERV = 1.1
    HEI2020_MIN_ADDEDSUGAR_SERV = 26
    HEI2020_MAX_ADDEDSUGAR_SERV = 6.5
    HEI2020_MIN_SATFAT_SERV = 16
    HEI2020_MAX_SATFAT_SERV = 8

    ## Create variables needed for HEI-Toddlers-2020 calculation
    HEI2020_TODDLERS_MIN_TOTALFRT_SERV = 0
    HEI2020_TODDLERS_MAX_TOTALFRT_SERV = 0.7
    HEI2020_TODDLERS_MIN_FRT_SERV = 0
    HEI2020_TODDLERS_MAX_FRT_SERV = 0.3
    HEI2020_TODDLERS_MIN_VEG_SERV = 0
    HEI2020_TODDLERS_MAX_VEG_SERV = 0.9
    HEI2020_TODDLERS_MIN_GREENNBEAN_SERV = 0
    HEI2020_TODDLERS_MAX_GREENNBEAN_SERV = 0.1
    HEI2020_TODDLERS_MIN_TOTALPRO_SERV = 0
    HEI2020_TODDLERS_MAX_TOTALPRO_SERV = 2.0
    HEI2020_TODDLERS_MIN_SEAPLANTPRO_SERV = 0
    HEI2020_TODDLERS_MAX_SEAPLANTPRO_SERV = 0.5
    HEI2020_TODDLERS_MIN_WHOLEGRAIN_SERV = 0
    HEI2020_TODDLERS_MAX_WHOLEGRAIN_SERV = 1.5
    HEI2020_TODDLERS_MIN_DAIRY_SERV = 0
    HEI2020_TODDLERS_MAX_DAIRY_SERV = 2.0
    HEI2020_TODDLERS_MIN_FATTYACID_SERV = 0.9
    HEI2020_TODDLERS_MAX_FATTYACID_SERV = 1.5

    HEI2020_TODDLERS_MIN_REFINEDGRAIN_SERV = 3.4
    HEI2020_TODDLERS_MAX_REFINEDGRAIN_SERV = 1.5
    HEI2020_TODDLERS_MIN_SODIUM_SERV = 1.7
    HEI2020_TODDLERS_MAX_SODIUM_SERV = 1.1
    HEI2020_TODDLERS_MIN_ADDEDSUGAR_SERV = 13.8
    HEI2020_TODDLERS_MAX_ADDEDSUGAR_SERV = 0
    HEI2020_TODDLERS_MIN_SATFAT_SERV = 18.2
    HEI2020_TODDLERS_MAX_SATFAT_SERV = 12.2

    # create functions for HEI2020 calculation
    HEI2020_HEALTHY1 = function(actual, min, max) {
        case_when(
            actual >= max ~ HEI2020_MAX1,
            actual <= min ~ HEI2020_MIN,
            TRUE ~ HEI2020_MIN + (actual - min) * HEI2020_MAX1 / (max - min)
        )
    }

    HEI2020_HEALTHY2 = function(actual, min, max) {
        case_when(
            actual >= max ~ HEI2020_MAX2,
            actual <= min ~ HEI2020_MIN,
            TRUE ~ HEI2020_MIN + (actual - min) * HEI2020_MAX2 / (max - min)
        )
    }

    HEI2020_UNHEALTHY = function(actual, min, max) {
        case_when(
            actual >= min ~ HEI2020_MIN,
            actual <= max ~ HEI2020_MAX2,
            TRUE ~ HEI2020_MIN + (actual - min) * HEI2020_MAX2 / (max - min)
        )
    }

    # stop if the input data is not provided for any day
    if (is.null(FPED_PATH) & is.null(NUTRIENT_PATH) & is.null(FPED_PATH2) & is.null(NUTRIENT_PATH2)) {
        stop("Please provide the file path for the FPED and NUTRIENT data, day 1 or day 2 or day 1 and day 2.")
    }

    if (!is.null(FPED_PATH) & !is.null(NUTRIENT_PATH)) {
        # start with the first day data calculation
        if (is.character(FPED_PATH) == TRUE) {
            FPED = read_sas(FPED_PATH)
        } else {
            FPED = FPED_PATH
        }

        if (is.character(NUTRIENT_PATH) == TRUE) {
            NUTRIENT = read_xpt(NUTRIENT_PATH)
        } else {
            NUTRIENT = NUTRIENT_PATH
        }

        if (is.character(DEMO_PATH) == TRUE) {
            DEMO = read_xpt(DEMO_PATH)
        } else {
            DEMO = DEMO_PATH
        }

        # stop if the user provides the individual-level data
        if ("DR1ILINE" %in% colnames(FPED) | "DR1ILINE" %in% colnames(NUTRIENT)) {
            stop("Please use the population-level data for the first day data. The file name should contain: TOT")
        }

        NUTRIENT = NUTRIENT %>%
            filter(DR1DRSTZ == 1) %>%
            dplyr::select(SEQN, DR1TKCAL, DR1TSFAT, DR1TALCO, DR1TSODI, DR1DRSTZ, DR1TMFAT, DR1TPFAT) %>%
            arrange(SEQN)


        DEMO = DEMO %>%
            # only calculate HEI2020 for toddlers 1 year and older and adults
            filter(RIDAGEYR >= 1) %>%
            dplyr::select(SEQN, RIDAGEYR, RIAGENDR, SDDSRVYR, SDMVPSU, SDMVSTRA) %>%
            arrange(SEQN)

        FPED = FPED %>%
            arrange(SEQN) %>%
            # only calculate HEI2020 for toddlers 1 year and older and adults
            filter(RIDAGEYR >= 1)

        # merge the NUTRIENT, DEMO, and FPED data
        COHORT = NUTRIENT %>%
            inner_join(DEMO, by = c("SEQN" = "SEQN")) %>%
            inner_join(FPED, by = c("SEQN" = "SEQN"))

        # calculate the HEI2020 food group serving size / 1000 kcal
        COHORT = COHORT %>%
            dplyr::mutate(
                RIDAGEYR = RIDAGEYR.x,
                TOTALFRT_SERV = DR1T_F_TOTAL / (DR1TKCAL / 1000),
                FRT_SERV = (DR1T_F_CITMLB + DR1T_F_OTHER) / (DR1TKCAL / 1000),
                VEG_SERV = (DR1T_V_TOTAL + DR1T_V_LEGUMES) / (DR1TKCAL / 1000),
                GREENNBEAN_SERV = (DR1T_V_DRKGR + DR1T_V_LEGUMES) / (DR1TKCAL / 1000),
                TOTALPRO_SERV = (DR1T_PF_MPS_TOTAL + DR1T_PF_EGGS + DR1T_PF_NUTSDS + DR1T_PF_SOY + DR1T_PF_LEGUMES) / (DR1TKCAL / 1000),
                SEAPLANTPRO_SERV = (DR1T_PF_SEAFD_HI + DR1T_PF_SEAFD_LOW + DR1T_PF_NUTSDS + DR1T_PF_SOY + DR1T_PF_LEGUMES) / (DR1TKCAL / 1000),
                WHOLEGRAIN_SERV = DR1T_G_WHOLE / (DR1TKCAL / 1000),
                DAIRY_SERV = DR1T_D_TOTAL / (DR1TKCAL / 1000),
                FATTYACID_SERV = case_when(
                    DR1TSFAT == 0 ~ 0,
                    TRUE ~ (DR1TMFAT + DR1TPFAT) / DR1TSFAT
                ),
                REFINEDGRAIN_SERV = DR1T_G_REFINED / (DR1TKCAL / 1000),
                SODIUM_SERV = (DR1TSODI / 1000) / (DR1TKCAL / 1000),
                ADDEDSUGAR_SERV = ((DR1T_ADD_SUGARS * 4 * 4) / DR1TKCAL) * 100,
                SATFAT_SERV = ((DR1TSFAT * 9) / DR1TKCAL) * 100,
                TOTALKCAL = DR1TKCAL
            )

        # calculate the HEI2020 component scores
        COHORT = COHORT %>%
            dplyr::mutate(
                HEI2020_TOTALFRT = case_when(
                    # only calculate HEI2020 for children 2 years and older and adults
                    RIDAGEYR >= 2 ~ HEI2020_HEALTHY1(TOTALFRT_SERV, HEI2020_MIN_TOTALFRT_SERV, HEI2020_MAX_TOTALFRT_SERV),
                    # for children under 2 years old, use the toddler's standard
                    RIDAGEYR < 2 & RIDAGEYR >= 1 & RIDAGEYR >= 1 ~ HEI2020_HEALTHY1(TOTALFRT_SERV, HEI2020_TODDLERS_MIN_TOTALFRT_SERV, HEI2020_TODDLERS_MAX_TOTALFRT_SERV),
                ),
                HEI2020_FRT = case_when(
                    RIDAGEYR >= 2 ~ HEI2020_HEALTHY1(FRT_SERV, HEI2020_MIN_FRT_SERV, HEI2020_MAX_FRT_SERV),
                    RIDAGEYR < 2 & RIDAGEYR >= 1 ~ HEI2020_HEALTHY1(FRT_SERV, HEI2020_TODDLERS_MIN_FRT_SERV, HEI2020_TODDLERS_MAX_FRT_SERV),
                ),
                HEI2020_VEG = case_when(
                    RIDAGEYR >= 2 ~ HEI2020_HEALTHY1(VEG_SERV, HEI2020_MIN_VEG_SERV, HEI2020_MAX_VEG_SERV),
                    RIDAGEYR < 2 & RIDAGEYR >= 1 ~ HEI2020_HEALTHY1(VEG_SERV, HEI2020_TODDLERS_MIN_VEG_SERV, HEI2020_TODDLERS_MAX_VEG_SERV),
                ),
                HEI2020_GREENNBEAN = case_when(
                    RIDAGEYR >= 2 ~ HEI2020_HEALTHY1(GREENNBEAN_SERV, HEI2020_MIN_GREENNBEAN_SERV, HEI2020_MAX_GREENNBEAN_SERV),
                    RIDAGEYR < 2 & RIDAGEYR >= 1 ~ HEI2020_HEALTHY1(GREENNBEAN_SERV, HEI2020_TODDLERS_MIN_GREENNBEAN_SERV, HEI2020_TODDLERS_MAX_GREENNBEAN_SERV),
                ),
                HEI2020_TOTALPRO = case_when(
                    RIDAGEYR >= 2 ~ HEI2020_HEALTHY1(TOTALPRO_SERV, HEI2020_MIN_TOTALPRO_SERV, HEI2020_MAX_TOTALPRO_SERV),
                    RIDAGEYR < 2 & RIDAGEYR >= 1 ~ HEI2020_HEALTHY1(TOTALPRO_SERV, HEI2020_TODDLERS_MIN_TOTALPRO_SERV, HEI2020_TODDLERS_MAX_TOTALPRO_SERV),
                ),
                HEI2020_SEAPLANTPRO = case_when(
                    RIDAGEYR >= 2 ~ HEI2020_HEALTHY1(SEAPLANTPRO_SERV, HEI2020_MIN_SEAPLANTPRO_SERV, HEI2020_MAX_SEAPLANTPRO_SERV),
                    RIDAGEYR < 2 & RIDAGEYR >= 1 ~ HEI2020_HEALTHY1(SEAPLANTPRO_SERV, HEI2020_TODDLERS_MIN_SEAPLANTPRO_SERV, HEI2020_TODDLERS_MAX_SEAPLANTPRO_SERV),
                ),
                HEI2020_WHOLEGRAIN = case_when(
                    RIDAGEYR >= 2 ~ HEI2020_HEALTHY2(WHOLEGRAIN_SERV, HEI2020_MIN_WHOLEGRAIN_SERV, HEI2020_MAX_WHOLEGRAIN_SERV),
                    RIDAGEYR < 2 & RIDAGEYR >= 1 ~ HEI2020_HEALTHY2(WHOLEGRAIN_SERV, HEI2020_TODDLERS_MIN_WHOLEGRAIN_SERV, HEI2020_TODDLERS_MAX_WHOLEGRAIN_SERV),
                ),
                HEI2020_DAIRY = case_when(
                    RIDAGEYR >= 2 ~ HEI2020_HEALTHY2(DAIRY_SERV, HEI2020_MIN_DAIRY_SERV, HEI2020_MAX_DAIRY_SERV),
                    RIDAGEYR < 2 & RIDAGEYR >= 1 ~ HEI2020_HEALTHY2(DAIRY_SERV, HEI2020_TODDLERS_MIN_DAIRY_SERV, HEI2020_TODDLERS_MAX_DAIRY_SERV),
                ),
                HEI2020_FATTYACID = case_when(
                    RIDAGEYR >= 2 ~ HEI2020_HEALTHY2(FATTYACID_SERV, HEI2020_MIN_FATTYACID_SERV, HEI2020_MAX_FATTYACID_SERV),
                    RIDAGEYR < 2 & RIDAGEYR >= 1 ~ HEI2020_HEALTHY2(FATTYACID_SERV, HEI2020_TODDLERS_MIN_FATTYACID_SERV, HEI2020_TODDLERS_MAX_FATTYACID_SERV),
                ),
                HEI2020_REFINEDGRAIN = case_when(
                    RIDAGEYR >= 2 ~ HEI2020_UNHEALTHY(REFINEDGRAIN_SERV, HEI2020_MIN_REFINEDGRAIN_SERV, HEI2020_MAX_REFINEDGRAIN_SERV),
                    RIDAGEYR < 2 & RIDAGEYR >= 1 ~ HEI2020_UNHEALTHY(REFINEDGRAIN_SERV, HEI2020_TODDLERS_MIN_REFINEDGRAIN_SERV, HEI2020_TODDLERS_MAX_REFINEDGRAIN_SERV),
                ),
                HEI2020_SODIUM = case_when(
                    RIDAGEYR >= 2 ~ HEI2020_UNHEALTHY(SODIUM_SERV, HEI2020_MIN_SODIUM_SERV, HEI2020_MAX_SODIUM_SERV),
                    RIDAGEYR < 2 & RIDAGEYR >= 1 ~ HEI2020_UNHEALTHY(SODIUM_SERV, HEI2020_TODDLERS_MIN_SODIUM_SERV, HEI2020_TODDLERS_MAX_SODIUM_SERV),
                ),
                HEI2020_ADDEDSUGAR = case_when(
                    RIDAGEYR >= 2 ~ HEI2020_UNHEALTHY(ADDEDSUGAR_SERV, HEI2020_MIN_ADDEDSUGAR_SERV, HEI2020_MAX_ADDEDSUGAR_SERV),
                    RIDAGEYR < 2 & RIDAGEYR >= 1 ~ HEI2020_UNHEALTHY(ADDEDSUGAR_SERV, HEI2020_TODDLERS_MIN_ADDEDSUGAR_SERV, HEI2020_TODDLERS_MAX_ADDEDSUGAR_SERV),
                ),
                HEI2020_SATFAT = case_when(
                    RIDAGEYR >= 2 ~ HEI2020_UNHEALTHY(SATFAT_SERV, HEI2020_MIN_SATFAT_SERV, HEI2020_MAX_SATFAT_SERV),
                    RIDAGEYR < 2 & RIDAGEYR >= 1 ~ HEI2020_UNHEALTHY(SATFAT_SERV, HEI2020_TODDLERS_MIN_SATFAT_SERV, HEI2020_TODDLERS_MAX_SATFAT_SERV),
                ),
                HEI2020_ALL = HEI2020_TOTALFRT + HEI2020_FRT + HEI2020_VEG + HEI2020_GREENNBEAN +
                    HEI2020_TOTALPRO + HEI2020_SEAPLANTPRO + HEI2020_WHOLEGRAIN + HEI2020_DAIRY +
                    HEI2020_FATTYACID + HEI2020_REFINEDGRAIN + HEI2020_SODIUM + HEI2020_ADDEDSUGAR +
                    HEI2020_SATFAT
            )

        for (i in 1:length(COHORT$TOTALKCAL)) {
            if (COHORT$TOTALKCAL[i] == 0) {
                COHORT$HEI2020_TOTALFRT[i] = 0
                COHORT$HEI2020_FRT[i] = 0
                COHORT$HEI2020_VEG[i] = 0
                COHORT$HEI2020_GREENNBEAN[i] = 0
                COHORT$HEI2020_TOTALPRO[i] = 0
                COHORT$HEI2020_SEAPLANTPRO[i] = 0
                COHORT$HEI2020_WHOLEGRAIN[i] = 0
                COHORT$HEI2020_DAIRY[i] = 0
                COHORT$HEI2020_FATTYACID[i] = 0
                COHORT$HEI2020_REFINEDGRAIN[i] = 0
                COHORT$HEI2020_ADDEDSUGAR[i] = 0
                COHORT$HEI2020_ALL[i] = 0
            }
        }

        # select the columns that are needed for the HEI2020
        COHORT = COHORT %>%
            dplyr::select(
                SEQN, RIDAGEYR, HEI2020_ALL, HEI2020_TOTALFRT, HEI2020_FRT, HEI2020_VEG, HEI2020_GREENNBEAN,
                HEI2020_TOTALPRO, HEI2020_SEAPLANTPRO, HEI2020_WHOLEGRAIN, HEI2020_DAIRY,
                HEI2020_FATTYACID, HEI2020_REFINEDGRAIN, HEI2020_SODIUM, HEI2020_ADDEDSUGAR,
                HEI2020_SATFAT
            )
    }

    # the second day data calculation if the user provides the second day data for FPED_PATH2 and NUTRIENT_PATH2
    if (!is.null(FPED_PATH2) & !is.null(NUTRIENT_PATH2)) {
        if (is.character(FPED_PATH2) == TRUE) {
            FPED2 = read_sas(FPED_PATH2)
        } else {
            FPED2 = FPED_PATH2
        }

        if (is.character(NUTRIENT_PATH2) == TRUE) {
            NUTRIENT2 = read_xpt(NUTRIENT_PATH2)
        } else {
            NUTRIENT2 = NUTRIENT_PATH2
        }

        if (is.character(DEMO_PATH) == TRUE) {
            DEMO = read_xpt(DEMO_PATH)
        } else {
            DEMO = DEMO_PATH
        }

        if ("DR2ILINE" %in% colnames(FPED2) | "DR2ILINE" %in% colnames(NUTRIENT2)) {
            stop("Please use the population-level data for the second day data. The file name should contain: TOT")
        }

        NUTRIENT2 = NUTRIENT2 %>%
            filter(DR2DRSTZ == 1) %>%
            dplyr::select(SEQN, DR2TKCAL, DR2TSFAT, DR2TALCO, DR2TSODI, DR2DRSTZ, DR2TMFAT, DR2TPFAT) %>%
            arrange(SEQN)


        DEMO = DEMO %>%
            # only select the people who are 1 years old or older
            filter(RIDAGEYR >= 1) %>%
            dplyr::select(SEQN, RIDAGEYR, RIAGENDR, SDDSRVYR, SDMVPSU, SDMVSTRA) %>%
            arrange(SEQN)

        FPED2 = FPED2 %>%
            arrange(SEQN) %>%
            # only select the people who are 1 years old or older
            filter(RIDAGEYR >= 1)

        COHORT2 = NUTRIENT2 %>%
            inner_join(DEMO, by = c("SEQN" = "SEQN")) %>%
            inner_join(FPED2, by = c("SEQN" = "SEQN"))

        COHORT2 = COHORT2 %>%
            dplyr::mutate(
                RIDAGEYR = RIDAGEYR.x,
                TOTALFRT_SERV = DR2T_F_TOTAL / (DR2TKCAL / 1000),
                FRT_SERV = (DR2T_F_CITMLB + DR2T_F_OTHER) / (DR2TKCAL / 1000),
                VEG_SERV = (DR2T_V_TOTAL + DR2T_V_LEGUMES) / (DR2TKCAL / 1000),
                GREENNBEAN_SERV = (DR2T_V_DRKGR + DR2T_V_LEGUMES) / (DR2TKCAL / 1000),
                TOTALPRO_SERV = (DR2T_PF_MPS_TOTAL + DR2T_PF_EGGS + DR2T_PF_NUTSDS + DR2T_PF_SOY + DR2T_PF_LEGUMES) / (DR2TKCAL / 1000),
                SEAPLANTPRO_SERV = (DR2T_PF_SEAFD_HI + DR2T_PF_SEAFD_LOW + DR2T_PF_NUTSDS + DR2T_PF_SOY + DR2T_PF_LEGUMES) / (DR2TKCAL / 1000),
                WHOLEGRAIN_SERV = DR2T_G_WHOLE / (DR2TKCAL / 1000),
                DAIRY_SERV = DR2T_D_TOTAL / (DR2TKCAL / 1000),
                FATTYACID_SERV = case_when(
                    DR2TSFAT == 0 ~ 0,
                    TRUE ~ (DR2TMFAT + DR2TPFAT) / DR2TSFAT
                ),
                REFINEDGRAIN_SERV = DR2T_G_REFINED / (DR2TKCAL / 1000),
                SODIUM_SERV = (DR2TSODI / 1000) / (DR2TKCAL / 1000),
                ADDEDSUGAR_SERV = ((DR2T_ADD_SUGARS * 4 * 4) / DR2TKCAL) * 100,
                SATFAT_SERV = ((DR2TSFAT * 9) / DR2TKCAL) * 100,
                TOTALKCAL = DR2TKCAL
            )

        COHORT2 = COHORT2 %>%
            dplyr::mutate(
                HEI2020_TOTALFRT = case_when(
                    # only calculate HEI2020 for children 2 years and older and adults
                    RIDAGEYR >= 2 ~ HEI2020_HEALTHY1(TOTALFRT_SERV, HEI2020_MIN_TOTALFRT_SERV, HEI2020_MAX_TOTALFRT_SERV),
                    # for children under 2 years old, use the toddler's standard
                    RIDAGEYR < 2 & RIDAGEYR >= 1 ~ HEI2020_HEALTHY1(TOTALFRT_SERV, HEI2020_TODDLERS_MIN_TOTALFRT_SERV, HEI2020_TODDLERS_MAX_TOTALFRT_SERV),
                ),
                HEI2020_FRT = case_when(
                    RIDAGEYR >= 2 ~ HEI2020_HEALTHY1(FRT_SERV, HEI2020_MIN_FRT_SERV, HEI2020_MAX_FRT_SERV),
                    RIDAGEYR < 2 & RIDAGEYR >= 1 ~ HEI2020_HEALTHY1(FRT_SERV, HEI2020_TODDLERS_MIN_FRT_SERV, HEI2020_TODDLERS_MAX_FRT_SERV),
                ),
                HEI2020_VEG = case_when(
                    RIDAGEYR >= 2 ~ HEI2020_HEALTHY1(VEG_SERV, HEI2020_MIN_VEG_SERV, HEI2020_MAX_VEG_SERV),
                    RIDAGEYR < 2 & RIDAGEYR >= 1 ~ HEI2020_HEALTHY1(VEG_SERV, HEI2020_TODDLERS_MIN_VEG_SERV, HEI2020_TODDLERS_MAX_VEG_SERV),
                ),
                HEI2020_GREENNBEAN = case_when(
                    RIDAGEYR >= 2 ~ HEI2020_HEALTHY1(GREENNBEAN_SERV, HEI2020_MIN_GREENNBEAN_SERV, HEI2020_MAX_GREENNBEAN_SERV),
                    RIDAGEYR < 2 & RIDAGEYR >= 1 ~ HEI2020_HEALTHY1(GREENNBEAN_SERV, HEI2020_TODDLERS_MIN_GREENNBEAN_SERV, HEI2020_TODDLERS_MAX_GREENNBEAN_SERV),
                ),
                HEI2020_TOTALPRO = case_when(
                    RIDAGEYR >= 2 ~ HEI2020_HEALTHY1(TOTALPRO_SERV, HEI2020_MIN_TOTALPRO_SERV, HEI2020_MAX_TOTALPRO_SERV),
                    RIDAGEYR < 2 & RIDAGEYR >= 1 ~ HEI2020_HEALTHY1(TOTALPRO_SERV, HEI2020_TODDLERS_MIN_TOTALPRO_SERV, HEI2020_TODDLERS_MAX_TOTALPRO_SERV),
                ),
                HEI2020_SEAPLANTPRO = case_when(
                    RIDAGEYR >= 2 ~ HEI2020_HEALTHY1(SEAPLANTPRO_SERV, HEI2020_MIN_SEAPLANTPRO_SERV, HEI2020_MAX_SEAPLANTPRO_SERV),
                    RIDAGEYR < 2 & RIDAGEYR >= 1 ~ HEI2020_HEALTHY1(SEAPLANTPRO_SERV, HEI2020_TODDLERS_MIN_SEAPLANTPRO_SERV, HEI2020_TODDLERS_MAX_SEAPLANTPRO_SERV),
                ),
                HEI2020_WHOLEGRAIN = case_when(
                    RIDAGEYR >= 2 ~ HEI2020_HEALTHY2(WHOLEGRAIN_SERV, HEI2020_MIN_WHOLEGRAIN_SERV, HEI2020_MAX_WHOLEGRAIN_SERV),
                    RIDAGEYR < 2 & RIDAGEYR >= 1 ~ HEI2020_HEALTHY2(WHOLEGRAIN_SERV, HEI2020_TODDLERS_MIN_WHOLEGRAIN_SERV, HEI2020_TODDLERS_MAX_WHOLEGRAIN_SERV),
                ),
                HEI2020_DAIRY = case_when(
                    RIDAGEYR >= 2 ~ HEI2020_HEALTHY2(DAIRY_SERV, HEI2020_MIN_DAIRY_SERV, HEI2020_MAX_DAIRY_SERV),
                    RIDAGEYR < 2 & RIDAGEYR >= 1 ~ HEI2020_HEALTHY2(DAIRY_SERV, HEI2020_TODDLERS_MIN_DAIRY_SERV, HEI2020_TODDLERS_MAX_DAIRY_SERV),
                ),
                HEI2020_FATTYACID = case_when(
                    RIDAGEYR >= 2 ~ HEI2020_HEALTHY2(FATTYACID_SERV, HEI2020_MIN_FATTYACID_SERV, HEI2020_MAX_FATTYACID_SERV),
                    RIDAGEYR < 2 & RIDAGEYR >= 1 ~ HEI2020_HEALTHY2(FATTYACID_SERV, HEI2020_TODDLERS_MIN_FATTYACID_SERV, HEI2020_TODDLERS_MAX_FATTYACID_SERV),
                ),
                HEI2020_REFINEDGRAIN = case_when(
                    RIDAGEYR >= 2 ~ HEI2020_UNHEALTHY(REFINEDGRAIN_SERV, HEI2020_MIN_REFINEDGRAIN_SERV, HEI2020_MAX_REFINEDGRAIN_SERV),
                    RIDAGEYR < 2 & RIDAGEYR >= 1 ~ HEI2020_UNHEALTHY(REFINEDGRAIN_SERV, HEI2020_TODDLERS_MIN_REFINEDGRAIN_SERV, HEI2020_TODDLERS_MAX_REFINEDGRAIN_SERV),
                ),
                HEI2020_SODIUM = case_when(
                    RIDAGEYR >= 2 ~ HEI2020_UNHEALTHY(SODIUM_SERV, HEI2020_MIN_SODIUM_SERV, HEI2020_MAX_SODIUM_SERV),
                    RIDAGEYR < 2 & RIDAGEYR >= 1 ~ HEI2020_UNHEALTHY(SODIUM_SERV, HEI2020_TODDLERS_MIN_SODIUM_SERV, HEI2020_TODDLERS_MAX_SODIUM_SERV),
                ),
                HEI2020_ADDEDSUGAR = case_when(
                    RIDAGEYR >= 2 ~ HEI2020_UNHEALTHY(ADDEDSUGAR_SERV, HEI2020_MIN_ADDEDSUGAR_SERV, HEI2020_MAX_ADDEDSUGAR_SERV),
                    RIDAGEYR < 2 & RIDAGEYR >= 1 ~ HEI2020_UNHEALTHY(ADDEDSUGAR_SERV, HEI2020_TODDLERS_MIN_ADDEDSUGAR_SERV, HEI2020_TODDLERS_MAX_ADDEDSUGAR_SERV),
                ),
                HEI2020_SATFAT = case_when(
                    RIDAGEYR >= 2 ~ HEI2020_UNHEALTHY(SATFAT_SERV, HEI2020_MIN_SATFAT_SERV, HEI2020_MAX_SATFAT_SERV),
                    RIDAGEYR < 2 & RIDAGEYR >= 1 ~ HEI2020_UNHEALTHY(SATFAT_SERV, HEI2020_TODDLERS_MIN_SATFAT_SERV, HEI2020_TODDLERS_MAX_SATFAT_SERV),
                ),
                HEI2020_ALL = HEI2020_TOTALFRT + HEI2020_FRT + HEI2020_VEG + HEI2020_GREENNBEAN +
                    HEI2020_TOTALPRO + HEI2020_SEAPLANTPRO + HEI2020_WHOLEGRAIN + HEI2020_DAIRY +
                    HEI2020_FATTYACID + HEI2020_REFINEDGRAIN + HEI2020_SODIUM + HEI2020_ADDEDSUGAR +
                    HEI2020_SATFAT
            )

        for (i in 1:length(COHORT2$TOTALKCAL)) {
            if (COHORT2$TOTALKCAL[i] == 0) {
                COHORT2$HEI2020_TOTALFRT[i] = 0
                COHORT2$HEI2020_FRT[i] = 0
                COHORT2$HEI2020_VEG[i] = 0
                COHORT2$HEI2020_GREENNBEAN[i] = 0
                COHORT2$HEI2020_TOTALPRO[i] = 0
                COHORT2$HEI2020_SEAPLANTPRO[i] = 0
                COHORT2$HEI2020_WHOLEGRAIN[i] = 0
                COHORT2$HEI2020_DAIRY[i] = 0
                COHORT2$HEI2020_FATTYACID[i] = 0
                COHORT2$HEI2020_REFINEDGRAIN[i] = 0
                COHORT2$HEI2020_ADDEDSUGAR[i] = 0
                COHORT2$HEI2020_ALL[i] = 0
            }
        }
        COHORT2 = COHORT2 %>%
            dplyr::select(
                SEQN, RIDAGEYR, HEI2020_ALL, HEI2020_TOTALFRT, HEI2020_FRT, HEI2020_VEG, HEI2020_GREENNBEAN,
                HEI2020_TOTALPRO, HEI2020_SEAPLANTPRO, HEI2020_WHOLEGRAIN, HEI2020_DAIRY,
                HEI2020_FATTYACID, HEI2020_REFINEDGRAIN, HEI2020_SODIUM, HEI2020_ADDEDSUGAR,
                HEI2020_SATFAT
            )
    }

    if (!is.null(FPED_PATH) & !is.null(NUTRIENT_PATH) & is.null(FPED_PATH2) & is.null(NUTRIENT_PATH2)) {
        return(COHORT)
    }

    if (is.null(FPED_PATH) & is.null(NUTRIENT_PATH) & !is.null(FPED_PATH2) & !is.null(NUTRIENT_PATH2)) {
        return(COHORT2)
    }

    # merge two days data if they both exist
    if (!is.null(FPED_PATH) & !is.null(NUTRIENT_PATH) & !is.null(FPED_PATH2) & !is.null(NUTRIENT_PATH2)) {
        COHORT12 = inner_join(COHORT, COHORT2, by = "SEQN") %>%
            mutate(
                RIDAGEYR = RIDAGEYR.x,
                HEI2020_ALL = (HEI2020_ALL.x + HEI2020_ALL.y) / 2,
                HEI2020_TOTALFRT = (HEI2020_TOTALFRT.x + HEI2020_TOTALFRT.y) / 2,
                HEI2020_FRT = (HEI2020_FRT.x + HEI2020_FRT.y) / 2,
                HEI2020_VEG = (HEI2020_VEG.x + HEI2020_VEG.y) / 2,
                HEI2020_GREENNBEAN = (HEI2020_GREENNBEAN.x + HEI2020_GREENNBEAN.y) / 2,
                HEI2020_TOTALPRO = (HEI2020_TOTALPRO.x + HEI2020_TOTALPRO.y) / 2,
                HEI2020_SEAPLANTPRO = (HEI2020_SEAPLANTPRO.x + HEI2020_SEAPLANTPRO.y) / 2,
                HEI2020_WHOLEGRAIN = (HEI2020_WHOLEGRAIN.x + HEI2020_WHOLEGRAIN.y) / 2,
                HEI2020_DAIRY = (HEI2020_DAIRY.x + HEI2020_DAIRY.y) / 2,
                HEI2020_FATTYACID = (HEI2020_FATTYACID.x + HEI2020_FATTYACID.y) / 2,
                HEI2020_REFINEDGRAIN = (HEI2020_REFINEDGRAIN.x + HEI2020_REFINEDGRAIN.y) / 2,
                HEI2020_SODIUM = (HEI2020_SODIUM.x + HEI2020_SODIUM.y) / 2,
                HEI2020_ADDEDSUGAR = (HEI2020_ADDEDSUGAR.x + HEI2020_ADDEDSUGAR.y) / 2,
                HEI2020_SATFAT = (HEI2020_SATFAT.x + HEI2020_SATFAT.y) / 2
            ) %>%
            dplyr::select(
                SEQN, RIDAGEYR, HEI2020_ALL, HEI2020_TOTALFRT, HEI2020_FRT, HEI2020_VEG, HEI2020_GREENNBEAN,
                HEI2020_TOTALPRO, HEI2020_SEAPLANTPRO, HEI2020_WHOLEGRAIN, HEI2020_DAIRY,
                HEI2020_FATTYACID, HEI2020_REFINEDGRAIN, HEI2020_SODIUM, HEI2020_ADDEDSUGAR,
                HEI2020_SATFAT
            )
        return(COHORT12)
    }
}
