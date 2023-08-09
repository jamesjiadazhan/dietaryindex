#' HEI2015_NHANES_FPED
#'
#' Calculate the HEI2015 for the NHANES_FPED data (after 2005) within 1 step for day 1, day 2, or day 1 and 2 combined
#' @import dplyr
#' @import readr
#' @import haven
#' @param FPED_PATH The file path for the FPED data. The file name should be like: fped_dr1tot_1112.sas7bdat
#' @param NUTRIENT_PATH The file path for the NUTRIENT data. The file name should be like: DR1TOT_J.XPT
#' @param DEMO_PATH The file path for the DEMOGRAPHIC data. The file name should be like: DEMO_J.XPT
#' @param FPED_PATH2 The file path for the FPED2 data. The file name should be like: fped_DR2tot_1112.sas7bdat
#' @param NUTRIENT_PATH2 The file path for the NUTRIENT2 data. The file name should be like: DR2TOT_J.XPT
#' @return The HEI2015 and its component scores and serving sizes
#' @examples
#' data("NHANES_20172018")
#' HEI2015_NHANES_FPED(FPED_PATH = NHANES_20172018$FPED, NUTRIENT_PATH = NHANES_20172018$NUTRIENT, DEMO_PATH = NHANES_20172018$DEMO, FPED_PATH2 = NHANES_20172018$FPED2, NUTRIENT_PATH2 = NHANES_20172018$NUTRIENT2)
#' @export

HEI2015_NHANES_FPED = function(FPED_PATH = NULL, NUTRIENT_PATH = NULL, DEMO_PATH, FPED_PATH2 = NULL, NUTRIENT_PATH2 = NULL) {
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

        if ("DR1ILINE" %in% colnames(FPED) | "DR1ILINE" %in% colnames(NUTRIENT)) {
            stop("Please use the population-level data for the first day data. The file name should contain: TOT")
        }

        NUTRIENT = NUTRIENT %>%
            filter(DR1DRSTZ == 1) %>%
            dplyr::select(SEQN, DR1TKCAL, DR1TSFAT, DR1TALCO, DR1TSODI, DR1DRSTZ, DR1TMFAT, DR1TPFAT) %>%
            arrange(SEQN)


        DEMO = DEMO %>%
            filter(RIDAGEYR >= 2) %>%
            dplyr::select(SEQN, RIDAGEYR, RIAGENDR, SDDSRVYR, SDMVPSU, SDMVSTRA) %>%
            arrange(SEQN)

        FPED = FPED %>%
            arrange(SEQN)

        COHORT = NUTRIENT %>%
            inner_join(DEMO, by = c("SEQN" = "SEQN")) %>%
            left_join(FPED, by = c("SEQN" = "SEQN"))

        COHORT = COHORT %>%
            dplyr::mutate(
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

        COHORT = COHORT %>%
            dplyr::select(
                SEQN, HEI2015_ALL, HEI2015_TOTALFRT, HEI2015_FRT, HEI2015_VEG, HEI2015_GREENNBEAN,
                HEI2015_TOTALPRO, HEI2015_SEAPLANTPRO, HEI2015_WHOLEGRAIN, HEI2015_DAIRY,
                HEI2015_FATTYACID, HEI2015_REFINEDGRAIN, HEI2015_SODIUM, HEI2015_ADDEDSUGAR,
                HEI2015_SATFAT
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
            filter(RIDAGEYR >= 2) %>%
            dplyr::select(SEQN, RIDAGEYR, RIAGENDR, SDDSRVYR, SDMVPSU, SDMVSTRA) %>%
            arrange(SEQN)

        FPED2 = FPED2 %>%
            arrange(SEQN)

        COHORT2 = NUTRIENT2 %>%
            inner_join(DEMO, by = c("SEQN" = "SEQN")) %>%
            left_join(FPED2, by = c("SEQN" = "SEQN"))

        COHORT2 = COHORT2 %>%
            dplyr::mutate(
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

        for (i in 1:length(COHORT2$TOTALKCAL)) {
            if (COHORT2$TOTALKCAL[i] == 0) {
                COHORT2$HEI2015_TOTALFRT[i] = 0
                COHORT2$HEI2015_FRT[i] = 0
                COHORT2$HEI2015_VEG[i] = 0
                COHORT2$HEI2015_GREENNBEAN[i] = 0
                COHORT2$HEI2015_TOTALPRO[i] = 0
                COHORT2$HEI2015_SEAPLANTPRO[i] = 0
                COHORT2$HEI2015_WHOLEGRAIN[i] = 0
                COHORT2$HEI2015_DAIRY[i] = 0
                COHORT2$HEI2015_FATTYACID[i] = 0
                COHORT2$HEI2015_REFINEDGRAIN[i] = 0
                COHORT2$HEI2015_ADDEDSUGAR[i] = 0
                COHORT2$HEI2015_ALL[i] = 0
            }
        }
        COHORT2 = COHORT2 %>%
            dplyr::select(
                SEQN, HEI2015_ALL, HEI2015_TOTALFRT, HEI2015_FRT, HEI2015_VEG, HEI2015_GREENNBEAN,
                HEI2015_TOTALPRO, HEI2015_SEAPLANTPRO, HEI2015_WHOLEGRAIN, HEI2015_DAIRY,
                HEI2015_FATTYACID, HEI2015_REFINEDGRAIN, HEI2015_SODIUM, HEI2015_ADDEDSUGAR,
                HEI2015_SATFAT
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
        COHORT12 <- inner_join(COHORT, COHORT2, by = "SEQN") %>%
            mutate(
                HEI2015_ALL = (HEI2015_ALL.x + HEI2015_ALL.y) / 2,
                HEI2015_TOTALFRT = (HEI2015_TOTALFRT.x + HEI2015_TOTALFRT.y) / 2,
                HEI2015_FRT = (HEI2015_FRT.x + HEI2015_FRT.y) / 2,
                HEI2015_VEG = (HEI2015_VEG.x + HEI2015_VEG.y) / 2,
                HEI2015_GREENNBEAN = (HEI2015_GREENNBEAN.x + HEI2015_GREENNBEAN.y) / 2,
                HEI2015_TOTALPRO = (HEI2015_TOTALPRO.x + HEI2015_TOTALPRO.y) / 2,
                HEI2015_SEAPLANTPRO = (HEI2015_SEAPLANTPRO.x + HEI2015_SEAPLANTPRO.y) / 2,
                HEI2015_WHOLEGRAIN = (HEI2015_WHOLEGRAIN.x + HEI2015_WHOLEGRAIN.y) / 2,
                HEI2015_DAIRY = (HEI2015_DAIRY.x + HEI2015_DAIRY.y) / 2,
                HEI2015_FATTYACID = (HEI2015_FATTYACID.x + HEI2015_FATTYACID.y) / 2,
                HEI2015_REFINEDGRAIN = (HEI2015_REFINEDGRAIN.x + HEI2015_REFINEDGRAIN.y) / 2,
                HEI2015_SODIUM = (HEI2015_SODIUM.x + HEI2015_SODIUM.y) / 2,
                HEI2015_ADDEDSUGAR = (HEI2015_ADDEDSUGAR.x + HEI2015_ADDEDSUGAR.y) / 2,
                HEI2015_SATFAT = (HEI2015_SATFAT.x + HEI2015_SATFAT.y) / 2
            ) %>%
            dplyr::select(
                SEQN, HEI2015_ALL, HEI2015_TOTALFRT, HEI2015_FRT, HEI2015_VEG, HEI2015_GREENNBEAN,
                HEI2015_TOTALPRO, HEI2015_SEAPLANTPRO, HEI2015_WHOLEGRAIN, HEI2015_DAIRY,
                HEI2015_FATTYACID, HEI2015_REFINEDGRAIN, HEI2015_SODIUM, HEI2015_ADDEDSUGAR,
                HEI2015_SATFAT
            )
        return(COHORT12)
    }
}
