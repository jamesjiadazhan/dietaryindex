#' MED_NHANES_FPED
#'
#' Calculate the MED for the NHANES_FPED data (after 2005) within 1 step
#' @import dplyr
#' @import readr
#' @import haven
#' @param FPED_PATH The file path for the FPED data. The file name should be like: fpre_dr1tot_1718.sas7bdat
#' @param NUTRIENT_PATH The file path for the NUTRIENT data. The file name should be like: DR1TOT_J.XPT
#' @param DEMO_PATH The file path for the DEMOGRAPHIC data. The file name should be like: DEMO_J.XPT
#' @param FPED_PATH2 The file path for the FPED data. The file name should be like: fpre_dr2tot_1718.sas7bdat
#' @param NUTRIENT_PATH2 The file path for the NUTRIENT data. The file name should be like: DR2TOT_J.XPT
#' @return The MED and its component scores
#' @examples
#' data("NHANES_20172018")
#' MED_NHANES_FPED(FPED_PATH = NHANES_20172018$FPED, NUTRIENT_PATH = NHANES_20172018$NUTRIENT, DEMO_PATH = NHANES_20172018$DEMO, FPED_PATH2 = NHANES_20172018$FPED2, NUTRIENT_PATH2 = NHANES_20172018$NUTRIENT2)
#' @export

MED_NHANES_FPED = function(FPED_PATH = NULL, NUTRIENT_PATH = NULL, DEMO_PATH, FPED_PATH2 = NULL, NUTRIENT_PATH2 = NULL) {
    # stop if the input data is not provided for any day
    if (is.null(FPED_PATH) & is.null(NUTRIENT_PATH) & is.null(FPED_PATH2) & is.null(NUTRIENT_PATH2)) {
        stop("Please provide the file path for the FPED and NUTRIENT data, day 1 or day 2 or day 1 and day 2.")
    }

    # if only day 1 data is provided
    if (!is.null(FPED_PATH) & !is.null(NUTRIENT_PATH)) {
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
            stop("Please use the population-level first day data. The file name should be like: Totals.csv")
        }

        # Select only the high quality data
        NUTRIENT = NUTRIENT %>%
            filter(DR1DRSTZ == 1) %>%
            arrange(SEQN)

        DEMO = DEMO %>%
            filter(RIDAGEYR >= 2) %>%
            dplyr::select(SEQN, RIDAGEYR, RIAGENDR, SDDSRVYR, SDMVPSU, SDMVSTRA) %>%
            arrange(SEQN)

        FPED = FPED %>%
            arrange(SEQN)

        # Merge demographic data with nutrient data and FPED data
        COHORT = NUTRIENT %>%
            inner_join(DEMO, by = c("SEQN" = "SEQN")) %>%
            left_join(FPED, by = c("SEQN" = "SEQN"))


        # Calculate the serving size for MED index
        COHORT = COHORT %>%
            filter(DR1TKCAL > 0) %>%
            dplyr::mutate(
                FRT_FRTJ_SERV = DR1T_F_TOTAL,
                VEG_SERV = DR1T_V_DRKGR + (DR1T_V_REDOR_TOTAL + DR1T_V_OTHER + DR1T_V_STARCHY_OTHER) / 0.5,
                WGRAIN_SERV = DR1T_G_WHOLE,
                LEGUMES_SERV = (DR1T_PF_LEGUMES) + DR1T_PF_SOY,
                NUTS_SERV = DR1T_PF_NUTSDS,
                FISH_SERV = DR1T_PF_SEAFD_HI + DR1T_PF_SEAFD_LOW,
                REDPROC_MEAT_SERV = (DR1T_PF_CUREDMEAT / 1.5) + ((DR1T_PF_MEAT + DR1T_PF_ORGAN + DR1T_PF_POULT) / 4),
                MONSATFAT_SERV = case_when(
                    DR1TSFAT == 0 ~ 0,
                    TRUE ~ DR1TMFAT / DR1TSFAT
                ),
                ALCOHOL_SERV = DR1TALCO
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

        # Calculate the MED index
        COHORT = COHORT %>%
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
                SEQN, MED_ALL, MED_NOETOH, MED_FRT, MED_VEG, MED_WGRAIN, MED_LEGUMES, MED_NUTS,
                MED_FISH, MED_REDPROC_MEAT, MED_MONSATFAT, MED_ALCOHOL
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
            stop("Please use the population-level second day data. The file name should be like: Totals.csv")
        }

        NUTRIENT2 = NUTRIENT2 %>%
            filter(DR2DRSTZ == 1) %>%
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


        # Match participant response food frequency to the standard food frequency response code

        COHORT2 = COHORT2 %>%
            filter(DR2TKCAL > 0) %>%
            dplyr::mutate(
                FRT_FRTJ_SERV = DR2T_F_TOTAL,
                VEG_SERV = DR2T_V_DRKGR + (DR2T_V_REDOR_TOTAL + DR2T_V_OTHER + DR2T_V_STARCHY_OTHER) / 0.5,
                WGRAIN_SERV = DR2T_G_WHOLE,
                LEGUMES_SERV = (DR2T_PF_LEGUMES) + DR2T_PF_SOY,
                NUTS_SERV = DR2T_PF_NUTSDS,
                FISH_SERV = DR2T_PF_SEAFD_HI + DR2T_PF_SEAFD_LOW,
                REDPROC_MEAT_SERV = (DR2T_PF_CUREDMEAT / 1.5) + ((DR2T_PF_MEAT + DR2T_PF_ORGAN + DR2T_PF_POULT) / 4),
                MONSATFAT_SERV = case_when(
                    DR2TSFAT == 0 ~ 0,
                    TRUE ~ DR2TMFAT / DR2TSFAT
                ),
                ALCOHOL_SERV = DR2TALCO
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


        COHORT2 = COHORT2 %>%
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
                SEQN, MED_ALL, MED_NOETOH, MED_FRT, MED_VEG, MED_WGRAIN, MED_LEGUMES, MED_NUTS,
                MED_FISH, MED_REDPROC_MEAT, MED_MONSATFAT, MED_ALCOHOL
            )
    }

    if (!is.null(FPED_PATH) & !is.null(NUTRIENT_PATH) & is.null(FPED_PATH2) & is.null(NUTRIENT_PATH2)) {
        print("Reminder: this MED index uses medians to rank participants' food/drink serving sizes and then calculate MED component scores, which may generate results that are specific to your study population but not comparable to other populations.")
        return(COHORT)
    }

    if (is.null(FPED_PATH) & is.null(NUTRIENT_PATH) & !is.null(FPED_PATH2) & !is.null(NUTRIENT_PATH2)) {
        print("Reminder: this MED index uses medians to rank participants' food/drink serving sizes and then calculate MED component scores, which may generate results that are specific to your study population but not comparable to other populations.")
        return(COHORT2)
    }

    # merge two days data if they both exist
    if (!is.null(FPED_PATH) & !is.null(NUTRIENT_PATH) & !is.null(FPED_PATH2) & !is.null(NUTRIENT_PATH2)) {
        COHORT12 <- inner_join(COHORT, COHORT2, by = "SEQN") %>%
            dplyr::mutate(
                MED_ALL = (MED_ALL.x + MED_ALL.y) / 2,
                MED_NOETOH = (MED_NOETOH.x + MED_NOETOH.y) / 2,
                MED_FRT = (MED_FRT.x + MED_FRT.y) / 2,
                MED_VEG = (MED_VEG.x + MED_VEG.y) / 2,
                MED_WGRAIN = (MED_WGRAIN.x + MED_WGRAIN.y) / 2,
                MED_LEGUMES = (MED_LEGUMES.x + MED_LEGUMES.y) / 2,
                MED_NUTS = (MED_NUTS.x + MED_NUTS.y) / 2,
                MED_FISH = (MED_FISH.x + MED_FISH.y) / 2,
                MED_REDPROC_MEAT = (MED_REDPROC_MEAT.x + MED_REDPROC_MEAT.y) / 2,
                MED_MONSATFAT = (MED_MONSATFAT.x + MED_MONSATFAT.y) / 2,
                MED_ALCOHOL = (MED_ALCOHOL.x + MED_ALCOHOL.y) / 2
            ) %>%
            dplyr::select(
                SEQN, MED_ALL, MED_NOETOH, MED_FRT, MED_VEG, MED_WGRAIN, MED_LEGUMES, MED_NUTS,
                MED_FISH, MED_REDPROC_MEAT, MED_MONSATFAT, MED_ALCOHOL
            )
        print("Reminder: this MED index uses medians to rank participants' food/drink serving sizes and then calculate MED component scores, which may generate results that are specific to your study population but not comparable to other populations.")
        return(COHORT12)
    }
}
