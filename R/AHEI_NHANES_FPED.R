#' AHEI_NHANES_FPED
#'
#' Calculate the AHEI for the NHANES_FPED data (after 2005) within 1 step
#' @import dplyr
#' @import readr
#' @import haven
#' @param FPED_IND_PATH The file path for the FPED IND data for day 1. The file name should be like: fped_dr1iff.sas7bdat
#' @param NUTRIENT_IND_PATH The file path for the NUTRIENT IND data for day 1. The file name should be like: DR1IFF_J
#' @param FPED_IND_PATH2 The file path for the FPED IND data for day 2. The file name should be like: fped_dr2iff.sas7bdat
#' @param NUTRIENT_IND_PATH2 The file path for the NUTRIENT IND data for day 2. The file name should be like: DR2IFF_J
#' @param SSB_code The food code for sugar sweetened beverage, default is the SSB code from 17-18 FNDDS file.
#' @return The AHEI and its component scores and serving sizes
#' @examples
#' data("NHANES_20172018")
#' AHEI_NHANES_FPED(NHANES_20172018$FPED_IND, NHANES_20172018$NUTRIENT_IND, NHANES_20172018$FPED_IND2, NHANES_20172018$NUTRIENT_IND2)
#' @export

AHEI_NHANES_FPED = function(FPED_IND_PATH = NULL, NUTRIENT_IND_PATH = NULL, FPED_IND_PATH2 = NULL, NUTRIENT_IND_PATH2 = NULL, SSB_code = NULL) {
    # stop if the input data is not provided for any day
    if (is.null(FPED_IND_PATH) & is.null(NUTRIENT_IND_PATH) & is.null(FPED_IND_PATH2) & is.null(NUTRIENT_IND_PATH2)) {
        stop("Please provide the file path for the FPED and NUTRIENT data, day 1 or day 2 or day 1 and day 2.")
    }

    if (is.null(SSB_code)) {
        # load the SSB codes from 17-18 FNDDS file as default
        data("SSB_FNDDS_1718")
        SSB = unique(SSB_FNDDS_1718$`Food code`)
        print("Since no SSB code is provided, the default SSB code from 17-18 FNDDS file is used.")
    } else {
        # use the provided SSB code
        SSB = SSB_code
    }

    ## Create variables needed for AHEI calculation
    AHEI_MIN = 0
    AHEI_MAX = 10
    AHEI_MIN_VEG_SERV = 0
    AHEI_MAX_VEG_SERV = 5
    AHEI_MIN_FRT_SERV = 0
    AHEI_MAX_FRT_SERV = 4
    AHEI_MIN_WGRAIN_F_SERV = 0
    AHEI_MAX_WGRAIN_F_SERV = 75
    AHEI_MIN_WGRAIN_M_SERV = 0
    AHEI_MAX_WGRAIN_M_SERV = 90
    AHEI_MIN_NUTSLEG_SERV = 0
    AHEI_MAX_NUTSLEG_SERV = 1
    AHEI_MIN_N3FAT_SERV = 0
    AHEI_MAX_N3FAT_SERV = 250
    AHEI_MIN_PUFA_SERV = 2
    AHEI_MAX_PUFA_SERV = 10
    AHEI_MIN_SSB_FRTJ_SERV = 1
    AHEI_MAX_SSB_FRTJ_SERV = 0
    AHEI_MIN_REDPROC_MEAT_SERV = 1.5
    AHEI_MAX_REDPROC_MEAT_SERV = 0

    ## create functions for calculating AHEI scores
    SCORE_HEALTHY = function(actual_serv, min_serv, max_serv, min_score, max_score) {
        case_when(
            actual_serv >= max_serv ~ max_score,
            actual_serv <= min_serv ~ min_score,
            TRUE ~ min_score + (actual_serv - min_serv) * max_score / (max_serv - min_serv)
        )
    }

    SCORE_UNHEALTHY = function(actual_serv, min_serv, max_serv, min_score, max_score) {
        case_when(
            actual_serv >= min_serv ~ min_score,
            actual_serv <= max_serv ~ max_score,
            TRUE ~ min_score + (actual_serv - min_serv) * max_score / (max_serv - min_serv)
        )
    }

    # if only day 1 data is provided
    if (!is.null(FPED_IND_PATH) & !is.null(NUTRIENT_IND_PATH)) {
        if (is.character(FPED_IND_PATH) == TRUE) {
            FPED_IND = read_sas(FPED_IND_PATH)
        } else {
            FPED_IND = FPED_IND_PATH
        }

        if (is.character(NUTRIENT_IND_PATH) == TRUE) {
            NUTRIENT_IND = read_xpt(NUTRIENT_IND_PATH)
        } else {
            NUTRIENT_IND = NUTRIENT_IND_PATH
        }

        if (!("DR1ILINE" %in% colnames(FPED_IND)) | !("DR1ILINE" %in% colnames(NUTRIENT_IND))) {
            stop("Please use individual-level first day data. Individual-level nutrient data should be like DR1IFF_J.XPT. Individual-level FPED data should be like fped_dr1iff_1718.sas7bdat")
        }

        # if (is.null(NUTRIENT_IND_PATH$DR1ILINE) | is.null(FPED_IND_PATH$DR1ILINE)) {
        #   stop("Please use individual-level data for this function. Individual-level nutrient data should be like DR1IFF_J.XPT. Individual-level FPED data should be like fped_dr1iff_1718.sas7bdat")
        # }

        # Select only the high quality data
        NUTRIENT_IND = NUTRIENT_IND %>%
            filter(DR1DRSTZ == 1) %>%
            arrange(SEQN)

        FPED_IND = FPED_IND %>%
            arrange(SEQN)

        # merge the FPED and nutrient data
        COHORT = NUTRIENT_IND %>%
            left_join(FPED_IND, by = c("SEQN", "DR1ILINE"))

        # Create the serving size variables for AHEI calculation
        COHORT = COHORT %>%
            dplyr::mutate(
                # create the variable for added sugars from SSB
                ADDED_SUGAR_SSB_SERV = case_when(
                    DR1IFDCD.x %in% SSB ~ DR1I_ADD_SUGARS,
                    TRUE ~ 0
                )
            ) %>%
            # group by individual
            dplyr::group_by(SEQN) %>%
            # summarize to get the total servings of each food group for each individual
            dplyr::summarize(
                ENERGY = sum(DR1IKCAL),
                RIAGENDR = min(RIAGENDR),
                VEG_SERV = sum(DR1I_V_DRKGR + (DR1I_V_REDOR_TOTAL + DR1I_V_OTHER + DR1I_V_STARCHY_OTHER) / 0.5),
                FRT_SERV = sum(DR1I_F_TOTAL - DR1I_F_JUICE),
                WGRAIN_SERV = sum(DR1I_G_WHOLE / 0.035274),
                NUTSLEG_SERV = sum(DR1I_PF_LEGUMES + DR1I_PF_NUTSDS + DR1I_PF_SOY),
                PUFA_SERV = ((sum(DR1IP182 + DR1IP183 + DR1IP184 + DR1IP204 + DR1IP225) * 9) / ENERGY) * 100,
                N3FAT_SERV = sum((DR1IP205 + DR1IP226) * 1000),
                SSB_FRTJ_SERV = sum((ADDED_SUGAR_SSB_SERV * 4 / 26)),
                REDPROC_MEAT_SERV = sum((DR1I_PF_CUREDMEAT / 1.5) + ((DR1I_PF_MEAT + DR1I_PF_ORGAN) / 4)),
                SODIUM_SERV = sum(DR1ISODI) / (ENERGY / 2000),
                ALCOHOL_SERV = sum(DR1I_A_DRINKS)
            )

        # Rank the sodium by decile
        SODIUM_DECILE = quantile(COHORT$SODIUM_SERV, probs = seq(0, 1, by = 1 / 11))

        ## AHEI score calculation
        COHORT = COHORT %>%
            dplyr::mutate(
                AHEI_VEG = SCORE_HEALTHY(VEG_SERV, AHEI_MIN_VEG_SERV, AHEI_MAX_VEG_SERV, AHEI_MIN, AHEI_MAX),
                AHEI_FRT = SCORE_HEALTHY(FRT_SERV, AHEI_MIN_FRT_SERV, AHEI_MAX_FRT_SERV, AHEI_MIN, AHEI_MAX),
                AHEI_WGRAIN = case_when(
                    # 2 is female and 1 is male
                    RIAGENDR == 2 ~ SCORE_HEALTHY(WGRAIN_SERV, AHEI_MIN_WGRAIN_F_SERV, AHEI_MAX_WGRAIN_F_SERV, AHEI_MIN, AHEI_MAX),
                    RIAGENDR == 1 ~ SCORE_HEALTHY(WGRAIN_SERV, AHEI_MIN_WGRAIN_M_SERV, AHEI_MAX_WGRAIN_M_SERV, AHEI_MIN, AHEI_MAX)
                ),
                AHEI_NUTSLEG = SCORE_HEALTHY(NUTSLEG_SERV, AHEI_MIN_NUTSLEG_SERV, AHEI_MAX_NUTSLEG_SERV, AHEI_MIN, AHEI_MAX),
                AHEI_N3FAT = SCORE_HEALTHY(N3FAT_SERV, AHEI_MIN_N3FAT_SERV, AHEI_MAX_N3FAT_SERV, AHEI_MIN, AHEI_MAX),
                AHEI_PUFA = SCORE_HEALTHY(PUFA_SERV, AHEI_MIN_PUFA_SERV, AHEI_MAX_PUFA_SERV, AHEI_MIN, AHEI_MAX),
                AHEI_SSB_FRTJ = SCORE_UNHEALTHY(SSB_FRTJ_SERV, AHEI_MIN_SSB_FRTJ_SERV, AHEI_MAX_SSB_FRTJ_SERV, AHEI_MIN, AHEI_MAX),
                AHEI_REDPROC_MEAT = SCORE_UNHEALTHY(REDPROC_MEAT_SERV, AHEI_MIN_REDPROC_MEAT_SERV, AHEI_MAX_REDPROC_MEAT_SERV, AHEI_MIN, AHEI_MAX),
                AHEI_SODIUM = case_when(
                    SODIUM_SERV <= SODIUM_DECILE[12] & SODIUM_SERV >= SODIUM_DECILE[11] ~ 0,
                    SODIUM_SERV <= SODIUM_DECILE[11] & SODIUM_SERV >= SODIUM_DECILE[10] ~ 1,
                    SODIUM_SERV < SODIUM_DECILE[10] & SODIUM_SERV >= SODIUM_DECILE[9] ~ 2,
                    SODIUM_SERV < SODIUM_DECILE[9] & SODIUM_SERV >= SODIUM_DECILE[8] ~ 3,
                    SODIUM_SERV < SODIUM_DECILE[8] & SODIUM_SERV >= SODIUM_DECILE[7] ~ 4,
                    SODIUM_SERV < SODIUM_DECILE[7] & SODIUM_SERV >= SODIUM_DECILE[6] ~ 5,
                    SODIUM_SERV < SODIUM_DECILE[6] & SODIUM_SERV >= SODIUM_DECILE[5] ~ 6,
                    SODIUM_SERV < SODIUM_DECILE[5] & SODIUM_SERV >= SODIUM_DECILE[4] ~ 7,
                    SODIUM_SERV < SODIUM_DECILE[4] & SODIUM_SERV >= SODIUM_DECILE[3] ~ 8,
                    SODIUM_SERV < SODIUM_DECILE[3] & SODIUM_SERV >= SODIUM_DECILE[2] ~ 9,
                    SODIUM_SERV < SODIUM_DECILE[2] & SODIUM_SERV >= SODIUM_DECILE[1] ~ 10
                ),
                AHEI_ALCOHOL =
                    case_when(
                        RIAGENDR == 2 & ALCOHOL_SERV >= 2.5 ~ 0,
                        RIAGENDR == 2 & ALCOHOL_SERV < 2.5 & ALCOHOL_SERV > 1.5 ~ 0 + (ALCOHOL_SERV - 2.5) * 10 / (1.5 - 2.5),
                        RIAGENDR == 2 & ALCOHOL_SERV <= 1.5 & ALCOHOL_SERV >= 0.5 ~ 10,
                        RIAGENDR == 2 & ALCOHOL_SERV < 0.5 ~ 0 + (ALCOHOL_SERV - 0) * 10 / (0.5 - 0),
                        RIAGENDR == 2 & ALCOHOL_SERV <= 0.125 ~ 2.5,
                        RIAGENDR == 1 & ALCOHOL_SERV >= 3.5 ~ 0,
                        RIAGENDR == 1 & ALCOHOL_SERV < 3.5 & ALCOHOL_SERV > 2 ~ 0 + (ALCOHOL_SERV - 3.5) * 10 / (2 - 3.5),
                        RIAGENDR == 1 & ALCOHOL_SERV <= 2 & ALCOHOL_SERV >= 0.5 ~ 10,
                        RIAGENDR == 1 & ALCOHOL_SERV < 0.5 ~ (ALCOHOL_SERV - 0) * 10 / (0.5 - 0),
                        RIAGENDR == 1 & ALCOHOL_SERV <= 0.125 ~ 2.5
                    ),
                AHEI_ALL = AHEI_VEG + AHEI_FRT + AHEI_WGRAIN + AHEI_NUTSLEG + AHEI_N3FAT +
                    AHEI_PUFA + AHEI_SSB_FRTJ + AHEI_REDPROC_MEAT + AHEI_SODIUM + AHEI_ALCOHOL,
                AHEI_NOETOH = AHEI_VEG + AHEI_FRT + AHEI_WGRAIN + AHEI_NUTSLEG + AHEI_N3FAT +
                    AHEI_PUFA + AHEI_SSB_FRTJ + AHEI_REDPROC_MEAT + AHEI_SODIUM
            ) %>%
            dplyr::select(
                SEQN, AHEI_ALL, AHEI_NOETOH, AHEI_VEG, AHEI_FRT, AHEI_WGRAIN, AHEI_NUTSLEG, AHEI_N3FAT,
                AHEI_PUFA, AHEI_SSB_FRTJ, AHEI_REDPROC_MEAT, AHEI_SODIUM, AHEI_ALCOHOL
            )
    }

    # if only day 2 data is provided
    if (!is.null(FPED_IND_PATH2) & !is.null(NUTRIENT_IND_PATH2)) {
        if (is.character(FPED_IND_PATH2) == TRUE) {
            FPED_IND2 = read_sas(FPED_IND_PATH2)
        } else {
            FPED_IND2 = FPED_IND_PATH2
        }

        if (is.character(NUTRIENT_IND_PATH2) == TRUE) {
            NUTRIENT_IND2 = read_xpt(NUTRIENT_IND_PATH2)
        } else {
            NUTRIENT_IND2 = NUTRIENT_IND_PATH2
        }

        if (!("DR2ILINE" %in% colnames(FPED_IND2)) | !("DR2ILINE" %in% colnames(NUTRIENT_IND2))) {
            stop("Please use individual-level second day data. Individual-level nutrient data should be like DR2IFF_J.XPT. Individual-level FPED data should be like fped_dr2iff_1718.sas7bdat")
        }


        NUTRIENT_IND2 = NUTRIENT_IND2 %>%
            filter(DR2DRSTZ == 1) %>%
            arrange(SEQN)

        FPED_IND2 = FPED_IND2 %>%
            arrange(SEQN)

        COHORT2 = NUTRIENT_IND2 %>%
            left_join(FPED_IND2, by = c("SEQN", "DR2ILINE"))

        COHORT2 = COHORT2 %>%
            dplyr::mutate(
                # create the variable for added sugars from SSB
                ADDED_SUGAR_SSB_SERV = case_when(
                    DR2IFDCD.x %in% SSB ~ DR2I_ADD_SUGARS,
                    TRUE ~ 0
                )
            ) %>%
            # group by individual
            dplyr::group_by(SEQN) %>%
            # summarize to get the total servings of each food group for each individual
            dplyr::summarize(
                ENERGY = sum(DR2IKCAL),
                RIAGENDR = min(RIAGENDR),
                VEG_SERV = sum(DR2I_V_DRKGR + (DR2I_V_REDOR_TOTAL + DR2I_V_OTHER + DR2I_V_STARCHY_OTHER) / 0.5),
                FRT_SERV = sum(DR2I_F_TOTAL - DR2I_F_JUICE),
                WGRAIN_SERV = sum(DR2I_G_WHOLE / 0.035274),
                NUTSLEG_SERV = sum(DR2I_PF_LEGUMES + DR2I_PF_NUTSDS + DR2I_PF_SOY),
                PUFA_SERV = ((sum(DR2IP182 + DR2IP183 + DR2IP184 + DR2IP204 + DR2IP225) * 9) / ENERGY) * 100,
                N3FAT_SERV = sum((DR2IP205 + DR2IP226) * 1000),
                SSB_FRTJ_SERV = sum((ADDED_SUGAR_SSB_SERV * 4 / 26)),
                REDPROC_MEAT_SERV = sum((DR2I_PF_CUREDMEAT / 1.5) + ((DR2I_PF_MEAT + DR2I_PF_ORGAN) / 4)),
                SODIUM_SERV = sum(DR2ISODI) / (ENERGY / 2000),
                ALCOHOL_SERV = sum(DR2I_A_DRINKS)
            )

        SODIUM_DECILE = quantile(COHORT2$SODIUM_SERV, probs = seq(0, 1, by = 1 / 11))

        ## AHEI calculation
        COHORT2 = COHORT2 %>%
            dplyr::mutate(
                AHEI_VEG = SCORE_HEALTHY(VEG_SERV, AHEI_MIN_VEG_SERV, AHEI_MAX_VEG_SERV, AHEI_MIN, AHEI_MAX),
                AHEI_FRT = SCORE_HEALTHY(FRT_SERV, AHEI_MIN_FRT_SERV, AHEI_MAX_FRT_SERV, AHEI_MIN, AHEI_MAX),
                AHEI_WGRAIN = case_when(
                    # 2 is female and 1 is male
                    RIAGENDR == 2 ~ SCORE_HEALTHY(WGRAIN_SERV, AHEI_MIN_WGRAIN_F_SERV, AHEI_MAX_WGRAIN_F_SERV, AHEI_MIN, AHEI_MAX),
                    RIAGENDR == 1 ~ SCORE_HEALTHY(WGRAIN_SERV, AHEI_MIN_WGRAIN_M_SERV, AHEI_MAX_WGRAIN_M_SERV, AHEI_MIN, AHEI_MAX)
                ),
                AHEI_NUTSLEG = SCORE_HEALTHY(NUTSLEG_SERV, AHEI_MIN_NUTSLEG_SERV, AHEI_MAX_NUTSLEG_SERV, AHEI_MIN, AHEI_MAX),
                AHEI_N3FAT = SCORE_HEALTHY(N3FAT_SERV, AHEI_MIN_N3FAT_SERV, AHEI_MAX_N3FAT_SERV, AHEI_MIN, AHEI_MAX),
                AHEI_PUFA = SCORE_HEALTHY(PUFA_SERV, AHEI_MIN_PUFA_SERV, AHEI_MAX_PUFA_SERV, AHEI_MIN, AHEI_MAX),
                AHEI_SSB_FRTJ = SCORE_UNHEALTHY(SSB_FRTJ_SERV, AHEI_MIN_SSB_FRTJ_SERV, AHEI_MAX_SSB_FRTJ_SERV, AHEI_MIN, AHEI_MAX),
                AHEI_REDPROC_MEAT = SCORE_UNHEALTHY(REDPROC_MEAT_SERV, AHEI_MIN_REDPROC_MEAT_SERV, AHEI_MAX_REDPROC_MEAT_SERV, AHEI_MIN, AHEI_MAX),
                AHEI_SODIUM = case_when(
                    SODIUM_SERV <= SODIUM_DECILE[12] & SODIUM_SERV >= SODIUM_DECILE[11] ~ 0,
                    SODIUM_SERV <= SODIUM_DECILE[11] & SODIUM_SERV >= SODIUM_DECILE[10] ~ 1,
                    SODIUM_SERV < SODIUM_DECILE[10] & SODIUM_SERV >= SODIUM_DECILE[9] ~ 2,
                    SODIUM_SERV < SODIUM_DECILE[9] & SODIUM_SERV >= SODIUM_DECILE[8] ~ 3,
                    SODIUM_SERV < SODIUM_DECILE[8] & SODIUM_SERV >= SODIUM_DECILE[7] ~ 4,
                    SODIUM_SERV < SODIUM_DECILE[7] & SODIUM_SERV >= SODIUM_DECILE[6] ~ 5,
                    SODIUM_SERV < SODIUM_DECILE[6] & SODIUM_SERV >= SODIUM_DECILE[5] ~ 6,
                    SODIUM_SERV < SODIUM_DECILE[5] & SODIUM_SERV >= SODIUM_DECILE[4] ~ 7,
                    SODIUM_SERV < SODIUM_DECILE[4] & SODIUM_SERV >= SODIUM_DECILE[3] ~ 8,
                    SODIUM_SERV < SODIUM_DECILE[3] & SODIUM_SERV >= SODIUM_DECILE[2] ~ 9,
                    SODIUM_SERV < SODIUM_DECILE[2] & SODIUM_SERV >= SODIUM_DECILE[1] ~ 10
                ),
                AHEI_ALCOHOL =
                    case_when(
                        RIAGENDR == 2 & ALCOHOL_SERV >= 2.5 ~ 0,
                        RIAGENDR == 2 & ALCOHOL_SERV < 2.5 & ALCOHOL_SERV > 1.5 ~ 0 + (ALCOHOL_SERV - 2.5) * 10 / (1.5 - 2.5),
                        RIAGENDR == 2 & ALCOHOL_SERV <= 1.5 & ALCOHOL_SERV >= 0.5 ~ 10,
                        RIAGENDR == 2 & ALCOHOL_SERV < 0.5 ~ 0 + (ALCOHOL_SERV - 0) * 10 / (0.5 - 0),
                        RIAGENDR == 2 & ALCOHOL_SERV <= 0.125 ~ 2.5,
                        RIAGENDR == 1 & ALCOHOL_SERV >= 3.5 ~ 0,
                        RIAGENDR == 1 & ALCOHOL_SERV < 3.5 & ALCOHOL_SERV > 2 ~ 0 + (ALCOHOL_SERV - 3.5) * 10 / (2 - 3.5),
                        RIAGENDR == 1 & ALCOHOL_SERV <= 2 & ALCOHOL_SERV >= 0.5 ~ 10,
                        RIAGENDR == 1 & ALCOHOL_SERV < 0.5 ~ (ALCOHOL_SERV - 0) * 10 / (0.5 - 0),
                        RIAGENDR == 1 & ALCOHOL_SERV <= 0.125 ~ 2.5
                    ),
                AHEI_ALL = AHEI_VEG + AHEI_FRT + AHEI_WGRAIN + AHEI_NUTSLEG + AHEI_N3FAT +
                    AHEI_PUFA + AHEI_SSB_FRTJ + AHEI_REDPROC_MEAT + AHEI_SODIUM + AHEI_ALCOHOL,
                AHEI_NOETOH = AHEI_VEG + AHEI_FRT + AHEI_WGRAIN + AHEI_NUTSLEG + AHEI_N3FAT +
                    AHEI_PUFA + AHEI_SSB_FRTJ + AHEI_REDPROC_MEAT + AHEI_SODIUM
            ) %>%
            dplyr::select(
                SEQN, AHEI_ALL, AHEI_NOETOH, AHEI_VEG, AHEI_FRT, AHEI_WGRAIN, AHEI_NUTSLEG, AHEI_N3FAT,
                AHEI_PUFA, AHEI_SSB_FRTJ, AHEI_REDPROC_MEAT, AHEI_SODIUM, AHEI_ALCOHOL
            )
    }

    if (!is.null(FPED_IND_PATH) & !is.null(NUTRIENT_IND_PATH) & is.null(FPED_IND_PATH2) & is.null(NUTRIENT_IND_PATH2)) {
        return(COHORT)
    }

    if (is.null(FPED_IND_PATH) & is.null(NUTRIENT_IND_PATH) & !is.null(FPED_IND_PATH2) & !is.null(NUTRIENT_IND_PATH2)) {
        return(COHORT2)
    }

    # merge two days data if they both exist by creating columns with the same name but taking average of the original column
    if (!is.null(FPED_IND_PATH) & !is.null(NUTRIENT_IND_PATH) & !is.null(FPED_IND_PATH2) & !is.null(NUTRIENT_IND_PATH2)) {
        COHORT12 <- inner_join(COHORT, COHORT2, by = "SEQN") %>%
            mutate(
                AHEI_ALL = (AHEI_ALL.x + AHEI_ALL.y) / 2,
                AHEI_NOETOH = (AHEI_NOETOH.x + AHEI_NOETOH.y) / 2,
                AHEI_VEG = (AHEI_VEG.x + AHEI_VEG.y) / 2,
                AHEI_FRT = (AHEI_FRT.x + AHEI_FRT.y) / 2,
                AHEI_WGRAIN = (AHEI_WGRAIN.x + AHEI_WGRAIN.y) / 2,
                AHEI_NUTSLEG = (AHEI_NUTSLEG.x + AHEI_NUTSLEG.y) / 2,
                AHEI_N3FAT = (AHEI_N3FAT.x + AHEI_N3FAT.y) / 2,
                AHEI_PUFA = (AHEI_PUFA.x + AHEI_PUFA.y) / 2,
                AHEI_SSB_FRTJ = (AHEI_SSB_FRTJ.x + AHEI_SSB_FRTJ.y) / 2,
                AHEI_REDPROC_MEAT = (AHEI_REDPROC_MEAT.x + AHEI_REDPROC_MEAT.y) / 2,
                AHEI_SODIUM = (AHEI_SODIUM.x + AHEI_SODIUM.y) / 2,
                AHEI_ALCOHOL = (AHEI_ALCOHOL.x + AHEI_ALCOHOL.y) / 2
            ) %>%
            select(
                SEQN, AHEI_ALL, AHEI_NOETOH, AHEI_VEG, AHEI_FRT, AHEI_WGRAIN, AHEI_NUTSLEG, AHEI_N3FAT,
                AHEI_PUFA, AHEI_SSB_FRTJ, AHEI_REDPROC_MEAT, AHEI_SODIUM, AHEI_ALCOHOL
            )
        return(COHORT12)
    }
}
