#' AHEI_DHQ3
#'
#' Calculate the AHEI for the DHQ3 data within 1 step
#' @import dplyr
#' @import readr
#' @import haven
#' @param DATA_PATH The file path for the data. The data is Detailed analysis file, ending with detail.csv
#' @return The AHEI and its component scores
#' @examples
#' data("DHQ3_exp_detailed")
#' AHEI_DHQ3(DHQ3_exp_detailed)
#' @export

AHEI_DHQ3 = function(DATA_PATH) {
    if (is.character(DATA_PATH) == TRUE) {
        COHORT = read_csv(DATA_PATH)
    } else {
        COHORT = DATA_PATH
    }

    if (!("Food ID" %in% colnames(COHORT))) {
        stop("Please use individual-level data for this function. Individual-level data should be like detail.csv")
    }

    COFFEE_TEA = c(16.1, 17.1, 64.1, 64.2, 1081.1, 1123.1, 1123.2, 1130.2, 1130.5)
    DRINK = c(10.6, 10.9, 11.1, 11.2, 1140.1, 1140.2, 1144.1, 1150.1, 1152.1)
    SSB = c(COFFEE_TEA, DRINK)

    COHORT = COHORT %>%
        dplyr::mutate(
            ADDED_SUGAR_SSB_SERV = case_when(
                `Food ID` %in% SSB ~ `*Added sugars (g)`,
                TRUE ~ 0
            )
        ) %>%
        dplyr::group_by(`Respondent ID`) %>%
        dplyr::summarize(
            `Sex (1=male; 2=female)` = min(`Sex (1=male; 2=female)`),
            KCAL = sum(`Energy (kcal)`),
            VEG_SERV = sum(`Dark-green vegetable (cups)` + (`Total red/orange vegetable (cups)` + `Other starchy vegetable (cups)` + `Other vegetable (cups)`) / 0.5),
            FRT_SERV = sum(`Total fruit (cups)` - `Juice fruit (cups)`),
            WGRAIN_SERV = sum(`Whole grain (oz)` / 0.035274),
            NUTSLEG_SERV = sum(`Nuts, seeds, soy, and legumes (oz)`),
            PUFA_SERV = (sum((`Total polyunsaturated fatty acids (g)` - `PFA 20:5 (Eicosapentaenoic) (g)` - `PFA 22:6 (Docosahexaenoic) (g)`) * 9) / KCAL) * 100,
            N3FAT_SERV = sum((`PFA 20:5 (Eicosapentaenoic) (g)` + `PFA 22:6 (Docosahexaenoic) (g)`) * 1000),
            SSB_FRTJ_SERV = sum(ADDED_SUGAR_SSB_SERV / 26),
            REDPROC_MEAT_SERV = sum((`Cured meat protein foods (oz)` / 1.5) + ((`Meat from beef, pork, veal, lamb, and game protein foods (oz)` + `Meat from organ meat protein foods (oz)`) / 4)),
            TRANS_SERV = (sum(`*Total trans fatty acitds (g)` * 9) / KCAL) * 100,
            SODIUM_SERV = sum(`Sodium (mg)` / (KCAL / 2000)),
            ALCOHOL_SERV = sum(`Alcohol (drink(s))`)
        )


    ## Create variables and functions needed for AHEI calculation
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
    AHEI_MIN_TRANS_SERV = 4
    AHEI_MAX_TRANS_SERV = 0.5

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

    SODIUM_DECILE = quantile(COHORT$SODIUM_SERV, probs = seq(0, 1, by = 1 / 11))


    COHORT %>%
        dplyr::mutate(
            RESPONDENTID = `Respondent ID`,
            GENDER = `Sex (1=male; 2=female)`,
            AHEI_VEG = SCORE_HEALTHY(VEG_SERV, AHEI_MIN_VEG_SERV, AHEI_MAX_VEG_SERV, AHEI_MIN, AHEI_MAX),
            AHEI_FRT = SCORE_HEALTHY(FRT_SERV, AHEI_MIN_FRT_SERV, AHEI_MAX_FRT_SERV, AHEI_MIN, AHEI_MAX),
            AHEI_WGRAIN = case_when(
                # GENDER = 2 is female
                GENDER == 2 & WGRAIN_SERV >= AHEI_MAX_WGRAIN_F_SERV ~ AHEI_MAX,
                GENDER == 2 & WGRAIN_SERV <= AHEI_MIN_WGRAIN_F_SERV ~ AHEI_MIN,
                GENDER == 2 & WGRAIN_SERV > AHEI_MIN_WGRAIN_F_SERV & WGRAIN_SERV < AHEI_MAX_WGRAIN_F_SERV ~ AHEI_MIN + (WGRAIN_SERV - AHEI_MIN_WGRAIN_F_SERV) * AHEI_MAX / (AHEI_MAX_WGRAIN_F_SERV - AHEI_MIN_WGRAIN_F_SERV),

                # GENDER = 1 is male
                GENDER == 1 & WGRAIN_SERV >= AHEI_MAX_WGRAIN_M_SERV ~ AHEI_MAX,
                GENDER == 1 & WGRAIN_SERV <= AHEI_MIN_WGRAIN_M_SERV ~ AHEI_MIN,
                GENDER == 1 & WGRAIN_SERV > AHEI_MIN_WGRAIN_M_SERV & WGRAIN_SERV < AHEI_MAX_WGRAIN_M_SERV ~ AHEI_MIN + (WGRAIN_SERV - AHEI_MIN_WGRAIN_M_SERV) * AHEI_MAX / (AHEI_MAX_WGRAIN_M_SERV - AHEI_MIN_WGRAIN_M_SERV),
            ),
            AHEI_NUTSLEG = SCORE_HEALTHY(NUTSLEG_SERV, AHEI_MIN_NUTSLEG_SERV, AHEI_MAX_NUTSLEG_SERV, AHEI_MIN, AHEI_MAX),
            AHEI_N3FAT = SCORE_HEALTHY(N3FAT_SERV, AHEI_MIN_N3FAT_SERV, AHEI_MAX_N3FAT_SERV, AHEI_MIN, AHEI_MAX),
            AHEI_PUFA = SCORE_HEALTHY(PUFA_SERV, AHEI_MIN_PUFA_SERV, AHEI_MAX_PUFA_SERV, AHEI_MIN, AHEI_MAX),
            AHEI_SSB_FRTJ = SCORE_UNHEALTHY(SSB_FRTJ_SERV, AHEI_MIN_SSB_FRTJ_SERV, AHEI_MAX_SSB_FRTJ_SERV, AHEI_MIN, AHEI_MAX),
            AHEI_REDPROC_MEAT = SCORE_UNHEALTHY(REDPROC_MEAT_SERV, AHEI_MIN_REDPROC_MEAT_SERV, AHEI_MAX_REDPROC_MEAT_SERV, AHEI_MIN, AHEI_MAX),
            AHEI_TRANS = SCORE_UNHEALTHY(TRANS_SERV, AHEI_MIN_TRANS_SERV, AHEI_MAX_TRANS_SERV, AHEI_MIN, AHEI_MAX),
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
            AHEI_ALCOHOL = case_when(
                ## GENDER = 2 is female
                GENDER == 2 & ALCOHOL_SERV >= 2.5 ~ 0,
                GENDER == 2 & ALCOHOL_SERV < 2.5 & ALCOHOL_SERV > 1.5 ~ 0 + (ALCOHOL_SERV - 2.5) * 10 / (1.5 - 2.5),
                GENDER == 2 & ALCOHOL_SERV <= 1.5 & ALCOHOL_SERV >= 0.5 ~ 10,
                GENDER == 2 & ALCOHOL_SERV < 0.5 & ALCOHOL_SERV > 0.125 ~ 0 + (ALCOHOL_SERV - 0) * 10 / (0.5 - 0),
                GENDER == 2 & ALCOHOL_SERV <= 0.125 ~ 2.5,

                # GENDER = 1 is male
                GENDER == 1 & ALCOHOL_SERV >= 3.5 ~ 0,
                GENDER == 1 & ALCOHOL_SERV < 3.5 & ALCOHOL_SERV > 2 ~ 0 + (ALCOHOL_SERV - 3.5) * 10 / (2 - 3.5),
                GENDER == 1 & ALCOHOL_SERV <= 2 & ALCOHOL_SERV >= 0.5 ~ 10,
                GENDER == 1 & ALCOHOL_SERV < 0.5 & ALCOHOL_SERV > 0.125 ~ 0 + (ALCOHOL_SERV - 0) * 10 / (0.5 - 0),
                GENDER == 1 & ALCOHOL_SERV <= 0.125 ~ 2.5,
            ),
            AHEI_ALL = AHEI_VEG + AHEI_FRT + AHEI_WGRAIN + AHEI_NUTSLEG + AHEI_N3FAT +
                AHEI_PUFA + AHEI_SSB_FRTJ + AHEI_REDPROC_MEAT + AHEI_TRANS + AHEI_SODIUM + AHEI_ALCOHOL,
            AHEI_NOETOH = AHEI_VEG + AHEI_FRT + AHEI_WGRAIN + AHEI_NUTSLEG + AHEI_N3FAT +
                AHEI_PUFA + AHEI_SSB_FRTJ + AHEI_REDPROC_MEAT + AHEI_TRANS + AHEI_SODIUM
        ) %>%
        dplyr::select(
            RESPONDENTID, GENDER, AHEI_ALL, AHEI_NOETOH, AHEI_VEG, AHEI_FRT, AHEI_WGRAIN, AHEI_NUTSLEG, AHEI_N3FAT,
            AHEI_PUFA, AHEI_SSB_FRTJ, AHEI_REDPROC_MEAT, AHEI_TRANS, AHEI_SODIUM, AHEI_ALCOHOL
        )
}
