#' AHEI Calculation
#'
#' Calculate the AHEI dietary index, Alternative Healthy Eating Index, using given the serving sizes of foods and nutrients consumed per 1 day
#' @import dplyr
#' @import readr
#' @import haven
#' @param SERV_DATA The raw data file that includes all the serving sizes of foods and nutrients
#' @param RESPONDENTID The unique participant ID for each participant
#' @param GENDER The gender of the participant. 2 is female and 1 is male.
#' @param TOTALKCAL_AHEI The total kcal, for adjusting sodium intake
#' @param VEG_SERV_AHEI The serving size of All vegetable except potatoes and legume, unit=servings/day (0.5 c of vege; 1 cup of green leafy (1 cup = 236.59 g)
#' @param FRT_SERV_AHEI The serving size of All whole fruits and no fruit juice, unit=servings/day (0.5 c of berries; 1 cup=236.59 g; 1 med fruit (1 cup = 236.59 g)
#' @param WGRAIN_SERV_AHEI The serving size of whole grains, unit=grams/day
#' @param NUTSLEG_SERV_AHEI The serving size of Nuts, legumes, and vegetable protein (e.g., tofu), unit=servings/day = 1 srv=1oz (28.35 g) of nuts and legume or 1 TBLSP peanut butter (15 mL), 1 cup legume = 4 oz
#' @param N3FAT_SERV_AHEI The serving size of omega 3 fatty acid, unit=mg/day ( oz. = 28.35 g)
#' @param PUFA_SERV_AHEI The serving size of PUFA, unit=\% of energy
#' @param SSB_FRTJ_SERV_AHEI The serving size of sugar-sweetened beverages and non-100\% fruit juice, unit=servings/day = 1 ser= 8oz (1 oz. = 28.35 g)
#' @param REDPROC_MEAT_SERV_AHEI The serving size of red and processed meats, including Beef, pork, lamb, goat, veal, sausages, bacon, salami, ham, hot dog, deli meat, unit=servings/day; 1 srv= 4 oz. unprocessed meat; 1.5 oz. processed meat (1 oz. = 28.35 g)
#' @param TRANS_SERV_AHEI The serving size of trans fat, unit=\% of energy
#' @param SODIUM_SERV_AHEI The serving size of sodium, unit=mg/day per 2000 kcal
#' @param ALCOHOL_SERV_AHEI The serving size of alcohol, including Wine, beer, "light" beer, liquor, unit=drink/day (12 oz beer; 5 oz wine; 1.5 oz spirits) 1 oz = 28.35 g
#' @return The AHEI index/score, AHEI
#' @examples
#' data("AHEI_VALIDATION")
#' AHEI(SERV_DATA = AHEI_VALIDATION, RESPONDENTID = AHEI_VALIDATION$id, GENDER = AHEI_VALIDATION$gender, TOTALKCAL_AHEI = AHEI_VALIDATION$kcal, VEG_SERV_AHEI = AHEI_VALIDATION$vegetable, FRT_SERV_AHEI = AHEI_VALIDATION$fruit, WGRAIN_SERV_AHEI = AHEI_VALIDATION$whole_grain, NUTSLEG_SERV_AHEI = AHEI_VALIDATION$nut_legume, N3FAT_SERV_AHEI = AHEI_VALIDATION$n3_fat, PUFA_SERV_AHEI = AHEI_VALIDATION$pufa, SSB_FRTJ_SERV_AHEI = AHEI_VALIDATION$ssb_fruit_juice, REDPROC_MEAT_SERV_AHEI = AHEI_VALIDATION$red_processed_meat, TRANS_SERV_AHEI = AHEI_VALIDATION$trans_fat, SODIUM_SERV_AHEI = AHEI_VALIDATION$sodium, ALCOHOL_SERV_AHEI = AHEI_VALIDATION$alcohol)
#' @export


# Score calculation for AHEI
AHEI = function(SERV_DATA, RESPONDENTID, GENDER, TOTALKCAL_AHEI, VEG_SERV_AHEI, FRT_SERV_AHEI, WGRAIN_SERV_AHEI, NUTSLEG_SERV_AHEI, N3FAT_SERV_AHEI, PUFA_SERV_AHEI,
                SSB_FRTJ_SERV_AHEI, REDPROC_MEAT_SERV_AHEI, TRANS_SERV_AHEI, SODIUM_SERV_AHEI, ALCOHOL_SERV_AHEI) {
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

    SERV_DATA = SERV_DATA %>%
        mutate(SODIUM_SERV_AHEI = SODIUM_SERV_AHEI / (TOTALKCAL_AHEI / 2000))

    SODIUM_DECILE = quantile(SERV_DATA$SODIUM_SERV_AHEI, probs = seq(0, 1, by = 1 / 11), na.rm = TRUE)

    ## AHEI calculation
    SERV_DATA = SERV_DATA %>%
        dplyr::mutate(
            RESPONDENTID = RESPONDENTID,
            GENDER = GENDER,
            AHEI_VEG = SCORE_HEALTHY(VEG_SERV_AHEI, AHEI_MIN_VEG_SERV, AHEI_MAX_VEG_SERV, AHEI_MIN, AHEI_MAX),
            AHEI_FRT = SCORE_HEALTHY(FRT_SERV_AHEI, AHEI_MIN_FRT_SERV, AHEI_MAX_FRT_SERV, AHEI_MIN, AHEI_MAX),
            AHEI_WGRAIN = case_when(
                # GENDER = 2 is female
                GENDER == 2 & WGRAIN_SERV_AHEI >= AHEI_MAX_WGRAIN_F_SERV ~ AHEI_MAX,
                GENDER == 2 & WGRAIN_SERV_AHEI <= AHEI_MIN_WGRAIN_F_SERV ~ AHEI_MIN,
                GENDER == 2 & WGRAIN_SERV_AHEI > AHEI_MIN_WGRAIN_F_SERV & WGRAIN_SERV_AHEI < AHEI_MAX_WGRAIN_F_SERV ~ AHEI_MIN + (WGRAIN_SERV_AHEI - AHEI_MIN_WGRAIN_F_SERV) * AHEI_MAX / (AHEI_MAX_WGRAIN_F_SERV - AHEI_MIN_WGRAIN_F_SERV),
                GENDER == 1 & WGRAIN_SERV_AHEI >= AHEI_MAX_WGRAIN_M_SERV ~ AHEI_MAX,
                GENDER == 1 & WGRAIN_SERV_AHEI <= AHEI_MIN_WGRAIN_M_SERV ~ AHEI_MIN,
                GENDER == 1 & WGRAIN_SERV_AHEI > AHEI_MIN_WGRAIN_M_SERV & WGRAIN_SERV_AHEI < AHEI_MAX_WGRAIN_M_SERV ~ AHEI_MIN + (WGRAIN_SERV_AHEI - AHEI_MIN_WGRAIN_M_SERV) * AHEI_MAX / (AHEI_MAX_WGRAIN_M_SERV - AHEI_MIN_WGRAIN_M_SERV),
            ),
            AHEI_NUTSLEG = SCORE_HEALTHY(NUTSLEG_SERV_AHEI, AHEI_MIN_NUTSLEG_SERV, AHEI_MAX_NUTSLEG_SERV, AHEI_MIN, AHEI_MAX),
            AHEI_N3FAT = SCORE_HEALTHY(N3FAT_SERV_AHEI, AHEI_MIN_N3FAT_SERV, AHEI_MAX_N3FAT_SERV, AHEI_MIN, AHEI_MAX),
            AHEI_PUFA = SCORE_HEALTHY(PUFA_SERV_AHEI, AHEI_MIN_PUFA_SERV, AHEI_MAX_PUFA_SERV, AHEI_MIN, AHEI_MAX),
            AHEI_SSB_FRTJ = SCORE_UNHEALTHY(SSB_FRTJ_SERV_AHEI, AHEI_MIN_SSB_FRTJ_SERV, AHEI_MAX_SSB_FRTJ_SERV, AHEI_MIN, AHEI_MAX),
            AHEI_REDPROC_MEAT = SCORE_UNHEALTHY(REDPROC_MEAT_SERV_AHEI, AHEI_MIN_REDPROC_MEAT_SERV, AHEI_MAX_REDPROC_MEAT_SERV, AHEI_MIN, AHEI_MAX),
            AHEI_TRANS = SCORE_UNHEALTHY(TRANS_SERV_AHEI, AHEI_MIN_TRANS_SERV, AHEI_MAX_TRANS_SERV, AHEI_MIN, AHEI_MAX),
            AHEI_SODIUM = case_when(
                SODIUM_SERV_AHEI <= SODIUM_DECILE[12] & SODIUM_SERV_AHEI >= SODIUM_DECILE[11] ~ 0,
                SODIUM_SERV_AHEI <= SODIUM_DECILE[11] & SODIUM_SERV_AHEI >= SODIUM_DECILE[10] ~ 1,
                SODIUM_SERV_AHEI < SODIUM_DECILE[10] & SODIUM_SERV_AHEI >= SODIUM_DECILE[9] ~ 2,
                SODIUM_SERV_AHEI < SODIUM_DECILE[9] & SODIUM_SERV_AHEI >= SODIUM_DECILE[8] ~ 3,
                SODIUM_SERV_AHEI < SODIUM_DECILE[8] & SODIUM_SERV_AHEI >= SODIUM_DECILE[7] ~ 4,
                SODIUM_SERV_AHEI < SODIUM_DECILE[7] & SODIUM_SERV_AHEI >= SODIUM_DECILE[6] ~ 5,
                SODIUM_SERV_AHEI < SODIUM_DECILE[6] & SODIUM_SERV_AHEI >= SODIUM_DECILE[5] ~ 6,
                SODIUM_SERV_AHEI < SODIUM_DECILE[5] & SODIUM_SERV_AHEI >= SODIUM_DECILE[4] ~ 7,
                SODIUM_SERV_AHEI < SODIUM_DECILE[4] & SODIUM_SERV_AHEI >= SODIUM_DECILE[3] ~ 8,
                SODIUM_SERV_AHEI < SODIUM_DECILE[3] & SODIUM_SERV_AHEI >= SODIUM_DECILE[2] ~ 9,
                SODIUM_SERV_AHEI < SODIUM_DECILE[2] & SODIUM_SERV_AHEI >= SODIUM_DECILE[1] ~ 10
            ),
            AHEI_ALCOHOL = case_when(
                ## GENDER = 2 is female
                GENDER == 2 & ALCOHOL_SERV_AHEI >= 2.5 ~ 0,
                GENDER == 2 & ALCOHOL_SERV_AHEI < 2.5 & ALCOHOL_SERV_AHEI > 1.5 ~ 0 + (ALCOHOL_SERV_AHEI - 2.5) * 10 / (1.5 - 2.5),
                GENDER == 2 & ALCOHOL_SERV_AHEI <= 1.5 & ALCOHOL_SERV_AHEI >= 0.5 ~ 10,
                GENDER == 2 & ALCOHOL_SERV_AHEI < 0.5 & ALCOHOL_SERV_AHEI > 0.125 ~ 0 + (ALCOHOL_SERV_AHEI - 0) * 10 / (0.5 - 0),
                GENDER == 2 & ALCOHOL_SERV_AHEI <= 0.125 ~ 2.5,

                ## GENDER = 1 is male
                GENDER == 1 & ALCOHOL_SERV_AHEI >= 3.5 ~ 0,
                GENDER == 1 & ALCOHOL_SERV_AHEI < 3.5 & ALCOHOL_SERV_AHEI > 2 ~ 0 + (ALCOHOL_SERV_AHEI - 3.5) * 10 / (2.0 - 3.5),
                GENDER == 1 & ALCOHOL_SERV_AHEI <= 2 & ALCOHOL_SERV_AHEI >= 0.5 ~ 10,
                GENDER == 1 & ALCOHOL_SERV_AHEI < 0.5 & ALCOHOL_SERV_AHEI > 0.125 ~ 0 + (ALCOHOL_SERV_AHEI - 0) * 10 / (0.5 - 0),
                GENDER == 1 & ALCOHOL_SERV_AHEI <= 0.125 ~ 2.5,
            )
        ) 
    

    ## when the column of AHEI_TRANS has all missing values, the AHEI_ALL and AHEI_NOETOH are calculated by the sum of all components except AHEI_TRANS
    if (all(is.na(SERV_DATA$AHEI_TRANS))) {
        SERV_DATA_FINAL = SERV_DATA %>%
            mutate(
                AHEI_ALL = AHEI_VEG + AHEI_FRT + AHEI_WGRAIN + AHEI_NUTSLEG + AHEI_N3FAT +
                    AHEI_PUFA + AHEI_SSB_FRTJ + AHEI_REDPROC_MEAT + AHEI_SODIUM + AHEI_ALCOHOL,
                AHEI_NOETOH = AHEI_VEG + AHEI_FRT + AHEI_WGRAIN + AHEI_NUTSLEG + AHEI_N3FAT +
                    AHEI_PUFA + AHEI_SSB_FRTJ + AHEI_REDPROC_MEAT + AHEI_SODIUM
                ) %>%
            dplyr::select(
                RESPONDENTID, GENDER, AHEI_ALL, AHEI_NOETOH, AHEI_VEG, AHEI_FRT, AHEI_WGRAIN, AHEI_NUTSLEG, AHEI_N3FAT,
                AHEI_PUFA, AHEI_SSB_FRTJ, AHEI_REDPROC_MEAT, AHEI_TRANS, AHEI_SODIUM, AHEI_ALCOHOL
            )
    } else {
        SERV_DATA_FINAL = SERV_DATA %>%
            mutate(
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

    return(SERV_DATA_FINAL)
}