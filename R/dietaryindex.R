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
#' @param SODIUM_SERV_AHEI The serving size of sodium, unit=mg/day 
#' @param ALCOHOL_SERV_AHEI The serving size of alcohol, including Wine, beer, "light" beer, liquor, unit=drink/day (12 oz beer; 5 oz wine; 1.5 oz spirits) 1 oz = 28.35 g
#' @return The AHEI index/score, AHEI
#' @examples
#' AHEI(SERV_DATA, SERV_DATA$RESPONDENTID, SERV_DATA$GENDER, SERV_DATA$TOTALKCAL_AHEI, SERV_DATA$VEG_SERV_AHEI, SERV_DATA$FRT_SERV_AHEI, SERV_DATA$WGRAIN_SERV_AHEI, SERV_DATA$NUTSLEG_SERV_AHEI, SERV_DATA$N3FAT_SERV_AHEI, SERV_DATA$PUFA_SERV_AHEI, SERV_DATA$SSB_FRTJ_SERV_AHEI, SERV_DATA$REDPROC_MEAT_SERV_AHEI, SERV_DATA$TRANS_SERV_AHEI,SODIUM_SERV_AHEI, SERV_DATA$ALCOHOL_SERV_AHEI)
#' @export


#Score calculation for AHEI
AHEI = function(SERV_DATA, RESPONDENTID, GENDER, TOTALKCAL_AHEI, VEG_SERV_AHEI, FRT_SERV_AHEI, WGRAIN_SERV_AHEI, NUTSLEG_SERV_AHEI, N3FAT_SERV_AHEI, PUFA_SERV_AHEI,
                SSB_FRTJ_SERV_AHEI, REDPROC_MEAT_SERV_AHEI, TRANS_SERV_AHEI, SODIUM_SERV_AHEI, ALCOHOL_SERV_AHEI){
  ##Create variables and functions needed for AHEI calculation
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
  
  SCORE_HEALTHY = function(actual_serv, min_serv, max_serv, min_score, max_score){
    case_when(
      actual_serv >= max_serv ~ max_score,
      actual_serv <= min_serv ~ min_score,
      TRUE ~ min_score+(actual_serv-min_serv)*max_score/(max_serv-min_serv)
    )
  }
  
  
  SCORE_UNHEALTHY = function(actual_serv, min_serv, max_serv, min_score, max_score){
    case_when(
      actual_serv >= min_serv ~ min_score ,
      actual_serv <= max_serv ~ max_score,
      TRUE ~ min_score+(actual_serv-min_serv)*max_score/(max_serv-min_serv)
    )
  }
  
  SERV_DATA = SERV_DATA %>%
    mutate(SODIUM_SERV_AHEI=SODIUM_SERV_AHEI/(TOTALKCAL_AHEI/1000))
  
  SODIUM_DECILE = quantile(SERV_DATA$SODIUM_SERV_AHEI, probs=seq(0, 1, by=1/11))
  
  ##AHEI calculation
  SERV_DATA %>%
    dplyr::mutate(
      RESPONDENTID = RESPONDENTID,
      GENDER = GENDER,
      
      AHEI_VEG = SCORE_HEALTHY(VEG_SERV_AHEI, AHEI_MIN_VEG_SERV, AHEI_MAX_VEG_SERV, AHEI_MIN, AHEI_MAX),
      AHEI_FRT = SCORE_HEALTHY(FRT_SERV_AHEI, AHEI_MIN_FRT_SERV, AHEI_MAX_FRT_SERV, AHEI_MIN, AHEI_MAX),
      
      AHEI_WGRAIN = case_when(
        #GENDER = 2 is female
        GENDER == 2 & WGRAIN_SERV_AHEI >= AHEI_MAX_WGRAIN_F_SERV ~ AHEI_MAX,
        GENDER == 2 & WGRAIN_SERV_AHEI <= AHEI_MIN_WGRAIN_F_SERV ~ AHEI_MIN,
        GENDER == 2 & WGRAIN_SERV_AHEI > AHEI_MIN_WGRAIN_F_SERV & WGRAIN_SERV_AHEI < AHEI_MAX_WGRAIN_F_SERV ~ AHEI_MIN+(WGRAIN_SERV_AHEI-AHEI_MIN_WGRAIN_F_SERV)*AHEI_MAX/(AHEI_MAX_WGRAIN_F_SERV-AHEI_MIN_WGRAIN_F_SERV),
        
        GENDER == 1 & WGRAIN_SERV_AHEI >= AHEI_MAX_WGRAIN_M_SERV ~ AHEI_MAX,
        GENDER == 1 & WGRAIN_SERV_AHEI <= AHEI_MIN_WGRAIN_M_SERV ~ AHEI_MIN,
        GENDER == 1 & WGRAIN_SERV_AHEI > AHEI_MIN_WGRAIN_M_SERV & WGRAIN_SERV_AHEI < AHEI_MAX_WGRAIN_M_SERV ~ AHEI_MIN+(WGRAIN_SERV_AHEI-AHEI_MIN_WGRAIN_M_SERV)*AHEI_MAX/(AHEI_MAX_WGRAIN_M_SERV-AHEI_MIN_WGRAIN_M_SERV),
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
        ##GENDER = 2 is female
        GENDER == 2  & ALCOHOL_SERV_AHEI >= 2.5 ~ 0,
        GENDER == 2  & ALCOHOL_SERV_AHEI < 2.5 & ALCOHOL_SERV_AHEI > 1.5 ~ 0 + (ALCOHOL_SERV_AHEI-2.5)*10/(1.5-2.5),
        GENDER == 2  & ALCOHOL_SERV_AHEI <= 1.5 & ALCOHOL_SERV_AHEI >= 0.5 ~ 10,
        GENDER == 2  & ALCOHOL_SERV_AHEI < 0.5 & ALCOHOL_SERV_AHEI > 0.125 ~ 0 + (ALCOHOL_SERV_AHEI-0)*10/(0.5-0),
        GENDER == 2  & ALCOHOL_SERV_AHEI <= 0.125 ~ 2.5,

        ##GENDER = 1 is male
        GENDER == 1  & ALCOHOL_SERV_AHEI >= 3.5 ~ 0,
        GENDER == 1  & ALCOHOL_SERV_AHEI < 3.5 & ALCOHOL_SERV_AHEI > 2 ~ 0 + (ALCOHOL_SERV_AHEI-2.5)*10/(1.5-2.5),
        GENDER == 1  & ALCOHOL_SERV_AHEI <= 2 & ALCOHOL_SERV_AHEI >= 0.5 ~ 10,
        GENDER == 1  & ALCOHOL_SERV_AHEI < 0.5 & ALCOHOL_SERV_AHEI > 0.125 ~ 0 + (ALCOHOL_SERV_AHEI-0)*10/(0.5-0),
        GENDER == 1  & ALCOHOL_SERV_AHEI <= 0.125 ~ 2.5,
      ),
      AHEI_ALL = AHEI_VEG + AHEI_FRT + AHEI_WGRAIN + AHEI_NUTSLEG + AHEI_N3FAT +
        AHEI_PUFA + AHEI_SSB_FRTJ + AHEI_REDPROC_MEAT + AHEI_TRANS + AHEI_SODIUM + AHEI_ALCOHOL,
      
      AHEI_NOETOH = AHEI_VEG + AHEI_FRT + AHEI_WGRAIN + AHEI_NUTSLEG + AHEI_N3FAT +
        AHEI_PUFA + AHEI_SSB_FRTJ + AHEI_REDPROC_MEAT + AHEI_TRANS + AHEI_SODIUM
    ) %>%
    dplyr::select(RESPONDENTID, GENDER, AHEI_ALL, AHEI_NOETOH, AHEI_VEG, AHEI_FRT, AHEI_WGRAIN, AHEI_NUTSLEG, AHEI_N3FAT,
                  AHEI_PUFA, AHEI_SSB_FRTJ, AHEI_REDPROC_MEAT, AHEI_TRANS, AHEI_SODIUM, AHEI_ALCOHOL)
  
}

#' DASH Calculation
#'
#' Calculate the DASH dietary index, Dietary Approaches to Stop Hypertension, using given the serving sizes of foods and nutrients consumed per 1 day
#' @import dplyr
#' @import readr
#' @import haven
#' @param SERV_DATA The raw data file that includes all the serving sizes of foods and nutrients
#' @param RESPONDENTID The unique participant ID for each participant
#' @param FRT_FRTJ_SERV_DASH The serving size of fruits and 100\% fruit juice, unit=servings/day (0.5 c of berries; 1 cup=236.59 g; 1 med fruit (1 cup = 236.59 g); 1 cup fruit juice
#' @param VEG_SERV_DASH The serving size of All vegetable except potatoes and legume, unit=servings/day (0.5 c of vege; 1 cup of green leafy (1 cup = 236.59 g)
#' @param NUTSLEG_SERV_DASH The serving size of Nuts, legumes, and vegetable protein (e.g., tofu), unit=servings/day = 1 srv=1oz (28.35 g) of nuts or 1 TBLSP peanut butter (15 mL), 1 cup legume = 4 oz
#' @param WGRAIN_SERV_DASH The serving size of whole grains, unit=1oz
#' @param LOWF_DAIRY_SERV_DASH The serving size of low fat dairy, including 2\% or less fat milk + yogurt + low-fat ice cream and frozen yogurt + low-fat cheese, unit=servings/day = 1 glass milk + 1 cup yogurt + 1/2 cup ice cream/frozen yogurt + 1 slice cheese
#' @param SODIUM_SERV_DASH The serving size of sodium, unit=mg/day
#' @param REDPROC_MEAT_SERV_DASH The serving size of red and processed meats, including Beef, pork, lamb, goat, veal, sausages, bacon, salami, ham, hot dog, deli meat, unit=servings/day; 1 srv= 4 oz. unprocessed meat; 1.5 oz. processed meat (1 oz. = 28.35 g)
#' @param SSB_FRTJ_SERV_DASH The serving size of sugar-sweetened beverages and non-100\% fruit juice, unit=servings/day = 1 ser= 8oz (1 oz. = 28.35 g)
#' @return The DASH index/score
#' @examples
#' DASH(SERV_DATA, SERV_DATA$RESPONDENTID, SERV_DATA$FRT_FRTJ_SERV_DASH, SERV_DATA$VEG_SERV_DASH, SERV_DATA$NUTSLEG_SERV_DASH, SERV_DATA$WGRAIN_SERV_DASH, SERV_DATA$LOWF_DAIRY_SERV_DASH, SERV_DATA$SODIUM_SERV_DASH, SERV_DATA$REDPROC_MEAT_SERV_DASH, SERV_DATA$SSB_FRTJ_SERV_DASH)
#' @export

#Score calculation for DASH
DASH = function(SERV_DATA, RESPONDENTID, FRT_FRTJ_SERV_DASH, VEG_SERV_DASH, NUTSLEG_SERV_DASH, WGRAIN_SERV_DASH, LOWF_DAIRY_SERV_DASH,
                SODIUM_SERV_DASH, REDPROC_MEAT_SERV_DASH, SSB_FRTJ_SERV_DASH){
  ##Create variables and functions needed for DASH calculation
  quintile_healthy = function(actual){
    quintile = quantile(actual, probs=seq(0, 1, by=0.2))
    case_when(
      actual <= quintile[6] & actual >= quintile[5] ~ 5,
      actual < quintile[5] & actual >= quintile[4] ~ 4,
      actual < quintile[4] & actual >= quintile[3] ~ 3,
      actual < quintile[3] & actual >= quintile[2] ~ 2,
      actual < quintile[2] & actual >= quintile[1] ~ 1
    )
  }

  quintile_unhealthy = function(actual){
    quintile = quantile(actual, probs=seq(0, 1, by=0.2))
    case_when(
      actual <= quintile[6] & actual >= quintile[5] ~ 1,
      actual < quintile[5] & actual >= quintile[4] ~ 2,
      actual < quintile[4] & actual >= quintile[3] ~ 3,
      actual < quintile[3] & actual >= quintile[2] ~ 4,
      actual < quintile[2] & actual >= quintile[1] ~ 5
    )
  }

  print("Reminder: this DASH index uses quintiles to rank participants' food/drink serving sizes and then calculate DASH component scores, which may generate results that are specific to your study population but not comparable to other populations.")
  
  ##DASH calculation
  SERV_DATA %>%
    dplyr::mutate(
      RESPONDENTID = RESPONDENTID,
      
      DASH_FRT = quintile_healthy(FRT_FRTJ_SERV_DASH),
      DASH_VEG = quintile_healthy(VEG_SERV_DASH),
      DASH_NUTSLEG = quintile_healthy(NUTSLEG_SERV_DASH),
      DASH_WGRAIN = quintile_healthy(WGRAIN_SERV_DASH),
      DASH_LOWF_DAIRY = quintile_healthy(LOWF_DAIRY_SERV_DASH),
      DASH_SODIUM = quintile_unhealthy(SODIUM_SERV_DASH),
      DASH_REDPROC_MEAT = quintile_unhealthy(REDPROC_MEAT_SERV_DASH),
      DASH_SSB_FRTJ = quintile_unhealthy(SSB_FRTJ_SERV_DASH),
      DASH_ALL = DASH_FRT+DASH_VEG+DASH_NUTSLEG+DASH_WGRAIN+DASH_LOWF_DAIRY+
        DASH_SODIUM+DASH_REDPROC_MEAT+DASH_SSB_FRTJ
    )%>%
    dplyr::select(RESPONDENTID, DASH_ALL, DASH_FRT, DASH_VEG, DASH_NUTSLEG, DASH_WGRAIN, DASH_LOWF_DAIRY,
           DASH_SODIUM, DASH_REDPROC_MEAT, DASH_SSB_FRTJ)
}

#' DASHI Calculation
#'
#' Calculate the DASHI dietary index (serving size-based), Dietary Approaches to Stop Hypertension, using given the serving sizes of foods and nutrients consumed per 1 day
#' @import dplyr
#' @import readr
#' @import haven
#' @param SERV_DATA The raw data file that includes all the serving sizes of foods and nutrients
#' @param RESPONDENTID The unique participant ID for each participant
#' @param VEG_SERV_DASHI The serving size of All vegetable except potatoes and legume, unit=servings/day (0.5 c of vege; 1 cup of green leafy (1 cup = 236.59 g)
#' @param FRT_FRTJ_SERV_DASHI The serving size of All whole fruits + 100\% juice,  unit=servings/day (0.5 c of berries; 1 cup=236.59 g; 1 med fruit (1 cup = 236.59 g); 1 cup fruit juice
#' @param NUTSLEG_SERV_DASHI The serving size of Nuts, legumes, and vegetable protein (e.g., tofu), unit=servings/day = 1 srv=1oz (28.35 g) of nuts or 1 TBLSP peanut butter (15 mL), 1 cup legume = 4 oz
#' @param LOWF_DAIRY_SERV_DASHI The serving size of low fat dairy, including 2\% or less fat milk + yogurt + low-fat ice cream and frozen yogurt + low-fat cheese, unit=servings/day = 1 glass milk + 1 cup yogurt + 1/2 cup ice cream/frozen yogurt + 1 slice cheese
#' @param WGRAIN_SERV_DASHI The serving size of whole grains, unit=1oz
#' @param ALLMEAT_SERV_DASHI The serving size of all meat consumption, including meat, fish, and poultry, unit=servings/day = 1oz
#' @param REDPROC_MEAT_SERV_DASHI The serving size of red and processed meats, including Beef, pork, lamb, goat, veal, sausages, bacon, salami, ham, hot dog, deli meat, servings/day; 1 srv= 4 oz. unprocessed meat; 1.5 oz. processed meat (1 oz. = 28.35 g)
#' @param FATOIL_SERV_DASHI The serving size of discretionary fats and oils, including added Plant oil + Animal fat, unit=servings/day = 1tbsp = 14 g
#' @param ADDEDSUGAR_SERV_DASHI The serving size of added sugar, unit=\% of total energy, 1 tsp = 4g, 1g = 4kcal
#' @param SODIUM_SERV_DASHI The serving size of sodium, unit=mg/day
#' @return The DASHI index/score
#' @examples
#' DASHI(SERV_DATA, SERV_DATA$RESPONDENTID, SERV_DATA$VEG_SERV_DASHI, SERV_DATA$FRT_FRTJ_SERV_DASHI, SERV_DATA$NUTSLEG_SERV_DASHI, SERV_DATA$LOWF_DAIRY_SERV_DASHI, SERV_DATA$WGRAIN_SERV_DASHI, SERV_DATA$ALLMEAT_SERV_DASHI, SERV_DATA$REDPROC_MEAT_SERV_DASHI, SERV_DATA$FATOIL_SERV_DASHI, SERV_DATA$ADDEDSUGAR_SERV_DASHI, SERV_DATA$SODIUM_SERV_DASHI)
#' @export

#Score calculation for DASHI
DASHI = function(SERV_DATA, RESPONDENTID, VEG_SERV_DASHI, FRT_FRTJ_SERV_DASHI, NUTSLEG_SERV_DASHI, LOWF_DAIRY_SERV_DASHI, WGRAIN_SERV_DASHI,
                 ALLMEAT_SERV_DASHI, REDPROC_MEAT_SERV_DASHI, FATOIL_SERV_DASHI, ADDEDSUGAR_SERV_DASHI, SODIUM_SERV_DASHI){
  ##Create variables and functions needed for DASHI calculation
  DASHI_MIN = 0
  DASHI_MAX = 5

  DASHI_MIN_VEG_SERV = 0
  DASHI_MAX_VEG_SERV = 4
  DASHI_MIN_FRT_FRTJ_SERV = 0
  DASHI_MAX_FRT_FRTJ_SERV = 4
  DASHI_MIN_NUTSLEG_SERV = 0
  DASHI_MAX_NUTSLEG_SERV = 4/7
  DASHI_MIN_LOWF_DAIRY_SERV = 0
  DASHI_MAX_LOWF_DAIRY_SERV = 2
  DASHI_MIN_WGRAIN_SERV = 0
  DASHI_MAX_WGRAIN_SERV = 3

  DASHI_MIN_ALLMEAT_SERV = 6
  DASHI_MAX_ALLMEAT_SERV= 2
  DASHI_MIN_REDPROC_MEAT_SERV = 1.5
  DASHI_MAX_REDPROC_MEAT_SERV = 0.5
  DASHI_MIN_FATOIL_SERV = 3
  DASHI_MAX_FATOIL_SERV = 2
  DASHI_MIN_ADDEDSUGAR_SERV = 10
  DASHI_MAX_ADDEDSUGAR_SERV = 0
  DASHI_MIN_SODIUM_SERV = 2300
  DASHI_MAX_SODIUM_SERV = 0

  DASHI_HEALTHY = function(actual, min, max){
    case_when(
      actual >= max ~ DASHI_MAX,
      actual <= min ~ DASHI_MIN,
      TRUE ~ DASHI_MIN+(actual-min)*DASHI_MAX/(max-min)
    )
  }

  DASHI_UNHEALTHY = function(actual, min, max){
    case_when(
      actual >= min ~ DASHI_MIN ,
      actual <= max ~ DASHI_MAX,
      TRUE ~ DASHI_MIN+(actual-min)*DASHI_MAX/(max-min)
    )
  }

  
  SCORE_HEALTHY = function(actual_serv, min_serv, max_serv, min_score, max_score){
    case_when(
      actual_serv >= max_serv ~ max_score,
      actual_serv <= min_serv ~ min_score,
      TRUE ~ min_score+(actual_serv-min_serv)*max_score/(max_serv-min_serv)
    )
  }
  
  
  SCORE_UNHEALTHY = function(actual_serv, min_serv, max_serv, min_score, max_score){
    case_when(
      actual_serv >= min_serv ~ min_score ,
      actual_serv <= max_serv ~ max_score,
      TRUE ~ min_score+(actual_serv-min_serv)*max_score/(max_serv-min_serv)
    )
  }
  ##DASHI calculation
  SERV_DATA %>%
    dplyr::mutate(
      RESPONDENTID = RESPONDENTID,
      
      DASHI_VEG = DASHI_HEALTHY(VEG_SERV_DASHI, DASHI_MIN_VEG_SERV, DASHI_MAX_VEG_SERV),
      DASHI_FRT = DASHI_HEALTHY(FRT_FRTJ_SERV_DASHI, DASHI_MIN_FRT_FRTJ_SERV, DASHI_MAX_FRT_FRTJ_SERV),
      DASHI_NUTSLEG = DASHI_HEALTHY(NUTSLEG_SERV_DASHI, DASHI_MIN_NUTSLEG_SERV, DASHI_MAX_NUTSLEG_SERV),
      DASHI_LOWFATDAIRY = DASHI_HEALTHY(LOWF_DAIRY_SERV_DASHI, DASHI_MIN_LOWF_DAIRY_SERV, DASHI_MAX_LOWF_DAIRY_SERV),
      DASHI_WGRAIN = DASHI_HEALTHY(WGRAIN_SERV_DASHI, DASHI_MIN_WGRAIN_SERV, DASHI_MAX_WGRAIN_SERV),

      DASHI_ALLMEAT = DASHI_UNHEALTHY(ALLMEAT_SERV_DASHI, DASHI_MIN_ALLMEAT_SERV, DASHI_MAX_ALLMEAT_SERV),
      DASHI_REDPROC_MEAT = DASHI_UNHEALTHY(REDPROC_MEAT_SERV_DASHI, DASHI_MIN_REDPROC_MEAT_SERV, DASHI_MAX_REDPROC_MEAT_SERV),
      DASHI_FATOIL = DASHI_UNHEALTHY(FATOIL_SERV_DASHI, DASHI_MIN_FATOIL_SERV, DASHI_MAX_FATOIL_SERV),
      DASHI_ADDEDSUGAR = DASHI_UNHEALTHY(ADDEDSUGAR_SERV_DASHI, DASHI_MIN_ADDEDSUGAR_SERV, DASHI_MAX_ADDEDSUGAR_SERV),
      DASHI_SODIUM = DASHI_UNHEALTHY(SODIUM_SERV_DASHI, DASHI_MIN_SODIUM_SERV, DASHI_MAX_SODIUM_SERV),
      DASHI_ALL= DASHI_VEG + DASHI_FRT + DASHI_NUTSLEG + DASHI_LOWFATDAIRY +
        DASHI_WGRAIN + DASHI_ALLMEAT + DASHI_REDPROC_MEAT + DASHI_FATOIL + DASHI_ADDEDSUGAR + DASHI_SODIUM
    )%>%
    dplyr::select(RESPONDENTID, DASHI_ALL, DASHI_VEG, DASHI_FRT, DASHI_NUTSLEG, DASHI_LOWFATDAIRY, DASHI_WGRAIN,
           DASHI_ALLMEAT, DASHI_REDPROC_MEAT, DASHI_FATOIL, DASHI_ADDEDSUGAR, DASHI_SODIUM)
}

#' MED Calculation
#'
#' Calculate the MED dietary index, Mediterranean, using given the serving sizes of foods and nutrients consumed per 1 day
#' @import dplyr
#' @import readr
#' @import haven
#' @param SERV_DATA The raw data file that includes all the serving sizes of foods and nutrients
#' @param RESPONDENTID The unique participant ID for each participant
#' @param FRT_FRTJ_SERV_MED The serving size of All fruits and 100\% fruit juices, unit=servings/day (0.5 c of berries; 1 cup=236.59 g; 1 med fruit (1 cup = 236.59 g); 1 cup fruit juice
#' @param VEG_SERV_MED The serving size of All vegetables except potatoes and legumes, unit=0.5 c of vege; 1 cup of green leafy
#' @param WGRAIN_SERV_MED The serving size of whole grains, including Whole-grain ready-to-eat cereals, cooked cereals, crackers, dark breads, brown rice, other grains, wheat germ, bran, popcorn, unit=1oz
#' @param LEGUMES_SERV_MED The serving size of legumes, including Tofu, string beans, peas, beans, unit=*oz, 1 cup legume = 4 oz
#' @param NUTS_SERV_MED The serving size of nuts, including Nuts, peanut butter, unit=1oz
#' @param FISH_SERV_MED The serving size of all fish, including Fish and shrimp, breaded fish, unit=4oz
#' @param REDPROC_MEAT_SERV_MED The serving size of red and processed meats, including Beef, pork, lamb, goat, veal, sausages, bacon, salami, ham, hot dog, deli meat, unit=serving (4 oz. unprocessed meat; 1.5 oz. processed meat)
#' @param MONSATFAT_SERV_MED The serving size of the ratio of monounsaturated fat to saturated fat, unit=ratio
#' @param ALCOHOL_SERV_MED The serving size of alcohol, unit=13g
#' @return The MED index/score
#' @examples
#' MED(SERV_DATA, SERV_DATA$RESPONDENTID, SERV_DATA$FRT_FRTJ_SERV_MED, SERV_DATA$VEG_SERV_MED, SERV_DATA$WGRAIN_SERV_MED, SERV_DATA$LEGUMES_SERV_MED, SERV_DATA$NUTS_SERV_MED,FISH_SERV_MED, SERV_DATA$REDPROC_MEAT_SERV_MED, SERV_DATA$MONSATFAT_SERV_MED, SERV_DATA$ALCOHOL_SERV_MED)
#' @export

#Score calculation for MED
MED = function(SERV_DATA, RESPONDENTID, FRT_FRTJ_SERV_MED, VEG_SERV_MED, WGRAIN_SERV_MED, LEGUMES_SERV_MED, NUTS_SERV_MED,
               FISH_SERV_MED, REDPROC_MEAT_SERV_MED, MONSATFAT_SERV_MED, ALCOHOL_SERV_MED){
  ##Create variables and functions needed for MED
  median_healthy = function(actual){
    median_score = median(actual)
    case_when(
      actual < median_score ~ 0,
      actual >= median_score ~ 1
    )
  }

  median_unhealthy = function(actual){
    median_score = median(actual)
    case_when(
      actual < median_score ~ 1,
      actual >= median_score ~ 0
    )
  }

  print("Reminder: this MED index uses medians to rank participants' food/drink serving sizes and then calculate MED component scores, which may generate results that are specific to your study population but not comparable to other populations.")
  
  
  SERV_DATA %>%
    dplyr::mutate(
      RESPONDENTID = RESPONDENTID,
      
      MED_FRT = median_healthy(FRT_FRTJ_SERV_MED),
      MED_VEG = median_healthy(VEG_SERV_MED),
      MED_WGRAIN = median_healthy(WGRAIN_SERV_MED),
      MED_LEGUMES = median_healthy(LEGUMES_SERV_MED),
      MED_NUTS = median_healthy(NUTS_SERV_MED),
      MED_FISH = median_healthy(FISH_SERV_MED),
      MED_REDPROC_MEAT = median_unhealthy(REDPROC_MEAT_SERV_MED),
      MED_MONSATFAT = median_healthy(MONSATFAT_SERV_MED),
      MED_ALCOHOL = case_when(
        ALCOHOL_SERV_MED <=25 & ALCOHOL_SERV_MED >= 10 ~ 1, 
        TRUE ~ 0),

      MED_ALL = MED_FRT+MED_VEG+MED_WGRAIN+MED_LEGUMES+MED_NUTS+MED_FISH+MED_REDPROC_MEAT+MED_MONSATFAT+MED_ALCOHOL,
      MED_NOETOH = MED_FRT+MED_VEG+MED_WGRAIN+MED_LEGUMES+MED_NUTS+MED_FISH+MED_REDPROC_MEAT+MED_MONSATFAT
    )%>%
    dplyr::select(RESPONDENTID, MED_ALL, MED_NOETOH, MED_FRT, MED_VEG, MED_WGRAIN, MED_LEGUMES, MED_NUTS,
           MED_FISH, MED_REDPROC_MEAT, MED_MONSATFAT, MED_ALCOHOL)
}

#' MEDI Calculation
#'
#' Calculate the MEDI dietary index (serving size-based), Mediterranean, using given the serving sizes of foods and nutrients consumed per 1 day
#' @import dplyr
#' @import readr
#' @import haven
#' @param SERV_DATA The raw data file that includes all the serving sizes of foods and nutrients
#' @param RESPONDENTID The unique participant ID for each participant
#' @param FRT_FRTJ_SERV_MEDI The serving size of All fruits and 100\% fruit juices, unit=servings/day (0.5 c of berries; 1 cup=236.59 g; 1 med fruit (1 cup = 236.59 g); 1 cup fruit juice
#' @param VEG_SERV_MEDI The serving size of All raw and cooked vegetables, unit=0.5 c of vege; 1 cup of green leafy
#' @param LEGUMES_SERV_MEDI The serving size of legumes, including Dried beans, lentils, peas, soups (split pea), tofu, soymilk, unit=1oz (28.35 g) of nuts and legume or 1 TBLSP peanut butter (15 mL), 1 cup legume = 4 oz
#' @param WGRAIN_SERV_MEDI The serving size of whole grains, including Whole wheat bread and flour, whole wheat pasta, brown rice, rusks,  whole grain breakfast cereals, couscous, semolina, unit=1 oz
#' @param FISH_SERV_MEDI The serving size of all fish, including Fresh-water and sea-water fish; preserved fish such as salted fish, canned fish; shellfish (squid, prawns, mollusks), unit=serving (4 oz)
#' @param DAIRY_SERV_MEDI The serving size of all dairy, including Milk, yogurt, cheese, custard, milk puddings, other milk products, unit=1 serving = 1 cup milk, 1 cup yogurt, 1 ½ ounces hard cheese (cheddar, mozzarella, Swiss, Parmesan), 1/3 cup shredded cheese, 2 ounces processed cheese (American), ½ cup ricotta cheese, 2 cups cottage cheese, 1 cup pudding made with milk, 1 cup frozen yogurt, 1 ½ cups ice cream
#' @param REDPROC_MEAT_SERV_MEDI The serving size of red and processed meats, including Beef, pork, lamb, goat, veal, sausages, bacon, salami, ham, hot dog, deli meat, unit=serving (4 oz. unprocessed meat; 1.5 oz. processed meat)
#' @param NUTS_SERV_MEDI The serving size of nuts, including Peanuts, almonds, sunflower seeds, cashews, walnuts, unit=1oz
#' @param MONSATFAT_SERV_MEDI The serving size of the ratio of monounsaturated fat to saturated fat, unit=ratio
#' @param ALCOHOL_SERV_MEDI The serving size of alcohol, including Wine, beer, "light" beer, liquor, unit=13g
#' @return The MEDI index/score
#' @examples
#' MED(SERV_DATA, SERV_DATA$RESPONDENTID, SERV_DATA$FRT_FRTJ_SERV_MEDI, SERV_DATA$VEG_SERV_MEDI, SERV_DATA$WGRAIN_SERV_MEDI, SERV_DATA$LEGUMES_SERV_MEDI, SERV_DATA$NUTS_SERV_MEDI,FISH_SERV_MEDI, SERV_DATA$REDPROC_MEAT_SERV_MEDI, SERV_DATA$MONSATFAT_SERV_MEDI, SERV_DATA$ALCOHOL_SERV_MEDI)
#' @export

#Score calculation for MEDI
MEDI = function(SERV_DATA, RESPONDENTID, FRT_FRTJ_SERV_MEDI, VEG_SERV_MEDI, LEGUMES_SERV_MEDI, WGRAIN_SERV_MEDI, FISH_SERV_MEDI, DAIRY_SERV_MEDI, REDPROC_MEAT_SERV_MEDI,
                NUTS_SERV_MEDI, MONSATFAT_SERV_MEDI, ALCOHOL_SERV_MEDI){
  SERV_DATA %>%
    dplyr::mutate(
      RESPONDENTID = RESPONDENTID,
      
      MEDI_FRT = case_when(FRT_FRTJ_SERV_MEDI >=3 ~ 1, TRUE ~ 0),
      MEDI_VEG = case_when(VEG_SERV_MEDI >= 3 ~ 1, TRUE ~ 0),
      MEDI_LEGUMES = case_when(LEGUMES_SERV_MEDI >= 1.5 ~ 1, TRUE ~ 0),
      MEDI_WGRAIN = case_when(WGRAIN_SERV_MEDI >= 3 ~ 1, TRUE ~ 0),
      MEDI_FISH = case_when(FISH_SERV_MEDI >= 2 ~ 1, TRUE ~ 0),
      MEDI_DAIRY = case_when(DAIRY_SERV_MEDI >= 2 ~ 1, TRUE ~ 0),
      MEDI_REDPROC_MEAT = case_when(REDPROC_MEAT_SERV_MEDI < 4.5 ~ 1, TRUE ~ 0),
      MEDI_NUTS = case_when(NUTS_SERV_MEDI >= 2 ~ 1, TRUE ~ 0),
      MEDI_MONSATFAT = case_when(MONSATFAT_SERV_MEDI >= 1.6 ~ 1, TRUE ~ 0),
      MEDI_ALCOHOL = case_when(ALCOHOL_SERV_MEDI <=25 & ALCOHOL_SERV_MEDI >= 10 ~ 1, TRUE ~ 0),

      MEDI_ALL = MEDI_FRT+MEDI_VEG+MEDI_LEGUMES+MEDI_WGRAIN+MEDI_FISH+MEDI_DAIRY+MEDI_REDPROC_MEAT+
        MEDI_NUTS+MEDI_MONSATFAT+MEDI_ALCOHOL,
      MEDI_NOETOH = MEDI_FRT+MEDI_VEG+MEDI_LEGUMES+MEDI_WGRAIN+MEDI_FISH+MEDI_DAIRY+MEDI_REDPROC_MEAT
      +MEDI_NUTS+MEDI_MONSATFAT
    )%>%
    dplyr::select(RESPONDENTID, MEDI_ALL, MEDI_NOETOH, MEDI_FRT, MEDI_VEG, MEDI_LEGUMES, MEDI_WGRAIN, MEDI_FISH,
           MEDI_DAIRY, MEDI_REDPROC_MEAT, MEDI_NUTS, MEDI_MONSATFAT, MEDI_ALCOHOL)
}

#' AHEIP Calculation
#'
#' Calculate the AHEIP dietary index (serving size-based), Mediterranean, using given the serving sizes of foods and nutrients consumed per 1 day
#' @import dplyr
#' @import readr
#' @import haven
#' @param SERV_DATA The raw data file that includes all the serving sizes of foods and nutrients
#' @param RESPONDENTID The unique participant ID for each participant
#' @param VEG_SERV_AHEIP The serving size of All vegetable except potatoes and legume, unit=servings/day (0.5 c of vege; 1 cup of green leafy (1 cup = 236.59 g)
#' @param FRT_SERV_AHEIP The serving size of All whole fruits and no fruit juice, unit=servings/day (0.5 c of berries; 1 cup=236.59 g; 1 med fruit (1 cup = 236.59 g)
#' @param WHITERED_RT_SERV_AHEIP The serving size of the ratio of white and red meats, White meat = poultry + all fish, red meat = pork + beef + lamb + all organ meats + processed meat, unit=servings/day; 1 srv= 4 oz. unprocessed meat; 1.5 oz. processed meat (1 oz. = 28.35 g)
#' @param FIBER_SERV_AHEIP The serving size of fibers, unit=grams/day
#' @param TRANS_SERV_AHEIP The serving size of trans fat, unit='\% of energy'
#' @param POLYSAT_RT_SERV_AHEIP The serving size of polyunsaturated/saturated fats, unit=ratio
#' @param CALCIUM_SERV_AHEIP The serving size of calcium, unit=mg/day
#' @param FOLATE_SERV_AHEIP The serving size of folate, unit=mcg/day
#' @param IRON_SERV_AHEIP The serving size of iron, unit=mg/day
#' @return The AHEIP index/score
#' @examples
#' AHEIP(SERV_DATA, SERV_DATA$RESPONDENTID, SERV_DATA$VEG_SERV_AHEIP, SERV_DATA$FRT_SERV_AHEIP, SERV_DATA$WHITERED_RT_SERV_AHEIP, SERV_DATA$FIBER_SERV_AHEIP, SERV_DATA$TRANS_SERV_AHEIP, SERV_DATA$POLYSAT_RT_SERV_AHEIP, SERV_DATA$CALCIUM_SERV_AHEIP, SERV_DATA$FOLATE_SERV_AHEIP, SERV_DATA$IRON_SERV_AHEIP)
#' @export


#Score calculation for AHEIP
AHEIP = function(SERV_DATA, RESPONDENTID, VEG_SERV_AHEIP, FRT_SERV_AHEIP, WHITERED_RT_SERV_AHEIP, FIBER_SERV_AHEIP, TRANS_SERV_AHEIP, POLYSAT_RT_SERV_AHEIP,
                 CALCIUM_SERV_AHEIP, FOLATE_SERV_AHEIP, IRON_SERV_AHEIP) {

  ##Create variables and functions needed for AHEIP calculation
  AHEIP_MIN = 0
  AHEIP_MAX = 10

  AHEIP_MIN_VEG_SERV = 0
  AHEIP_MAX_VEG_SERV = 5
  AHEIP_MIN_FRT_SERV = 0
  AHEIP_MAX_FRT_SERV = 4
  AHEIP_MIN_WHITERED_SERV = 0
  AHEIP_MAX_WHITERED_SERV = 4
  AHEIP_MIN_FIBER_SERV = 0
  AHEIP_MAX_FIBER_SERV = 25

  AHEIP_MIN_TRANS_SERV = 4
  AHEIP_MAX_TRANS_SERV = 0.5
  AHEIP_MIN_POLYSAT_SERV = 0.1
  AHEIP_MAX_POLYSAT_SERV = 1
  AHEIP_MIN_CALCIUM_SERV = 0
  AHEIP_MAX_CALCIUM_SERV = 1200
  AHEIP_MIN_FOLATE_SERV = 0
  AHEIP_MAX_FOLATE_SERV = 600
  AHEIP_MIN_IRON_SERV = 0
  AHEIP_MAX_IRON_SERV = 27

  AHEIP_HEALTHY = function(actual, min, max){
    case_when(
      actual >= max ~ AHEIP_MAX,
      actual <= min ~ AHEIP_MIN,
      TRUE ~ AHEIP_MIN+(actual-min)*AHEIP_MAX/(max-min)
    )
  }

  AHEIP_UNHEALTHY = function(actual, min, max){
    case_when(
      actual >= min ~ AHEIP_MIN ,
      actual <= max ~ AHEIP_MAX,
      TRUE ~ AHEIP_MIN+(actual-min)*AHEIP_MAX/(max-min)
    )
  }

  SERV_DATA %>%
    dplyr::mutate(
      RESPONDENTID = RESPONDENTID,
      
      AHEIP_VEG = AHEIP_HEALTHY(VEG_SERV_AHEIP, AHEIP_MIN_VEG_SERV, AHEIP_MAX_VEG_SERV),
      AHEIP_FRT = AHEIP_HEALTHY(FRT_SERV_AHEIP, AHEIP_MIN_FRT_SERV, AHEIP_MAX_FRT_SERV),
      AHEIP_WHITEREAD = AHEIP_HEALTHY(WHITERED_RT_SERV_AHEIP, AHEIP_MIN_WHITERED_SERV, AHEIP_MAX_WHITERED_SERV),
      AHEIP_FIBER = AHEIP_HEALTHY(FIBER_SERV_AHEIP, AHEIP_MIN_FIBER_SERV, AHEIP_MAX_FIBER_SERV),
      AHEIP_TRANS = AHEIP_UNHEALTHY(TRANS_SERV_AHEIP, AHEIP_MIN_TRANS_SERV, AHEIP_MAX_TRANS_SERV),
      AHEIP_POLYSAT = AHEIP_HEALTHY(POLYSAT_RT_SERV_AHEIP, AHEIP_MIN_POLYSAT_SERV, AHEIP_MAX_POLYSAT_SERV),
      AHEIP_CALCIUM = AHEIP_HEALTHY(CALCIUM_SERV_AHEIP, AHEIP_MIN_CALCIUM_SERV, AHEIP_MAX_CALCIUM_SERV),
      AHEIP_FOLATE= AHEIP_HEALTHY(FOLATE_SERV_AHEIP, AHEIP_MIN_FOLATE_SERV, AHEIP_MAX_FOLATE_SERV),
      AHEIP_IRON = AHEIP_HEALTHY(IRON_SERV_AHEIP, AHEIP_MIN_IRON_SERV, AHEIP_MAX_IRON_SERV),
      AHEIP_ALL = AHEIP_VEG + AHEIP_FRT + AHEIP_WHITEREAD + AHEIP_FIBER + AHEIP_TRANS +
        AHEIP_POLYSAT + AHEIP_CALCIUM + AHEIP_FOLATE + AHEIP_IRON
    )%>%
    dplyr::select(RESPONDENTID, AHEIP_ALL, AHEIP_VEG, AHEIP_FRT, AHEIP_WHITEREAD, AHEIP_FIBER, AHEIP_TRANS,
           AHEIP_POLYSAT, AHEIP_CALCIUM, AHEIP_FOLATE, AHEIP_IRON)
}

#' HEI2015 Calculation
#'
#' Calculate the HEI2015 dietary index, Healthy eating index 2015, using given the serving sizes of foods and nutrients consumed per 1 day
#' @import dplyr
#' @import readr
#' @import haven
#' @param SERV_DATA The raw data file that includes all the serving sizes of foods and nutrients
#' @param RESPONDENTID The unique participant ID for each participant
#' @param TOTALKCAL_HEI2015 The total calorie from all foods and drinks 
#' @param TOTALFRT_SERV_HEI2015 The serving size of total fruits including fruit juice, unit= cup eq.
#' @param FRT_SERV_HEI2015 The serving size of Citrus, Melons, Berries + Other Intact Fruits, unit= cup eq.
#' @param VEG_SERV_HEI2015 The serving size of vegetables Total Vegetables + Legumes (Beans and Peas) in cup equivalents, unit= cup eq.
#' @param GREENNBEAN_SERV_HEI2015 The serving size of Dark Green Vegetables + Legumes (Beans and Peas) in cup equivalents, unit= cup eq.
#' @param TOTALPRO_SERV_HEI2015 The serving size of Total Meat, Poultry, and Seafood (including organ meats and cured meats) + Eggs + Nuts and Seeds + Soy + Legumes (Beans and Peas) in oz equivalents, unit=oz. eq., 1 cup legume = 4 oz
#' @param SEAPLANTPRO_SERV_HEI2015 The serving size of Seafood (high in n-3) + Seafood (low in n-3) + Soy + Nuts and Seeds + Legumes (Beans and Peas) in oz equivalents, unit=oz. eq., 1 cup legume = 4 oz
#' @param WHOLEGRAIN_SERV_HEI2015 The serving size of whole grains, unit=oz. eq.
#' @param DAIRY_SERV_HEI2015 The serving size of all dairy, unit=cup eq.
#' @param FATTYACID_SERV_HEI2015 The serving size of (Total Monounsaturated Fatty Acids + Total Polyunsaturated Fatty Acids)/Total Saturated Fatty Acids, unit=g
#' @param REFINEDGRAIN_SERV_HEI2015 The serving size of refined grains, unit=oz. eq.
#' @param SODIUM_SERV_HEI2015 The serving size of sodium, unit=g
#' @param ADDEDSUGAR_SERV_HEI2015 The serving size of added sugar, unit=\% of total energy, 1 tsp = 4g, 1g = 4kcal
#' @param SATFAT_SERV_HEI2015 The serving size of Total Saturated Fatty Acids, unit=\% of energy, 1g = 9 kcal
#' @return The HEI2015 index/score
#' @examples
#' HEI2015(SERV_DATA, SERV_DATA$RESPONDENTID, SERV_DATA$TOTALKCAL_HEI2015, SERV_DATA$TOTALFRT_SERV_HEI2015, SERV_DATA$FRT_SERV_HEI2015, SERV_DATA$VEG_SERV_HEI2015, SERV_DATA$GREENNBEAN_SERV_HEI2015, SERV_DATA$TOTALPRO_SERV_HEI2015,  SERV_DATA$SEAPLANTPRO_SERV_HEI2015, SERV_DATA$WHOLEGRAIN_SERV_HEI2015, SERV_DATA$DAIRY_SERV_HEI2015, SERV_DATA$FATTYACID_SERV_HEI2015, SERV_DATA$REFINEDGRAIN_SERV_HEI2015,  SERV_DATA$SODIUM_SERV_HEI2015, SERV_DATA$ADDEDSUGAR_SERV_HEI2015, SERV_DATA$SATFAT_SERV_HEI2015)
#' @export

#Score calculation for HEI2015
HEI2015 = function(SERV_DATA, RESPONDENTID, TOTALKCAL_HEI2015, TOTALFRT_SERV_HEI2015, FRT_SERV_HEI2015, VEG_SERV_HEI2015, GREENNBEAN_SERV_HEI2015, TOTALPRO_SERV_HEI2015,
                   SEAPLANTPRO_SERV_HEI2015, WHOLEGRAIN_SERV_HEI2015, DAIRY_SERV_HEI2015, FATTYACID_SERV_HEI2015, REFINEDGRAIN_SERV_HEI2015,
                   SODIUM_SERV_HEI2015, ADDEDSUGAR_SERV_HEI2015, SATFAT_SERV_HEI2015){
  
  ##Create variables needed for HEI2015 calculation
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
  
  HEI2015_HEALTHY1 = function(actual, min, max){
    case_when(
      actual >= max ~ HEI2015_MAX1,
      actual <= min ~ HEI2015_MIN,
      TRUE ~ HEI2015_MIN+(actual-min)*HEI2015_MAX1/(max-min)
    )
  }
  
  HEI2015_HEALTHY2 = function(actual, min, max){
    case_when(
      actual >= max ~ HEI2015_MAX2,
      actual <= min ~ HEI2015_MIN,
      TRUE ~ HEI2015_MIN+(actual-min)*HEI2015_MAX2/(max-min)
    )
  }
  
  HEI2015_UNHEALTHY = function(actual, min, max){
    case_when(
      actual >= min ~ HEI2015_MIN,
      actual <= max ~ HEI2015_MAX2,
      TRUE ~ HEI2015_MIN+(actual-min)*HEI2015_MAX2/(max-min)
    )
  }
  
  SERV_DATA=SERV_DATA %>%
    dplyr::mutate(
      RESPONDENTID = RESPONDENTID,
      TOTALKCAL_HEI2015 = TOTALKCAL_HEI2015,
      TOTALFRT_SERV_HEI2015 = TOTALFRT_SERV_HEI2015/(TOTALKCAL_HEI2015/1000),
      FRT_SERV_HEI2015 = (FRT_SERV_HEI2015)/(TOTALKCAL_HEI2015/1000),
      VEG_SERV_HEI2015 = (VEG_SERV_HEI2015)/(TOTALKCAL_HEI2015/1000),
      GREENNBEAN_SERV_HEI2015 = (GREENNBEAN_SERV_HEI2015)/(TOTALKCAL_HEI2015/1000),
      TOTALPRO_SERV_HEI2015 = (TOTALPRO_SERV_HEI2015)/(TOTALKCAL_HEI2015/1000),
      SEAPLANTPRO_SERV_HEI2015 = (SEAPLANTPRO_SERV_HEI2015)/(TOTALKCAL_HEI2015/1000),
      WHOLEGRAIN_SERV_HEI2015 = WHOLEGRAIN_SERV_HEI2015/(TOTALKCAL_HEI2015/1000),
      DAIRY_SERV_HEI2015 = DAIRY_SERV_HEI2015/(TOTALKCAL_HEI2015/1000),
      FATTYACID_SERV_HEI2015 = FATTYACID_SERV_HEI2015,
      
      REFINEDGRAIN_SERV_HEI2015 = REFINEDGRAIN_SERV_HEI2015/(TOTALKCAL_HEI2015/1000),
      SODIUM_SERV_HEI2015 = (SODIUM_SERV_HEI2015)/(TOTALKCAL_HEI2015/1000),
      ADDEDSUGAR_SERV_HEI2015 = ((ADDEDSUGAR_SERV_HEI2015*4*4) / TOTALKCAL_HEI2015)*100,
      SATFAT_SERV_HEI2015 = ((SATFAT_SERV_HEI2015*9)/TOTALKCAL_HEI2015)*100,
      
      HEI2015_TOTALFRT = HEI2015_HEALTHY1(TOTALFRT_SERV_HEI2015, HEI2015_MIN_TOTALFRT_SERV, HEI2015_MAX_TOTALFRT_SERV),
      HEI2015_FRT = HEI2015_HEALTHY1(FRT_SERV_HEI2015, HEI2015_MIN_FRT_SERV, HEI2015_MAX_FRT_SERV),
      HEI2015_VEG = HEI2015_HEALTHY1(VEG_SERV_HEI2015, HEI2015_MIN_VEG_SERV, HEI2015_MAX_VEG_SERV),
      HEI2015_GREENNBEAN = HEI2015_HEALTHY1(GREENNBEAN_SERV_HEI2015, HEI2015_MIN_GREENNBEAN_SERV, HEI2015_MAX_GREENNBEAN_SERV),
      HEI2015_TOTALPRO = HEI2015_HEALTHY1(TOTALPRO_SERV_HEI2015, HEI2015_MIN_TOTALPRO_SERV, HEI2015_MAX_TOTALPRO_SERV),
      HEI2015_SEAPLANTPRO = HEI2015_HEALTHY1(SEAPLANTPRO_SERV_HEI2015, HEI2015_MIN_SEAPLANTPRO_SERV, HEI2015_MAX_SEAPLANTPRO_SERV),
      HEI2015_WHOLEGRAIN = HEI2015_HEALTHY2(WHOLEGRAIN_SERV_HEI2015, HEI2015_MIN_WHOLEGRAIN_SERV, HEI2015_MAX_WHOLEGRAIN_SERV),
      HEI2015_DAIRY = HEI2015_HEALTHY2(DAIRY_SERV_HEI2015, HEI2015_MIN_DAIRY_SERV, HEI2015_MAX_DAIRY_SERV),
      HEI2015_FATTYACID = HEI2015_HEALTHY2(FATTYACID_SERV_HEI2015, HEI2015_MIN_FATTYACID_SERV, HEI2015_MAX_FATTYACID_SERV),
      
      HEI2015_REFINEDGRAIN = HEI2015_UNHEALTHY(REFINEDGRAIN_SERV_HEI2015, HEI2015_MIN_REFINEDGRAIN_SERV, HEI2015_MAX_REFINEDGRAIN_SERV),
      HEI2015_SODIUM = HEI2015_UNHEALTHY(SODIUM_SERV_HEI2015, HEI2015_MIN_SODIUM_SERV, HEI2015_MAX_SODIUM_SERV),
      HEI2015_ADDEDSUGAR = HEI2015_UNHEALTHY(ADDEDSUGAR_SERV_HEI2015, HEI2015_MIN_ADDEDSUGAR_SERV, HEI2015_MAX_ADDEDSUGAR_SERV),
      HEI2015_SATFAT = HEI2015_UNHEALTHY(SATFAT_SERV_HEI2015, HEI2015_MIN_SATFAT_SERV, HEI2015_MAX_SATFAT_SERV),
      
      HEI2015_ALL= HEI2015_TOTALFRT + HEI2015_FRT + HEI2015_VEG + HEI2015_GREENNBEAN +
        HEI2015_TOTALPRO + HEI2015_SEAPLANTPRO + HEI2015_WHOLEGRAIN + HEI2015_DAIRY +
        HEI2015_FATTYACID + HEI2015_REFINEDGRAIN + HEI2015_SODIUM + HEI2015_ADDEDSUGAR +
        HEI2015_SATFAT
    ) 
  
  for(i in 1:length(SERV_DATA$TOTALKCAL_HEI2015)){
    if (SERV_DATA$TOTALKCAL_HEI2015[i] == 0){
      SERV_DATA$HEI2015_TOTALFRT[i] = 0
      SERV_DATA$HEI2015_FRT[i] = 0
      SERV_DATA$HEI2015_VEG[i] = 0
      SERV_DATA$HEI2015_GREENNBEAN[i] = 0
      SERV_DATA$HEI2015_TOTALPRO[i] = 0
      SERV_DATA$HEI2015_SEAPLANTPRO[i] = 0
      SERV_DATA$HEI2015_WHOLEGRAIN[i] = 0
      SERV_DATA$HEI2015_DAIRY[i] = 0
      SERV_DATA$HEI2015_FATTYACID[i] = 0
      SERV_DATA$HEI2015_REFINEDGRAIN[i] = 0
      SERV_DATA$HEI2015_ADDEDSUGAR[i] = 0
      SERV_DATA$HEI2015_ALL[i] = 0
    }
  }
  
  SERV_DATA %>%
    dplyr::select(RESPONDENTID, TOTALKCAL_HEI2015, HEI2015_ALL, HEI2015_TOTALFRT, HEI2015_FRT, HEI2015_VEG, HEI2015_GREENNBEAN,
                  HEI2015_TOTALPRO, HEI2015_SEAPLANTPRO, HEI2015_WHOLEGRAIN, HEI2015_DAIRY,
                  HEI2015_FATTYACID, HEI2015_REFINEDGRAIN, HEI2015_SODIUM, HEI2015_ADDEDSUGAR,
                  HEI2015_SATFAT)
}

#' DII
#'
#' Calculate the DII dietary index, Dietary Inflammation Index, using given the serving sizes of foods and nutrients consumed per 1 day. Not all parameters are needed. You can give as many parameters as you have, and the results will only include scores for the parameters you included.
#' @import dplyr
#' @import readr
#' @import haven
#' @param SERV_DATA The raw data file that includes all the serving sizes of foods and nutrients
#' @param RESPONDENTID The unique participant ID for each participant
#' @param REPEATNUM The number of repeated record with each participant, 1st collection=1, 2nd collection =2, etc. If no repeat number is given, the default is 1
#' @param ALCOHOL_DII Unit=g
#' @param VITB12_DII Unit=μg
#' @param VITB6_DII Unit=mg
#' @param BCAROTENE_DII Unit=μg
#' @param CAFFEINE_DII Unit=g
#' @param CARB_DII Unit=g
#' @param CHOLES_DII Unit=mg
#' @param KCAL_DII Unit=KCAL_DII
#' @param EUGENOL_DII Unit=mg
#' @param TOTALFAT_DII Unit=g
#' @param FIBER_DII Unit=g
#' @param FOLICACID_DII Unit=μg
#' @param GARLIC_DII Unit=g
#' @param GINGER_DII Unit=g
#' @param IRON_DII Unit=mg
#' @param MG_DII Unit=mg
#' @param MUFA_DII Unit=g
#' @param NIACIN_DII Unit=mg
#' @param N3FAT_DII Unit=g
#' @param N6FAT_DII Unit=g
#' @param ONION_DII Unit=g
#' @param PROTEIN_DII Unit=g
#' @param PUFA_DII Unit=g
#' @param RIBOFLAVIN_DII Unit=mg
#' @param SAFFRON_DII Unit=g
#' @param SATFAT_DII Unit=g
#' @param SE_DII Unit=μg
#' @param THIAMIN_DII Unit=mg
#' @param TRANSFAT_DII Unit=g
#' @param TURMERIC_DII Unit=mg
#' @param VITA_DII Unit=RE
#' @param VITC_DII Unit=mg
#' @param VITD_DII Unit=μg
#' @param VITE_DII Unit=mg
#' @param ZN_DII Unit=mg
#' @param TEA_DII Unit=g
#' @param FLA3OL_DII Unit=mg
#' @param FLAVONES_DII Unit=mg
#' @param FLAVONOLS_DII Unit=mg
#' @param FLAVONONES_DII Unit=mg
#' @param ANTHOC_DII Unit=mg
#' @param ISOFLAVONES_DII Unit=mg
#' @param PEPPER_DII Unit=g
#' @param THYME_DII Unit=mg
#' @param ROSEMARY_DII Unit=mg
#' @return The DII index/score
#' @examples
#' DII(SERV_DATA, SERV_DATA$RESPONDENTID, REPEATNUM=1, SERV_DATA$ALCOHOL_DII, SERV_DATA$VITB12_DII, SERV_DATA$VITB6_DII, SERV_DATA$BCAROTENE_DII, SERV_DATA$CAFFEINE_DII, SERV_DATA$CARB_DII, SERV_DATA$CHOLES_DII, SERV_DATA$KCAL_DII, SERV_DATA$EUGENOL_DII, SERV_DATA$TOTALFAT_DII, SERV_DATA$FIBER_DII, SERV_DATA$FOLICACID_DII, SERV_DATA$GARLIC_DII, SERV_DATA$GINGER_DII, SERV_DATA$IRON_DII, SERV_DATA$MG_DII, SERV_DATA$MUFA_DII, SERV_DATA$NIACIN_DII, SERV_DATA$N3FAT_DII, SERV_DATA$N6FAT_DII, SERV_DATA$ONION_DII, SERV_DATA$PROTEIN_DII, SERV_DATA$PUFA_DII, SERV_DATA$RIBOFLAVIN_DII, SERV_DATA$SAFFRON_DII, SERV_DATA$SATFAT_DII, SERV_DATA$SE_DII, SERV_DATA$THIAMIN_DII, SERV_DATA$TRANSFAT_DII, SERV_DATA$TURMERIC_DII, SERV_DATA$VITA_DII, SERV_DATA$VITC_DII, SERV_DATA$VITD_DII, SERV_DATA$VITE_DII, SERV_DATA$ZN_DII, SERV_DATA$TEA_DII, SERV_DATA$FLA3OL_DII, SERV_DATA$FLAVONES_DII, SERV_DATA$FLAVONOLS_DII, SERV_DATA$FLAVONONES_DII, SERV_DATA$ANTHOC_DII, SERV_DATA$ISOFLAVONES_DII, SERV_DATA$PEPPER_DII, SERV_DATA$THYME_DII, SERV_DATA$ROSEMARY_DII)
#' @export

#Score calculation for DII

DII = function(SERV_DATA, RESPONDENTID, REPEATNUM=1, ALCOHOL_DII=NULL, VITB12_DII=NULL, VITB6_DII=NULL, BCAROTENE_DII=NULL, 
               CAFFEINE_DII=NULL, CARB_DII=NULL, CHOLES_DII=NULL, KCAL_DII=NULL, EUGENOL_DII=NULL,
               TOTALFAT_DII=NULL, FIBER_DII=NULL, FOLICACID_DII=NULL, GARLIC_DII=NULL, GINGER_DII=NULL,IRON_DII=NULL, MG_DII=NULL, 
               MUFA_DII=NULL, NIACIN_DII=NULL, N3FAT_DII=NULL, N6FAT_DII=NULL,ONION_DII=NULL, PROTEIN_DII=NULL, PUFA_DII=NULL, 
               RIBOFLAVIN_DII=NULL,SAFFRON_DII=NULL, SATFAT_DII=NULL, SE_DII=NULL, THIAMIN_DII=NULL, TRANSFAT_DII=NULL,TURMERIC_DII=NULL, 
               VITA_DII=NULL, VITC_DII=NULL, VITD_DII=NULL, VITE_DII=NULL, ZN_DII=NULL, TEA_DII=NULL,
               FLA3OL_DII=NULL,FLAVONES_DII=NULL,FLAVONOLS_DII=NULL,FLAVONONES_DII=NULL,ANTHOC_DII=NULL,ISOFLAVONES_DII=NULL,
               PEPPER_DII=NULL,THYME_DII=NULL,ROSEMARY_DII=NULL){
  
  SERV_DATA = SERV_DATA %>%
    mutate(
      RESPONDENTID = RESPONDENTID, 
      REPEATNUM = REPEATNUM,
      ALCOHOL_DII = ALCOHOL_DII, 
      VITB12_DII = VITB12_DII, 
      VITB6_DII = VITB6_DII, 
      BCAROTENE_DII = BCAROTENE_DII, 
      CAFFEINE_DII = CAFFEINE_DII, 
      CARB_DII = CARB_DII, 
      CHOLES_DII = CHOLES_DII, 
      KCAL_DII = KCAL_DII, 
      EUGENOL_DII = EUGENOL_DII,
      TOTALFAT_DII = TOTALFAT_DII, 
      FIBER_DII = FIBER_DII, 
      FOLICACID_DII = FOLICACID_DII,
      GARLIC_DII = GARLIC_DII, 
      GINGER_DII = GINGER_DII,
      IRON_DII = IRON_DII, 
      MG_DII = MG_DII, 
      MUFA_DII = MUFA_DII, 
      NIACIN_DII = NIACIN_DII, 
      N3FAT_DII = N3FAT_DII, 
      N6FAT_DII = N6FAT_DII,
      ONION_DII = ONION_DII,
      PROTEIN_DII = PROTEIN_DII, 
      PUFA_DII = PUFA_DII, 
      RIBOFLAVIN_DII = RIBOFLAVIN_DII,
      SAFFRON_DII = SAFFRON_DII, 
      SATFAT_DII = SATFAT_DII, 
      SE_DII = SE_DII, 
      THIAMIN_DII = THIAMIN_DII,
      TRANSFAT_DII = TRANSFAT_DII,
      TURMERIC_DII = TURMERIC_DII, 
      VITA_DII = VITA_DII,
      VITC_DII = VITC_DII, 
      VITD_DII = VITD_DII, 
      VITE_DII = VITE_DII, 
      ZN_DII = ZN_DII, 
      TEA_DII = TEA_DII,
      FLA3OL_DII = FLA3OL_DII,
      FLAVONES_DII = FLAVONES_DII,
      FLAVONOLS_DII = FLAVONOLS_DII,
      FLAVONONES_DII = FLAVONONES_DII,
      ANTHOC_DII = ANTHOC_DII,
      ISOFLAVONES_DII = ISOFLAVONES_DII,
      PEPPER_DII = PEPPER_DII,
      THYME_DII = THYME_DII,
      ROSEMARY_DII = ROSEMARY_DII)
  
  print("It is normal to see warnings if you do not provide all arguments using DII. The algorithm will only count the arguments you enter to calculate the DII. All warnings are about the first column you don't use. For example, if you only entered alcohol, vitamin b12, and vitamin b6, all warnings would remind you that bcarotene is not found.")
  
  COHORT = SERV_DATA %>%
    dplyr::select(RESPONDENTID, REPEATNUM, ALCOHOL_DII, VITB12_DII, VITB6_DII, BCAROTENE_DII, CAFFEINE_DII, CARB_DII, CHOLES_DII, KCAL_DII, EUGENOL_DII,
                  TOTALFAT_DII, FIBER_DII, FOLICACID_DII, GARLIC_DII, GINGER_DII,IRON_DII, MG_DII, MUFA_DII, NIACIN_DII, N3FAT_DII, N6FAT_DII,ONION_DII, PROTEIN_DII, PUFA_DII,
                  RIBOFLAVIN_DII,SAFFRON_DII, SATFAT_DII, SE_DII, THIAMIN_DII, TRANSFAT_DII,TURMERIC_DII, VITA_DII, VITC_DII, VITD_DII, VITE_DII, ZN_DII, TEA_DII,
                  FLA3OL_DII,FLAVONES_DII,FLAVONOLS_DII,FLAVONONES_DII,ANTHOC_DII,ISOFLAVONES_DII,PEPPER_DII,THYME_DII,ROSEMARY_DII)%>%
    tidyr::pivot_longer(-c(RESPONDENTID, REPEATNUM), names_to="Variable", values_to="Value")
  
  Variable = c("ALCOHOL_DII", "VITB12_DII", "VITB6_DII", "BCAROTENE_DII", "CAFFEINE_DII", "CARB_DII", "CHOLES_DII", "KCAL_DII", "EUGENOL_DII",
               "TOTALFAT_DII", "FIBER_DII", "FOLICACID_DII","GARLIC_DII", "GINGER_DII","IRON_DII", "MG_DII", "MUFA_DII", "NIACIN_DII", "N3FAT_DII", "N6FAT_DII","ONION_DII", "PROTEIN_DII", "PUFA_DII",
               "RIBOFLAVIN_DII","SAFFRON_DII", "SATFAT_DII", "SE_DII", "THIAMIN_DII","TRANSFAT_DII","TURMERIC_DII", "VITA_DII","VITC_DII", "VITD_DII", "VITE_DII", "ZN_DII", "TEA_DII",
               "FLA3OL_DII","FLAVONES_DII","FLAVONOLS_DII","FLAVONONES_DII","ANTHOC_DII","ISOFLAVONES_DII","PEPPER_DII","THYME_DII","ROSEMARY_DII")
  
  Overall_inflammatory_score = c(-0.278, 0.106, -0.365, -0.584, -0.11, 0.097, 0.11, 0.18, -0.14, 0.298, -0.663, -0.19, -0.412, -0.453, 0.032, -0.484, -0.009,
                                 -0.246, -0.436, -0.159, -0.301, 0.021, -0.337, -0.068, -0.14, 0.373, -0.191, -0.098,0.229,-0.785, -0.401, -0.424, -0.446, -0.419, -0.313,
                                 -0.536,-0.415,-0.616,-0.467,-0.25,-0.131,-0.593,-0.131,-0.102,-0.013)
  
  Global_mean = c(13.98,5.15,1.47,3718,8.05,272.2,279.4,2056,0.01,71.4,18.8,273,4.35,59,13.35,310.1,27,25.9,1.06,10.8,35.9,
                  79.4,13.88,1.7,0.37,28.6,67,1.7,3.15,533.6,983.9,118.2,6.26,8.73,9.84,
                  1.69,95.8,1.55,17.7,11.7,18.05,1.2,10,0.33,1)
  
  SD = c(3.72,2.7,0.74,1720,6.67,40,51.2,338,0.08,19.4,4.9,70.7,2.9,63.2,3.71,139.4,6.1,11.77,1.06,7.5,18.4,13.9,3.76,0.79,1.78,
         8,25.1,0.66,3.75,754.3,518.6,43.46,2.21,1.49,2.19,
         1.53,85.9,0.07,6.79,3.82,21.14,0.2,7.07,0.99,15)
  
  DII_STD = base::data.frame(Variable, Overall_inflammatory_score, Global_mean, SD)
  
  #Score calculation for DII
  
  COHORT = COHORT %>%
    dplyr::inner_join(DII_STD, by=c("Variable")) %>%
    dplyr::mutate(
      Z_SCORE = (Value - Global_mean)/SD,
      PERCENTILE = pnorm(Z_SCORE)*2 - 1,
      IND_DII_SCORE = PERCENTILE*Overall_inflammatory_score) %>%
    tidyr::pivot_wider(names_from = Variable, values_from = IND_DII_SCORE) %>%
    dplyr::group_by(RESPONDENTID, REPEATNUM) %>%
    dplyr::summarize(
      ALCOHOL_DII = base::sum(ALCOHOL_DII, na.rm = TRUE),
      VITB12_DII = base::sum(VITB12_DII, na.rm = TRUE),
      VITB6_DII = base::sum(VITB6_DII, na.rm = TRUE),
      BCAROTENE_DII = base::sum(BCAROTENE_DII, na.rm = TRUE),
      CAFFEINE_DII = base::sum(CAFFEINE_DII, na.rm = TRUE),
      CARB_DII = base::sum(CARB_DII, na.rm = TRUE),
      CHOLES_DII = base::sum(CHOLES_DII, na.rm = TRUE),
      KCAL_DII= base::sum(KCAL_DII, na.rm = TRUE),
      EUGENOL_DII= base::sum(EUGENOL_DII, na.rm = TRUE),
      TOTALFAT_DII= base::sum(TOTALFAT_DII, na.rm = TRUE),
      FIBER_DII  = base::sum(FIBER_DII, na.rm = TRUE),
      FOLICACID_DII  = base::sum(FOLICACID_DII, na.rm = TRUE),
      GARLIC_DII  = base::sum(GARLIC_DII, na.rm = TRUE),
      GINGER_DII  = base::sum(GINGER_DII, na.rm = TRUE),
      IRON_DII  = base::sum(IRON_DII, na.rm = TRUE),
      MG_DII  = base::sum(MG_DII, na.rm = TRUE),
      MUFA_DII  = base::sum(MUFA_DII, na.rm = TRUE),
      NIACIN_DII  = base::sum(NIACIN_DII, na.rm = TRUE),
      N3FAT_DII  = base::sum(N3FAT_DII, na.rm = TRUE),
      N6FAT_DII  = base::sum(N6FAT_DII, na.rm = TRUE),
      ONION_DII  = base::sum(ONION_DII, na.rm = TRUE),
      PROTEIN_DII  = base::sum(PROTEIN_DII, na.rm = TRUE),
      PUFA_DII  = base::sum(PUFA_DII, na.rm = TRUE),
      RIBOFLAVIN_DII  = base::sum(RIBOFLAVIN_DII, na.rm = TRUE),
      SAFFRON_DII  = base::sum(SAFFRON_DII, na.rm = TRUE),
      SATFAT_DII  = base::sum(SATFAT_DII, na.rm = TRUE),
      SE_DII  = base::sum(SE_DII, na.rm = TRUE),
      THIAMIN_DII  = base::sum(THIAMIN_DII, na.rm = TRUE),
      TRANSFAT_DII  = base::sum(TRANSFAT_DII, na.rm = TRUE),
      TURMERIC_DII  = base::sum(TURMERIC_DII, na.rm = TRUE),
      VITA_DII  = base::sum(VITA_DII, na.rm = TRUE),
      VITC_DII  = base::sum(VITC_DII, na.rm = TRUE),
      VITD_DII  = base::sum(VITD_DII, na.rm = TRUE),
      VITE_DII = base::sum(VITE_DII, na.rm = TRUE),
      ZN_DII = base::sum(ZN_DII, na.rm = TRUE),
      TEA_DII = base::sum(TEA_DII, na.rm = TRUE),
      FLA3OL_DII = base::sum(FLA3OL_DII, na.rm = TRUE),
      FLAVONES_DII = base::sum(FLAVONES_DII, na.rm = TRUE),
      FLAVONOLS_DII = base::sum(FLAVONOLS_DII, na.rm = TRUE),
      FLAVONONES_DII = base::sum(FLAVONONES_DII, na.rm = TRUE),
      ANTHOC_DII = base::sum(ANTHOC_DII, na.rm = TRUE),
      ISOFLAVONES_DII = base::sum(ISOFLAVONES_DII, na.rm = TRUE),
      PEPPER_DII = base::sum(PEPPER_DII, na.rm = TRUE),
      THYME_DII = base::sum(THYME_DII, na.rm = TRUE),
      ROSEMARY_DII = base::sum(ROSEMARY_DII, na.rm = TRUE),
    )
  
  for(i in 1:length(COHORT$RESPONDENTID)){
    if (is.null(SERV_DATA$ALCOHOL_DII) == TRUE){
      COHORT$ALCOHOL_DII[i] = 0
    }
    else if (is.null(SERV_DATA$VITB12_DII) == TRUE){
      COHORT$VITB12_DII[i] = 0
    }
    else if (is.null(SERV_DATA$VITB6_DII) == TRUE){
      COHORT$VITB6_DII[i] = 0
    }
    else if (is.null(SERV_DATA$BCAROTENE_DII) == TRUE){
      COHORT$BCAROTENE_DII[i] = 0
    }
    else if (is.null(SERV_DATA$CAFFEINE_DII) == TRUE){
      COHORT$CAFFEINE_DII[i] = 0
    }
    else if (is.null(SERV_DATA$CARB_DII) == TRUE){
      COHORT$CARB_DII[i] = 0
    }
    else if (is.null(SERV_DATA$CHOLES_DII) == TRUE){
      COHORT$CHOLES_DII[i] = 0
    }
    else if (is.null(SERV_DATA$KCAL_DII) == TRUE){
      COHORT$KCAL_DII[i] = 0
    }
    else if (is.null(SERV_DATA$EUGENOL_DII) == TRUE){
      COHORT$EUGENOL_DII[i] = 0
    }
    else if (is.null(SERV_DATA$TOTALFAT_DII) == TRUE){
      COHORT$TOTALFAT_DII[i] = 0
    }
    else if (is.null(SERV_DATA$FIBER_DII) == TRUE){
      COHORT$FIBER_DII[i] = 0
    }
    else if (is.null(SERV_DATA$FOLICACID_DII) == TRUE){
      COHORT$FOLICACID_DII[i] = 0
    }
    else if (is.null(SERV_DATA$GARLIC_DII) == TRUE){
      COHORT$GARLIC_DII[i] = 0
    }
    else if (is.null(SERV_DATA$GINGER_DII) == TRUE){
      COHORT$GINGER_DII[i] = 0
    }
    else if (is.null(SERV_DATA$IRON_DII) == TRUE){
      COHORT$IRON_DII[i] = 0
    }
    else if (is.null(SERV_DATA$MG_DII) == TRUE){
      COHORT$MG_DII[i] = 0
    }
    else if (is.null(SERV_DATA$MUFA_DII) == TRUE){
      COHORT$MUFA_DII[i] = 0
    }
    else if (is.null(SERV_DATA$NIACIN_DII) == TRUE){
      COHORT$NIACIN_DII[i] = 0
    }
    else if (is.null(SERV_DATA$N3FAT_DII) == TRUE){
      COHORT$N3FAT_DII[i] = 0
    }
    else if (is.null(SERV_DATA$N6FAT_DII) == TRUE){
      COHORT$N6FAT_DII[i] = 0
    }
    else if (is.null(SERV_DATA$ONION_DII) == TRUE){
      COHORT$ONION_DII[i] = 0
    }
    else if (is.null(SERV_DATA$PROTEIN_DII) == TRUE){
      COHORT$PROTEIN_DII[i] = 0
    }
    else if (is.null(SERV_DATA$PUFA_DII) == TRUE){
      COHORT$PUFA_DII[i] = 0
    }
    else if (is.null(SERV_DATA$RIBOFLAVIN_DII) == TRUE){
      COHORT$RIBOFLAVIN_DII[i] = 0
    }
    else if (is.null(SERV_DATA$SAFFRON_DII) == TRUE){
      COHORT$SAFFRON_DII[i] = 0
    }
    else if (is.null(SERV_DATA$SATFAT_DII) == TRUE){
      COHORT$SATFAT_DII[i] = 0
    }
    else if (is.null(SERV_DATA$SE_DII) == TRUE){
      COHORT$SE_DII[i] = 0
    }
    else if (is.null(SERV_DATA$THIAMIN_DII) == TRUE){
      COHORT$THIAMIN_DII[i] = 0
    }
    else if (is.null(SERV_DATA$TRANSFAT_DII) == TRUE){
      COHORT$TRANSFAT_DII[i] = 0
    }
    else if (is.null(SERV_DATA$TURMERIC_DII) == TRUE){
      COHORT$TURMERIC_DII[i] = 0
    }
    else if (is.null(SERV_DATA$VITA_DII) == TRUE){
      COHORT$VITA_DII[i] = 0
    }
    else if (is.null(SERV_DATA$VITC_DII) == TRUE){
      COHORT$VITC_DII[i] = 0
    }
    else if (is.null(SERV_DATA$VITD_DII) == TRUE){
      COHORT$VITD_DII[i] = 0
    }
    else if (is.null(SERV_DATA$VITE_DII) == TRUE){
      COHORT$VITE_DII[i] = 0
    }
    else if (is.null(SERV_DATA$ZN_DII) == TRUE){
      COHORT$ZN_DII[i] = 0
    }
    else if (is.null(SERV_DATA$TEA_DII) == TRUE){
      COHORT$TEA_DII[i] = 0
    }
    else if (is.null(SERV_DATA$FLA3OL_DII) == TRUE){
      COHORT$FLA3OL_DII[i] = 0
    }
    else if (is.null(SERV_DATA$FLAVONES_DII) == TRUE){
      COHORT$FLAVONES_DII[i] = 0
    }
    else if (is.null(SERV_DATA$FLAVONOLS_DII) == TRUE){
      COHORT$FLAVONOLS_DII[i] = 0
    }
    else if (is.null(SERV_DATA$FLAVONONES_DII) == TRUE){
      COHORT$FLAVONONES_DII[i] = 0
    }
    else if (is.null(SERV_DATA$ANTHOC_DII) == TRUE){
      COHORT$ANTHOC_DII[i] = 0
    }
    else if (is.null(SERV_DATA$ISOFLAVONES_DII) == TRUE){
      COHORT$ISOFLAVONES_DII[i] = 0
    }
    else if (is.null(SERV_DATA$PEPPER_DII) == TRUE){
      COHORT$PEPPER_DII[i] = 0
    }
    else if (is.null(SERV_DATA$THYME_DII) == TRUE){
      COHORT$THYME_DII[i] = 0
    }
    else if (is.null(SERV_DATA$ROSEMARY_DII) == TRUE){
      COHORT$ROSEMARY_DII[i] = 0
    }
  }
  
  
  COHORT %>%
    dplyr::mutate(
      DII_ALL = ALCOHOL_DII + VITB12_DII + VITB6_DII + BCAROTENE_DII + CAFFEINE_DII + CARB_DII + CHOLES_DII + KCAL_DII + EUGENOL_DII +
        TOTALFAT_DII + FIBER_DII + FOLICACID_DII + GARLIC_DII + GINGER_DII + IRON_DII + MG_DII + MUFA_DII + NIACIN_DII + N3FAT_DII + N6FAT_DII +ONION_DII + PROTEIN_DII + PUFA_DII +
        RIBOFLAVIN_DII +SAFFRON_DII + SATFAT_DII + SE_DII + THIAMIN_DII +TRANSFAT_DII +TURMERIC_DII + VITA_DII +VITC_DII + VITD_DII + VITE_DII + ZN_DII + TEA_DII +
        FLA3OL_DII +FLAVONES_DII +FLAVONOLS_DII +FLAVONONES_DII +ANTHOC_DII +ISOFLAVONES_DII +PEPPER_DII +THYME_DII +ROSEMARY_DII,
      
      DII_NOETOH = VITB12_DII + VITB6_DII + BCAROTENE_DII + CAFFEINE_DII + CARB_DII + CHOLES_DII + KCAL_DII + EUGENOL_DII +
        TOTALFAT_DII + FIBER_DII + FOLICACID_DII + GARLIC_DII + GINGER_DII + IRON_DII + MG_DII + MUFA_DII + NIACIN_DII + N3FAT_DII + N6FAT_DII +ONION_DII + PROTEIN_DII + PUFA_DII +
        RIBOFLAVIN_DII +SAFFRON_DII + SATFAT_DII + SE_DII + THIAMIN_DII +TRANSFAT_DII +TURMERIC_DII + VITA_DII +VITC_DII + VITD_DII + VITE_DII + ZN_DII + TEA_DII +
        FLA3OL_DII +FLAVONES_DII +FLAVONOLS_DII +FLAVONONES_DII +ANTHOC_DII +ISOFLAVONES_DII +PEPPER_DII +THYME_DII +ROSEMARY_DII,
    ) %>%
    dplyr::select(RESPONDENTID, DII_ALL, DII_NOETOH, everything())
  
}

#' ACS2020_V1 
#'
#' Calculate the American Cancer Society 2020 dietary index, using the daily serving sizes of foods consumed to calculate the vegetable, fruit, whole grain, red and processed meat, and sugar-sweetened beverages components. This version uses the percent of daily calories from highly processed foods and refined grains to calculate that component’s scores and is the preferred method of calculating the ACS 2020 diet score.  
#' @import dplyr
#' @import readr
#' @import haven
#' @param SERV_DATA The raw data file that includes all the serving sizes of foods and nutrients
#' @param RESPONDENTID The unique participant ID for each participant
#' @param GENDER The gender for each participant, 1=male, and 2=female
#' @param VEG_SERV_ACS2020 The serving size of All vegetable except potatoes and starchy vegetable, unit=servings/day (0.5 c of vege; 1 cup of green leafy (1 cup = 236.59 g)
#' @param VEG_ITEMS_SERV_ACS2020 The total number of unique vegetables (e.g., vegetable line items) reported by the participant. For example, a report of 3 servings of lettuce, 1 serving of kale, and 0.5 servings of broccoli would count as 3 vegetable line items.
#' @param FRT_SERV_ACS2020 The serving size of All whole fruits and no fruit juice, unit=servings/day (0.5 c of berries; 1 cup other fruits=236.59 g; 1 med fruit; 0.5 medium avocado)
#' @param FRT_ITEMS_SERV_ACS2020 The number of unique fruits (e.g., fruit line items) asked about on that survey/app (measuring the variety of fruits). For example, 3 serving of apple, 3 servings of banana, and 3 servings of blueberry are just 3 total number line items
#' @param WGRAIN_SERV_ACS2020 The serving size of whole grains, unit=grams/day
#' @param SSB_FRTJ_SERV_ACS2020 The serving size of sugar-sweetened beverages and non-100\% fruit juice, unit=servings/day = 1 ser= 8oz (1 oz. = 28.35 g)
#' @param REDPROC_MEAT_SERV_ACS2020 The serving size of red and processed meats, including Beef, pork, lamb, goat, veal, sausages, bacon, salami, ham, hot dog, deli meat, unit=servings/day; 1 srv= 4 oz. unprocessed meat; 1.5 oz. processed meat (1 oz. = 28.35 g)
#' @param HPFRG_RATIO_SERV_ACS2020 The ratio of calories from highly processed foods and refined grains to the total daily calories (e.g. 35 \% calories from HPF and ref grains), note: the ultra-processed variable for the score should not double count foods included in other parts of the score, for example, sugar-sweetened beverages or processed meats
#' @return The ACS2020_V1 index/score and its component scores 
#' @examples
#' ACS2020_V1(SERV_DATA, SERV_DATA$RESPONDENTID, SERV_DATA$GENDER, SERV_DATA$VEG_SERV_ACS2020, SERV_DATA$VEG_ITEMS_SERV_ACS2020, SERV_DATA$FRT_SERV_ACS2020, SERV_DATA$FRT_ITEMS_SERV_ACS2020, SERV_DATA$WGRAIN_SERV_ACS2020, SERV_DATA$SSB_FRTJ_SERV_ACS2020, SERV_DATA$REDPROC_MEAT_SERV_ACS2020, SERV_DATA$HPFRG_RATIO_SERV_ACS2020)
#' @export


#Score calculation for ACS2020_V1
ACS2020_V1 = function(SERV_DATA, RESPONDENTID, GENDER, VEG_SERV_ACS2020, VEG_ITEMS_SERV_ACS2020, FRT_SERV_ACS2020, FRT_ITEMS_SERV_ACS2020, 
                      WGRAIN_SERV_ACS2020, SSB_FRTJ_SERV_ACS2020, REDPROC_MEAT_SERV_ACS2020, HPFRG_RATIO_SERV_ACS2020){

  ##Create variables and functions needed for ACS2020_V1 calculation
  quintile_healthy1 = function(actual){
    quintile= quantile(actual, probs=seq(0, 1, by=0.25), na.rm=TRUE)
    case_when(
      actual <= quintile[5] & actual >= quintile[4] ~ 0.75,
      actual < quintile[4] & actual >= quintile[3] ~ 0.5,
      actual < quintile[3] & actual >= quintile[2] ~ 0.25,
      actual < quintile[2] & actual >= quintile[1] ~ 0
    )
  }
  
  quintile_healthy4 = function(actual){
    quintile = quantile(actual, probs=seq(0, 1, by=0.25), na.rm=TRUE)
    case_when(
      actual <= quintile[5] & actual >= quintile[4] ~ 3,
      actual < quintile[4] & actual >= quintile[3] ~ 2,
      actual < quintile[3] & actual >= quintile[2] ~ 1,
      actual < quintile[2] & actual >= quintile[1] ~ 0
    )
  }
  
  quintile_unhealthy2 = function(actual){
    quintile = quantile(actual, probs=seq(0, 1, by=0.25), na.rm=TRUE)
    case_when(
      actual <= quintile[5] & actual >= quintile[4] ~ 0,
      actual < quintile[4] & actual >= quintile[3] ~ 0.5,
      actual < quintile[3] & actual >= quintile[2] ~ 1,
      actual < quintile[2] & actual >= quintile[1] ~ 1.5
    )
  }
  
  quintile_unhealthy4 = function(actual){
    quintile = quantile(actual, probs=seq(0, 1, by=0.25), na.rm=TRUE)
    case_when(
      actual <= quintile[5] & actual >= quintile[4] ~ 0,
      actual < quintile[4] & actual >= quintile[3] ~ 1,
      actual < quintile[3] & actual >= quintile[2] ~ 2,
      actual < quintile[2] & actual >= quintile[1] ~ 3
    )
  }
  
  print("Reminder: this ACS2020_V1 index uses quartiles to rank participants' food/drink serving sizes and then calculate the component scores, which may generate results that are specific to your study population but not comparable to other populations.")
  
  ##ACS2020 calculation
  SERV_DATA %>%
    dplyr::mutate(
      RESPONDENTID = RESPONDENTID,
      GENDER = GENDER,
      VEG_SERV_ACS2020=VEG_SERV_ACS2020,
      VEG_ITEMS_SERV_ACS2020=VEG_ITEMS_SERV_ACS2020,
      FRT_SERV_ACS2020=FRT_SERV_ACS2020,
      FRT_ITEMS_SERV_ACS2020=FRT_ITEMS_SERV_ACS2020,
      WGRAIN_SERV_ACS2020=WGRAIN_SERV_ACS2020,
      SSB_FRTJ_SERV_ACS2020=SSB_FRTJ_SERV_ACS2020,
      REDPROC_MEAT_SERV_ACS2020=REDPROC_MEAT_SERV_ACS2020,
      HPFRG_RATIO_SERV_ACS2020=HPFRG_RATIO_SERV_ACS2020) %>%
    group_by(GENDER) %>%
    dplyr::mutate(
      ACS2020_VEG = quintile_healthy1(VEG_SERV_ACS2020),
      ACS2020_VEG_ITEMS = quintile_healthy1(VEG_ITEMS_SERV_ACS2020),
      ACS2020_FRT = quintile_healthy1(FRT_SERV_ACS2020),
      ACS2020_FRT_ITEMS = quintile_healthy1(FRT_ITEMS_SERV_ACS2020),
      ACS2020_WGRAIN = quintile_healthy4(WGRAIN_SERV_ACS2020),
      ACS2020_REDPROC_MEAT = quintile_unhealthy4(REDPROC_MEAT_SERV_ACS2020),
      ACS2020_HPFRG_RATIO = quintile_unhealthy2(HPFRG_RATIO_SERV_ACS2020)
    ) %>%
    ungroup() %>%
    dplyr::mutate(
      ACS2020_SSB_FRTJ = case_when(
        SSB_FRTJ_SERV_ACS2020 >= 1 ~ 0 ,
        SSB_FRTJ_SERV_ACS2020 < 1 & SSB_FRTJ_SERV_ACS2020 >= 3/7 ~ 0.5,
        SSB_FRTJ_SERV_ACS2020 < 3/7 ~ 1,
        SSB_FRTJ_SERV_ACS2020 <= 0 ~ 1.5,
      ),
      ACS2020_V1_ALL = ACS2020_VEG+ACS2020_VEG_ITEMS+ACS2020_FRT+ACS2020_FRT_ITEMS+ACS2020_WGRAIN+
        ACS2020_SSB_FRTJ+ACS2020_REDPROC_MEAT+ACS2020_HPFRG_RATIO
    ) %>%
    dplyr::select(RESPONDENTID, GENDER, ACS2020_V1_ALL, ACS2020_VEG, ACS2020_VEG_ITEMS, ACS2020_FRT, ACS2020_FRT_ITEMS, ACS2020_WGRAIN,
                  ACS2020_SSB_FRTJ, ACS2020_REDPROC_MEAT, ACS2020_HPFRG_RATIO)
}

#' ACS2020_V2
#'
#' Calculate the American Cancer Society 2020 dietary index version 2 (using servings/d per 1000 kcal), using given the serving sizes of foods and nutrients consumed per 1 day. 
#' @import dplyr
#' @import readr
#' @import haven
#' @param SERV_DATA The raw data file that includes all the serving sizes of foods and nutrients
#' @param RESPONDENTID The unique participant ID for each participant
#' @param GENDER The gender for each participant, 1=male, and 2=female
#' @param TOTALKCAL_ACS2020 The total kcal
#' @param VEG_SERV_ACS2020 The serving size of All vegetable except potatoes and starchy vegetable, unit=servings/day (0.5 c of vege; 1 cup of green leafy (1 cup = 236.59 g)
#' @param VEG_ITEMS_SERV_ACS2020 The total number of unique vegetables (e.g., vegetable line items) reported by the participant. For example, a report of 3 servings of lettuce, 1 serving of kale, and 0.5 servings of broccoli would count as 3 vegetable line items.
#' @param FRT_SERV_ACS2020 The serving size of All whole fruits and no fruit juice, unit=servings/day (0.5 c of berries; 1 cup=236.59 g; 1 med fruit; 0.5 medium avocado)
#' @param FRT_ITEMS_SERV_ACS2020 The number of unique fruits (e.g., fruit line items) asked about on that survey/app (measuring the variety of fruits). For example, 3 serving of apple, 3 servings of banana, and 3 servings of blueberry are just 3 total number line items
#' @param WGRAIN_SERV_ACS2020 The serving size of whole grains, unit=grams/day
#' @param SSB_FRTJ_SERV_ACS2020 The serving size of sugar-sweetened beverages and non-100\% fruit juice, unit=servings/day = 1 ser= 8oz (1 oz. = 28.35 g)
#' @param REDPROC_MEAT_SERV_ACS2020 The serving size of red and processed meats, including Beef, pork, lamb, goat, veal, sausages, bacon, salami, ham, hot dog, deli meat, unit=servings/day; 1 srv= 4 oz. unprocessed meat; 1.5 oz. processed meat (1 oz. = 28.35 g)
#' @param HPFRG_SERV_ACS2020 The daily servings of highly processed foods and refined grains per 1000 kcal(e.g. 35 \% calories from HPF and ref grains), note: the ultra-processed variable for the score should not double count foods included in other parts of the score, for example, sugar-sweetened beverages or processed meats
#' @return The ACS2020_V2 index/score and its component scores 
#' @examples
#' ACS2020_V2(SERV_DATA, SERV_DATA$RESPONDENTID, SERV_DATA$GENDER, SERV_DATA$TOTALKCAL_ACS2020, SERV_DATA$VEG_SERV_ACS2020, SERV_DATA$VEG_ITEMS_SERV_ACS2020, SERV_DATA$FRT_SERV_ACS2020, SERV_DATA$FRT_ITEMS_SERV_ACS2020, SERV_DATA$WGRAIN_SERV_ACS2020, SERV_DATA$SSB_FRTJ_SERV_ACS2020, SERV_DATA$REDPROC_MEAT_SERV_ACS2020, SERV_DATA$HPFRG_SERV_ACS2020)
#' @export


#Score calculation for ACS2020_V2
ACS2020_V2 = function(SERV_DATA, RESPONDENTID, GENDER, TOTALKCAL_ACS2020, VEG_SERV_ACS2020, VEG_ITEMS_SERV_ACS2020, 
                      FRT_SERV_ACS2020, FRT_ITEMS_SERV_ACS2020, WGRAIN_SERV_ACS2020,
                      SSB_FRTJ_SERV_ACS2020, REDPROC_MEAT_SERV_ACS2020, HPFRG_SERV_ACS2020){
  
  ##Create variables and functions needed for ACS2020_V2 calculation
  quintile_healthy1 = function(actual){
    quintile = quantile(actual, probs=seq(0, 1, by=0.25), na.rm=TRUE)
    case_when(
      actual <= quintile[5] & actual >= quintile[4] ~ 0.75,
      actual < quintile[4] & actual >= quintile[3] ~ 0.5,
      actual < quintile[3] & actual >= quintile[2] ~ 0.25,
      actual < quintile[2] & actual >= quintile[1] ~ 0
    )
  }
  
  quintile_healthy4 = function(actual){
    quintile = quantile(actual, probs=seq(0, 1, by=0.25), na.rm=TRUE)
    case_when(
      actual <= quintile[5] & actual >= quintile[4] ~ 3,
      actual < quintile[4] & actual >= quintile[3] ~ 2,
      actual < quintile[3] & actual >= quintile[2] ~ 1,
      actual < quintile[2] & actual >= quintile[1] ~ 0
    )
  }
  
  quintile_unhealthy2 = function(actual){
    quintile = quantile(actual, probs=seq(0, 1, by=0.25), na.rm=TRUE)
    case_when(
      actual <= quintile[5] & actual >= quintile[4] ~ 0,
      actual < quintile[4] & actual >= quintile[3] ~ 0.5,
      actual < quintile[3] & actual >= quintile[2] ~ 1,
      actual < quintile[2] & actual >= quintile[1] ~ 1.5
    )
  }
  
  quintile_unhealthy4 = function(actual){
    quintile = quantile(actual, probs=seq(0, 1, by=0.25), na.rm=TRUE)
    case_when(
      actual <= quintile[5] & actual >= quintile[4] ~ 0,
      actual < quintile[4] & actual >= quintile[3] ~ 1,
      actual < quintile[3] & actual >= quintile[2] ~ 2,
      actual < quintile[2] & actual >= quintile[1] ~ 3
    )
  }
  
  print("Reminder: this ACS2020_V2 index uses quartiles to rank participants' food/drink serving sizes and then calculate the component scores, which may generate results that are specific to your study population but not comparable to other populations.")
  
  ##ACS2020 calculation
  SERV_DATA %>%
    dplyr::mutate(
      RESPONDENTID = RESPONDENTID,
      GENDER = GENDER,
      TOTALKCAL_ACS2020 = TOTALKCAL_ACS2020,
      VEG_SERV_ACS2020=VEG_SERV_ACS2020,
      VEG_ITEMS_SERV_ACS2020=VEG_ITEMS_SERV_ACS2020,
      FRT_SERV_ACS2020=FRT_SERV_ACS2020,
      FRT_ITEMS_SERV_ACS2020=FRT_ITEMS_SERV_ACS2020,
      WGRAIN_SERV_ACS2020=WGRAIN_SERV_ACS2020,
      SSB_FRTJ_SERV_ACS2020=SSB_FRTJ_SERV_ACS2020,
      REDPROC_MEAT_SERV_ACS2020=REDPROC_MEAT_SERV_ACS2020,
      HPFRG_SERV_ACS2020=HPFRG_SERV_ACS2020) %>%
    group_by(GENDER) %>%
    dplyr::mutate(
      ACS2020_VEG = quintile_healthy1(VEG_SERV_ACS2020),
      ACS2020_VEG_ITEMS = quintile_healthy1(VEG_ITEMS_SERV_ACS2020),
      ACS2020_FRT = quintile_healthy1(FRT_SERV_ACS2020),
      ACS2020_FRT_ITEMS = quintile_healthy1(FRT_ITEMS_SERV_ACS2020),
      ACS2020_WGRAIN = quintile_healthy4(WGRAIN_SERV_ACS2020),
      ACS2020_REDPROC_MEAT = quintile_unhealthy4(REDPROC_MEAT_SERV_ACS2020),
      ACS2020_HPFRG = quintile_unhealthy2(HPFRG_SERV_ACS2020/(TOTALKCAL_ACS2020/1000)),
    ) %>%
    ungroup() %>%
    dplyr::mutate(
      ACS2020_SSB_FRTJ = case_when(
        SSB_FRTJ_SERV_ACS2020 >= 1 ~ 0 ,
        SSB_FRTJ_SERV_ACS2020 < 1 & SSB_FRTJ_SERV_ACS2020 >= 3/7 ~ 0.5,
        SSB_FRTJ_SERV_ACS2020 < 3/7 ~ 1,
        SSB_FRTJ_SERV_ACS2020 <= 0 ~ 1.5,
      ),
      ACS2020_V2_ALL = ACS2020_VEG+ACS2020_VEG_ITEMS+ACS2020_FRT+ACS2020_FRT_ITEMS+ACS2020_WGRAIN+
        ACS2020_SSB_FRTJ+ACS2020_REDPROC_MEAT+ACS2020_HPFRG
    ) %>%
    dplyr::select(RESPONDENTID, GENDER, ACS2020_V2_ALL, TOTALKCAL_ACS2020, ACS2020_VEG, ACS2020_VEG_ITEMS, ACS2020_FRT, ACS2020_FRT_ITEMS, ACS2020_WGRAIN,
                  ACS2020_SSB_FRTJ, ACS2020_REDPROC_MEAT, ACS2020_HPFRG)
}

#' AHEI_BLOCK Calculation
#'
#' Calculate the AHEI dietary index for the Block FFQ (2013) per 1 day
#' @import dplyr
#' @import readr
#' @import haven
#' @param RAW_DATA The raw data file that includes results and raw responses of the dietary assessment
#' @return The AHEI and its component scores. Sodium is energy adjusted: sodium_serv/(total kcal / 1000)
#' @examples
#' AHEI_BLOCK(RAW_DATA)
#' @export

AHEI_BLOCK = function(RAW_DATA){
  
  if (is.character(RAW_DATA) == TRUE){
    RAW_DATA = read_csv(RAW_DATA)
  } else {
    RAW_DATA = RAW_DATA
  }
  
  STD_FOOD_FREQ = c(1, 2, 3, 4, 5, 6, 7, 8, 9)
  STD_FREQ_SERV = c(0, 1/90, 1/30, 2.5/30, 1/7, 2/7, 3.5/7, 5.5/7, 1)
  STD_FOOD_PORT = c(1, 2, 3, 4)
  STD_PORT_SERV = c(0.25, 0.5, 1, 2)
  STD_FOOD_FREQ_DF = data.frame(STD_FOOD_FREQ, STD_FREQ_SERV, stringsAsFactors=FALSE)
  STD_FOOD_PORT_DF= data.frame(STD_FOOD_PORT, STD_PORT_SERV, stringsAsFactors=FALSE)
  
  #Functions to match actual food frequency and portion to the standards
  foodfreq = function(actual, reference=STD_FOOD_FREQ_DF){
    reference[match(actual, reference[,1]),2]
  }
  
  foodport = function(actual, reference=STD_FOOD_PORT_DF){
    reference[match(actual, reference[,1]),2]
  }
  
  #Serving calculation for AHEI 2010
  SERV_DATA = RAW_DATA %>%
    dplyr::mutate(
      VEG_SERV = V_DPYEL + 0.5*V_DRKGR + V_OTHER + V_STARCY + V_TOMATO,
      F_BERRIES = foodfreq(STRAWBERRIESFREQ)*foodport(STRAWBERRIESQUAN),
      F_WHOLE = F_SOLID - F_BERRIES + F_BERRIES*2,
      FRT_SERV = F_WHOLE,
      WGRAIN_SERV = G_WHL/0.035274,
      NUTSLEG_SERV = (LEGUMES*4) + M_NUTSD + M_SOY,
      N3FAT_SERV = (DT_FA205 + DT_FA226)*1000,
      PUFA_SERV = (((DT_TOTN6 + DT_TOTN3 - DT_FA205 - DT_FA226)*9)/ DT_KCAL)*100,
      SSB_FRTJ_SERV = (GROUP_SUGARYBEVG_TOTAL_GRAMS / 240) + F_JUICE,
      REDPROC_MEAT_SERV = (M_FRANK /1.5) + ((M_MEAT+M_ORGAN)/4),
      TRANS_SERV = ((DT_TRFAT * 9) / DT_KCAL)*100,
      ALCOHOL_SERV=A_BEV,
      SODIUM_SERV = DT_SODI/(DT_KCAL/1000)
    ) 
  
  ##Create variables and functions needed for AHEI calculation
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
  
  SCORE_HEALTHY = function(actual_serv, min_serv, max_serv, min_score, max_score){
    case_when(
      actual_serv >= max_serv ~ max_score,
      actual_serv <= min_serv ~ min_score,
      TRUE ~ min_score+(actual_serv-min_serv)*max_score/(max_serv-min_serv)
    )
  }
  
  
  SCORE_UNHEALTHY = function(actual_serv, min_serv, max_serv, min_score, max_score){
    case_when(
      actual_serv >= min_serv ~ min_score ,
      actual_serv <= max_serv ~ max_score,
      TRUE ~ min_score+(actual_serv-min_serv)*max_score/(max_serv-min_serv)
    )
  }
  
  SODIUM_DECILE = quantile(SERV_DATA$SODIUM_SERV, probs=seq(0, 1, by=1/11))
  
  SERV_DATA %>%
    dplyr::mutate(
      RESPONDENTID = RESPONDENTID,
      GENDER = SEX,
      
      AHEI_VEG = SCORE_HEALTHY(VEG_SERV, AHEI_MIN_VEG_SERV, AHEI_MAX_VEG_SERV, AHEI_MIN, AHEI_MAX),
      AHEI_FRT = SCORE_HEALTHY(FRT_SERV, AHEI_MIN_FRT_SERV, AHEI_MAX_FRT_SERV, AHEI_MIN, AHEI_MAX),
      
      AHEI_WGRAIN = case_when(
        #GENDER = 2 is female
        GENDER == 2 & WGRAIN_SERV >= AHEI_MAX_WGRAIN_F_SERV ~ AHEI_MAX,
        GENDER == 2 & WGRAIN_SERV <= AHEI_MIN_WGRAIN_F_SERV ~ AHEI_MIN,
        GENDER == 2 & WGRAIN_SERV > AHEI_MIN_WGRAIN_F_SERV & WGRAIN_SERV < AHEI_MAX_WGRAIN_F_SERV ~ AHEI_MIN+(WGRAIN_SERV-AHEI_MIN_WGRAIN_F_SERV)*AHEI_MAX/(AHEI_MAX_WGRAIN_F_SERV-AHEI_MIN_WGRAIN_F_SERV),
        
        GENDER == 1 & WGRAIN_SERV >= AHEI_MAX_WGRAIN_M_SERV ~ AHEI_MAX,
        GENDER == 1 & WGRAIN_SERV <= AHEI_MIN_WGRAIN_M_SERV ~ AHEI_MIN,
        GENDER == 1 & WGRAIN_SERV > AHEI_MIN_WGRAIN_M_SERV & WGRAIN_SERV < AHEI_MAX_WGRAIN_M_SERV ~ AHEI_MIN+(WGRAIN_SERV-AHEI_MIN_WGRAIN_M_SERV)*AHEI_MAX/(AHEI_MAX_WGRAIN_M_SERV-AHEI_MIN_WGRAIN_M_SERV),
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
        ##GENDER = 2 is female
        GENDER == 2  & ALCOHOL_SERV >= 2.5 ~ 0,
        GENDER == 2  & ALCOHOL_SERV < 2.5 & ALCOHOL_SERV > 1.5 ~ 0 + (ALCOHOL_SERV-2.5)*10/(1.5-2.5),
        GENDER == 2  & ALCOHOL_SERV <= 1.5 & ALCOHOL_SERV >= 0.5 ~ 10,
        GENDER == 2  & ALCOHOL_SERV < 0.5 & ALCOHOL_SERV > 0.125 ~ 0 + (ALCOHOL_SERV-0)*10/(0.5-0),
        GENDER == 2  & ALCOHOL_SERV <= 0.125 ~ 2.5,
        
        #GENDER = 1 is male
        GENDER == 1  & ALCOHOL_SERV >= 3.5 ~ 0,
        GENDER == 1  & ALCOHOL_SERV < 3.5 & ALCOHOL_SERV > 2 ~ 0 + (ALCOHOL_SERV-2.5)*10/(1.5-2.5),
        GENDER == 1  & ALCOHOL_SERV <= 2 & ALCOHOL_SERV >= 0.5 ~ 10,
        GENDER == 1  & ALCOHOL_SERV < 0.5 & ALCOHOL_SERV > 0.125 ~ 0 + (ALCOHOL_SERV-0)*10/(0.5-0),
        GENDER == 1  & ALCOHOL_SERV <= 0.125 ~ 2.5,
      ),
      AHEI_ALL = AHEI_VEG + AHEI_FRT + AHEI_WGRAIN + AHEI_NUTSLEG + AHEI_N3FAT +
        AHEI_PUFA + AHEI_SSB_FRTJ + AHEI_REDPROC_MEAT + AHEI_TRANS + AHEI_SODIUM + AHEI_ALCOHOL,
      
      AHEI_NOETOH = AHEI_VEG + AHEI_FRT + AHEI_WGRAIN + AHEI_NUTSLEG + AHEI_N3FAT +
        AHEI_PUFA + AHEI_SSB_FRTJ + AHEI_REDPROC_MEAT + AHEI_TRANS + AHEI_SODIUM
    ) %>%
    dplyr::select(RESPONDENTID, GENDER, AHEI_ALL, AHEI_NOETOH, AHEI_VEG, AHEI_FRT, AHEI_WGRAIN, AHEI_NUTSLEG, AHEI_N3FAT,
                  AHEI_PUFA, AHEI_SSB_FRTJ, AHEI_REDPROC_MEAT, AHEI_TRANS, AHEI_SODIUM, AHEI_ALCOHOL,
                  
                  DT_KCAL, VEG_SERV, F_BERRIES, F_WHOLE, FRT_SERV, WGRAIN_SERV, NUTSLEG_SERV, N3FAT_SERV, PUFA_SERV, SSB_FRTJ_SERV, REDPROC_MEAT_SERV, TRANS_SERV, ALCOHOL_SERV, SODIUM_SERV)
}


#' DASH_BLOCK Calculation
#'
#' Calculate the DASH dietary index for Block FFQ (2013) per 1 day
#' @import dplyr
#' @import readr
#' @import haven
#' @param RAW_DATA The raw data file that includes results and raw data of the dietary assessment
#' @return The DASH and its component scores
#' @examples
#' DASH_BLOCK(RAW_DATA)
#' @export

DASH_BLOCK = function(RAW_DATA){
  
  if (is.character(RAW_DATA) == TRUE){
    RAW_DATA = read_csv(RAW_DATA)
  } else {
    RAW_DATA = RAW_DATA
  }
  
  STD_FOOD_FREQ = c(1, 2, 3, 4, 5, 6, 7, 8, 9)
  STD_FREQ_SERV = c(0, 1/90, 1/30, 2.5/30, 1/7, 2/7, 3.5/7, 5.5/7, 1)
  STD_FOOD_PORT = c(1, 2, 3, 4)
  STD_PORT_SERV = c(0.25, 0.5, 1, 2)
  STD_FOOD_FREQ_DF = data.frame(STD_FOOD_FREQ, STD_FREQ_SERV, stringsAsFactors=FALSE)
  STD_FOOD_PORT_DF= data.frame(STD_FOOD_PORT, STD_PORT_SERV, stringsAsFactors=FALSE)
  
  #Functions to match actual food frequency and portion to the standards
  foodfreq = function(actual, reference=STD_FOOD_FREQ_DF){
    reference[match(actual, reference[,1]),2]
  }
  
  foodport = function(actual, reference=STD_FOOD_PORT_DF){
    reference[match(actual, reference[,1]),2]
  }
  
  #Match participant response food frequency to the standard food frequency response code
  YOGURT_FOOD_PORT = c(2, 3)
  YOGURT_PORT_SERV = c(0.5, 1)
  YOGURT_PORT_DF = data.frame(STD_FOOD_PORT, STD_PORT_SERV)
  
  BUTTERMILK_FOOD_PORT = c(1, 2, 3, 4)
  BUTTERMILK_PORT_SERV = c(0.25, 0.5, 1, 2)
  BUTTERMILK_PORT_DF = data.frame(STD_FOOD_PORT, STD_PORT_SERV)
  
  SERV_DATA=RAW_DATA %>%
    dplyr::mutate(
      F_BERRIES = foodfreq(STRAWBERRIESFREQ)*foodport(STRAWBERRIESQUAN),
      F_WHOLE = F_SOLID - F_BERRIES + F_BERRIES*2,
      FRT_FRTJ_SERV = F_WHOLE + JUICE100,
      VEG_SERV = V_DPYEL + 0.5*V_DRKGR + V_OTHER + V_STARCY + V_TOMATO,
      NUTSLEG_SERV = (LEGUMES*4) + M_NUTSD + M_SOY,
      WGRAIN_SERV = G_WHL,
      LOWF_MILK_SERV = case_when(
        MILKTYPE==4 ~ foodfreq(MILKFREQ) * MILKQUAN, 
        TRUE ~ 0),
      YOGURT_SERV = (foodfreq(YOGURTONLYFREQ) * 
                       foodport(YOGURTONLYQUAN, ref=YOGURT_PORT_DF)) +
        (foodfreq(BUTTERMILKFREQ) * 
           foodport(BUTTERMILKQUAN, ref=BUTTERMILK_PORT_DF)),
      LOWF_ICECREAMFROYO_SERV = case_when(
        ICECREAMFROYOTYPE == 2 ~ foodfreq(ICECREAMFROYOFREQ) * foodport(ICECREAMFROYOQUAN)*2,
        TRUE ~ 0),
      LOWF_CHEESE_SERV = case_when(
        CHEESETYPE == 1 ~foodfreq(CHEESEFREQ) * CHEESEQUAN, 
        TRUE ~ 0),
      LOWF_DAIRY_SERV = LOWF_MILK_SERV+YOGURT_SERV+LOWF_ICECREAMFROYO_SERV+LOWF_CHEESE_SERV,
      SODIUM_SERV = DT_SODI/(DT_KCAL/1000),
      REDPROC_MEAT_SERV = (M_FRANK /1.5) + ((M_MEAT+M_ORGAN)/4),
      SSB_FRTJ_SERV = (GROUP_SUGARYBEVG_TOTAL_GRAMS / 240) + F_JUICE - JUICE100
    ) 
  
  ##Create variables and functions needed for AHEI calculation
  quintile_healthy = function(actual){
    quintile = quantile(actual, probs=seq(0, 1, by=0.2))
    case_when(
      actual <= quintile[6] & actual >= quintile[5] ~ 5,
      actual < quintile[5] & actual >= quintile[4] ~ 4,
      actual < quintile[4] & actual >= quintile[3] ~ 3,
      actual < quintile[3] & actual >= quintile[2] ~ 2,
      actual < quintile[2] & actual >= quintile[1] ~ 1
    )
  }
  
  quintile_unhealthy = function(actual){
    quintile = quantile(actual, probs=seq(0, 1, by=0.2))
    case_when(
      actual <= quintile[6] & actual >= quintile[5] ~ 1,
      actual < quintile[5] & actual >= quintile[4] ~ 2,
      actual < quintile[4] & actual >= quintile[3] ~ 3,
      actual < quintile[3] & actual >= quintile[2] ~ 4,
      actual < quintile[2] & actual >= quintile[1] ~ 5
    )
  }
  
  print("Reminder: this DASH index uses quintiles to rank participants' food/drink serving sizes and then calculate DASH component scores, which may generate results that are specific to your study population but not comparable to other populations.")
  
  ##DASH calculation
  SERV_DATA %>%
    dplyr::mutate(
      DASH_FRT = quintile_healthy(FRT_FRTJ_SERV),
      DASH_VEG = quintile_healthy(VEG_SERV),
      DASH_NUTSLEG = quintile_healthy(NUTSLEG_SERV),
      DASH_WGRAIN = quintile_healthy(WGRAIN_SERV),
      DASH_LOWF_DAIRY = quintile_healthy(LOWF_DAIRY_SERV),
      DASH_SODIUM = quintile_unhealthy(SODIUM_SERV),
      DASH_REDPROC_MEAT = quintile_unhealthy(REDPROC_MEAT_SERV),
      DASH_SSB_FRTJ = quintile_unhealthy(SSB_FRTJ_SERV),
      DASH_ALL = DASH_FRT+DASH_VEG+DASH_NUTSLEG+DASH_WGRAIN+DASH_LOWF_DAIRY+
        DASH_SODIUM+DASH_REDPROC_MEAT+DASH_SSB_FRTJ
    )%>%
    dplyr::select(RESPONDENTID, DASH_ALL, DASH_FRT, DASH_VEG, DASH_NUTSLEG, DASH_WGRAIN, DASH_LOWF_DAIRY,
           DASH_SODIUM, DASH_REDPROC_MEAT, DASH_SSB_FRTJ,
           
           DT_KCAL, F_BERRIES, F_WHOLE, FRT_FRTJ_SERV, VEG_SERV, NUTSLEG_SERV, WGRAIN_SERV, LOWF_MILK_SERV, YOGURT_SERV, LOWF_ICECREAMFROYO_SERV, LOWF_CHEESE_SERV, LOWF_DAIRY_SERV, SODIUM_SERV, SODIUM_SERV, REDPROC_MEAT_SERV, SSB_FRTJ_SERV)
  
}

#' DASHI_BLOCK Calculation
#'
#' Calculate the DASHI dietary index (serving size based) per 1 day
#' @import dplyr
#' @import readr
#' @import haven
#' @param RAW_DATA The raw data file that includes results and raw data of the dietary assessment
#' @return The DASHI index/score and its components
#' @examples
#' DASHI_BLOCK(RAW_DATA)
#' @export

DASHI_BLOCK = function(RAW_DATA){
  
  if (is.character(RAW_DATA) == TRUE){
    RAW_DATA = read_csv(RAW_DATA)
  } else {
    RAW_DATA = RAW_DATA
  }
  
  STD_FOOD_FREQ = c(1, 2, 3, 4, 5, 6, 7, 8, 9)
  STD_FREQ_SERV = c(0, 1/90, 1/30, 2.5/30, 1/7, 2/7, 3.5/7, 5.5/7, 1)
  STD_FOOD_PORT = c(1, 2, 3, 4)
  STD_PORT_SERV = c(0.25, 0.5, 1, 2)
  STD_FOOD_FREQ_DF = data.frame(STD_FOOD_FREQ, STD_FREQ_SERV, stringsAsFactors=FALSE)
  STD_FOOD_PORT_DF= data.frame(STD_FOOD_PORT, STD_PORT_SERV, stringsAsFactors=FALSE)
  
  #Functions to match actual food frequency and portion to the standards
  foodfreq = function(actual, reference=STD_FOOD_FREQ_DF){
    reference[match(actual, reference[,1]),2]
  }
  
  foodport = function(actual, reference=STD_FOOD_PORT_DF){
    reference[match(actual, reference[,1]),2]
  }
  
  #Match participant response food frequency to the standard food frequency response code
  YOGURT_FOOD_PORT = c(2, 3)
  YOGURT_PORT_SERV = c(0.5, 1)
  YOGURT_PORT_DF = data.frame(STD_FOOD_PORT, STD_PORT_SERV)
  
  BUTTERMILK_FOOD_PORT = c(1, 2, 3, 4)
  BUTTERMILK_PORT_SERV = c(0.25, 0.5, 1, 2)
  BUTTERMILK_PORT_DF = data.frame(STD_FOOD_PORT, STD_PORT_SERV)
  
  SERV_DATA=RAW_DATA %>%
    dplyr::mutate(
      F_BERRIES = foodfreq(STRAWBERRIESFREQ)*foodport(STRAWBERRIESQUAN),
      F_WHOLE = F_SOLID - F_BERRIES + F_BERRIES*2,
      VEG_SERV = V_DPYEL + 0.5*V_DRKGR + V_OTHER + V_STARCY + V_TOMATO,
      FRT_SERV = F_WHOLE + JUICE100,
      NUTSLEG_SERV = (LEGUMES*4) + M_NUTSD + M_SOY,
      LOWF_MILK_SERV = case_when(
        MILKTYPE==2 | MILKTYPE==3 | MILKTYPE==4 ~ foodfreq(MILKFREQ) * MILKQUAN, 
        TRUE ~ 0),
      YOGURT_SERV = (foodfreq(YOGURTONLYFREQ) * foodport(YOGURTONLYQUAN, ref=YOGURT_PORT_DF)) +
        (foodfreq(BUTTERMILKFREQ) * foodport(BUTTERMILKQUAN, ref=BUTTERMILK_PORT_DF)),
      LOWF_ICECREAMFROYO_SERV = case_when(
        ICECREAMFROYOTYPE == 2 ~ foodfreq(ICECREAMFROYOFREQ) * foodport(ICECREAMFROYOQUAN)*2,
        TRUE ~0),
      LOWF_CHEESE_SERV = case_when(
        CHEESETYPE == 1 ~ foodfreq(CHEESEFREQ) * CHEESEQUAN, 
        TRUE ~ 0),
      LOWFATDAIRY_SERV = LOWF_MILK_SERV+YOGURT_SERV+LOWF_ICECREAMFROYO_SERV+LOWF_CHEESE_SERV,
      WGRAIN_SERV = G_WHL,
      ALLMEAT_SERV = M_MPF,
      REDPROC_MEAT_SERV = (M_FRANK /1.5) + ((M_MEAT+M_ORGAN)/4),
      FATOIL_SERV = (DFAT_OIL+DFAT_SOL)/14,
      ADDEDSUGAR_SERV = ((ADD_SUG*4*4) / DT_KCAL)*100,
      SODIUM_SERV = DT_SODI/(DT_KCAL/1000)
    ) 
  
  ##Create variables and functions needed for DASHI calculation
  DASHI_MIN = 0
  DASHI_MAX = 5
  
  DASHI_MIN_VEG_SERV = 0
  DASHI_MAX_VEG_SERV = 4
  DASHI_MIN_FRT_SERV = 0
  DASHI_MAX_FRT_SERV = 4
  DASHI_MIN_NUTSLEG_SERV = 0
  DASHI_MAX_NUTSLEG_SERV = 4/7
  DASHI_MIN_LOWFATDAIRY_SERV = 0
  DASHI_MAX_LOWFATDAIRY_SERV = 2
  DASHI_MIN_WGRAIN_SERV = 0
  DASHI_MAX_WGRAIN_SERV = 3
  
  DASHI_MIN_ALLMEAT_SERV = 6
  DASHI_MAX_ALLMEAT_SERV= 2
  DASHI_MIN_REDPROC_MEAT_SERV = 1.5
  DASHI_MAX_REDPROC_MEAT_SERV = 0.5
  DASHI_MIN_FATOIL_SERV = 3
  DASHI_MAX_FATOIL_SERV = 2
  DASHI_MIN_ADDEDSUGAR_SERV = 10
  DASHI_MAX_ADDEDSUGAR_SERV = 0
  DASHI_MIN_SODIUM_SERV = 2300
  DASHI_MAX_SODIUM_SERV = 0
  
  DASHI_HEALTHY = function(actual, min, max){
    case_when(
      actual >= max ~ DASHI_MAX,
      actual <= min ~ DASHI_MIN,
      TRUE ~ DASHI_MIN+(actual-min)*DASHI_MAX/(max-min)
    )
  }
  
  DASHI_UNHEALTHY = function(actual, min, max){
    case_when(
      actual >= min ~ DASHI_MIN ,
      actual <= max ~ DASHI_MAX,
      TRUE ~ DASHI_MIN+(actual-min)*DASHI_MAX/(max-min)
    )
  }
  
  ##DASHI calculation
  SERV_DATA %>%
    dplyr::mutate(
      DASHI_VEG = DASHI_HEALTHY(VEG_SERV, DASHI_MIN_VEG_SERV, DASHI_MAX_VEG_SERV),
      DASHI_FRT = DASHI_HEALTHY(FRT_SERV, DASHI_MIN_FRT_SERV, DASHI_MAX_FRT_SERV),
      DASHI_NUTSLEG = DASHI_HEALTHY(NUTSLEG_SERV, DASHI_MIN_NUTSLEG_SERV, DASHI_MAX_NUTSLEG_SERV),
      DASHI_LOWFATDAIRY = DASHI_HEALTHY(LOWFATDAIRY_SERV, DASHI_MIN_LOWFATDAIRY_SERV, DASHI_MAX_LOWFATDAIRY_SERV),
      DASHI_WGRAIN = DASHI_HEALTHY(WGRAIN_SERV, DASHI_MIN_WGRAIN_SERV, DASHI_MAX_WGRAIN_SERV),
      
      DASHI_ALLMEAT = DASHI_UNHEALTHY(ALLMEAT_SERV, DASHI_MIN_ALLMEAT_SERV, DASHI_MAX_ALLMEAT_SERV),
      DASHI_REDPROC_MEAT = DASHI_UNHEALTHY(REDPROC_MEAT_SERV, DASHI_MIN_REDPROC_MEAT_SERV, DASHI_MAX_REDPROC_MEAT_SERV),
      DASHI_FATOIL = DASHI_UNHEALTHY(FATOIL_SERV, DASHI_MIN_FATOIL_SERV, DASHI_MAX_FATOIL_SERV),
      DASHI_ADDEDSUGAR = DASHI_UNHEALTHY(ADDEDSUGAR_SERV, DASHI_MIN_ADDEDSUGAR_SERV, DASHI_MAX_ADDEDSUGAR_SERV),
      DASHI_SODIUM = DASHI_UNHEALTHY(SODIUM_SERV, DASHI_MIN_SODIUM_SERV, DASHI_MAX_SODIUM_SERV),
      DASHI_ALL= DASHI_VEG + DASHI_FRT + DASHI_NUTSLEG + DASHI_LOWFATDAIRY +
        DASHI_WGRAIN + DASHI_ALLMEAT + DASHI_REDPROC_MEAT + DASHI_FATOIL + DASHI_ADDEDSUGAR + DASHI_SODIUM
    )%>%
    dplyr::select(RESPONDENTID, DASHI_ALL, DASHI_VEG, DASHI_FRT, DASHI_NUTSLEG, DASHI_LOWFATDAIRY, DASHI_WGRAIN,
           DASHI_ALLMEAT, DASHI_REDPROC_MEAT, DASHI_FATOIL, DASHI_ADDEDSUGAR, DASHI_SODIUM,
           
           DT_KCAL, F_BERRIES, F_WHOLE, VEG_SERV, FRT_SERV, NUTSLEG_SERV, LOWF_MILK_SERV, YOGURT_SERV, LOWF_ICECREAMFROYO_SERV, 
           LOWF_CHEESE_SERV, LOWFATDAIRY_SERV, WGRAIN_SERV, ALLMEAT_SERV, REDPROC_MEAT_SERV, FATOIL_SERV, ADDEDSUGAR_SERV, SODIUM_SERV)
  
}

#' MED_BLOCK Calculation
#'
#' Calculate the MED dietary index (serving size based) per 1 day
#' @import dplyr
#' @import readr
#' @import haven
#' @param RAW_DATA The raw data file that includes results and raw data of the dietary assessment
#' @return The MED index/score and its components
#' @examples
#' MED_BLOCK(RAW_DATA)
#' @export

MED_BLOCK = function(RAW_DATA){
  
  if (is.character(RAW_DATA) == TRUE){
    RAW_DATA = read_csv(RAW_DATA)
  } else {
    RAW_DATA = RAW_DATA
  }
  
  STD_FOOD_FREQ = c(1, 2, 3, 4, 5, 6, 7, 8, 9)
  STD_FREQ_SERV = c(0, 1/90, 1/30, 2.5/30, 1/7, 2/7, 3.5/7, 5.5/7, 1)
  STD_FOOD_PORT = c(1, 2, 3, 4)
  STD_PORT_SERV = c(0.25, 0.5, 1, 2)
  STD_FOOD_FREQ_DF = data.frame(STD_FOOD_FREQ, STD_FREQ_SERV, stringsAsFactors=FALSE)
  STD_FOOD_PORT_DF= data.frame(STD_FOOD_PORT, STD_PORT_SERV, stringsAsFactors=FALSE)
  
  #Functions to match actual food frequency and portion to the standards
  foodfreq = function(actual, reference=STD_FOOD_FREQ_DF){
    reference[match(actual, reference[,1]),2]
  }
  
  foodport = function(actual, reference=STD_FOOD_PORT_DF){
    reference[match(actual, reference[,1]),2]
  }
  
  #Match participant response food frequency to the standard food frequency response code
  SERV_DATA=RAW_DATA %>%
    dplyr::mutate(
      F_BERRIES = foodfreq(STRAWBERRIESFREQ)*foodport(STRAWBERRIESQUAN),
      F_WHOLE = F_SOLID - F_BERRIES + F_BERRIES*2,
      FRT_FRTJ_SERV = F_WHOLE + JUICE100,
      VEG_SERV = V_DPYEL + 0.5*V_DRKGR + V_OTHER + V_STARCY + V_TOMATO,
      WGRAIN_SERV = G_WHL,
      LEGUMES_SERV = (LEGUMES*4) + M_SOY,
      NUTS_SERV = M_NUTSD,
      FISH_SERV = (M_FISH_HI+M_FISH_LO)/4,
      REDPROC_MEAT_SERV = (M_FRANK/1.5) +  ((M_MEAT+M_ORGAN)/4),
      MONSATFAT_SERV = DT_MFAT/DT_SFAT,
      ALCOHOL_SERV=DT_ALCO
    ) 
  
  median_healthy = function(actual){
    median_score = median(actual)
    case_when(
      actual < median_score ~ 0,
      actual >= median_score ~ 1
    )
  }
  
  median_unhealthy = function(actual){
    median_score = median(actual)
    case_when(
      actual < median_score ~ 1,
      actual >= median_score ~ 0
    )
  }
  
  print("Reminder: this MED index uses medians to rank participants' food/drink serving sizes and then calculate MED component scores, which may generate results that are specific to your study population but not comparable to other populations.")
  
  SERV_DATA %>%
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
        ALCOHOL_SERV <=25 & ALCOHOL_SERV >= 10 ~ 1, 
        TRUE ~ 0),
      
      MED_ALL = MED_FRT+MED_VEG+MED_WGRAIN+MED_LEGUMES+MED_NUTS+MED_FISH+MED_REDPROC_MEAT+MED_MONSATFAT+MED_ALCOHOL,
      MED_NOETOH = MED_FRT+MED_VEG+MED_WGRAIN+MED_LEGUMES+MED_NUTS+MED_FISH+MED_REDPROC_MEAT+MED_MONSATFAT
    )%>%
    dplyr::select(RESPONDENTID, MED_ALL, MED_NOETOH, MED_FRT, MED_VEG, MED_WGRAIN, MED_LEGUMES, MED_NUTS,
           MED_FISH, MED_REDPROC_MEAT, MED_MONSATFAT, MED_ALCOHOL,
           
           F_BERRIES, F_WHOLE, FRT_FRTJ_SERV, VEG_SERV, WGRAIN_SERV, LEGUMES_SERV, NUTS_SERV, FISH_SERV, 
           REDPROC_MEAT_SERV, MONSATFAT_SERV, ALCOHOL_SERV)
  
}

#' MEDI_BLOCK Calculation
#'
#' Calculate the MEDI dietary index (serving size based) per 1 day
#' @import dplyr
#' @import readr
#' @import haven
#' @param RAW_DATA The raw data file that includes results and raw data of the dietary assessment
#' @return The MEDI index/score and its components
#' @examples
#' MEDI_BLOCK(RAW_DATA)
#' @export

MEDI_BLOCK = function(RAW_DATA){
  
  if (is.character(RAW_DATA) == TRUE){
    RAW_DATA = read_csv(RAW_DATA)
  } else {
    RAW_DATA = RAW_DATA
  }
  
  STD_FOOD_FREQ = c(1, 2, 3, 4, 5, 6, 7, 8, 9)
  STD_FREQ_SERV = c(0, 1/90, 1/30, 2.5/30, 1/7, 2/7, 3.5/7, 5.5/7, 1)
  STD_FOOD_PORT = c(1, 2, 3, 4)
  STD_PORT_SERV = c(0.25, 0.5, 1, 2)
  STD_FOOD_FREQ_DF = data.frame(STD_FOOD_FREQ, STD_FREQ_SERV, stringsAsFactors=FALSE)
  STD_FOOD_PORT_DF= data.frame(STD_FOOD_PORT, STD_PORT_SERV, stringsAsFactors=FALSE)
  
  #Functions to match actual food frequency and portion to the standards
  foodfreq = function(actual, reference=STD_FOOD_FREQ_DF){
    reference[match(actual, reference[,1]),2]
  }
  
  foodport = function(actual, reference=STD_FOOD_PORT_DF){
    reference[match(actual, reference[,1]),2]
  }
  
  #Match participant response food frequency to the standard food frequency response code
  SERV_DATA=RAW_DATA %>%
    dplyr::mutate(
      F_BERRIES = foodfreq(STRAWBERRIESFREQ)*foodport(STRAWBERRIESQUAN),
      F_WHOLE = F_SOLID - F_BERRIES + F_BERRIES*2,
      FRT_FRTJ_SERV = F_WHOLE + JUICE100,
      VEG_SERV = V_DPYEL + 0.5*V_DRKGR + V_OTHER + V_STARCY + V_TOMATO,
      LEGUMES_SERV = (LEGUMES*4) + M_SOY,
      WGRAIN_SERV = G_WHL,
      FISH_SERV = (M_FISH_HI+M_FISH_LO)/4,
      DAIRY_SERV = D_TOTAL,
      REDPROC_MEAT_SERV = (M_FRANK/1.5) +  ((M_MEAT+M_ORGAN)/4),
      NUTS_SERV = M_NUTSD,
      MONSATFAT_SERV = DT_MFAT/DT_SFAT,
      ALCOHOL_SERV=DT_ALCO
    ) 
  
  SERV_DATA %>%
    dplyr::mutate(
      MEDI_FRT = case_when(FRT_FRTJ_SERV >=3 ~ 1, TRUE ~ 0),
      MEDI_VEG = case_when(VEG_SERV >= 3 ~ 1, TRUE ~ 0),
      MEDI_LEGUMES = case_when(LEGUMES_SERV >= 1.5 ~ 1, TRUE ~ 0),
      MEDI_WGRAIN = case_when(WGRAIN_SERV >= 3 ~ 1, TRUE ~ 0),
      MEDI_FISH = case_when(FISH_SERV >= 2 ~ 1, TRUE ~ 0),
      MEDI_DAIRY = case_when(DAIRY_SERV >= 2 ~ 1, TRUE ~ 0),
      MEDI_REDPROC_MEAT = case_when(REDPROC_MEAT_SERV < 4.5 ~ 1, TRUE ~ 0),
      MEDI_NUTS = case_when(NUTS_SERV >= 2 ~ 1, TRUE ~ 0),
      MEDI_MONSATFAT = case_when(MONSATFAT_SERV >= 1.6 ~ 1, TRUE ~ 0),
      MEDI_ALCOHOL = case_when(ALCOHOL_SERV <=25 & ALCOHOL_SERV >= 10 ~ 1, TRUE ~ 0),
      
      MEDI_ALL = MEDI_FRT+MEDI_VEG+MEDI_LEGUMES+MEDI_WGRAIN+MEDI_FISH+MEDI_DAIRY+MEDI_REDPROC_MEAT+
        MEDI_NUTS+MEDI_MONSATFAT+MEDI_ALCOHOL,
      MEDI_NOETOH = MEDI_FRT+MEDI_VEG+MEDI_LEGUMES+MEDI_WGRAIN+MEDI_FISH+MEDI_DAIRY+MEDI_REDPROC_MEAT
      +MEDI_NUTS+MEDI_MONSATFAT
    )%>%
    dplyr::select(RESPONDENTID, MEDI_ALL, MEDI_NOETOH, MEDI_FRT, MEDI_VEG, MEDI_LEGUMES, MEDI_WGRAIN, MEDI_FISH,
           MEDI_DAIRY, MEDI_REDPROC_MEAT, MEDI_NUTS, MEDI_MONSATFAT, MEDI_ALCOHOL,
           
           F_BERRIES, F_WHOLE, FRT_FRTJ_SERV, VEG_SERV, LEGUMES_SERV, WGRAIN_SERV, FISH_SERV, DAIRY_SERV, REDPROC_MEAT_SERV, NUTS_SERV, MONSATFAT_SERV, ALCOHOL_SERV)
}


#' AHEIP_BLOCK Calculation
#'
#' Calculate the AHEIP dietary index (serving size based) per 1 day
#' @import dplyr
#' @import readr
#' @import haven
#' @param RAW_DATA The raw data file that includes results and raw data of the dietary assessment
#' @return The AHEIP index/score and its components
#' @examples
#' AHEIP_BLOCK(RAW_DATA)
#' @export

AHEIP_BLOCK = function(RAW_DATA){
  
  if (is.character(RAW_DATA) == TRUE){
    RAW_DATA = read_csv(RAW_DATA)
  } else {
    RAW_DATA = RAW_DATA
  }
  
  STD_FOOD_FREQ = c(1, 2, 3, 4, 5, 6, 7, 8, 9)
  STD_FREQ_SERV = c(0, 1/90, 1/30, 2.5/30, 1/7, 2/7, 3.5/7, 5.5/7, 1)
  STD_FOOD_PORT = c(1, 2, 3, 4)
  STD_PORT_SERV = c(0.25, 0.5, 1, 2)
  STD_FOOD_FREQ_DF = data.frame(STD_FOOD_FREQ, STD_FREQ_SERV, stringsAsFactors=FALSE)
  STD_FOOD_PORT_DF= data.frame(STD_FOOD_PORT, STD_PORT_SERV, stringsAsFactors=FALSE)
  
  #Functions to match actual food frequency and portion to the standards
  foodfreq = function(actual, reference=STD_FOOD_FREQ_DF){
    reference[match(actual, reference[,1]),2]
  }
  
  foodport = function(actual, reference=STD_FOOD_PORT_DF){
    reference[match(actual, reference[,1]),2]
  }
  
  #Match participant response food frequency to the standard food frequency response code
  SERV_DATA=RAW_DATA %>%
    dplyr::mutate(
      F_BERRIES = foodfreq(STRAWBERRIESFREQ)*foodport(STRAWBERRIESQUAN),
      F_WHOLE = F_SOLID - F_BERRIES + F_BERRIES*2,
      VEG_SERV = V_DPYEL + 0.5*V_DRKGR + V_OTHER + V_STARCY + V_TOMATO,
      FRT_SERV = F_WHOLE,
      WHITERED_RT_SERV = ((M_POULT+M_FISH_HI+M_FISH_LO)/4) /  ((M_FRANK /1.5) + ((M_MEAT+M_ORGAN)/4)),
      FIBER_SERV = DT_FIBE,
      TRANS_SERV = ((DT_TRFAT * 9) / DT_KCAL)*100,
      POLYSAT_RT = DT_PFAT / DT_SFAT,
      CALCIUM_SERV = DT_CALC,
      FOLATE_SERV = DT_FOLFD,
      IRON_SERV = DT_IRON
    ) 
  
  AHEIP_MIN = 0
  AHEIP_MAX = 10
  
  AHEIP_MIN_VEG_SERV = 0
  AHEIP_MAX_VEG_SERV = 5
  AHEIP_MIN_FRT_SERV = 0
  AHEIP_MAX_FRT_SERV = 4
  AHEIP_MIN_WHITERED_SERV = 0
  AHEIP_MAX_WHITERED_SERV = 4
  AHEIP_MIN_FIBER_SERV = 0
  AHEIP_MAX_FIBER_SERV = 25
  
  AHEIP_MIN_TRANS_SERV = 4
  AHEIP_MAX_TRANS_SERV = 0.5
  AHEIP_MIN_POLYSAT_SERV = 0.1
  AHEIP_MAX_POLYSAT_SERV = 1
  AHEIP_MIN_CALCIUM_SERV = 0
  AHEIP_MAX_CALCIUM_SERV = 1200
  AHEIP_MIN_FOLATE_SERV = 0
  AHEIP_MAX_FOLATE_SERV = 600
  AHEIP_MIN_IRON_SERV = 0
  AHEIP_MAX_IRON_SERV = 27
  
  AHEIP_HEALTHY = function(actual, min, max){
    case_when(
      actual >= max ~ AHEIP_MAX,
      actual <= min ~ AHEIP_MIN,
      TRUE ~ AHEIP_MIN+(actual-min)*AHEIP_MAX/(max-min)
    )
  }
  
  AHEIP_UNHEALTHY = function(actual, min, max){
    case_when(
      actual >= min ~ AHEIP_MIN ,
      actual <= max ~ AHEIP_MAX,
      TRUE ~ AHEIP_MIN+(actual-min)*AHEIP_MAX/(max-min)
    )
  }
  
  SERV_DATA %>%
    dplyr::mutate(
      AHEIP_VEG = AHEIP_HEALTHY(VEG_SERV, AHEIP_MIN_VEG_SERV, AHEIP_MAX_VEG_SERV),
      AHEIP_FRT = AHEIP_HEALTHY(FRT_SERV, AHEIP_MIN_FRT_SERV, AHEIP_MAX_FRT_SERV),
      AHEIP_WHITEREAD = AHEIP_HEALTHY(WHITERED_RT_SERV, AHEIP_MIN_WHITERED_SERV, AHEIP_MAX_WHITERED_SERV),
      AHEIP_FIBER = AHEIP_HEALTHY(FIBER_SERV, AHEIP_MIN_FIBER_SERV, AHEIP_MAX_FIBER_SERV),
      AHEIP_TRANS = AHEIP_UNHEALTHY(TRANS_SERV, AHEIP_MIN_TRANS_SERV, AHEIP_MAX_TRANS_SERV),
      AHEIP_POLYSAT = AHEIP_HEALTHY(POLYSAT_RT, AHEIP_MIN_POLYSAT_SERV, AHEIP_MAX_POLYSAT_SERV),
      AHEIP_CALCIUM = AHEIP_HEALTHY(CALCIUM_SERV, AHEIP_MIN_CALCIUM_SERV, AHEIP_MAX_CALCIUM_SERV),
      AHEIP_FOLATE= AHEIP_HEALTHY(FOLATE_SERV, AHEIP_MIN_FOLATE_SERV, AHEIP_MAX_FOLATE_SERV),
      AHEIP_IRON = AHEIP_HEALTHY(IRON_SERV, AHEIP_MIN_IRON_SERV, AHEIP_MAX_IRON_SERV),
      AHEIP_ALL = AHEIP_VEG + AHEIP_FRT + AHEIP_WHITEREAD + AHEIP_FIBER + AHEIP_TRANS +
        AHEIP_POLYSAT + AHEIP_CALCIUM + AHEIP_FOLATE + AHEIP_IRON
    )%>%
    dplyr::select(RESPONDENTID, AHEIP_ALL, AHEIP_VEG, AHEIP_FRT, AHEIP_WHITEREAD, AHEIP_FIBER, AHEIP_TRANS,
           AHEIP_POLYSAT, AHEIP_CALCIUM, AHEIP_FOLATE, AHEIP_IRON,
           
           F_BERRIES, F_WHOLE, VEG_SERV, FRT_SERV, WHITERED_RT_SERV, FIBER_SERV, TRANS_SERV, POLYSAT_RT, CALCIUM_SERV, FOLATE_SERV, IRON_SERV)
  
}


#' HEI2015_BLOCK Calculation
#'
#' Calculate the HEI2015 dietary index (serving size based) per 1 day
#' @import dplyr
#' @import readr
#' @import haven
#' @param RAW_DATA The raw data file that includes results and raw data of the dietary assessment
#' @return The HEI2015 index/score and its components
#' @examples
#' HEI2015_BLOCK(RAW_DATA)
#' @export


HEI2015_BLOCK = function(RAW_DATA){
  
  if (is.character(RAW_DATA) == TRUE){
    RAW_DATA = read_csv(RAW_DATA)
  } else {
    RAW_DATA = RAW_DATA
  }
  
  #Match participant response food frequency to the standard food frequency response code
  SERV_DATA=RAW_DATA %>%
    dplyr::mutate(
      TOTALFRT_SERV = F_TOTAL/(DT_KCAL/1000),
      FRT_SERV = F_SOLID/(DT_KCAL/1000),
      VEG_SERV = (V_TOTAL+LEGUMES)/(DT_KCAL/1000),
      GREEN_N_BEAN_SERV = (V_DRKGR+LEGUMES)/(DT_KCAL/1000),
      TOTALPRO_SERV = (M_MPF+M_EGG+M_NUTSD+M_SOY+(LEGUMES*4))/(DT_KCAL/1000),
      SEAPLANTPRO_SERV = (M_FISH_HI+M_FISH_LO+M_SOY+M_NUTSD+(LEGUMES*4))/(DT_KCAL/1000),
      WHOLEGRAIN_SERV = G_WHL/(DT_KCAL/1000),
      DAIRY_SERV = D_TOTAL/(DT_KCAL/1000),
      FATTYACID_SERV = case_when(
        DT_SFAT == 0 ~ 0, 
        TRUE ~ (DT_MFAT + DT_PFAT)/DT_SFAT
        ),
      
      REFINEDGRAIN_SERV = G_NWHL/(DT_KCAL/1000),
      SODIUM_SERV = (DT_SODI/1000)/(DT_KCAL/1000),
      ADDEDSUGAR_SERV = ((ADD_SUG*4*4) / DT_KCAL)*100,
      SATFAT_SERV = ((DT_SFAT*9)/DT_KCAL)*100
      
    ) 
  
  ##Create variables needed for HEI2015 calculation
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
  
  HEI2015_HEALTHY1 = function(actual, min, max){
    case_when(
      actual >= max ~ HEI2015_MAX1,
      actual <= min ~ HEI2015_MIN,
      TRUE ~ HEI2015_MIN+(actual-min)*HEI2015_MAX1/(max-min)
    )
  }
  
  HEI2015_HEALTHY2 = function(actual, min, max){
    case_when(
      actual >= max ~ HEI2015_MAX2,
      actual <= min ~ HEI2015_MIN,
      TRUE ~ HEI2015_MIN+(actual-min)*HEI2015_MAX2/(max-min)
    )
  }
  
  HEI2015_UNHEALTHY = function(actual, min, max){
    case_when(
      actual >= min ~ HEI2015_MIN,
      actual <= max ~ HEI2015_MAX2,
      TRUE ~ HEI2015_MIN+(actual-min)*HEI2015_MAX2/(max-min)
    )
  }
  
  SERV_DATA=SERV_DATA %>%
    dplyr::mutate(
      HEI2015_TOTALFRT = HEI2015_HEALTHY1(TOTALFRT_SERV, HEI2015_MIN_TOTALFRT_SERV, HEI2015_MAX_TOTALFRT_SERV),
      HEI2015_FRT = HEI2015_HEALTHY1(FRT_SERV, HEI2015_MIN_FRT_SERV, HEI2015_MAX_FRT_SERV),
      HEI2015_VEG = HEI2015_HEALTHY1(VEG_SERV, HEI2015_MIN_VEG_SERV, HEI2015_MAX_VEG_SERV),
      HEI2015_GREENNBEAN = HEI2015_HEALTHY1(GREEN_N_BEAN_SERV, HEI2015_MIN_GREENNBEAN_SERV, HEI2015_MAX_GREENNBEAN_SERV),
      HEI2015_TOTALPRO = HEI2015_HEALTHY1(TOTALPRO_SERV, HEI2015_MIN_TOTALPRO_SERV, HEI2015_MAX_TOTALPRO_SERV),
      HEI2015_SEAPLANTPRO = HEI2015_HEALTHY1(SEAPLANTPRO_SERV, HEI2015_MIN_SEAPLANTPRO_SERV, HEI2015_MAX_SEAPLANTPRO_SERV),
      HEI2015_WHOLEGRAIN = HEI2015_HEALTHY2(WHOLEGRAIN_SERV, HEI2015_MIN_WHOLEGRAIN_SERV, HEI2015_MAX_WHOLEGRAIN_SERV),
      HEI2015_DAIRY = HEI2015_HEALTHY2(DAIRY_SERV, HEI2015_MIN_DAIRY_SERV, HEI2015_MAX_DAIRY_SERV),
      HEI2015_FATTYACID = HEI2015_HEALTHY2(FATTYACID_SERV, HEI2015_MIN_FATTYACID_SERV, HEI2015_MAX_FATTYACID_SERV),
      
      HEI2015_REFINEDGRAIN = HEI2015_UNHEALTHY(REFINEDGRAIN_SERV, HEI2015_MIN_REFINEDGRAIN_SERV, HEI2015_MAX_REFINEDGRAIN_SERV),
      HEI2015_SODIUM = HEI2015_UNHEALTHY(SODIUM_SERV, HEI2015_MIN_SODIUM_SERV, HEI2015_MAX_SODIUM_SERV),
      HEI2015_ADDEDSUGAR = HEI2015_UNHEALTHY(ADDEDSUGAR_SERV, HEI2015_MIN_ADDEDSUGAR_SERV, HEI2015_MAX_ADDEDSUGAR_SERV),
      HEI2015_SATFAT = HEI2015_UNHEALTHY(SATFAT_SERV, HEI2015_MIN_SATFAT_SERV, HEI2015_MAX_SATFAT_SERV),
      
      HEI2015_ALL= HEI2015_TOTALFRT + HEI2015_FRT + HEI2015_VEG + HEI2015_GREENNBEAN +
        HEI2015_TOTALPRO + HEI2015_SEAPLANTPRO + HEI2015_WHOLEGRAIN + HEI2015_DAIRY +
        HEI2015_FATTYACID + HEI2015_REFINEDGRAIN + HEI2015_SODIUM + HEI2015_ADDEDSUGAR +
        HEI2015_SATFAT
    ) 
  
  
  SERV_DATA %>%
    dplyr::select(RESPONDENTID, HEI2015_ALL, HEI2015_TOTALFRT, HEI2015_FRT, HEI2015_VEG, HEI2015_GREENNBEAN,
           HEI2015_TOTALPRO, HEI2015_SEAPLANTPRO, HEI2015_WHOLEGRAIN, HEI2015_DAIRY,
           HEI2015_FATTYACID, HEI2015_REFINEDGRAIN, HEI2015_SODIUM, HEI2015_ADDEDSUGAR,
           HEI2015_SATFAT,
           
           TOTALFRT_SERV, FRT_SERV, VEG_SERV, GREEN_N_BEAN_SERV, TOTALPRO_SERV, SEAPLANTPRO_SERV, WHOLEGRAIN_SERV, DAIRY_SERV, FATTYACID_SERV, REFINEDGRAIN_SERV, SODIUM_SERV, ADDEDSUGAR_SERV, SATFAT_SERV)
  
}

#' DII_BLOCK Calculation
#'
#' Calculate the DII dietary index (serving size based) per 1 day
#' @import dplyr
#' @import readr
#' @import haven
#' @param RAW_DATA The raw data file that includes results and raw data of the dietary assessment
#' @return The DII index/score and its components
#' @examples
#' DII_BLOCK(RAW_DATA)
#' @export

DII_BLOCK = function(RAW_DATA){
  
  if (is.character(RAW_DATA) == TRUE){
    RAW_DATA = read_csv(RAW_DATA)
  } else {
    RAW_DATA = RAW_DATA
  }
  
  STD_FOOD_FREQ = c(1, 2, 3, 4, 5, 6, 7, 8, 9)
  STD_FREQ_SERV = c(0, 1/90, 1/30, 2.5/30, 1/7, 2/7, 3.5/7, 5.5/7, 1)
  STD_FOOD_PORT = c(1, 2, 3, 4)
  STD_PORT_SERV = c(0.25, 0.5, 1, 2)
  STD_FOOD_FREQ_DF = data.frame(STD_FOOD_FREQ, STD_FREQ_SERV, stringsAsFactors=FALSE)
  STD_FOOD_PORT_DF= data.frame(STD_FOOD_PORT, STD_PORT_SERV, stringsAsFactors=FALSE)
  
  #Functions to match actual food frequency and portion to the standards
  foodfreq = function(actual, reference=STD_FOOD_FREQ_DF){
    reference[match(actual, reference[,1]),2]
  }
  
  foodport = function(actual, reference=STD_FOOD_PORT_DF){
    reference[match(actual, reference[,1]),2]
  }
  
  #Serving size calculation for DII
  COHORT = RAW_DATA %>%
    dplyr::mutate(
      ALCOHOL = DT_ALCO,
      VITB12 = DT_VB12,
      VITB6 = DT_VITB6,
      BCAROTENE = DT_BCARO,
      CAFFEINE = DT_CAFFN/1000,
      CARB = DT_CARB,
      CHOLES = DT_CHOL,
      KCAL = DT_KCAL,
      TOTALFAT = DT_TFAT,
      FIBER = DT_FIBE,
      FOLICACID = DT_FOLAC,
      IRON = DT_IRON,
      MG = DT_MAGN,
      MUFA = DT_MFAT,
      NIACIN = DT_NIAC,
      N3FAT = DT_TOTN3,
      N6FAT = DT_TOTN6,
      PROTEIN = DT_PROT,
      PUFA = DT_PFAT,
      RIBOFLAVIN = DT_RIBO,
      SATFAT = DT_SFAT,
      SE = DT_SEL,
      THIAMIN = DT_THIA,
      TRANSFAT = DT_TRFAT,
      VITA = DT_VARAE,
      VITC = DT_VITC,
      VITD = DT_VITD*0.025,
      VITE = DT_ATOC,
      ZN = DT_ZINC,
      TEA = foodfreq(ICEDTEAFREQ)*ICEDTEAQUAN*240 + foodfreq(HOTTEAFREQ)*HOTTEAQUAN*240,
      ISOFLAVONES = DT_ISOFLV
    ) 
  
  COHORT1 = COHORT %>%
    dplyr::select(RESPONDENTID, ALCOHOL, VITB12, VITB6, BCAROTENE,CAFFEINE,CARB,CHOLES,KCAL,TOTALFAT,FIBER,FOLICACID,
                  IRON,MG,MUFA,NIACIN,N3FAT,N6FAT,PROTEIN,PUFA,RIBOFLAVIN,SATFAT,SE,THIAMIN,TRANSFAT,
                  VITA,VITC,VITD,VITE,ZN,TEA,ISOFLAVONES)
  
  COHORT1 = COHORT1 %>%
    tidyr::pivot_longer(-RESPONDENTID, names_to="Variable", values_to="Value")
  
  
  Variable = c("ALCOHOL", "VITB12", "VITB6", "BCAROTENE", "CAFFEINE", "CARB", "CHOLES", "KCAL", "EUGENOL",
               "TOTALFAT", "FIBER", "FOLICACID","GARLIC", "GINGER","IRON", "MG", "MUFA", "NIACIN", "N3FAT", "N6FAT","ONION", "PROTEIN", "PUFA", 
               "RIBOFLAVIN","SAFFRON", "SATFAT", "SE", "THIAMIN","TRANSFAT","TURMERIC", "VITA","VITC", "VITD", "VITE", "ZN", "TEA",
               "FLA3OL","FLAVONES","FLAVONOLS","FLAVONONES","ANTHOC","ISOFLAVONES","PEPPER","THYME","ROSEMARY")
  
  Overall_inflammatory_score = c(-0.278, 0.106, -0.365, -0.584, -0.11, 0.097, 0.11, 0.18, -0.14, 0.298, -0.663, -0.19, -0.412, -0.453, 0.032, -0.484, -0.009,
                                 -0.246, -0.436, -0.159, -0.301, 0.021, -0.337, -0.068, -0.14, 0.373, -0.191, -0.098,0.229,-0.785, -0.401, -0.424, -0.446, -0.419, -0.313,
                                 -0.536,-0.415,-0.616,-0.467,-0.25,-0.131,-0.593,-0.131,-0.102,-0.013)
  
  Global_mean = c(13.98,5.15,1.47,3718,8.05,272.2,279.4,2056,0.01,71.4,18.8,273,4.35,59,13.35,310.1,27,25.9,1.06,10.8,35.9,
                  79.4,13.88,1.7,0.37,28.6,67,1.7,3.15,533.6,983.9,118.2,6.26,8.73,9.84,
                  1.69,95.8,1.55,17.7,11.7,18.05,1.2,10,0.33,1)
  
  SD = c(3.72,2.7,0.74,1720,6.67,40,51.2,338,0.08,19.4,4.9,70.7,2.9,63.2,3.71,139.4,6.1,11.77,1.06,7.5,18.4,13.9,3.76,0.79,1.78,
         8,25.1,0.66,3.75,754.3,518.6,43.46,2.21,1.49,2.19,
         1.53,85.9,0.07,6.79,3.82,21.14,0.2,7.07,0.99,15)
  
  DII_STD = data.frame(Variable, Overall_inflammatory_score, Global_mean, SD)
  
  #Score calculation for DII  
  
   COHORT1 %>%
    inner_join(DII_STD, by=c("Variable")) %>%
    dplyr::mutate(
      Z_SCORE = (Value - Global_mean)/SD,
      PERCENTILE = pnorm(Z_SCORE)*2 - 1,
      IND_DII_SCORE = PERCENTILE*Overall_inflammatory_score) %>%
    tidyr::pivot_wider(names_from = Variable, values_from = IND_DII_SCORE) %>%
    dplyr::group_by(RESPONDENTID) %>%
    dplyr::summarize(
      DII_ALL = sum(ALCOHOL, VITB12, VITB6, BCAROTENE,CAFFEINE,CARB,CHOLES,KCAL,TOTALFAT,FIBER,FOLICACID,
                    IRON,MG,MUFA,NIACIN,N3FAT,N6FAT,PROTEIN,PUFA,RIBOFLAVIN,SATFAT,SE,THIAMIN,TRANSFAT,
                    VITA,VITC,VITD,VITE,ZN,TEA,ISOFLAVONES, na.rm = TRUE),
      
      DII_NOETOH =  sum(VITB12, VITB6, BCAROTENE,CAFFEINE,CARB,CHOLES,KCAL,TOTALFAT,FIBER,FOLICACID,
                        IRON,MG,MUFA,NIACIN,N3FAT,N6FAT,PROTEIN,PUFA,RIBOFLAVIN,SATFAT,SE,THIAMIN,TRANSFAT,
                        VITA,VITC,VITD,VITE,ZN,TEA,ISOFLAVONES, na.rm = TRUE),
      
      ALCOHOL = sum(ALCOHOL, na.rm = TRUE), 
      VITB12 = sum(VITB12, na.rm = TRUE), 
      VITB6 = sum(VITB6, na.rm = TRUE), 
      BCAROTENE = sum(BCAROTENE, na.rm = TRUE), 
      CAFFEINE = sum(CAFFEINE, na.rm = TRUE), 
      CARB = sum(CARB, na.rm = TRUE), 
      CHOLES = sum(CHOLES, na.rm = TRUE), 
      KCAL= sum(KCAL, na.rm = TRUE), 
      TOTALFAT= sum(TOTALFAT, na.rm = TRUE), 
      FIBER  = sum(FIBER, na.rm = TRUE), 
      FOLICACID  = sum(FOLICACID, na.rm = TRUE),
      IRON  = sum(IRON, na.rm = TRUE), 
      MG  = sum(MG, na.rm = TRUE), 
      MUFA  = sum(MUFA, na.rm = TRUE), 
      NIACIN  = sum(NIACIN, na.rm = TRUE), 
      N3FAT  = sum(N3FAT, na.rm = TRUE), 
      N6FAT  = sum(N6FAT, na.rm = TRUE), 
      PROTEIN  = sum(PROTEIN, na.rm = TRUE), 
      PUFA  = sum(PUFA, na.rm = TRUE), 
      RIBOFLAVIN  = sum(RIBOFLAVIN, na.rm = TRUE), 
      SATFAT  = sum(SATFAT, na.rm = TRUE), 
      SE  = sum(SE, na.rm = TRUE), 
      THIAMIN  = sum(THIAMIN, na.rm = TRUE), 
      TRANSFAT = sum(TRANSFAT, na.rm = TRUE),
      VITA  = sum(VITA, na.rm = TRUE),
      VITC  = sum(VITC), na.rm = TRUE, 
      VITD  = sum(VITD, na.rm = TRUE), 
      VITE = sum(VITE, na.rm = TRUE), 
      ZN = sum(ZN, na.rm = TRUE),
      TEA = sum(TEA, na.rm = TRUE),
      ISOFLAVONES = sum(ISOFLAVONES, na.rm = TRUE)
    )
  
}

#' HEI2015_AARP Calculation
#'
#' Calculate the HEI2015 dietary index (serving size based) per 1 day
#' @import dplyr
#' @import readr
#' @import haven
#' @param RAW_DATA The raw data file that includes results and raw data of the dietary assessment
#' @return The HEI2015 index/score and its components
#' @examples
#' HEI2015_AARP(RAW_DATA)
#' @export

HEI2015_AARP = function(RAW_DATA){
  
  if (is.character(RAW_DATA) == TRUE){
    RAW_DATA = read_csv(RAW_DATA)
  } else {
    RAW_DATA = RAW_DATA
  }
  
  SERV_DATA=RAW_DATA %>%
    dplyr::mutate(
      mped_M_SOY=0,
      TOTALFRT_SERV = mped_f_total/(calories/1000),
      FRT_SERV = mped_f_nojuice/(calories/1000),
      VEG_SERV = (mped_v_total+mped_legumes)/(calories/1000),
      GREENNBEAN_SERV = (mped_v_drkgr+mped_legumes)/(calories/1000),
      TOTALPRO_SERV = (mped_M_MPF+mped_M_EGG+mped_M_NUTSD+mped_M_SOY+(mped_legumes*4))/(calories/1000),
      SEAPLANTPRO_SERV = (mped_M_FISH_HI+mped_M_FISH_LO+mped_M_SOY+(mped_legumes*4))/(calories/1000),
      WHOLEGRAIN_SERV = mped_g_whl/(calories/1000),
      DAIRY_SERV = mped_d_total/(calories/1000),
      FATTYACID_SERV = case_when(
        fatsaturated == 0 ~ 0, 
        TRUE ~ (fatmono+fatpoly)/fatsaturated
        ),
      
      REFINEDGRAIN_SERV = mped_G_NWHL/(calories/1000),
      SODIUM_SERV = (SODIUM/1000)/(calories/1000),
      ADDEDSUGAR_SERV = ((mped_add_sug*4*4) / calories)*100,
      SATFAT_SERV = ((fatsaturated*9)/calories)*100,
      
      TOTALKCAL = calories
    ) 
  
  ##Create variables needed for HEI2015 calculation
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
  
  HEI2015_HEALTHY1 = function(actual, min, max){
    case_when(
      actual >= max ~ HEI2015_MAX1,
      actual <= min ~ HEI2015_MIN,
      TRUE ~ HEI2015_MIN+(actual-min)*HEI2015_MAX1/(max-min)
    )
  }
  
  HEI2015_HEALTHY2 = function(actual, min, max){
    case_when(
      actual >= max ~ HEI2015_MAX2,
      actual <= min ~ HEI2015_MIN,
      TRUE ~ HEI2015_MIN+(actual-min)*HEI2015_MAX2/(max-min)
    )
  }
  
  HEI2015_UNHEALTHY = function(actual, min, max){
    case_when(
      actual >= min ~ HEI2015_MIN,
      actual <= max ~ HEI2015_MAX2,
      TRUE ~ HEI2015_MIN+(actual-min)*HEI2015_MAX2/(max-min)
    )
  }
  
  SERV_DATA=SERV_DATA %>%
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
      
      HEI2015_ALL= HEI2015_TOTALFRT + HEI2015_FRT + HEI2015_VEG + HEI2015_GREENNBEAN +
        HEI2015_TOTALPRO + HEI2015_SEAPLANTPRO + HEI2015_WHOLEGRAIN + HEI2015_DAIRY +
        HEI2015_FATTYACID + HEI2015_REFINEDGRAIN + HEI2015_SODIUM + HEI2015_ADDEDSUGAR +
        HEI2015_SATFAT
    ) 
  
  for(i in 1:length(SERV_DATA$TOTALKCAL)){
    if (TOTALKCAL[i] == 0){
      HEI2015_TOTALFRT[i] = 0
      HEI2015_FRT[i] = 0
      HEI2015_VEG[i] = 0
      HEI2015_GREENNBEAN[i] = 0
      HEI2015_TOTALPRO[i] = 0
      HEI2015_SEAPLANTPRO[i] = 0
      HEI2015_WHOLEGRAIN[i] = 0
      HEI2015_DAIRY[i] = 0
      HEI2015_FATTYACID[i] = 0
      HEI2015_REFINEDGRAIN[i] = 0
      HEI2015_ADDEDSUGAR[i] = 0
      HEI2015_ALL[i] = 0
    }
  }
  
  SERV_DATA %>%
    dplyr::select(RESPONDENTID, HEI2015_ALL, HEI2015_TOTALFRT, HEI2015_FRT, HEI2015_VEG, HEI2015_GREENNBEAN,
           HEI2015_TOTALPRO, HEI2015_SEAPLANTPRO, HEI2015_WHOLEGRAIN, HEI2015_DAIRY,
           HEI2015_FATTYACID, HEI2015_REFINEDGRAIN, HEI2015_SODIUM, HEI2015_ADDEDSUGAR,
           HEI2015_SATFAT,
           
           TOTALKCAL, TOTALFRT_SERV, FRT_SERV, VEG_SERV, GREENNBEAN_SERV, TOTALPRO_SERV, SEAPLANTPRO_SERV, WHOLEGRAIN_SERV, DAIRY_SERV, FATTYACID_SERV, REFINEDGRAIN_SERV, SODIUM_SERV, ADDEDSUGAR_SERV, SATFAT_SERV)
  
}

#' HEI2015_NHANES_FPED
#'
#' Calculate the HEI2015 for the NHANES_FPED data (after 2005) within 1 step  
#' @import dplyr
#' @import readr
#' @import haven
#' @param FPED_PATH The file path for the FPED data. The file name should be like: fped_dr1tot_1112.sas7bdat
#' @param NUTRIENT_PATH The file path for the NUTRIENT data. The file name should be like: DR1TOT_J.XPT
#' @param DEMO_PATH The file path for the DEMOGRAPHIC data. The file name should be like: DEMO_J.XPT
#' @return The AHEI and its component scores and serving sizes
#' @examples
#' FPED_PATH = "/Users/james/Desktop/data/fped_dr1tot_1112.sas7bdat"
#' NUTRIENT_PATH = "/Users/james/Desktop/data/DR1TOT_J.XPT"
#' DEMO_PATH = "/Users/james/Desktop/data/DEMO_J.XPT"
#' HEI2015_NHANES_FPED(FPED_PATH, NUTRIENT_PATH, DEMO_PATH)
#' @export

HEI2015_NHANES_FPED = function(FPED_PATH, NUTRIENT_PATH, DEMO_PATH){
  
  if (is.character(FPED_PATH) == TRUE){
    FPED = read_sas(FPED_PATH)
  } else {
    FPED = FPED_PATH
  }
  
  if (is.character(NUTRIENT_PATH) == TRUE){
    NUTRIENT = read_xpt(NUTRIENT_PATH)
  } else {
    NUTRIENT = NUTRIENT_PATH
  }
  
  if (is.character(DEMO_PATH) == TRUE){
    DEMO = read_xpt(DEMO_PATH)
  } else {
    DEMO = DEMO_PATH
  }
  
  if ("DR1ILINE" %in% colnames(FPED) | "DR1ILINE" %in% colnames(NUTRIENT)){
    stop("Please use the population-level data. The file name should be like: Totals.csv")
  }
  
  NUTRIENT = NUTRIENT %>%
    filter(DR1DRSTZ == 1) %>%
    dplyr::select(SEQN, WTDRD1, DR1TKCAL, DR1TSFAT, DR1TALCO, DR1TSODI, DR1DRSTZ, DR1TMFAT, DR1TPFAT) %>%
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
      TOTALFRT_SERV = DR1T_F_TOTAL/(DR1TKCAL/1000),
      FRT_SERV = (DR1T_F_CITMLB+DR1T_F_OTHER)/(DR1TKCAL/1000),
      VEG_SERV = (DR1T_V_TOTAL+DR1T_V_LEGUMES)/(DR1TKCAL/1000),
      GREENNBEAN_SERV = (DR1T_V_DRKGR+DR1T_V_LEGUMES)/(DR1TKCAL/1000),
      TOTALPRO_SERV = (DR1T_PF_MPS_TOTAL+DR1T_PF_EGGS+DR1T_PF_NUTSDS+DR1T_PF_SOY+DR1T_PF_LEGUMES)/(DR1TKCAL/1000),
      SEAPLANTPRO_SERV = (DR1T_PF_SEAFD_HI+DR1T_PF_SEAFD_LOW+DR1T_PF_NUTSDS+DR1T_PF_SOY+DR1T_PF_LEGUMES)/(DR1TKCAL/1000),
      WHOLEGRAIN_SERV = DR1T_G_WHOLE/(DR1TKCAL/1000),
      DAIRY_SERV = DR1T_D_TOTAL/(DR1TKCAL/1000),
      FATTYACID_SERV = case_when(
        DR1TSFAT == 0 ~ 0, 
        TRUE ~ (DR1TMFAT+DR1TPFAT)/DR1TSFAT
        ),
      
      REFINEDGRAIN_SERV = DR1T_G_REFINED/(DR1TKCAL/1000),
      SODIUM_SERV = (DR1TSODI/1000)/(DR1TKCAL/1000),
      ADDEDSUGAR_SERV = ((DR1T_ADD_SUGARS*4*4) / DR1TKCAL)*100,
      SATFAT_SERV = ((DR1TSFAT*9)/DR1TKCAL)*100,
      
      TOTALKCAL = DR1TKCAL)
  
  
  ##Create variables needed for HEI2015 calculation
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
  
  HEI2015_HEALTHY1 = function(actual, min, max){
    case_when(
      actual >= max ~ HEI2015_MAX1,
      actual <= min ~ HEI2015_MIN,
      TRUE ~ HEI2015_MIN+(actual-min)*HEI2015_MAX1/(max-min)
    )
  }
  
  HEI2015_HEALTHY2 = function(actual, min, max){
    case_when(
      actual >= max ~ HEI2015_MAX2,
      actual <= min ~ HEI2015_MIN,
      TRUE ~ HEI2015_MIN+(actual-min)*HEI2015_MAX2/(max-min)
    )
  }
  
  HEI2015_UNHEALTHY = function(actual, min, max){
    case_when(
      actual >= min ~ HEI2015_MIN,
      actual <= max ~ HEI2015_MAX2,
      TRUE ~ HEI2015_MIN+(actual-min)*HEI2015_MAX2/(max-min)
    )
  }
  
  COHORT=COHORT %>%
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
      
      HEI2015_ALL= HEI2015_TOTALFRT + HEI2015_FRT + HEI2015_VEG + HEI2015_GREENNBEAN +
        HEI2015_TOTALPRO + HEI2015_SEAPLANTPRO + HEI2015_WHOLEGRAIN + HEI2015_DAIRY +
        HEI2015_FATTYACID + HEI2015_REFINEDGRAIN + HEI2015_SODIUM + HEI2015_ADDEDSUGAR +
        HEI2015_SATFAT
    ) 
  
  for(i in 1:length(COHORT$TOTALKCAL)){
    if (COHORT$TOTALKCAL[i] == 0){
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
    dplyr::select(SEQN, HEI2015_ALL, HEI2015_TOTALFRT, HEI2015_FRT, HEI2015_VEG, HEI2015_GREENNBEAN,
           HEI2015_TOTALPRO, HEI2015_SEAPLANTPRO, HEI2015_WHOLEGRAIN, HEI2015_DAIRY,
           HEI2015_FATTYACID, HEI2015_REFINEDGRAIN, HEI2015_SODIUM, HEI2015_ADDEDSUGAR,
           HEI2015_SATFAT,
           TOTALKCAL, TOTALFRT_SERV, FRT_SERV, VEG_SERV, GREENNBEAN_SERV, TOTALPRO_SERV, 
           SEAPLANTPRO_SERV, WHOLEGRAIN_SERV, DAIRY_SERV, FATTYACID_SERV, REFINEDGRAIN_SERV, 
           SODIUM_SERV, ADDEDSUGAR_SERV, SATFAT_SERV)
}

#' AHEI_NHANES_FPED
#'
#' Calculate the AHEI for the NHANES_FPED data (after 2005) within 1 step  
#' @import dplyr
#' @import readr
#' @import haven
#' @param FPED_IND_PATH The file path for the FPED IND data. The file name should be like: fped_dr1iff.sas7bdat
#' @param NUTRIENT_IND_PATH The file path for the NUTRIENT IND data. The file name should be like: DR1IFF_J
#' @return The AHEI and its component scores and serving sizes
#' @examples
#' FPED_IND_PATH = "/Users/james/Desktop/data/fped_dr1iff.sas7bdat"
#' NUTRIENT_IND_PATH = "/Users/james/Desktop/data/DR1IFF_J"
#' AHEI_NHANES_FPED(FPED_IND_PATH, NUTRIENT_IND_PATH)
#' @export

AHEI_NHANES_FPED = function(FPED_IND_PATH, NUTRIENT_IND_PATH){
  
  if (is.character(FPED_IND_PATH) == TRUE){
    FPED_IND = read_sas(FPED_IND_PATH)
  } else {
    FPED_IND = FPED_IND_PATH
  }
  
  if (is.character(NUTRIENT_IND_PATH) == TRUE){
    NUTRIENT_IND = read_xpt(NUTRIENT_IND_PATH)
  } else {
    NUTRIENT_IND = NUTRIENT_IND_PATH
  }
  
  if (!("DR1ILINE" %in% colnames(FPED_IND)) | !("DR1ILINE" %in% colnames(NUTRIENT_IND)) ){
    stop("Please use individual-level data for this function. Individual-level nutrient data should be like DR1IFF_J.XPT. Individual-level FPED data should be like fped_dr1iff_1718.sas7bdat")
  }
  
  # if (is.null(NUTRIENT_IND_PATH$DR1ILINE) | is.null(FPED_IND_PATH$DR1ILINE)) {
  #   stop("Please use individual-level data for this function. Individual-level nutrient data should be like DR1IFF_J.XPT. Individual-level FPED data should be like fped_dr1iff_1718.sas7bdat")
  # }
  
  NUTRIENT_IND = NUTRIENT_IND %>%
    filter(DR1DRSTZ == 1) %>%
    arrange(SEQN)
  
  FPED_IND = FPED_IND %>%
    arrange(SEQN)
  
  COHORT = NUTRIENT_IND %>%
    left_join(FPED_IND, by = c("SEQN", "DR1ILINE"))
  
  
  COFFEE=c(12200100,12210200,12210210,12210260,12210270,12210280,12210310,12210400,12210420,12210430,12210440,12210505,12210520,91703600,92100000,92100500,92101000,92101500,92101600,92101610,92101630,92101700,92101800,92101810,92101820,92101850,92101851,92101900,92101901,92101903,92101904,92101905,92101906,92101910,92101911,92101913,92101917,92101918,92101919,92101920,92101921,92101923,92101925,92101926,92101928,92101930,92101931,92101933,92101935,92101936,92101938,92101950,92101955,92101960,92101965,92101970,92101975,92102000,92102010,92102020,92102030,92102040,92102050,92102060,92102070,92102080,92102090,92102100,92102110,92102400,92102401,92102450,92102500,92102501,92102502,92102503,92102504,92102505,92102510,92102511,92102512,92102513,92102514,92102515,92102600,92102601,92102602,92102610,92102611,92102612,92103000,92104000,92111000,92111010,92114000,92121000,92121001,92121010,92121020,92121030,92121040,92121041,92121050,92130000,92130001,92130005,92130006,92130010,92130011,92130020,92130021,92130030,92130031,92152000,92152010,92161000,92161001,92161002,92162000,92162001,92162002,92171000,92171010,92191100,92191105,92191200,92191400,92192000,92192030,92192040,92193000,92193005,92193020,92193025,92201010,92291300,93202000,93301400)
  TEA=c(53246000,92302000,92302500,92303010,92303100,92304100,92305010,92305040,92305050,92305090,92305110,92305180,92305900,92305910,92305920,92306000,92306090,92306700,92306800,92307000,92307400,92308000,92308010,92308020,92308030,92308040,92308050,92308500,92308510,92308520,92308530,92308540,92308550,92309000,92309010,92309020,92309030,92309040,92309050,92309500,92309510,92309520)
  COFFEE_TEA=c(COFFEE, TEA)
  DRINK=c(11511100,11511200,11511300,11511400,11511550,11511600,11511610,11511700,11512010,11512020,11512030,11512100,11512110,11512120,11553130,11560000,64134030,67260000,75200700,91301130,92101920,92101921,92101923,92101925,92101926,92101928,92101930,92101931,92101933,92101935,92101936,92101938,92102000,92102010,92102020,92102030,92102040,92102050,92102060,92102070,92102080,92102090,92102100,92102110,92307500,92307510,92307520,92400000,92400100,92410310,92410315,92410320,92410340,92410350,92410360,92410370,92410390,92410400,92410410,92410420,92410510,92410520,92410550,92410560,92410610,92410620,92410710,92410720,92410810,92410820,92411510,92411520,92411610,92411620,92432000,92433000,92510610,92510650,92510955,92510960,92511015,92513000,92513010,92530410,92530510,92530610,92530950,92531030,92541010,92542000,92550030,92550035,92550040,92550110,92550200,92550370,92550400,92550405,92550610,92550620,92552000,92552010,92552020,92552030,92582100,92582110,92900100,92900110,92900200,92900300,93301216,95101000,95101010,95102000,95103000,95103010,95104000,95105000,95106000,95106010,95110000,95110010,95110020,95120000,95120010,95120020,95120050,95310200,95310400,95310500,95310550,95310555,95310560,95310600,95310700,95310750,95310800,95311000,95312400,95312410,95312500,95312550,95312555,95312560,95312600,95312700,95312800,95312900,95312905,95313200,95320200,95320500,95321000,95322200,95322500,
          95323000)
  SSB = c(COFFEE_TEA, DRINK)
  
  
  COHORT = COHORT %>%
    dplyr::mutate(
      ADDED_SUGAR_SSB_SERV = case_when(
        DR1IFDCD.x %in% SSB ~ DR1I_ADD_SUGARS,
        TRUE ~ 0
      )
    ) %>%
    dplyr::group_by(SEQN) %>%
    dplyr::summarize(
      ENERGY=sum(DR1IKCAL),
      RIAGENDR = min(RIAGENDR),
      VEG_SERV = sum(DR1I_V_REDOR_TOTAL + 0.5*DR1I_V_DRKGR + DR1I_V_OTHER + DR1I_V_STARCHY_OTHER),
      FRT_SERV = sum(DR1I_F_TOTAL - DR1I_F_JUICE),
      WGRAIN_SERV = sum(DR1I_G_WHOLE/0.035274),
      NUTSLEG_SERV = sum((DR1I_V_LEGUMES*4) + DR1I_PF_NUTSDS + DR1I_PF_SOY),
      PUFA_SERV = ((sum(DR1IP182 + DR1IP183 + DR1IP184 + DR1IP204 + DR1IP225)*9)/ ENERGY)*100,
      N3FAT_SERV = sum((DR1IP205 + DR1IP226)*1000),
      SSB_FRTJ_SERV = sum((ADDED_SUGAR_SSB_SERV*4 / 26)),
      REDPROC_MEAT_SERV = sum((DR1I_PF_CUREDMEAT /1.5) + ((DR1I_PF_MEAT+DR1I_PF_ORGAN)/4)),
      TRANS_SERV = sum(0),
      SODIUM_SERV = sum(DR1ISODI)/(ENERGY/1000),
      ALCOHOL_SERV = sum(DR1I_A_DRINKS)
    )
  
  ##Create variables and functions needed for AHEI calculation
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
  
  SCORE_HEALTHY = function(actual_serv, min_serv, max_serv, min_score, max_score){
    case_when(
      actual_serv >= max_serv ~ max_score,
      actual_serv <= min_serv ~ min_score,
      TRUE ~ min_score+(actual_serv-min_serv)*max_score/(max_serv-min_serv)
    )
  }
  
  SCORE_UNHEALTHY = function(actual_serv, min_serv, max_serv, min_score, max_score){
    case_when(
      actual_serv >= min_serv ~ min_score ,
      actual_serv <= max_serv ~ max_score,
      TRUE ~ min_score+(actual_serv-min_serv)*max_score/(max_serv-min_serv)
    )
  }
  
  SODIUM_DECILE = quantile(COHORT$SODIUM_SERV, probs=seq(0, 1, by=1/11))
  
  ##AHEI calculation
  COHORT %>%
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
      AHEI_ALCOHOL = 
        case_when(
          RIAGENDR == 2 & ALCOHOL_SERV >= 2.5 ~ 0,
          RIAGENDR == 2 & ALCOHOL_SERV < 2.5 & ALCOHOL_SERV > 1.5 ~ 0 + (ALCOHOL_SERV-2.5)*10/(1.5-2.5),
          RIAGENDR == 2 & ALCOHOL_SERV <= 1.5 & ALCOHOL_SERV >= 0.5 ~ 10,
          RIAGENDR == 2 & ALCOHOL_SERV < 0.5 ~  0 + (ALCOHOL_SERV-0)*10/(0.5-0),
          RIAGENDR == 2 & ALCOHOL_SERV <= 0.125 ~ 2.5,
          RIAGENDR == 1 & ALCOHOL_SERV >= 3.5 ~ 0,
          RIAGENDR == 1 & ALCOHOL_SERV < 3.5 & ALCOHOL_SERV > 2 ~ 0 + (ALCOHOL_SERV-2.5)*10/(1.5-2.5),
          RIAGENDR == 1 & ALCOHOL_SERV <= 2 & ALCOHOL_SERV >= 0.5 ~ 10,
          RIAGENDR == 1 & ALCOHOL_SERV < 0.5 ~ (ALCOHOL_SERV-0)*10/(0.5-0),
          RIAGENDR == 1 & ALCOHOL_SERV <= 0.125 ~ 2.5
        )
      ,
      AHEI_ALL = AHEI_VEG + AHEI_FRT + AHEI_WGRAIN + AHEI_NUTSLEG + AHEI_N3FAT +
        AHEI_PUFA + AHEI_SSB_FRTJ + AHEI_REDPROC_MEAT + AHEI_TRANS + AHEI_SODIUM + AHEI_ALCOHOL,
      
      AHEI_NOETOH = AHEI_VEG + AHEI_FRT + AHEI_WGRAIN + AHEI_NUTSLEG + AHEI_N3FAT +
        AHEI_PUFA + AHEI_SSB_FRTJ + AHEI_REDPROC_MEAT + AHEI_TRANS + AHEI_SODIUM
    ) %>%
    dplyr::select(SEQN, AHEI_ALL, AHEI_NOETOH, AHEI_VEG, AHEI_FRT, AHEI_WGRAIN, AHEI_NUTSLEG, AHEI_N3FAT,
                  AHEI_PUFA, AHEI_SSB_FRTJ, AHEI_REDPROC_MEAT, AHEI_TRANS, AHEI_SODIUM, AHEI_ALCOHOL,
                  
                  ENERGY, VEG_SERV, FRT_SERV, WGRAIN_SERV, NUTSLEG_SERV, PUFA_SERV, N3FAT_SERV, 
                  SSB_FRTJ_SERV, REDPROC_MEAT_SERV, TRANS_SERV, SODIUM_SERV, ALCOHOL_SERV)
}

#' DASH_NHANES_FPED
#'
#' Calculate the DASH for the NHANES_FPED data (after 2005) within 1 step  
#' @import dplyr
#' @import readr
#' @import haven
#' @param FPED_IND_PATH The file path for the FPED IND data. The file name should be like: fped_dr1iff.sas7bdat
#' @param NUTRIENT_IND_PATH The file path for the NUTRIENT IND data. The file name should be like: DR1IFF_J
#' @return The DASH and its component scores and serving sizes
#' @examples
#' FPED_IND_PATH = "/Users/james/Desktop/data/fped_dr1iff.sas7bdat"
#' NUTRIENT_IND_PATH = "/Users/james/Desktop/data/DR1IFF_J"
#' DASH_NHANES_FPED(FPED_IND_PATH, NUTRIENT_IND_PATH)
#' @export


DASH_NHANES_FPED = function(FPED_IND_PATH, NUTRIENT_IND_PATH){
  
  if (is.character(FPED_IND_PATH) == TRUE){
    FPED_IND = read_sas(FPED_IND_PATH)
  } else {
    FPED_IND = FPED_IND_PATH
  }
  
  if (is.character(NUTRIENT_IND_PATH) == TRUE){
    NUTRIENT_IND = read_xpt(NUTRIENT_IND_PATH)
  } else {
    NUTRIENT_IND = NUTRIENT_IND_PATH
  }
  
  if (!("DR1ILINE" %in% colnames(FPED_IND)) | !("DR1ILINE" %in% colnames(NUTRIENT_IND)) ){
    stop("Please use individual-level data for this function. Individual-level nutrient data should be like DR1IFF_J.XPT. Individual-level FPED data should be like fped_dr1iff_1718.sas7bdat")
  }
  
  NUTRIENT_IND = NUTRIENT_IND %>%
    filter(DR1DRSTZ == 1) %>%
    arrange(SEQN)
  
  FPED_IND = FPED_IND %>%
    arrange(SEQN)
  
  COHORT = NUTRIENT_IND %>%
    left_join(FPED_IND, by = c("SEQN", "DR1ILINE"))
  
  
  LOWF_MILK=c(11111160,11111170,11112120,11112210,11113000,11114300,11114320,11115000,11115100,11115400,11120000,11121210,11121300,11212050,11511000,11511300,11511400,11511550,11511600,11511610,11511700,11512020,11512110,11513200,11513300,11513365,11513370,11513383,11513384,11513393,11513394,11513600,11513700,11513803,11513804,11513853,11513854,11514100,11514130,11514140,11514310,11514340,11514350,11519200,11519205,11519210)
  LOWF_CHEESE=c(14204010,14204020,14206010,14207010)
  
  COFFEE=c(12200100,12210200,12210210,12210260,12210270,12210280,12210310,12210400,12210420,12210430,12210440,12210505,12210520,91703600,92100000,92100500,92101000,92101500,92101600,92101610,92101630,92101700,92101800,92101810,92101820,92101850,92101851,92101900,92101901,92101903,92101904,92101905,92101906,92101910,92101911,92101913,92101917,92101918,92101919,92101920,92101921,92101923,92101925,92101926,92101928,92101930,92101931,92101933,92101935,92101936,92101938,92101950,92101955,92101960,92101965,92101970,92101975,92102000,92102010,92102020,92102030,92102040,92102050,92102060,92102070,92102080,92102090,92102100,92102110,92102400,92102401,92102450,92102500,92102501,92102502,92102503,92102504,92102505,92102510,92102511,92102512,92102513,92102514,92102515,92102600,92102601,92102602,92102610,92102611,92102612,92103000,92104000,92111000,92111010,92114000,92121000,92121001,92121010,92121020,92121030,92121040,92121041,92121050,92130000,92130001,92130005,92130006,92130010,92130011,92130020,92130021,92130030,92130031,92152000,92152010,92161000,92161001,92161002,92162000,92162001,92162002,92171000,92171010,92191100,92191105,92191200,92191400,92192000,92192030,92192040,92193000,92193005,92193020,92193025,92201010,92291300,93202000,93301400)
  TEA=c(53246000,92302000,92302500,92303010,92303100,92304100,92305010,92305040,92305050,92305090,92305110,92305180,92305900,92305910,92305920,92306000,92306090,92306700,92306800,92307000,92307400,92308000,92308010,92308020,92308030,92308040,92308050,92308500,92308510,92308520,92308530,92308540,92308550,92309000,92309010,92309020,92309030,92309040,92309050,92309500,92309510,92309520)
  COFFEE_TEA=c(COFFEE, TEA)
  DRINK=c(11511100,11511200,11511300,11511400,11511550,11511600,11511610,11511700,11512010,11512020,11512030,11512100,11512110,11512120,11553130,11560000,64134030,67260000,75200700,91301130,92101920,92101921,92101923,92101925,92101926,92101928,92101930,92101931,92101933,92101935,92101936,92101938,92102000,92102010,92102020,92102030,92102040,92102050,92102060,92102070,92102080,92102090,92102100,92102110,92307500,92307510,92307520,92400000,92400100,92410310,92410315,92410320,92410340,92410350,92410360,92410370,92410390,92410400,92410410,92410420,92410510,92410520,92410550,92410560,92410610,92410620,92410710,92410720,92410810,92410820,92411510,92411520,92411610,92411620,92432000,92433000,92510610,92510650,92510955,92510960,92511015,92513000,92513010,92530410,92530510,92530610,92530950,92531030,92541010,92542000,92550030,92550035,92550040,92550110,92550200,92550370,92550400,92550405,92550610,92550620,92552000,92552010,92552020,92552030,92582100,92582110,92900100,92900110,92900200,92900300,93301216,95101000,95101010,95102000,95103000,95103010,95104000,95105000,95106000,95106010,95110000,95110010,95110020,95120000,95120010,95120020,95120050,95310200,95310400,95310500,95310550,95310555,95310560,95310600,95310700,95310750,95310800,95311000,95312400,95312410,95312500,95312550,95312555,95312560,95312600,95312700,95312800,95312900,95312905,95313200,95320200,95320500,95321000,95322200,95322500,
          95323000)
  SSB = c(COFFEE_TEA, DRINK)
  
  
  COHORT = COHORT %>%
    dplyr::mutate(
      ADDED_SUGAR_SSB_SERV = case_when(
        DR1IFDCD.x %in% SSB ~ DR1I_ADD_SUGARS,
        TRUE ~ 0
      ),
      LOWF_MILK_SERV = case_when(
        DR1IFDCD.x %in% LOWF_MILK ~ DR1I_D_MILK,
        TRUE ~ 0
      ),
      LOWF_CHEESECREAM_SERV = case_when(
        DR1IFDCD.x %in% LOWF_CHEESE ~ DR1I_D_CHEESE*4,
        TRUE ~ 0
      ),
    ) %>%
    dplyr::group_by(SEQN) %>%
    dplyr::summarize(
      ENERGY = sum(DR1IKCAL),
      FRT_FRTJ_SERV = sum(DR1I_F_TOTAL),
      VEG_SERV = sum(DR1I_V_REDOR_TOTAL + 0.5*DR1I_V_DRKGR + DR1I_V_OTHER + DR1I_V_STARCHY_OTHER),
      NUTSLEG_SERV = sum((DR1I_V_LEGUMES*4) + DR1I_PF_NUTSDS + DR1I_PF_SOY),
      WGRAIN_SERV = sum(DR1I_G_WHOLE),
      LOWF_DAIRY_SERV = sum(LOWF_MILK_SERV+LOWF_CHEESECREAM_SERV+DR1I_D_YOGURT),
      SODIUM_SERV = sum(DR1ISODI)/(ENERGY/1000),
      REDPROC_MEAT_SERV = sum((DR1I_PF_CUREDMEAT /1.5) + ((DR1I_PF_MEAT+DR1I_PF_ORGAN)/4)),
      SSB_FRTJ_SERV = sum((ADDED_SUGAR_SSB_SERV*4 / 26)),
      
    )
  
  
  ##Create variables and functions needed for DASH calculation
  quintile_healthy = function(actual){
    quintile = quantile(actual, probs=seq(0, 1, by=0.2))
    case_when(
      actual <= quintile[6] & actual >= quintile[5] ~ 5,
      actual < quintile[5] & actual >= quintile[4] ~ 4,
      actual < quintile[4] & actual >= quintile[3] ~ 3,
      actual < quintile[3] & actual >= quintile[2] ~ 2,
      actual < quintile[2] & actual >= quintile[1] ~ 1
    )
  }
  
  quintile_unhealthy = function(actual){
    quintile = quantile(actual, probs=seq(0, 1, by=0.2))
    case_when(
      actual <= quintile[6] & actual >= quintile[5] ~ 1,
      actual < quintile[5] & actual >= quintile[4] ~ 2,
      actual < quintile[4] & actual >= quintile[3] ~ 3,
      actual < quintile[3] & actual >= quintile[2] ~ 4,
      actual < quintile[2] & actual >= quintile[1] ~ 5
    )
  }
  
  print("Reminder: this DASH index uses quintiles to rank participants' food/drink serving sizes and then calculate DASH component scores, which may generate results that are specific to your study population but not comparable to other populations.")
  
  
  ##DASH calculation
  COHORT %>%
    dplyr::mutate(
      DASH_FRT = quintile_healthy(FRT_FRTJ_SERV),
      DASH_VEG = quintile_healthy(VEG_SERV),
      DASH_NUTSLEG = quintile_healthy(NUTSLEG_SERV),
      DASH_WGRAIN = quintile_healthy(WGRAIN_SERV),
      DASH_LOWF_DAIRY = quintile_healthy(LOWF_DAIRY_SERV),
      DASH_SODIUM = quintile_unhealthy(SODIUM_SERV),
      DASH_REDPROC_MEAT = quintile_unhealthy(REDPROC_MEAT_SERV),
      DASH_SSB_FRTJ = quintile_unhealthy(SSB_FRTJ_SERV),
      DASH_ALL = DASH_FRT+DASH_VEG+DASH_NUTSLEG+DASH_WGRAIN+DASH_LOWF_DAIRY+
        DASH_SODIUM+DASH_REDPROC_MEAT+DASH_SSB_FRTJ
    )%>%
    dplyr::select(SEQN, DASH_ALL, DASH_FRT, DASH_VEG, DASH_NUTSLEG, DASH_WGRAIN, DASH_LOWF_DAIRY,
                  DASH_SODIUM, DASH_REDPROC_MEAT, DASH_SSB_FRTJ,
                  
                  ENERGY, FRT_FRTJ_SERV, VEG_SERV, NUTSLEG_SERV, WGRAIN_SERV, LOWF_DAIRY_SERV, SODIUM_SERV, REDPROC_MEAT_SERV, SSB_FRTJ_SERV)
}


#' MED_NHANES_FPED
#'
#' Calculate the MED for the NHANES_FPED data (after 2005) within 1 step  
#' @import dplyr
#' @import readr
#' @import haven
#' @param FPED_PATH The file path for the FPED data. The file name should be like: fpre_dr1tot_1718.sas7bdat
#' @param NUTRIENT_PATH The file path for the NUTRIENT data. The file name should be like: DR1TOT_J.XPT
#' @param DEMO_PATH The file path for the DEMOGRAPHIC data. The file name should be like: DEMO_J.XPT
#' @return The MED and its component scores
#' @examples
#' FPED_PATH = "/Users/james/Desktop/data/fpre_dr1tot_1718.sas7bdat"
#' NUTRIENT_PATH = "/Users/james/Desktop/data/DR1TOT_J.XPT"
#' DEMO_PATH = "/Users/james/Desktop/data/DEMO_J.XPT"
#' MED_NHANES_FPED(FPED_PATH, NUTRIENT_PATH, DEMO_PATH)
#' @export

MED_NHANES_FPED = function(FPED_PATH, NUTRIENT_PATH, DEMO_PATH){

  
  if (is.character(FPED_PATH) == TRUE){
    FPED = read_sas(FPED_PATH)
  } else {
    FPED = FPED_PATH
  }
  
  if (is.character(NUTRIENT_PATH) == TRUE){
    NUTRIENT = read_xpt(NUTRIENT_PATH)
  } else {
    NUTRIENT = NUTRIENT_PATH
  }
  
  if (is.character(DEMO_PATH) == TRUE){
    DEMO = read_xpt(DEMO_PATH)
  } else {
    DEMO = DEMO_PATH
  }
  
  if ("DR1ILINE" %in% colnames(FPED) | "DR1ILINE" %in% colnames(NUTRIENT)){
    stop("Please use the population-level data. The file name should be like: Totals.csv")
  }
  
  NUTRIENT = NUTRIENT %>%
    filter(DR1DRSTZ == 1) %>%
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
  
  
  #Match participant response food frequency to the standard food frequency response code
  
  COHORT = COHORT %>%
    filter(DR1TKCAL > 0) %>%
    dplyr::mutate(
      FRT_FRTJ_SERV = DR1T_F_TOTAL,
      VEG_SERV = DR1T_V_REDOR_TOTAL + 0.5*DR1T_V_DRKGR + DR1T_V_OTHER + DR1T_V_STARCHY_OTHER,
      WGRAIN_SERV = DR1T_G_WHOLE,
      LEGUMES_SERV = (DR1T_V_LEGUMES*4) + DR1T_PF_SOY,
      NUTS_SERV = DR1T_PF_NUTSDS,
      FISH_SERV = DR1T_PF_SEAFD_HI + DR1T_PF_SEAFD_LOW,
      REDPROC_MEAT_SERV = (DR1T_PF_CUREDMEAT /1.5) + ((DR1T_PF_MEAT+DR1T_PF_ORGAN+DR1T_PF_POULT)/4),
      MONSATFAT_SERV = case_when(
        DR1TSFAT == 0 ~ 0, 
        TRUE ~ DR1TMFAT/DR1TSFAT
      ),
      ALCOHOL_SERV = DR1TALCO
    ) 
  
  ##Create variables and functions needed for MED
  median_healthy = function(actual){
    median_score = median(actual)
    case_when(
      actual < median_score ~ 0,
      actual >= median_score ~ 1
    )
  }
  
  median_unhealthy = function(actual){
    median_score = median(actual)
    case_when(
      actual < median_score ~ 1,
      actual >= median_score ~ 0
    )
  }
  
  print("Reminder: this MED index uses medians to rank participants' food/drink serving sizes and then calculate MED component scores, which may generate results that are specific to your study population but not comparable to other populations.")
  
  
  COHORT %>%
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
        ALCOHOL_SERV <=25 & ALCOHOL_SERV >= 10 ~ 1, 
        TRUE ~ 0),
      
      MED_ALL = MED_FRT+MED_VEG+MED_WGRAIN+MED_LEGUMES+MED_NUTS+MED_FISH+MED_REDPROC_MEAT+MED_MONSATFAT+MED_ALCOHOL,
      MED_NOETOH = MED_FRT+MED_VEG+MED_WGRAIN+MED_LEGUMES+MED_NUTS+MED_FISH+MED_REDPROC_MEAT+MED_MONSATFAT
    )%>%
    dplyr::select(SEQN, MED_ALL, MED_NOETOH, MED_FRT, MED_VEG, MED_WGRAIN, MED_LEGUMES, MED_NUTS,
           MED_FISH, MED_REDPROC_MEAT, MED_MONSATFAT, MED_ALCOHOL,
           
           FRT_FRTJ_SERV, VEG_SERV, WGRAIN_SERV, LEGUMES_SERV, NUTS_SERV, FISH_SERV, REDPROC_MEAT_SERV, MONSATFAT_SERV, ALCOHOL_SERV)
}

#' DII_NHANES_FPED
#'
#' Calculate the DII for the NHANES_FPED data (after 2005) within 1 step  
#' @import dplyr
#' @import readr
#' @import haven
#' @param FPED_PATH The file path for the FPED data. The file name should be like: fpre_dr1tot_1718.sas7bdat
#' @param NUTRIENT_PATH The file path for the NUTRIENT data. The file name should be like: DR1TOT_J.XPT
#' @param DEMO_PATH The file path for the DEMOGRAPHIC data. The file name should be like: DEMO_J.XPT
#' @return The DII and its component scores
#' @examples
#' FPED_PATH = "/Users/james/Desktop/data/fpre_dr1tot_1718.sas7bdat"
#' NUTRIENT_PATH = "/Users/james/Desktop/data/DR1TOT_J.XPT"
#' DEMO_PATH = "/Users/james/Desktop/data/DEMO_J.XPT"
#' DII_NHANES_FPED(FPED_PATH, NUTRIENT_PATH, DEMO_PATH)
#' @export

DII_NHANES_FPED = function(FPED_PATH, NUTRIENT_PATH, DEMO_PATH){
  
  if (is.character(FPED_PATH) == TRUE){
    FPED = read_sas(FPED_PATH)
  } else {
    FPED = FPED_PATH
  }
  
  if (is.character(NUTRIENT_PATH) == TRUE){
    NUTRIENT = read_xpt(NUTRIENT_PATH)
  } else {
    NUTRIENT = NUTRIENT_PATH
  }
  
  if (is.character(DEMO_PATH) == TRUE){
    DEMO = read_xpt(DEMO_PATH)
  } else {
    DEMO = DEMO_PATH
  }
  
  if ("DR1ILINE" %in% colnames(FPED) | "DR1ILINE" %in% colnames(NUTRIENT)){
    stop("Please use the population-level data. The file name should be like: Totals.csv")
  }
  
  NUTRIENT = NUTRIENT %>%
    filter(DR1DRSTZ == 1) %>%
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
  
  #Serving size calculation for DII
  COHORT = COHORT %>%
    filter(DR1TKCAL > 0) %>%
    dplyr::mutate(
      ALCOHOL = DR1TALCO,
      VITB12 = DR1TVB12,
      VITB6 = DR1TVB6,
      BCAROTENE = DR1TBCAR,
      CAFFEINE = DR1TCAFF/1000,
      CARB = DR1TCARB,
      CHOLES = DR1TCHOL,
      KCAL = DR1TKCAL,
      TOTALFAT = DR1TTFAT,
      FIBER = DR1TFIBE,
      FOLICACID = DR1TFA,
      IRON = DR1TIRON,
      MG = DR1TMAGN,
      MUFA = DR1TMFAT,
      NIACIN = DR1TNIAC,
      N3FAT = DR1TP183 + DR1TP184 + DR1TP205 + DR1TP225 + DR1TP226,
      N6FAT = DR1TP183 + DR1TP204,
      PROTEIN = DR1TPROT,
      PUFA = DR1TPFAT,
      RIBOFLAVIN = DR1TVB2,
      SATFAT = DR1TSFAT,
      SE = DR1TSELE,
      THIAMIN = DR1TVB1,
      VITA = DR1TVARA,
      VITC = DR1TVC,
      VITD = tryCatch(DR1TVD*0.025, error = function(e) return(NULL)),
      VITE = DR1TATOC,
      ZN = DR1TZINC) %>%
    dplyr::select(SEQN, ALCOHOL, VITB12, VITB6, BCAROTENE,CAFFEINE,CARB,CHOLES,KCAL,TOTALFAT,FIBER,FOLICACID,
                  IRON,MG,MUFA,NIACIN,N3FAT,N6FAT,PROTEIN,PUFA,RIBOFLAVIN,SATFAT,SE,THIAMIN,
                  VITA,VITC,VITD,VITE,ZN)
  
  COHORT = COHORT %>%
    tidyr::pivot_longer(-SEQN, names_to="Variable", values_to="Value")

  Variable = c("ALCOHOL", "VITB12", "VITB6", "BCAROTENE", "CAFFEINE", "CARB", "CHOLES", "KCAL", "EUGENOL",
               "TOTALFAT", "FIBER", "FOLICACID","GARLIC", "GINGER","IRON", "MG", "MUFA", "NIACIN", "N3FAT", "N6FAT","ONION", "PROTEIN", "PUFA", 
               "RIBOFLAVIN","SAFFRON", "SATFAT", "SE", "THIAMIN","TRANSFAT","TURMERIC", "VITA","VITC", "VITD", "VITE", "ZN", "TEA",
               "FLA3OL","FLAVONES","FLAVONOLS","FLAVONONES","ANTHOC","ISOFLAVONES","PEPPER","THYME","ROSEMARY")
  
  Overall_inflammatory_score = c(-0.278, 0.106, -0.365, -0.584, -0.11, 0.097, 0.11, 0.18, -0.14, 0.298, -0.663, -0.19, -0.412, -0.453, 0.032, -0.484, -0.009,
                                 -0.246, -0.436, -0.159, -0.301, 0.021, -0.337, -0.068, -0.14, 0.373, -0.191, -0.098,0.229,-0.785, -0.401, -0.424, -0.446, -0.419, -0.313,
                                 -0.536,-0.415,-0.616,-0.467,-0.25,-0.131,-0.593,-0.131,-0.102,-0.013)
  
  Global_mean = c(13.98,5.15,1.47,3718,8.05,272.2,279.4,2056,0.01,71.4,18.8,273,4.35,59,13.35,310.1,27,25.9,1.06,10.8,35.9,
                  79.4,13.88,1.7,0.37,28.6,67,1.7,3.15,533.6,983.9,118.2,6.26,8.73,9.84,
                  1.69,95.8,1.55,17.7,11.7,18.05,1.2,10,0.33,1)
  
  SD = c(3.72,2.7,0.74,1720,6.67,40,51.2,338,0.08,19.4,4.9,70.7,2.9,63.2,3.71,139.4,6.1,11.77,1.06,7.5,18.4,13.9,3.76,0.79,1.78,
         8,25.1,0.66,3.75,754.3,518.6,43.46,2.21,1.49,2.19,
         1.53,85.9,0.07,6.79,3.82,21.14,0.2,7.07,0.99,15)
  
  DII_STD = base::data.frame(Variable, Overall_inflammatory_score, Global_mean, SD)
  
  #Score calculation for DII  
  
  COHORT %>%
    dplyr::inner_join(DII_STD, by=c("Variable")) %>%
    dplyr::mutate(
      Z_SCORE = (Value - Global_mean)/SD,
      PERCENTILE = pnorm(Z_SCORE)*2 - 1,
      IND_DII_SCORE = PERCENTILE*Overall_inflammatory_score) %>%
    tidyr::pivot_wider(names_from = Variable, values_from = IND_DII_SCORE) %>%
    dplyr::group_by(SEQN) %>%
    dplyr::summarize(
      DII_ALL = base::sum(ALCOHOL, VITB12, VITB6, BCAROTENE,CAFFEINE,CARB,CHOLES,KCAL,TOTALFAT,FIBER,FOLICACID,
                    IRON,MG,MUFA,NIACIN,N3FAT,N6FAT,PROTEIN,PUFA,RIBOFLAVIN,SATFAT,SE,THIAMIN,
                    VITA,VITC,VITD,VITE,ZN, na.rm = TRUE),
      
      DII_NOETOH =  base::sum(VITB12, VITB6, BCAROTENE,CAFFEINE,CARB,CHOLES,KCAL,TOTALFAT,FIBER,FOLICACID,
                        IRON,MG,MUFA,NIACIN,N3FAT,N6FAT,PROTEIN,PUFA,RIBOFLAVIN,SATFAT,SE,THIAMIN,
                        VITA,VITC,VITD,VITE,ZN, na.rm = TRUE),
      
      ALCOHOL = base::sum(ALCOHOL, na.rm = TRUE), 
      VITB12 = base::sum(VITB12, na.rm = TRUE), 
      VITB6 = base::sum(VITB6, na.rm = TRUE), 
      BCAROTENE = base::sum(BCAROTENE, na.rm = TRUE), 
      CAFFEINE = base::sum(CAFFEINE, na.rm = TRUE), 
      CARB = base::sum(CARB, na.rm = TRUE), 
      CHOLES = base::sum(CHOLES, na.rm = TRUE), 
      KCAL= base::sum(KCAL, na.rm = TRUE), 
      TOTALFAT= base::sum(TOTALFAT, na.rm = TRUE), 
      FIBER  = base::sum(FIBER, na.rm = TRUE), 
      FOLICACID  = base::sum(FOLICACID, na.rm = TRUE),
      IRON  = base::sum(IRON, na.rm = TRUE), 
      MG  = base::sum(MG, na.rm = TRUE), 
      MUFA  = base::sum(MUFA, na.rm = TRUE), 
      NIACIN  = base::sum(NIACIN, na.rm = TRUE), 
      N3FAT  = base::sum(N3FAT, na.rm = TRUE), 
      N6FAT  = base::sum(N6FAT, na.rm = TRUE), 
      PROTEIN  = base::sum(PROTEIN, na.rm = TRUE), 
      PUFA  = base::sum(PUFA, na.rm = TRUE), 
      RIBOFLAVIN  = base::sum(RIBOFLAVIN, na.rm = TRUE), 
      SATFAT  = base::sum(SATFAT, na.rm = TRUE), 
      SE  = base::sum(SE, na.rm = TRUE), 
      THIAMIN  = base::sum(THIAMIN, na.rm = TRUE), 
      VITA  = base::sum(VITA, na.rm = TRUE),
      VITC  = base::sum(VITC, na.rm = TRUE), 
      VITD  = base::sum(VITD, na.rm = TRUE), 
      VITE = base::sum(VITE, na.rm = TRUE), 
      ZN = base::sum(ZN, na.rm = TRUE)
    )
}

#' HEI2015_ASA24
#'
#' Calculate the HEI2015 for the ASA24 data within 1 step  
#' @import dplyr
#' @import readr
#' @import haven
#' @param DATA_PATH The file path for the data. The file name should be like: Totals.csv.
#' @return The HEI2015 and its component scores
#' @examples
#' DATA_PATH = "/Users/james/Desktop/data/Totals.csv"
#' HEI2015_ASA24(DATA_PATH)
#' @export


HEI2015_ASA24 = function(DATA_PATH){
  
  if (is.character(DATA_PATH) == TRUE){
    COHORT = read_csv(DATA_PATH)
  } else {
    COHORT = DATA_PATH
  }
  
  if ("FoodCode" %in% colnames(COHORT)){
    stop("Please use the population-level data. The file name should be like: Totals.csv")
  }
  
  COHORT = COHORT %>%
    dplyr::mutate(
      TOTALFRT_SERV = F_TOTAL/(KCAL/1000),
      FRT_SERV = (F_CITMLB+F_OTHER)/(KCAL/1000),
      VEG_SERV = (V_TOTAL+V_LEGUMES)/(KCAL/1000),
      GREENNBEAN_SERV = (V_DRKGR+V_LEGUMES)/(KCAL/1000),
      TOTALPRO_SERV = (PF_MPS_TOTAL+PF_EGGS+PF_NUTSDS+PF_SOY+PF_LEGUMES)/(KCAL/1000),
      SEAPLANTPRO_SERV = (PF_SEAFD_HI+PF_SEAFD_LOW+PF_NUTSDS+PF_SOY+PF_LEGUMES)/(KCAL/1000),
      WHOLEGRAIN_SERV = G_WHOLE/(KCAL/1000),
      DAIRY_SERV = D_TOTAL/(KCAL/1000),
      FATTYACID_SERV = case_when(
        SFAT == 0 ~ 0, 
        SFAT != 0 ~ (MFAT+PFAT)/SFAT
        ),
      REFINEDGRAIN_SERV = G_REFINED/(KCAL/1000),
      SODIUM_SERV = (SODI/1000)/(KCAL/1000),
      ADDEDSUGAR_SERV = ((ADD_SUGARS*4*4) / KCAL)*100,
      SATFAT_SERV = ((SFAT*9)/KCAL)*100,
      
      TOTALKCAL = KCAL
    )
  
  
  ##Create variables needed for HEI2015 calculation
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
  
  HEI2015_HEALTHY1 = function(actual, min, max){
    case_when(
      actual >= max ~ HEI2015_MAX1,
      actual <= min ~ HEI2015_MIN,
      TRUE ~ HEI2015_MIN+(actual-min)*HEI2015_MAX1/(max-min)
    )
  }
  
  HEI2015_HEALTHY2 = function(actual, min, max){
    case_when(
      actual >= max ~ HEI2015_MAX2,
      actual <= min ~ HEI2015_MIN,
      TRUE ~ HEI2015_MIN+(actual-min)*HEI2015_MAX2/(max-min)
    )
  }
  
  HEI2015_UNHEALTHY = function(actual, min, max){
    case_when(
      actual >= min ~ HEI2015_MIN,
      actual <= max ~ HEI2015_MAX2,
      TRUE ~ HEI2015_MIN+(actual-min)*HEI2015_MAX2/(max-min)
    )
  }
  
  COHORT=COHORT %>%
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
      
      HEI2015_ALL= HEI2015_TOTALFRT + HEI2015_FRT + HEI2015_VEG + HEI2015_GREENNBEAN +
        HEI2015_TOTALPRO + HEI2015_SEAPLANTPRO + HEI2015_WHOLEGRAIN + HEI2015_DAIRY +
        HEI2015_FATTYACID + HEI2015_REFINEDGRAIN + HEI2015_SODIUM + HEI2015_ADDEDSUGAR +
        HEI2015_SATFAT
    ) 
  
  for(i in 1:length(COHORT$TOTALKCAL)){
    if (COHORT$TOTALKCAL[i] == 0){
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
    dplyr::select(UserName, TOTALKCAL, HEI2015_ALL, HEI2015_TOTALFRT, HEI2015_FRT, HEI2015_VEG, HEI2015_GREENNBEAN,
           HEI2015_TOTALPRO, HEI2015_SEAPLANTPRO, HEI2015_WHOLEGRAIN, HEI2015_DAIRY,
           HEI2015_FATTYACID, HEI2015_REFINEDGRAIN, HEI2015_SODIUM, HEI2015_ADDEDSUGAR,
           HEI2015_SATFAT,
           
           TOTALKCAL, TOTALFRT_SERV, FRT_SERV, VEG_SERV, GREENNBEAN_SERV, TOTALPRO_SERV, SEAPLANTPRO_SERV, WHOLEGRAIN_SERV, 
           DAIRY_SERV, FATTYACID_SERV, REFINEDGRAIN_SERV, SODIUM_SERV, ADDEDSUGAR_SERV, SATFAT_SERV)
}


#' AHEI_F_ASA24
#'
#' Calculate the AHEI (female only) for the ASA24 data within 1 step  
#' @import dplyr
#' @import readr
#' @import haven
#' @param DATA_PATH The file path for the data. The file name should be like: Items.csv
#' @return The AHEI and its component scores
#' @examples
#' DATA_PATH = "/Users/james/Desktop/data/Items.csv"
#' AHEI_F_ASA24(DATA_PATH)
#' @export


AHEI_F_ASA24 = function(DATA_PATH){
  
  if (is.character(DATA_PATH) == TRUE){
    COHORT = read_csv(DATA_PATH)
  } else {
    COHORT = DATA_PATH
  }
  
  
  COFFEE=c(12200100,12210200,12210210,12210260,12210270,12210280,12210310,12210400,12210420,12210430,12210440,12210505,12210520,91703600,92100000,92100500,92101000,92101500,92101600,92101610,92101630,92101700,92101800,92101810,92101820,92101850,92101851,92101900,92101901,92101903,92101904,92101905,92101906,92101910,92101911,92101913,92101917,92101918,92101919,92101920,92101921,92101923,92101925,92101926,92101928,92101930,92101931,92101933,92101935,92101936,92101938,92101950,92101955,92101960,92101965,92101970,92101975,92102000,92102010,92102020,92102030,92102040,92102050,92102060,92102070,92102080,92102090,92102100,92102110,92102400,92102401,92102450,92102500,92102501,92102502,92102503,92102504,92102505,92102510,92102511,92102512,92102513,92102514,92102515,92102600,92102601,92102602,92102610,92102611,92102612,92103000,92104000,92111000,92111010,92114000,92121000,92121001,92121010,92121020,92121030,92121040,92121041,92121050,92130000,92130001,92130005,92130006,92130010,92130011,92130020,92130021,92130030,92130031,92152000,92152010,92161000,92161001,92161002,92162000,92162001,92162002,92171000,92171010,92191100,92191105,92191200,92191400,92192000,92192030,92192040,92193000,92193005,92193020,92193025,92201010,92291300,93202000,93301400)
  TEA=c(53246000,92302000,92302500,92303010,92303100,92304100,92305010,92305040,92305050,92305090,92305110,92305180,92305900,92305910,92305920,92306000,92306090,92306700,92306800,92307000,92307400,92308000,92308010,92308020,92308030,92308040,92308050,92308500,92308510,92308520,92308530,92308540,92308550,92309000,92309010,92309020,92309030,92309040,92309050,92309500,92309510,92309520)
  COFFEE_TEA=c(COFFEE, TEA)
  DRINK=c(11511100,11511200,11511300,11511400,11511550,11511600,11511610,11511700,11512010,11512020,11512030,11512100,11512110,11512120,11553130,11560000,64134030,67260000,75200700,91301130,92101920,92101921,92101923,92101925,92101926,92101928,92101930,92101931,92101933,92101935,92101936,92101938,92102000,92102010,92102020,92102030,92102040,92102050,92102060,92102070,92102080,92102090,92102100,92102110,92307500,92307510,92307520,92400000,92400100,92410310,92410315,92410320,92410340,92410350,92410360,92410370,92410390,92410400,92410410,92410420,92410510,92410520,92410550,92410560,92410610,92410620,92410710,92410720,92410810,92410820,92411510,92411520,92411610,92411620,92432000,92433000,92510610,92510650,92510955,92510960,92511015,92513000,92513010,92530410,92530510,92530610,92530950,92531030,92541010,92542000,92550030,92550035,92550040,92550110,92550200,92550370,92550400,92550405,92550610,92550620,92552000,92552010,92552020,92552030,92582100,92582110,92900100,92900110,92900200,92900300,93301216,95101000,95101010,95102000,95103000,95103010,95104000,95105000,95106000,95106010,95110000,95110010,95110020,95120000,95120010,95120020,95120050,95310200,95310400,95310500,95310550,95310555,95310560,95310600,95310700,95310750,95310800,95311000,95312400,95312410,95312500,95312550,95312555,95312560,95312600,95312700,95312800,95312900,95312905,95313200,95320200,95320500,95321000,95322200,95322500,
          95323000)
  SSB = c(COFFEE_TEA, DRINK)
  
  if (!("FoodCode" %in% colnames(COHORT))){
    stop("Please use the individual-level data since this function needs to calculate sugar sweetened beverage serving using data from individual food items. The file name should be like: Items.csv")
  }
  
  COHORT = COHORT %>%
    dplyr::mutate(
      ADDED_SUGAR_SSB_SERV = case_when(
        FoodCode %in% SSB ~ ADD_SUGARS,
        TRUE ~ 0
      )
    ) %>%
    dplyr::group_by(UserName) %>%
    dplyr::summarize(
      ENERGY=sum(KCAL),
      VEG_SERV = sum(V_REDOR_TOTAL + V_DRKGR*0.5 + V_STARCHY_OTHER + V_OTHER),
      FRT_SERV = sum(F_CITMLB+F_OTHER),
      WGRAIN_SERV = sum(G_WHOLE/0.035274),
      NUTSLEG_SERV = sum(PF_NUTSDS+PF_SOY+PF_LEGUMES),
      PUFA_SERV = sum(((PFAT-P205-P226)*9/ENERGY)*100),
      N3FAT_SERV = sum((P205+P226)*1000),
      SSB_FRTJ_SERV = sum((ADDED_SUGAR_SSB_SERV*4 / 26)),
      REDPROC_MEAT_SERV = sum((PF_CUREDMEAT/1.5) + ((PF_MEAT+PF_ORGAN)/4)),
      TRANS_SERV = sum(0),
      SODIUM_SERV = sum(SODI/(ENERGY/1000)),
      ALCOHOL_SERV = sum(A_DRINKS)
    )
  
  
  ##Create variables needed for AHEI calculation
  AHEI_MIN = 0
  AHEI_MAX = 10
  AHEI_MIN_VEG_SERV = 0
  AHEI_MAX_VEG_SERV = 5
  AHEI_MIN_FRT_SERV = 0
  AHEI_MAX_FRT_SERV = 4
  AHEI_MIN_WGRAIN_F_SERV = 0
  AHEI_MAX_WGRAIN_F_SERV = 75
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
  
  SCORE_HEALTHY = function(actual_serv, min_serv, max_serv, min_score, max_score){
    case_when(
      actual_serv >= max_serv ~ max_score,
      actual_serv <= min_serv ~ min_score,
      TRUE ~ min_score+(actual_serv-min_serv)*max_score/(max_serv-min_serv)
    )
  }
  
  SCORE_UNHEALTHY = function(actual_serv, min_serv, max_serv, min_score, max_score){
    case_when(
      actual_serv >= min_serv ~ min_score ,
      actual_serv <= max_serv ~ max_score,
      TRUE ~ min_score+(actual_serv-min_serv)*max_score/(max_serv-min_serv)
    )
  }
  
  SODIUM_DECILE = quantile(COHORT$SODIUM_SERV, probs=seq(0, 1, by=1/11))
  
  COHORT=COHORT %>%
    dplyr::mutate(
      AHEI_VEG = SCORE_HEALTHY(VEG_SERV, AHEI_MIN_VEG_SERV, AHEI_MAX_VEG_SERV, AHEI_MIN, AHEI_MAX),
      AHEI_FRT = SCORE_HEALTHY(FRT_SERV, AHEI_MIN_FRT_SERV, AHEI_MAX_FRT_SERV, AHEI_MIN, AHEI_MAX),
      AHEI_WGRAIN_F = SCORE_HEALTHY(WGRAIN_SERV, AHEI_MIN_WGRAIN_F_SERV, AHEI_MAX_WGRAIN_F_SERV, AHEI_MIN, AHEI_MAX),
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
      AHEI_ALCOHOL_F = case_when(
        ALCOHOL_SERV >= 2.5 ~ 0,
        ALCOHOL_SERV < 2.5 & ALCOHOL_SERV > 1.5 ~ 0 + (ALCOHOL_SERV-2.5)*10/(1.5-2.5),
        ALCOHOL_SERV <= 1.5 & ALCOHOL_SERV >= 0.5 ~ 10,
        ALCOHOL_SERV <= 0.125 ~ 2.5,
        TRUE ~  0 + (ALCOHOL_SERV-0)*10/(0.5-0)
      ),
      AHEI_ALL = AHEI_VEG + AHEI_FRT + AHEI_WGRAIN_F + AHEI_NUTSLEG + AHEI_N3FAT +
        AHEI_PUFA + AHEI_SSB_FRTJ + AHEI_REDPROC_MEAT + AHEI_TRANS + AHEI_SODIUM + AHEI_ALCOHOL_F,
      
      AHEI_NOETOH = AHEI_VEG + AHEI_FRT + AHEI_WGRAIN_F + AHEI_NUTSLEG + AHEI_N3FAT +
        AHEI_PUFA + AHEI_SSB_FRTJ + AHEI_REDPROC_MEAT + AHEI_TRANS + AHEI_SODIUM
    ) 
  
  print("Reminder: this AHEI index is for female only. Please stratify your data first and provide female only data.")
  
  COHORT %>%
    dplyr::select(UserName, AHEI_ALL, AHEI_NOETOH, AHEI_VEG, AHEI_FRT, AHEI_WGRAIN_F, AHEI_NUTSLEG, AHEI_N3FAT,
                  AHEI_PUFA, AHEI_SSB_FRTJ, AHEI_REDPROC_MEAT, AHEI_TRANS, AHEI_SODIUM, AHEI_ALCOHOL_F,SSB_FRTJ_SERV,
                  
                  ENERGY, VEG_SERV, FRT_SERV, WGRAIN_SERV, NUTSLEG_SERV, PUFA_SERV, N3FAT_SERV, SSB_FRTJ_SERV, 
                  REDPROC_MEAT_SERV, TRANS_SERV, SODIUM_SERV, ALCOHOL_SERV)
  
}

#' AHEI_M_ASA24
#'
#' Calculate the AHEI (male only) for the ASA24 data within 1 step  
#' @import dplyr
#' @import readr
#' @import haven
#' @param DATA_PATH The file path for the data. The file name should be like: Items.csv
#' @return The AHEI and its component scores
#' @examples
#' DATA_PATH = "/Users/james/Desktop/data/Items.csv"
#' AHEI_M_ASA24(DATA_PATH)
#' @export


AHEI_M_ASA24 = function(DATA_PATH){
  
  if (is.character(DATA_PATH) == TRUE){
    COHORT = read_csv(DATA_PATH)
  } else {
    COHORT = DATA_PATH
  }
  
  COFFEE=c(12200100,12210200,12210210,12210260,12210270,12210280,12210310,12210400,12210420,12210430,12210440,12210505,12210520,91703600,92100000,92100500,92101000,92101500,92101600,92101610,92101630,92101700,92101800,92101810,92101820,92101850,92101851,92101900,92101901,92101903,92101904,92101905,92101906,92101910,92101911,92101913,92101917,92101918,92101919,92101920,92101921,92101923,92101925,92101926,92101928,92101930,92101931,92101933,92101935,92101936,92101938,92101950,92101955,92101960,92101965,92101970,92101975,92102000,92102010,92102020,92102030,92102040,92102050,92102060,92102070,92102080,92102090,92102100,92102110,92102400,92102401,92102450,92102500,92102501,92102502,92102503,92102504,92102505,92102510,92102511,92102512,92102513,92102514,92102515,92102600,92102601,92102602,92102610,92102611,92102612,92103000,92104000,92111000,92111010,92114000,92121000,92121001,92121010,92121020,92121030,92121040,92121041,92121050,92130000,92130001,92130005,92130006,92130010,92130011,92130020,92130021,92130030,92130031,92152000,92152010,92161000,92161001,92161002,92162000,92162001,92162002,92171000,92171010,92191100,92191105,92191200,92191400,92192000,92192030,92192040,92193000,92193005,92193020,92193025,92201010,92291300,93202000,93301400)
  TEA=c(53246000,92302000,92302500,92303010,92303100,92304100,92305010,92305040,92305050,92305090,92305110,92305180,92305900,92305910,92305920,92306000,92306090,92306700,92306800,92307000,92307400,92308000,92308010,92308020,92308030,92308040,92308050,92308500,92308510,92308520,92308530,92308540,92308550,92309000,92309010,92309020,92309030,92309040,92309050,92309500,92309510,92309520)
  COFFEE_TEA=c(COFFEE, TEA)
  DRINK=c(11511100,11511200,11511300,11511400,11511550,11511600,11511610,11511700,11512010,11512020,11512030,11512100,11512110,11512120,11553130,11560000,64134030,67260000,75200700,91301130,92101920,92101921,92101923,92101925,92101926,92101928,92101930,92101931,92101933,92101935,92101936,92101938,92102000,92102010,92102020,92102030,92102040,92102050,92102060,92102070,92102080,92102090,92102100,92102110,92307500,92307510,92307520,92400000,92400100,92410310,92410315,92410320,92410340,92410350,92410360,92410370,92410390,92410400,92410410,92410420,92410510,92410520,92410550,92410560,92410610,92410620,92410710,92410720,92410810,92410820,92411510,92411520,92411610,92411620,92432000,92433000,92510610,92510650,92510955,92510960,92511015,92513000,92513010,92530410,92530510,92530610,92530950,92531030,92541010,92542000,92550030,92550035,92550040,92550110,92550200,92550370,92550400,92550405,92550610,92550620,92552000,92552010,92552020,92552030,92582100,92582110,92900100,92900110,92900200,92900300,93301216,95101000,95101010,95102000,95103000,95103010,95104000,95105000,95106000,95106010,95110000,95110010,95110020,95120000,95120010,95120020,95120050,95310200,95310400,95310500,95310550,95310555,95310560,95310600,95310700,95310750,95310800,95311000,95312400,95312410,95312500,95312550,95312555,95312560,95312600,95312700,95312800,95312900,95312905,95313200,95320200,95320500,95321000,95322200,95322500,
          95323000)
  SSB = c(COFFEE_TEA, DRINK)
  
  if (!("FoodCode" %in% colnames(COHORT))){
    stop("Please use the individual-level data since this function needs to calculate sugar sweetened beverage serving using data from individual food items. The file name should be like: Items.csv")
  }
  
  COHORT = COHORT %>%
    dplyr::mutate(
      ADDED_SUGAR_SSB_SERV = case_when(
        FoodCode %in% SSB ~ ADD_SUGARS,
        TRUE ~ 0
      )
    ) %>%
    dplyr::group_by(UserName) %>%
    dplyr::summarize(
      ENERGY=sum(KCAL),
      VEG_SERV = sum(V_REDOR_TOTAL + V_DRKGR*0.5 + V_STARCHY_OTHER + V_OTHER),
      FRT_SERV = sum(F_CITMLB+F_OTHER),
      WGRAIN_SERV = sum(G_WHOLE/0.035274),
      NUTSLEG_SERV = sum(PF_NUTSDS+PF_SOY+PF_LEGUMES),
      PUFA_SERV = sum(((PFAT-P205-P226)*9/ENERGY)*100),
      N3FAT_SERV = sum((P205+P226)*1000),
      SSB_FRTJ_SERV = sum((ADDED_SUGAR_SSB_SERV*4 / 26)),
      REDPROC_MEAT_SERV = sum((PF_CUREDMEAT/1.5) + ((PF_MEAT+PF_ORGAN)/4)),
      TRANS_SERV = sum(0),
      SODIUM_SERV = sum(SODI/(ENERGY/1000)),
      ALCOHOL_SERV = sum(A_DRINKS)
    )
  
  
  ##Create variables needed for AHEI calculation
  AHEI_MIN = 0
  AHEI_MAX = 10
  AHEI_MIN_VEG_SERV = 0
  AHEI_MAX_VEG_SERV = 5
  AHEI_MIN_FRT_SERV = 0
  AHEI_MAX_FRT_SERV = 4
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
  
  SCORE_HEALTHY = function(actual_serv, min_serv, max_serv, min_score, max_score){
    case_when(
      actual_serv >= max_serv ~ max_score,
      actual_serv <= min_serv ~ min_score,
      TRUE ~ min_score+(actual_serv-min_serv)*max_score/(max_serv-min_serv)
    )
  }
  
  SCORE_UNHEALTHY = function(actual_serv, min_serv, max_serv, min_score, max_score){
    case_when(
      actual_serv >= min_serv ~ min_score ,
      actual_serv <= max_serv ~ max_score,
      TRUE ~ min_score+(actual_serv-min_serv)*max_score/(max_serv-min_serv)
    )
  }
  
  SODIUM_DECILE = quantile(COHORT$SODIUM_SERV, probs=seq(0, 1, by=1/11))
  
  COHORT=COHORT %>%
    dplyr::mutate(
      AHEI_VEG = SCORE_HEALTHY(VEG_SERV, AHEI_MIN_VEG_SERV, AHEI_MAX_VEG_SERV, AHEI_MIN, AHEI_MAX),
      AHEI_FRT = SCORE_HEALTHY(FRT_SERV, AHEI_MIN_FRT_SERV, AHEI_MAX_FRT_SERV, AHEI_MIN, AHEI_MAX),
      AHEI_WGRAIN_M = SCORE_HEALTHY(WGRAIN_SERV, AHEI_MIN_WGRAIN_M_SERV, AHEI_MAX_WGRAIN_M_SERV, AHEI_MIN, AHEI_MAX),
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
      AHEI_ALCOHOL_M = case_when(
        ALCOHOL_SERV >= 3.5 ~ 0,
        ALCOHOL_SERV < 3.5 & ALCOHOL_SERV > 2 ~ 0 + (ALCOHOL_SERV-2.5)*10/(1.5-2.5),
        ALCOHOL_SERV <= 2 & ALCOHOL_SERV >= 0.5 ~ 10,
        ALCOHOL_SERV <= 0.125 ~ 2.5,
        TRUE ~  0 + (ALCOHOL_SERV-0)*10/(0.5-0)
      ),
      AHEI_ALL = AHEI_VEG + AHEI_FRT + AHEI_WGRAIN_M + AHEI_NUTSLEG + AHEI_N3FAT +
        AHEI_PUFA + AHEI_SSB_FRTJ + AHEI_REDPROC_MEAT + AHEI_TRANS + AHEI_SODIUM + AHEI_ALCOHOL_M,
      AHEI_NOETOH = AHEI_VEG + AHEI_FRT + AHEI_WGRAIN_M + AHEI_NUTSLEG + AHEI_N3FAT +
        AHEI_PUFA + AHEI_SSB_FRTJ + AHEI_REDPROC_MEAT + AHEI_TRANS + AHEI_SODIUM
    ) 
  
  print("Reminder: this AHEI index is for male only. Please stratify your data first and provide male only data.")
  
  
  COHORT %>%
    dplyr::select(UserName, AHEI_ALL, AHEI_NOETOH, AHEI_VEG, AHEI_FRT, AHEI_WGRAIN_M, AHEI_NUTSLEG, AHEI_N3FAT,
                  AHEI_PUFA, AHEI_SSB_FRTJ, AHEI_REDPROC_MEAT, AHEI_TRANS, AHEI_SODIUM, AHEI_ALCOHOL_M,
                  
                  ENERGY, VEG_SERV, FRT_SERV, WGRAIN_SERV, NUTSLEG_SERV, PUFA_SERV, N3FAT_SERV, SSB_FRTJ_SERV, 
                  REDPROC_MEAT_SERV, TRANS_SERV, SODIUM_SERV, ALCOHOL_SERV)
  
}

#' DASH_ASA24
#'
#' Calculate the DASH for the ASA24 data within 1 step  
#' @import dplyr
#' @import readr
#' @import haven
#' @param DATA_PATH The file path for the data. The file name should be like: Items.csv
#' @return The DASH and its component scores
#' @examples
#' DATA_PATH = "/Users/james/Desktop/data/Items.csv"
#' DASH_ASA24(DATA_PATH)
#' @export


DASH_ASA24 = function(DATA_PATH){
  
  if (is.character(DATA_PATH) == TRUE){
    COHORT = read_csv(DATA_PATH)
  } else {
    COHORT = DATA_PATH
  }
  
  LOWF_MILK=c(11111160,11111170,11112120,11112210,11113000,11114300,11114320,11115000,11115100,11115400,11120000,11121210,11121300,11212050,11511000,11511300,11511400,11511550,11511600,11511610,11511700,11512020,11512110,11513200,11513300,11513365,11513370,11513383,11513384,11513393,11513394,11513600,11513700,11513803,11513804,11513853,11513854,11514100,11514130,11514140,11514310,11514340,11514350,11519200,11519205,11519210)
  LOWF_CHEESE=c(14204010,14204020,14206010,14207010)
  
  COFFEE=c(12200100,12210200,12210210,12210260,12210270,12210280,12210310,12210400,12210420,12210430,12210440,12210505,12210520,91703600,92100000,92100500,92101000,92101500,92101600,92101610,92101630,92101700,92101800,92101810,92101820,92101850,92101851,92101900,92101901,92101903,92101904,92101905,92101906,92101910,92101911,92101913,92101917,92101918,92101919,92101920,92101921,92101923,92101925,92101926,92101928,92101930,92101931,92101933,92101935,92101936,92101938,92101950,92101955,92101960,92101965,92101970,92101975,92102000,92102010,92102020,92102030,92102040,92102050,92102060,92102070,92102080,92102090,92102100,92102110,92102400,92102401,92102450,92102500,92102501,92102502,92102503,92102504,92102505,92102510,92102511,92102512,92102513,92102514,92102515,92102600,92102601,92102602,92102610,92102611,92102612,92103000,92104000,92111000,92111010,92114000,92121000,92121001,92121010,92121020,92121030,92121040,92121041,92121050,92130000,92130001,92130005,92130006,92130010,92130011,92130020,92130021,92130030,92130031,92152000,92152010,92161000,92161001,92161002,92162000,92162001,92162002,92171000,92171010,92191100,92191105,92191200,92191400,92192000,92192030,92192040,92193000,92193005,92193020,92193025,92201010,92291300,93202000,93301400)
  TEA=c(53246000,92302000,92302500,92303010,92303100,92304100,92305010,92305040,92305050,92305090,92305110,92305180,92305900,92305910,92305920,92306000,92306090,92306700,92306800,92307000,92307400,92308000,92308010,92308020,92308030,92308040,92308050,92308500,92308510,92308520,92308530,92308540,92308550,92309000,92309010,92309020,92309030,92309040,92309050,92309500,92309510,92309520)
  COFFEE_TEA=c(COFFEE, TEA)
  DRINK=c(11511100,11511200,11511300,11511400,11511550,11511600,11511610,11511700,11512010,11512020,11512030,11512100,11512110,11512120,11553130,11560000,64134030,67260000,75200700,91301130,92101920,92101921,92101923,92101925,92101926,92101928,92101930,92101931,92101933,92101935,92101936,92101938,92102000,92102010,92102020,92102030,92102040,92102050,92102060,92102070,92102080,92102090,92102100,92102110,92307500,92307510,92307520,92400000,92400100,92410310,92410315,92410320,92410340,92410350,92410360,92410370,92410390,92410400,92410410,92410420,92410510,92410520,92410550,92410560,92410610,92410620,92410710,92410720,92410810,92410820,92411510,92411520,92411610,92411620,92432000,92433000,92510610,92510650,92510955,92510960,92511015,92513000,92513010,92530410,92530510,92530610,92530950,92531030,92541010,92542000,92550030,92550035,92550040,92550110,92550200,92550370,92550400,92550405,92550610,92550620,92552000,92552010,92552020,92552030,92582100,92582110,92900100,92900110,92900200,92900300,93301216,95101000,95101010,95102000,95103000,95103010,95104000,95105000,95106000,95106010,95110000,95110010,95110020,95120000,95120010,95120020,95120050,95310200,95310400,95310500,95310550,95310555,95310560,95310600,95310700,95310750,95310800,95311000,95312400,95312410,95312500,95312550,95312555,95312560,95312600,95312700,95312800,95312900,95312905,95313200,95320200,95320500,95321000,95322200,95322500,95323000)
  SSB = c(COFFEE_TEA, DRINK)
  
  if (!("FoodCode" %in% colnames(COHORT))){
    stop("Please use the individual-level data since this function needs to calculate sugar sweetened beverage serving using data from individual food items. The file name should be like: Items.csv")
  }
  
  #Match participant response food frequency to the standard food frequency response code
  COHORT = COHORT %>%
    dplyr::mutate(
      LOWF_MILK_SERV = case_when(
        FoodCode %in% LOWF_MILK ~ D_MILK,
        TRUE ~ 0
      ),
      LOWF_CHEESECREAM_SERV = case_when(
        FoodCode %in% LOWF_CHEESE ~ D_CHEESE*4,
        TRUE ~ 0
      ),
      ADDED_SUGAR_SSB_SERV = case_when(
        FoodCode %in% SSB ~ ADD_SUGARS,
        TRUE ~ 0
      )
    ) %>%
    dplyr::group_by(UserName) %>%
    dplyr::summarize(
      ENERGY=sum(KCAL),
      FRT_FRTJ_SERV = sum(F_TOTAL),
      VEG_SERV = sum(V_REDOR_TOTAL + V_DRKGR*0.5 + V_STARCHY_OTHER + V_OTHER),
      NUTSLEG_SERV = sum(PF_NUTSDS+PF_SOY+PF_LEGUMES),
      WGRAIN_SERV = sum(G_WHOLE),
      LOWF_DAIRY_SERV = sum(LOWF_MILK_SERV + LOWF_CHEESECREAM_SERV + D_YOGURT),
      SODIUM_SERV = sum(SODI/(ENERGY/1000)),
      REDPROC_MEAT_SERV = sum((PF_CUREDMEAT/1.5) + ((PF_MEAT+PF_ORGAN)/4)),
      SSB_FRTJ_SERV = sum((ADDED_SUGAR_SSB_SERV*4 / 26))
    ) 
  
  
  ##Create variables and functions needed for DASH calculation
  quintile_healthy = function(actual){
    quintile = quantile(actual, probs=seq(0, 1, by=0.2))
    case_when(
      actual <= quintile[6] & actual >= quintile[5] ~ 5,
      actual < quintile[5] & actual >= quintile[4] ~ 4,
      actual < quintile[4] & actual >= quintile[3] ~ 3,
      actual < quintile[3] & actual >= quintile[2] ~ 2,
      actual < quintile[2] & actual >= quintile[1] ~ 1
    )
  }
  
  quintile_unhealthy = function(actual){
    quintile = quantile(actual, probs=seq(0, 1, by=0.2))
    case_when(
      actual <= quintile[6] & actual >= quintile[5] ~ 1,
      actual < quintile[5] & actual >= quintile[4] ~ 2,
      actual < quintile[4] & actual >= quintile[3] ~ 3,
      actual < quintile[3] & actual >= quintile[2] ~ 4,
      actual < quintile[2] & actual >= quintile[1] ~ 5
    )
  }
  
  print("Reminder: this DASH index uses quintiles to rank participants' food/drink serving sizes and then calculate DASH component scores, which may generate results that are specific to your study population but not comparable to other populations.")
  
  ##DASH calculation
  COHORT %>%
    dplyr::mutate(
      DASH_FRT = quintile_healthy(FRT_FRTJ_SERV),
      DASH_VEG = quintile_healthy(VEG_SERV),
      DASH_NUTSLEG = quintile_healthy(NUTSLEG_SERV),
      DASH_WGRAIN = quintile_healthy(WGRAIN_SERV),
      DASH_LOWF_DAIRY = quintile_healthy(LOWF_DAIRY_SERV),
      DASH_SODIUM = quintile_unhealthy(SODIUM_SERV),
      DASH_REDPROC_MEAT = quintile_unhealthy(REDPROC_MEAT_SERV),
      DASH_SSB_FRTJ = quintile_unhealthy(SSB_FRTJ_SERV),
      DASH_ALL = DASH_FRT+DASH_VEG+DASH_NUTSLEG+DASH_WGRAIN+DASH_LOWF_DAIRY+
        DASH_SODIUM+DASH_REDPROC_MEAT+DASH_SSB_FRTJ
    )%>%
    dplyr::select(UserName, DASH_ALL, DASH_FRT, DASH_VEG, DASH_NUTSLEG, DASH_WGRAIN, DASH_LOWF_DAIRY,
                  DASH_SODIUM, DASH_REDPROC_MEAT, DASH_SSB_FRTJ,
                  
                  ENERGY, FRT_FRTJ_SERV, VEG_SERV, NUTSLEG_SERV, WGRAIN_SERV, LOWF_DAIRY_SERV, 
                  SODIUM_SERV, REDPROC_MEAT_SERV, SSB_FRTJ_SERV)
}

#' MED_ASA24
#'
#' Calculate the MED for the ASA24 data within 1 step  
#' @import dplyr
#' @import readr
#' @import haven
#' @param DATA_PATH The file path for the data. The file name should be like: Totals.csv.
#' @return The MED and its component scores
#' @examples
#' DATA_PATH = "/Users/james/Desktop/data/Totals.csv"
#' MED_ASA24(DATA_PATH)
#' @export

MED_ASA24 = function(DATA_PATH){
  
  if (is.character(DATA_PATH) == TRUE){
    COHORT = read_csv(DATA_PATH)
  } else {
    COHORT = DATA_PATH
  }
  
  if ("FoodCode" %in% colnames(COHORT)){
    stop("Please use the population-level data. The file name should be like: Totals.csv")
  }
  
  #Match participant response food frequency to the standard food frequency response code
  
  COHORT = COHORT %>%
    dplyr::mutate(
      FRT_FRTJ_SERV = F_TOTAL,
      VEG_SERV = V_REDOR_TOTAL + V_DRKGR*0.5 + V_STARCHY_OTHER + V_OTHER,
      WGRAIN_SERV = G_WHOLE,
      LEGUMES_SERV = PF_SOY+PF_LEGUMES,
      NUTS_SERV = PF_NUTSDS,
      FISH_SERV = PF_SEAFD_HI+PF_SEAFD_LOW,
      REDPROC_MEAT_SERV = (PF_CUREDMEAT/1.5) + ((PF_MEAT+PF_ORGAN)/4),
      MONSATFAT_SERV = case_when(
        SFAT == 0 ~ 0, 
        TRUE ~ MFAT/SFAT
      ),
      ALCOHOL_SERV = ALC,
    ) 
  
  ##Create variables and functions needed for MED
  median_healthy = function(actual){
    median_score = median(actual)
    case_when(
      actual < median_score ~ 0,
      actual >= median_score ~ 1
    )
  }
  
  median_unhealthy = function(actual){
    median_score = median(actual)
    case_when(
      actual < median_score ~ 1,
      actual >= median_score ~ 0
    )
  }
  
  print("Reminder: this MED index uses medians to rank participants' food/drink serving sizes and then calculate MED component scores, which may generate results that are specific to your study population but not comparable to other populations.")
  
  COHORT %>%
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
        ALCOHOL_SERV <=25 & ALCOHOL_SERV >= 10 ~ 1, 
        TRUE ~ 0),
      
      MED_ALL = MED_FRT+MED_VEG+MED_WGRAIN+MED_LEGUMES+MED_NUTS+MED_FISH+MED_REDPROC_MEAT+MED_MONSATFAT+MED_ALCOHOL,
      MED_NOETOH = MED_FRT+MED_VEG+MED_WGRAIN+MED_LEGUMES+MED_NUTS+MED_FISH+MED_REDPROC_MEAT+MED_MONSATFAT
    )%>%
    dplyr::select(UserName, MED_ALL, MED_NOETOH, MED_FRT, MED_VEG, MED_WGRAIN, MED_LEGUMES, MED_NUTS,
           MED_FISH, MED_REDPROC_MEAT, MED_MONSATFAT, MED_ALCOHOL,
           
           FRT_FRTJ_SERV, VEG_SERV, WGRAIN_SERV, LEGUMES_SERV, NUTS_SERV, FISH_SERV, REDPROC_MEAT_SERV, 
           MONSATFAT_SERV, ALCOHOL_SERV)
}

#' DII_ASA24
#'
#' Calculate the Dietary Inflammatory Index for the ASA24 data within 1 step  
#' @import dplyr
#' @import readr
#' @import haven
#' @param DATA_PATH The file path for the data. The file name should be like: Totals.csv.
#' @return The DII and its component scores
#' @examples
#' DATA_PATH = "/Users/james/Desktop/data/Totals.csv"
#' DII_ASA24(DATA_PATH)
#' @export

DII_ASA24 = function(DATA_PATH){
  
  if (is.character(DATA_PATH) == TRUE){
    COHORT = read_csv(DATA_PATH)
  } else {
    COHORT = DATA_PATH
  }
  
  if ("FoodCode" %in% colnames(COHORT)){
    stop("Please use the population-level data. The file name should be like: Totals.csv")
  }
  
  #Serving size calculation for DII
  COHORT1 = COHORT %>%
    dplyr::mutate(
      ALCOHOL = ALC,
      VITB12 = VB12,
      VITB6 = VB6,
      BCAROTENE = BCAR,
      CAFFEINE = CAFF/1000,
      CARB = CARB,
      CHOLES = CHOLE,
      KCAL = KCAL,
      TOTALFAT = TFAT,
      FIBER = FIBE,
      FOLICACID = FA,
      IRON = IRON,
      MG = MAGN,
      MUFA = MFAT,
      NIACIN = NIAC,
      N3FAT = P205 + P225 + P226,
      N6FAT = P182 + P183 + P184 + P204,
      PROTEIN = PROT,
      PUFA = PFAT,
      RIBOFLAVIN = VB2,
      SATFAT = SFAT,
      SE = SELE,
      THIAMIN = VB1,
      VITA = VARA,
      VITC = VC,
      VITD = VITD,
      VITE = ATOC,
      ZN = ZINC
    ) %>%
    dplyr::select(UserName, RecallNo, ALCOHOL, VITB12, VITB6, BCAROTENE, CAFFEINE, CARB, CHOLES, KCAL, TOTALFAT, FIBER, FOLICACID,
                  IRON, MG, MUFA, NIACIN, N3FAT, N6FAT, PROTEIN, PUFA, RIBOFLAVIN, SATFAT, SE, THIAMIN, VITA,
                  VITC, VITD, VITE, ZN)
  
  COHORT2 = COHORT1 %>%
    tidyr::pivot_longer(-c(UserName, RecallNo), names_to="Variable", values_to="Value")
  
  Variable = c("ALCOHOL", "VITB12", "VITB6", "BCAROTENE", "CAFFEINE", "CARB", "CHOLES", "KCAL", "EUGENOL",
               "TOTALFAT", "FIBER", "FOLICACID","GARLIC", "GINGER","IRON", "MG", "MUFA", "NIACIN", "N3FAT", "N6FAT","ONION", "PROTEIN", "PUFA", 
               "RIBOFLAVIN","SAFFRON", "SATFAT", "SE", "THIAMIN","TRANSFAT","TURMERIC", "VITA","VITC", "VITD", "VITE", "ZN", "TEA",
               "FLA3OL","FLAVONES","FLAVONOLS","FLAVONONES","ANTHOC","ISOFLAVONES","PEPPER","THYME","ROSEMARY")
  
  Overall_inflammatory_score = c(-0.278, 0.106, -0.365, -0.584, -0.11, 0.097, 0.11, 0.18, -0.14, 0.298, -0.663, -0.19, -0.412, -0.453, 0.032, -0.484, -0.009,
                                 -0.246, -0.436, -0.159, -0.301, 0.021, -0.337, -0.068, -0.14, 0.373, -0.191, -0.098,0.229,-0.785, -0.401, -0.424, -0.446, -0.419, -0.313,
                                 -0.536,-0.415,-0.616,-0.467,-0.25,-0.131,-0.593,-0.131,-0.102,-0.013)
  
  Global_mean = c(13.98,5.15,1.47,3718,8.05,272.2,279.4,2056,0.01,71.4,18.8,273,4.35,59,13.35,310.1,27,25.9,1.06,10.8,35.9,
                  79.4,13.88,1.7,0.37,28.6,67,1.7,3.15,533.6,983.9,118.2,6.26,8.73,9.84,
                  1.69,95.8,1.55,17.7,11.7,18.05,1.2,10,0.33,1)
  
  SD = c(3.72,2.7,0.74,1720,6.67,40,51.2,338,0.08,19.4,4.9,70.7,2.9,63.2,3.71,139.4,6.1,11.77,1.06,7.5,18.4,13.9,3.76,0.79,1.78,
         8,25.1,0.66,3.75,754.3,518.6,43.46,2.21,1.49,2.19,
         1.53,85.9,0.07,6.79,3.82,21.14,0.2,7.07,0.99,15)
  
  DII_STD = base::data.frame(Variable, Overall_inflammatory_score, Global_mean, SD)
  
  #Score calculation for DII  
  
  COHORT2 %>%
    dplyr::inner_join(DII_STD, by=c("Variable")) %>%
    dplyr::mutate(
      Z_SCORE = (Value - Global_mean)/SD,
      PERCENTILE = pnorm(Z_SCORE)*2 - 1,
      IND_DII_SCORE = PERCENTILE*Overall_inflammatory_score) %>%
    tidyr::pivot_wider(names_from = Variable, values_from = IND_DII_SCORE) %>%
    dplyr::group_by(UserName, RecallNo) %>% 
    dplyr::summarize(
      DII_ALL = base::sum(ALCOHOL, VITB12, VITB6, BCAROTENE, CAFFEINE, CARB, CHOLES, KCAL, TOTALFAT, FIBER, FOLICACID,
                          IRON, MG, MUFA, NIACIN, N3FAT, N6FAT, PROTEIN, PUFA, RIBOFLAVIN, SATFAT, SE, THIAMIN, VITA,
                          VITC, VITD, VITE, ZN, na.rm = TRUE),
      
      DII_NOETOH =  base::sum(VITB12, VITB6, BCAROTENE, CAFFEINE, CARB, CHOLES, KCAL, TOTALFAT, FIBER, FOLICACID,
                              IRON, MG, MUFA, NIACIN, N3FAT, N6FAT, PROTEIN, PUFA, RIBOFLAVIN, SATFAT, SE, THIAMIN, VITA,
                              VITC, VITD, VITE, ZN, na.rm = TRUE),
      
      ALCOHOL = base::sum(ALCOHOL, na.rm = TRUE), 
      VITB12 = base::sum(VITB12, na.rm = TRUE), 
      VITB6 = base::sum(VITB6, na.rm = TRUE), 
      BCAROTENE = base::sum(BCAROTENE, na.rm = TRUE), 
      CAFFEINE = base::sum(CAFFEINE, na.rm = TRUE), 
      CARB = base::sum(CARB, na.rm = TRUE), 
      CHOLES = base::sum(CHOLES, na.rm = TRUE), 
      KCAL= base::sum(KCAL, na.rm = TRUE), 
      TOTALFAT= base::sum(TOTALFAT, na.rm = TRUE), 
      FIBER  = base::sum(FIBER, na.rm = TRUE), 
      FOLICACID  = base::sum(FOLICACID, na.rm = TRUE),
      IRON  = base::sum(IRON, na.rm = TRUE), 
      MG  = base::sum(MG, na.rm = TRUE), 
      MUFA  = base::sum(MUFA, na.rm = TRUE), 
      NIACIN  = base::sum(NIACIN, na.rm = TRUE), 
      N3FAT  = base::sum(N3FAT, na.rm = TRUE), 
      N6FAT  = base::sum(N6FAT, na.rm = TRUE), 
      PROTEIN  = base::sum(PROTEIN, na.rm = TRUE), 
      PUFA  = base::sum(PUFA, na.rm = TRUE), 
      RIBOFLAVIN  = base::sum(RIBOFLAVIN, na.rm = TRUE), 
      SATFAT  = base::sum(SATFAT, na.rm = TRUE), 
      SE  = base::sum(SE, na.rm = TRUE), 
      THIAMIN  = base::sum(THIAMIN, na.rm = TRUE), 
      VITA  = base::sum(VITA, na.rm = TRUE),
      VITC  = base::sum(VITC, na.rm = TRUE), 
      VITD  = base::sum(VITD, na.rm = TRUE), 
      VITE = base::sum(VITE, na.rm = TRUE), 
      ZN = base::sum(ZN, na.rm = TRUE)
    )
  
}

#' HEI2015_DHQ3
#'
#' Calculate the HEI2015 for the DHQ3 data within 1 step  
#' @import dplyr
#' @import readr
#' @import haven
#' @param DATA_PATH The file path for the data. The data is Total Daily Results file, ending with results.csv
#' @return The HEI2015 and its component scores
#' @examples
#' DATA_PATH = "/Users/james/Desktop/data/results.csv"
#' HEI2015_DHQ3(DATA_PATH)
#' @export

HEI2015_DHQ3 = function(DATA_PATH){
  
  if (is.character(DATA_PATH) == TRUE){
    COHORT = read_csv(DATA_PATH)
  } else {
    COHORT = DATA_PATH
  }
  
  if ("Food ID" %in% colnames(COHORT) ){
    stop("Please use population-level data for this function. Population-level data should be like results.csv")
  }
  
  
  COHORT = COHORT %>%
    dplyr::mutate(
      TOTALKCAL = `Energy (kcal)`,
      TOTALFRT_SERV = `Total fruit (cups)`/(TOTALKCAL/1000),
      FRT_SERV = (`Total fruit (cups)`-`Juice fruit (cups)`)/(TOTALKCAL/1000),
      VEG_SERV = (`Total vegetable (cups)`+`Legumes vegetable (cups)`)/(TOTALKCAL/1000),
      GREENNBEAN_SERV = (`Dark-green vegetable (cups)`+`Legumes vegetable (cups)`)/(TOTALKCAL/1000),
      TOTALPRO_SERV = (`Total meat, poultry, seafood protein foods (oz)`+`Eggs protein foods (oz)`+`Nuts and seeds protein foods (oz)`+`Soy products protein foods (oz)`+`Legumes protein foods (oz)`)/(TOTALKCAL/1000),
      SEAPLANTPRO_SERV = (`Seafood (oz)`+`Nuts and seeds protein foods (oz)`+`Soy products protein foods (oz)`+`Legumes protein foods (oz)`)/(TOTALKCAL/1000),
      WHOLEGRAIN_SERV = (`Whole grain (oz)`)/(TOTALKCAL/1000),
      DAIRY_SERV = (`Total dairy (cups)`)/(TOTALKCAL/1000),
      FATTYACID_SERV = (`Total monounsaturated fatty acids (g)`+`Total polyunsaturated fatty acids (g)`)/`Total saturated fatty acids (g)`,
      
      REFINEDGRAIN_SERV = (`Refined grain (oz)`)/(TOTALKCAL/1000),
      SODIUM_SERV = (`Sodium (mg)`/1000)/(TOTALKCAL/1000),
      ADDEDSUGAR_SERV = ((`Added sugars (tsp)`*4*4)/(TOTALKCAL))*100,
      SATFAT_SERV = ((`Total saturated fatty acids (g)`*9)/(TOTALKCAL))*100
    )
  
  
  ##Create variables needed for HEI2015 calculation
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
  
  HEI2015_HEALTHY1 = function(actual, min, max){
    case_when(
      actual >= max ~ HEI2015_MAX1,
      actual <= min ~ HEI2015_MIN,
      TRUE ~ HEI2015_MIN+(actual-min)*HEI2015_MAX1/(max-min)
    )
  }
  
  HEI2015_HEALTHY2 = function(actual, min, max){
    case_when(
      actual >= max ~ HEI2015_MAX2,
      actual <= min ~ HEI2015_MIN,
      TRUE ~ HEI2015_MIN+(actual-min)*HEI2015_MAX2/(max-min)
    )
  }
  
  HEI2015_UNHEALTHY = function(actual, min, max){
    case_when(
      actual >= min ~ HEI2015_MIN,
      actual <= max ~ HEI2015_MAX2,
      TRUE ~ HEI2015_MIN+(actual-min)*HEI2015_MAX2/(max-min)
    )
  }
  
  COHORT=COHORT %>%
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
      
      HEI2015_ALL= HEI2015_TOTALFRT + HEI2015_FRT + HEI2015_VEG + HEI2015_GREENNBEAN +
        HEI2015_TOTALPRO + HEI2015_SEAPLANTPRO + HEI2015_WHOLEGRAIN + HEI2015_DAIRY +
        HEI2015_FATTYACID + HEI2015_REFINEDGRAIN + HEI2015_SODIUM + HEI2015_ADDEDSUGAR +
        HEI2015_SATFAT
    ) 
  
  for(i in 1:length(COHORT$TOTALKCAL)){
    if (COHORT$TOTALKCAL[i] == 0){
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
    dplyr::select(`Respondent ID`, TOTALKCAL, HEI2015_ALL, HEI2015_TOTALFRT, HEI2015_FRT, HEI2015_VEG, HEI2015_GREENNBEAN,
                  HEI2015_TOTALPRO, HEI2015_SEAPLANTPRO, HEI2015_WHOLEGRAIN, HEI2015_DAIRY,
                  HEI2015_FATTYACID, HEI2015_REFINEDGRAIN, HEI2015_SODIUM, HEI2015_ADDEDSUGAR,
                  HEI2015_SATFAT,
                  
                  TOTALKCAL, TOTALFRT_SERV, FRT_SERV, VEG_SERV, GREENNBEAN_SERV, TOTALPRO_SERV, SEAPLANTPRO_SERV, WHOLEGRAIN_SERV, DAIRY_SERV, FATTYACID_SERV, REFINEDGRAIN_SERV, SODIUM_SERV, ADDEDSUGAR_SERV, SATFAT_SERV) 
}

#' AHEI_DHQ3
#'
#' Calculate the AHEI for the DHQ3 data within 1 step  
#' @import dplyr
#' @import readr
#' @import haven
#' @param DATA_PATH The file path for the data. The data is Detailed analysis file, ending with detail.csv
#' @return The AHEI and its component scores
#' @examples
#' DATA_PATH = "/Users/james/Desktop/data/detail.csv"
#' AHEI_DHQ3(DATA_PATH)
#' @export

AHEI_DHQ3 = function(DATA_PATH){
  
  if (is.character(DATA_PATH) == TRUE){
    COHORT = read_csv(DATA_PATH)
  } else {
    COHORT = DATA_PATH
  }
  
  if (!("Food ID" %in% colnames(COHORT)) ){
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
      VEG_SERV = sum(`Total red/orange vegetable (cups)` + `Dark-green vegetable (cups)`*0.5 + `Other starchy vegetable (cups)` + `Other vegetable (cups)`),
      FRT_SERV = sum(`Total fruit (cups)`-`Juice fruit (cups)`),
      WGRAIN_SERV = sum(`Whole grain (oz)`/0.035274),
      NUTSLEG_SERV = sum(`Nuts, seeds, soy, and legumes (oz)`),
      N3FAT_SERV = sum((`PFA 20:5 (Eicosapentaenoic) (g)`+`PFA 22:6 (Docosahexaenoic) (g)`)*1000),
      PUFA_SERV = (sum((`Total polyunsaturated fatty acids (g)`-`PFA 20:5 (Eicosapentaenoic) (g)`-`PFA 22:6 (Docosahexaenoic) (g)`)*9)/KCAL)*100,
      SSB_FRTJ_SERV = sum(ADDED_SUGAR_SSB_SERV/ 26),
      REDPROC_MEAT_SERV = sum((`Cured meat protein foods (oz)`/1.5) + ((`Meat from beef, pork, veal, lamb, and game protein foods (oz)`+`Meat from organ meat protein foods (oz)`)/4)),
      TRANS_SERV = (sum(`*Total trans fatty acitds (g)`*9)/KCAL)*100,
      SODIUM_SERV = sum(`Sodium (mg)`/(KCAL/1000)),
      ALCOHOL_SERV = sum(`Alcohol (drink(s))`)
    )
  
  
  ##Create variables and functions needed for AHEI calculation
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
  
  SCORE_HEALTHY = function(actual_serv, min_serv, max_serv, min_score, max_score){
    case_when(
      actual_serv >= max_serv ~ max_score,
      actual_serv <= min_serv ~ min_score,
      TRUE ~ min_score+(actual_serv-min_serv)*max_score/(max_serv-min_serv)
    )
  }
  
  
  SCORE_UNHEALTHY = function(actual_serv, min_serv, max_serv, min_score, max_score){
    case_when(
      actual_serv >= min_serv ~ min_score ,
      actual_serv <= max_serv ~ max_score,
      TRUE ~ min_score+(actual_serv-min_serv)*max_score/(max_serv-min_serv)
    )
  }
  
  SODIUM_DECILE = quantile(COHORT$SODIUM_SERV, probs=seq(0, 1, by=1/11))
  
  
  COHORT %>%
    dplyr::mutate(
      RESPONDENTID = `Respondent ID`,
      GENDER = `Sex (1=male; 2=female)`,
      
      AHEI_VEG = SCORE_HEALTHY(VEG_SERV, AHEI_MIN_VEG_SERV, AHEI_MAX_VEG_SERV, AHEI_MIN, AHEI_MAX),
      AHEI_FRT = SCORE_HEALTHY(FRT_SERV, AHEI_MIN_FRT_SERV, AHEI_MAX_FRT_SERV, AHEI_MIN, AHEI_MAX),
      
      AHEI_WGRAIN = case_when(
        #GENDER = 2 is female
        GENDER == 2 & WGRAIN_SERV >= AHEI_MAX_WGRAIN_F_SERV ~ AHEI_MAX,
        GENDER == 2 & WGRAIN_SERV <= AHEI_MIN_WGRAIN_F_SERV ~ AHEI_MIN,
        GENDER == 2 & WGRAIN_SERV > AHEI_MIN_WGRAIN_F_SERV & WGRAIN_SERV < AHEI_MAX_WGRAIN_F_SERV ~ AHEI_MIN+(WGRAIN_SERV-AHEI_MIN_WGRAIN_F_SERV)*AHEI_MAX/(AHEI_MAX_WGRAIN_F_SERV-AHEI_MIN_WGRAIN_F_SERV),
        
        #GENDER = 1 is male
        GENDER == 1 & WGRAIN_SERV >= AHEI_MAX_WGRAIN_M_SERV ~ AHEI_MAX,
        GENDER == 1 & WGRAIN_SERV <= AHEI_MIN_WGRAIN_M_SERV ~ AHEI_MIN,
        GENDER == 1 & WGRAIN_SERV > AHEI_MIN_WGRAIN_M_SERV & WGRAIN_SERV < AHEI_MAX_WGRAIN_M_SERV ~ AHEI_MIN+(WGRAIN_SERV-AHEI_MIN_WGRAIN_M_SERV)*AHEI_MAX/(AHEI_MAX_WGRAIN_M_SERV-AHEI_MIN_WGRAIN_M_SERV),
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
        ##GENDER = 2 is female
        GENDER == 2  & ALCOHOL_SERV >= 2.5 ~ 0,
        GENDER == 2  & ALCOHOL_SERV < 2.5 & ALCOHOL_SERV > 1.5 ~ 0 + (ALCOHOL_SERV-2.5)*10/(1.5-2.5),
        GENDER == 2  & ALCOHOL_SERV <= 1.5 & ALCOHOL_SERV >= 0.5 ~ 10,
        GENDER == 2  & ALCOHOL_SERV < 0.5 & ALCOHOL_SERV > 0.125 ~ 0 + (ALCOHOL_SERV-0)*10/(0.5-0),
        GENDER == 2  & ALCOHOL_SERV <= 0.125 ~ 2.5,
        
        #GENDER = 1 is male
        GENDER == 1  & ALCOHOL_SERV >= 3.5 ~ 0,
        GENDER == 1  & ALCOHOL_SERV < 3.5 & ALCOHOL_SERV > 2 ~ 0 + (ALCOHOL_SERV-2.5)*10/(1.5-2.5),
        GENDER == 1  & ALCOHOL_SERV <= 2 & ALCOHOL_SERV >= 0.5 ~ 10,
        GENDER == 1  & ALCOHOL_SERV < 0.5 & ALCOHOL_SERV > 0.125 ~ 0 + (ALCOHOL_SERV-0)*10/(0.5-0),
        GENDER == 1  & ALCOHOL_SERV <= 0.125 ~ 2.5,
      ),
      AHEI_ALL = AHEI_VEG + AHEI_FRT + AHEI_WGRAIN + AHEI_NUTSLEG + AHEI_N3FAT +
        AHEI_PUFA + AHEI_SSB_FRTJ + AHEI_REDPROC_MEAT + AHEI_TRANS + AHEI_SODIUM + AHEI_ALCOHOL,
      
      AHEI_NOETOH = AHEI_VEG + AHEI_FRT + AHEI_WGRAIN + AHEI_NUTSLEG + AHEI_N3FAT +
        AHEI_PUFA + AHEI_SSB_FRTJ + AHEI_REDPROC_MEAT + AHEI_TRANS + AHEI_SODIUM
    ) %>%
    dplyr::select(RESPONDENTID, GENDER, AHEI_ALL, AHEI_NOETOH, AHEI_VEG, AHEI_FRT, AHEI_WGRAIN, AHEI_NUTSLEG, AHEI_N3FAT,
                  AHEI_PUFA, AHEI_SSB_FRTJ, AHEI_REDPROC_MEAT, AHEI_TRANS, AHEI_SODIUM, AHEI_ALCOHOL,
                  
                  KCAL, VEG_SERV, FRT_SERV, WGRAIN_SERV, NUTSLEG_SERV, N3FAT_SERV, PUFA_SERV, SSB_FRTJ_SERV, REDPROC_MEAT_SERV, TRANS_SERV, SODIUM_SERV, ALCOHOL_SERV)
}


#' MED_DHQ3
#'
#' Calculate the MED for the DHQ3 data within 1 step  
#' @import dplyr
#' @import readr
#' @import haven
#' @param DATA_PATH The file path for the data. The data is Total Daily Results file, ending with results.csv
#' @return The MED and its component scores
#' @examples
#' DATA_PATH = "/Users/james/Desktop/data/results.csv"
#' MED_DHQ3(DATA_PATH)
#' @export

MED_DHQ3 = function(DATA_PATH){
  
  if (is.character(DATA_PATH) == TRUE){
    COHORT = read_csv(DATA_PATH)
  } else {
    COHORT = DATA_PATH
  }
  
  if ("Food ID" %in% colnames(COHORT) ){
    stop("Please use population-level data for this function. Population-level data should be like results.csv")
  }
  
  #Match participant response food frequency to the standard food frequency response code
  
  COHORT = COHORT %>%
    dplyr::mutate(
      RESPONDENTID = `Respondent ID`,
      
      FRT_FRTJ_SERV = `Total fruit (cups)`,
      VEG_SERV = `Total red/orange vegetable (cups)` + `Dark-green vegetable (cups)`*0.5 + `Other starchy vegetable (cups)` + `Other vegetable (cups)`,
      WGRAIN_SERV = `Whole grain (oz)`,
      LEGUMES_SERV = `Soy products protein foods (oz)`+`Legumes protein foods (oz)`,
      NUTS_SERV = `Nuts and seeds protein foods (oz)`,
      FISH_SERV = `Seafood (oz)`,
      REDPROC_MEAT_SERV = (`Cured meat protein foods (oz)`/1.5) + ((`Meat from beef, pork, veal, lamb, and game protein foods (oz)`+`Meat from organ meat protein foods (oz)`)/4),
      MONSATFAT_SERV = case_when(
        `Total saturated fatty acids (g)` == 0 ~ 0, 
        TRUE ~ `Total monounsaturated fatty acids (g)`/`Total saturated fatty acids (g)`
      ),
      ALCOHOL_SERV = `Alcohol (g)`
    ) 
  
  ##Create variables and functions needed for MED
  median_healthy = function(actual){
    median_score = median(actual)
    case_when(
      actual < median_score ~ 0,
      actual >= median_score ~ 1
    )
  }
  
  median_unhealthy = function(actual){
    median_score = median(actual)
    case_when(
      actual < median_score ~ 1,
      actual >= median_score ~ 0
    )
  }
  
  COHORT %>%
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
        ALCOHOL_SERV <=25 & ALCOHOL_SERV >= 10 ~ 1, 
        TRUE ~ 0),
      
      MED_ALL = MED_FRT+MED_VEG+MED_WGRAIN+MED_LEGUMES+MED_NUTS+MED_FISH+MED_REDPROC_MEAT+MED_MONSATFAT+MED_ALCOHOL,
      MED_NOETOH = MED_FRT+MED_VEG+MED_WGRAIN+MED_LEGUMES+MED_NUTS+MED_FISH+MED_REDPROC_MEAT+MED_MONSATFAT
    )%>%
    dplyr::select(RESPONDENTID, MED_ALL, MED_NOETOH, MED_FRT, MED_VEG, MED_WGRAIN, MED_LEGUMES, MED_NUTS,
                  MED_FISH, MED_REDPROC_MEAT, MED_MONSATFAT, MED_ALCOHOL,
                  
                  FRT_FRTJ_SERV, VEG_SERV, WGRAIN_SERV, LEGUMES_SERV, NUTS_SERV, FISH_SERV, REDPROC_MEAT_SERV, MONSATFAT_SERV, ALCOHOL_SERV)
}


#' DASH_DHQ3
#'
#' Calculate the DASH for the DHQ3 data within 1 step  
#' @import dplyr
#' @import readr
#' @import haven
#' @param DATA_PATH The file path for the data. The data is Detailed analysis file, ending with detail.csv
#' @return The DASH and its component scores
#' @examples
#' DATA_PATH = "/Users/james/Desktop/data/detail.csv"
#' DASH_DHQ3(DATA_PATH)
#' @export

DASH_DHQ3 = function(DATA_PATH){
  
  if (is.character(DATA_PATH) == TRUE){
    COHORT = read_csv(DATA_PATH)
  } else {
    COHORT = DATA_PATH
  }
  
  if (!("Food ID" %in% colnames(COHORT)) ){
    stop("Please use individual-level data for this function. Individual-level data should be like detail.csv")
  }
  
  LOWF_MILK = c(5.3, 5.4, 6.3, 6.4, 10.3, 10.4)
  LOWF_CHEESE=c(50.2)
  LOWF_CREAM=c(64.2, 75.2, 80.1)
  COFFEE_TEA = c(16.1, 17.1, 64.1, 64.2, 1081.1, 1123.1, 1123.2, 1130.2, 1130.5)  
  DRINK = c(10.6, 10.9, 11.1, 11.2, 1140.1, 1140.2, 1144.1, 1150.1, 1152.1) 
  SSB = c(COFFEE_TEA, DRINK)
  
  COHORT = COHORT %>%
    dplyr::mutate(
      LOWF_MILK_SERV = case_when(
        `Food ID` %in% LOWF_MILK ~ `Milk (cups)`,
        TRUE ~ 0
      ),
      LOWF_CHEESECREAM_SERV = case_when(
        `Food ID` %in% LOWF_CHEESE ~ `Total dairy (cups)`*4,
        `Food ID` %in% LOWF_CREAM ~ `Total dairy (cups)`*2,
        TRUE ~ 0
      ),
      ADDED_SUGAR_SSB_SERV = case_when(
        `Food ID` %in% SSB ~ `*Added sugars (g)`,
        TRUE ~ 0
      )
    ) %>%
    dplyr::group_by(`Respondent ID`) %>%
    dplyr::summarize(
      KCAL = sum(`Energy (kcal)`),
      FRT_FRTJ_SERV = sum(`Total fruit (cups)`),
      VEG_SERV = sum(`Total red/orange vegetable (cups)` + `Dark-green vegetable (cups)`*0.5 + `Other starchy vegetable (cups)` + `Other vegetable (cups)`),
      NUTSLEG_SERV = sum(`Nuts, seeds, soy, and legumes (oz)`),
      WGRAIN_SERV = sum(`Whole grain (oz)`),
      LOWF_DAIRY_SERV = sum(LOWF_MILK_SERV+LOWF_CHEESECREAM_SERV+`Yogurt (cups)`),
      SODIUM_SERV = sum(`Sodium (mg)`/(KCAL/1000)),
      REDPROC_MEAT_SERV = sum((`Cured meat protein foods (oz)`/1.5) + ((`Meat from beef, pork, veal, lamb, and game protein foods (oz)`+`Meat from organ meat protein foods (oz)`)/4)),
      SSB_FRTJ_SERV = sum((ADDED_SUGAR_SSB_SERV / 26))
    )
  
  
  ##Create variables needed for DASH calculation
  ##Create variables and functions needed for DASH calculation
  quintile_healthy = function(actual){
    quintile = quantile(actual, probs=seq(0, 1, by=0.2))
    case_when(
      actual <= quintile[6] & actual >= quintile[5] ~ 5,
      actual < quintile[5] & actual >= quintile[4] ~ 4,
      actual < quintile[4] & actual >= quintile[3] ~ 3,
      actual < quintile[3] & actual >= quintile[2] ~ 2,
      actual < quintile[2] & actual >= quintile[1] ~ 1
    )
  }
  
  quintile_unhealthy = function(actual){
    quintile = quantile(actual, probs=seq(0, 1, by=0.2))
    case_when(
      actual <= quintile[6] & actual >= quintile[5] ~ 1,
      actual < quintile[5] & actual >= quintile[4] ~ 2,
      actual < quintile[4] & actual >= quintile[3] ~ 3,
      actual < quintile[3] & actual >= quintile[2] ~ 4,
      actual < quintile[2] & actual >= quintile[1] ~ 5
    )
  }
  
  print("Reminder: this DASH index uses quintiles to rank participants' food/drink serving sizes and then calculate DASH component scores, which may generate results that are specific to your study population but not comparable to other populations.")
  
  
  ##DASH calculation
  COHORT %>%
    dplyr::mutate(
      DASH_FRT = quintile_healthy(FRT_FRTJ_SERV),
      DASH_VEG = quintile_healthy(VEG_SERV),
      DASH_NUTSLEG = quintile_healthy(NUTSLEG_SERV),
      DASH_WGRAIN = quintile_healthy(WGRAIN_SERV),
      DASH_LOWF_DAIRY = quintile_healthy(LOWF_DAIRY_SERV),
      DASH_SODIUM = quintile_unhealthy(SODIUM_SERV),
      DASH_REDPROC_MEAT = quintile_unhealthy(REDPROC_MEAT_SERV),
      DASH_SSB_FRTJ = quintile_unhealthy(SSB_FRTJ_SERV),
      DASH_ALL = DASH_FRT+DASH_VEG+DASH_NUTSLEG+DASH_WGRAIN+DASH_LOWF_DAIRY+
        DASH_SODIUM+DASH_REDPROC_MEAT+DASH_SSB_FRTJ
    )%>%
    dplyr::select(`Respondent ID`, DASH_ALL, DASH_FRT, DASH_VEG, DASH_NUTSLEG, DASH_WGRAIN, DASH_LOWF_DAIRY,
                  DASH_SODIUM, DASH_REDPROC_MEAT, DASH_SSB_FRTJ,
                  
                  KCAL, FRT_FRTJ_SERV, VEG_SERV, NUTSLEG_SERV, WGRAIN_SERV, LOWF_DAIRY_SERV, SODIUM_SERV, REDPROC_MEAT_SERV, SSB_FRTJ_SERV)
}



