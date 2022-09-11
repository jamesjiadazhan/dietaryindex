#' AHEI Calculation
#'
#' Calculate the AHEI dietary index, Alternative Healthy Eating Index, using given the serving sizes of foods and nutrients consumed per 1 day
#' @import dplyr
#' @import readr
#' @import haven
#' @param SERV_DATA The raw data file that includes all the serving sizes of foods and nutrients
#' @param RESPONDENTID The unique participant ID for each participant
<<<<<<< HEAD
#' @param GENDER The gender of the participant. 2 is female and 1 is male.
=======
<<<<<<< HEAD
#' @param GENDER The gender of the participant. 2 is female and 1 is male.
=======
#' @param GENDER The gender of the participant. 0 is female and 1 is male.
>>>>>>> jamesjiadazhan-patch6
>>>>>>> jamesjiadazhan-patch6
#' @param VEG_SERV The serving size of All vegetable except potatoes and legume, unit=servings/day (0.5 c of vege; 1 cup of green leafy (1 cup = 236.59 g)
#' @param FRT_SERV The serving size of All whole fruits and no fruit juice, unit=servings/day (0.5 c of berries; 1 cup=236.59 g; 1 med fruit (1 cup = 236.59 g)
#' @param WGRAIN_SERV The serving size of whole grains, unit=grams/day
#' @param NUTSLEG_SERV The serving size of Nuts, legumes, and vegetable protein (e.g., tofu), unit=servings/day = 1 srv=1oz (28.35 g) of nuts and legume or 1 TBLSP peanut butter (15 mL), 1 cup legume = 4 oz
#' @param N3FAT_SERV The serving size of omega 3 fatty acid, unit=mg/day ( oz. = 28.35 g)
#' @param PUFA_SERV The serving size of PUFA, unit=\% of energy 
#' @param SSB_FRTJ_SERV The serving size of sugar-sweetened beverages and non-100\% fruit juice, unit=servings/day = 1 ser= 8oz (1 oz. = 28.35 g)
#' @param REDPROC_MEAT_SERV The serving size of red and processed meats, including Beef, pork, lamb, goat, veal, sausages, bacon, salami, ham, hot dog, deli meat, unit=servings/day; 1 srv= 4 oz. unprocessed meat; 1.5 oz. processed meat (1 oz. = 28.35 g)
#' @param TRANS_SERV The serving size of trans fat, unit=\% of energy
#' @param SODIUM_SERV The serving size of sodium, unit=mg/day 
#' @param ALCOHOL_SERV The serving size of alcohol, including Wine, beer, "light" beer, liquor, unit=drink/day (12 oz beer; 5 oz wine; 1.5 oz spirits) 1 oz = 28.35 g
#' @return The AHEI index/score, AHEI
#' @examples
#' AHEI(SERV_DATA, SERV_DATA$RESPONDENTID, SERV_DATA$GENDER, SERV_DATA$VEG_SERV, SERV_DATA$FRT_SERV, SERV_DATA$WGRAIN_SERV, SERV_DATA$NUTSLEG_SERV, SERV_DATA$N3FAT_SERV, SERV_DATA$PUFA_SERV, SERV_DATA$SSB_FRTJ_SERV, SERV_DATA$REDPROC_MEAT_SERV, SERV_DATA$TRANS_SERV,SODIUM_SERV, SERV_DATA$ALCOHOL_SERV)
#' @export


#Score calculation for AHEI
AHEI = function(SERV_DATA, RESPONDENTID, GENDER, VEG_SERV, FRT_SERV, WGRAIN_SERV, NUTSLEG_SERV, N3FAT_SERV, PUFA_SERV,
                SSB_FRTJ_SERV, REDPROC_MEAT_SERV, TRANS_SERV, SODIUM_SERV, ALCOHOL_SERV){
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
  
  SODIUM_DECILE = quantile(SERV_DATA$SODIUM_SERV, probs=seq(0, 1, by=0.1))
  
  ##AHEI calculation
  SERV_DATA %>%
    dplyr::mutate(
      RESPONDENTID = RESPONDENTID,
      GENDER = GENDER,
      
      AHEI_VEG = SCORE_HEALTHY(VEG_SERV, AHEI_MIN_VEG_SERV, AHEI_MAX_VEG_SERV, AHEI_MIN, AHEI_MAX),
      AHEI_FRT = SCORE_HEALTHY(FRT_SERV, AHEI_MIN_FRT_SERV, AHEI_MAX_FRT_SERV, AHEI_MIN, AHEI_MAX),
      
      AHEI_WGRAIN = case_when(
<<<<<<< HEAD
=======
<<<<<<< HEAD
>>>>>>> jamesjiadazhan-patch6
        #GENDER = 2 is female
        GENDER == 2 & WGRAIN_SERV >= AHEI_MAX_WGRAIN_F_SERV ~ AHEI_MAX,
        GENDER == 2 & WGRAIN_SERV <= AHEI_MIN_WGRAIN_F_SERV ~ AHEI_MIN,
        GENDER == 2 & WGRAIN_SERV > AHEI_MIN_WGRAIN_F_SERV & WGRAIN_SERV < AHEI_MAX_WGRAIN_F_SERV ~ AHEI_MIN+(WGRAIN_SERV-AHEI_MIN_WGRAIN_F_SERV)*AHEI_MAX/(AHEI_MAX_WGRAIN_F_SERV-AHEI_MIN_WGRAIN_F_SERV),
<<<<<<< HEAD
=======
=======
        #GENDER = 0 is female
        GENDER == 0 & WGRAIN_SERV >= AHEI_MAX_WGRAIN_F_SERV ~ AHEI_MAX,
        GENDER == 0 & WGRAIN_SERV <= AHEI_MIN_WGRAIN_F_SERV ~ AHEI_MIN,
        GENDER == 0 & WGRAIN_SERV > AHEI_MIN_WGRAIN_F_SERV & WGRAIN_SERV < AHEI_MAX_WGRAIN_F_SERV ~ AHEI_MIN+(WGRAIN_SERV-AHEI_MIN_WGRAIN_F_SERV)*AHEI_MAX/(AHEI_MAX_WGRAIN_F_SERV-AHEI_MIN_WGRAIN_F_SERV),
>>>>>>> jamesjiadazhan-patch6
>>>>>>> jamesjiadazhan-patch6
        
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
        SODIUM_SERV <= SODIUM_DECILE[11] & SODIUM_SERV >= SODIUM_DECILE[10] ~ 0,
        SODIUM_SERV < SODIUM_DECILE[10] & SODIUM_SERV >= SODIUM_DECILE[9] ~ 10/9,
        SODIUM_SERV < SODIUM_DECILE[9] & SODIUM_SERV >= SODIUM_DECILE[8] ~ 20/9,
        SODIUM_SERV < SODIUM_DECILE[8] & SODIUM_SERV >= SODIUM_DECILE[7] ~ 30/9,
        SODIUM_SERV < SODIUM_DECILE[7] & SODIUM_SERV >= SODIUM_DECILE[6] ~ 40/9,
        SODIUM_SERV < SODIUM_DECILE[6] & SODIUM_SERV >= SODIUM_DECILE[5] ~ 50/9,
        SODIUM_SERV < SODIUM_DECILE[5] & SODIUM_SERV >= SODIUM_DECILE[4] ~ 60/9,
        SODIUM_SERV < SODIUM_DECILE[4] & SODIUM_SERV >= SODIUM_DECILE[3] ~ 70/9,
        SODIUM_SERV < SODIUM_DECILE[3] & SODIUM_SERV >= SODIUM_DECILE[2] ~ 80/9,
        SODIUM_SERV < SODIUM_DECILE[2] & SODIUM_SERV >= SODIUM_DECILE[1] ~ 10
      ),
      AHEI_ALCOHOL = case_when(
<<<<<<< HEAD
=======
<<<<<<< HEAD
>>>>>>> jamesjiadazhan-patch6
        ##GENDER = 2 is female
        GENDER == 2  & ALCOHOL_SERV >= 2.5 ~ 0,
        GENDER == 2  & ALCOHOL_SERV < 2.5 & ALCOHOL_SERV > 1.5 ~ 0 + (ALCOHOL_SERV-2.5)*10/(1.5-2.5),
        GENDER == 2  & ALCOHOL_SERV <= 1.5 & ALCOHOL_SERV >= 0.5 ~ 10,
        GENDER == 2  & ALCOHOL_SERV < 0.5 & ALCOHOL_SERV > 0.125 ~ 0 + (ALCOHOL_SERV-0)*10/(0.5-0),
        GENDER == 2  & ALCOHOL_SERV <= 0.125 ~ 2.5,
<<<<<<< HEAD

=======
=======
        ##GENDER = 0 is female
        GENDER == 0  & ALCOHOL_SERV >= 2.5 ~ 0,
        GENDER == 0  & ALCOHOL_SERV < 2.5 & ALCOHOL_SERV > 1.5 ~ 0 + (ALCOHOL_SERV-2.5)*10/(1.5-2.5),
        GENDER == 0  & ALCOHOL_SERV <= 1.5 & ALCOHOL_SERV >= 0.5 ~ 10,
        GENDER == 0  & ALCOHOL_SERV < 0.5 & ALCOHOL_SERV > 0.125 ~ 0 + (ALCOHOL_SERV-0)*10/(0.5-0),
        GENDER == 0  & ALCOHOL_SERV <= 0.125 ~ 2.5,
>>>>>>> jamesjiadazhan-patch6
        
>>>>>>> jamesjiadazhan-patch6
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
#' @param FRT_FRTJ_SERV The serving size of fruits and 100\% fruit juice, unit=servings/day (0.5 c of berries; 1 cup=236.59 g; 1 med fruit (1 cup = 236.59 g); 1 cup fruit juice
#' @param VEG_SERV The serving size of All vegetable except potatoes and legume, unit=servings/day (0.5 c of vege; 1 cup of green leafy (1 cup = 236.59 g)
#' @param NUTSLEG_SERV The serving size of Nuts, legumes, and vegetable protein (e.g., tofu), unit=servings/day = 1 srv=1oz (28.35 g) of nuts or 1 TBLSP peanut butter (15 mL), 1 cup legume = 4 oz
#' @param WGRAIN_SERV The serving size of whole grains, unit=1oz
#' @param LOWF_DAIRY_SERV The serving size of low fat dairy, including 2\% or less fat milk + yogurt + low-fat ice cream and frozen yogurt + low-fat cheese, unit=servings/day = 1 glass milk + 1 cup yogurt + 1/2 cup ice cream/frozen yogurt + 1 slice cheese
#' @param SODIUM_SERV The serving size of sodium, unit=mg/day
#' @param REDPROC_MEAT_SERV The serving size of red and processed meats, including Beef, pork, lamb, goat, veal, sausages, bacon, salami, ham, hot dog, deli meat, unit=servings/day; 1 srv= 4 oz. unprocessed meat; 1.5 oz. processed meat (1 oz. = 28.35 g)
#' @param SSB_FRTJ_SERV The serving size of sugar-sweetened beverages and non-100\% fruit juice, unit=servings/day = 1 ser= 8oz (1 oz. = 28.35 g)
#' @return The DASH index/score
#' @examples
#' DASH(SERV_DATA, SERV_DATA$RESPONDENTID, SERV_DATA$FRT_FRTJ_SERV, SERV_DATA$VEG_SERV, SERV_DATA$NUTSLEG_SERV, SERV_DATA$WGRAIN_SERV, SERV_DATA$LOWF_DAIRY_SERV, SERV_DATA$SODIUM_SERV, SERV_DATA$REDPROC_MEAT_SERV, SERV_DATA$SSB_FRTJ_SERV)
#' @export

#Score calculation for DASH
DASH = function(SERV_DATA, RESPONDENTID, FRT_FRTJ_SERV, VEG_SERV, NUTSLEG_SERV, WGRAIN_SERV, LOWF_DAIRY_SERV,
                SODIUM_SERV, REDPROC_MEAT_SERV, SSB_FRTJ_SERV){
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

  ##DASH calculation
  SERV_DATA %>%
    dplyr::mutate(
      RESPONDENTID = RESPONDENTID,
      
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
#' @param VEG_SERV The serving size of All vegetable except potatoes and legume, unit=servings/day (0.5 c of vege; 1 cup of green leafy (1 cup = 236.59 g)
#' @param FRT_FRTJ_SERV The serving size of All whole fruits + 100\% juice,  unit=servings/day (0.5 c of berries; 1 cup=236.59 g; 1 med fruit (1 cup = 236.59 g); 1 cup fruit juice
#' @param NUTSLEG_SERV The serving size of Nuts, legumes, and vegetable protein (e.g., tofu), unit=servings/day = 1 srv=1oz (28.35 g) of nuts or 1 TBLSP peanut butter (15 mL), 1 cup legume = 4 oz
#' @param LOWF_DAIRY_SERV The serving size of low fat dairy, including 2\% or less fat milk + yogurt + low-fat ice cream and frozen yogurt + low-fat cheese, unit=servings/day = 1 glass milk + 1 cup yogurt + 1/2 cup ice cream/frozen yogurt + 1 slice cheese
#' @param WGRAIN_SERV The serving size of whole grains, unit=1oz
#' @param ALLMEAT_SERV The serving size of all meat consumption, including meat, fish, and poultry, unit=servings/day = 1oz
#' @param REDPROC_MEAT_SERV The serving size of red and processed meats, including Beef, pork, lamb, goat, veal, sausages, bacon, salami, ham, hot dog, deli meat, servings/day; 1 srv= 4 oz. unprocessed meat; 1.5 oz. processed meat (1 oz. = 28.35 g)
#' @param FATOIL_SERV The serving size of discretionary fats and oils, including added Plant oil + Animal fat, unit=servings/day = 1tbsp = 14 g
#' @param ADDEDSUGAR_SERV The serving size of added sugar, unit=\% of total energy, 1 tsp = 4g, 1g = 4kcal
#' @param SODIUM_SERV The serving size of sodium, unit=mg/day
#' @return The DASHI index/score
#' @examples
#' DASHI(SERV_DATA, SERV_DATA$RESPONDENTID, SERV_DATA$VEG_SERV, SERV_DATA$FRT_FRTJ_SERV, SERV_DATA$NUTSLEG_SERV, SERV_DATA$LOWF_DAIRY_SERV, SERV_DATA$WGRAIN_SERV, SERV_DATA$ALLMEAT_SERV, SERV_DATA$REDPROC_MEAT_SERV, SERV_DATA$FATOIL_SERV, SERV_DATA$ADDEDSUGAR_SERV, SERV_DATA$SODIUM_SERV)
#' @export

#Score calculation for DASHI
DASHI = function(SERV_DATA, RESPONDENTID, VEG_SERV, FRT_FRTJ_SERV, NUTSLEG_SERV, LOWF_DAIRY_SERV, WGRAIN_SERV,
                 ALLMEAT_SERV, REDPROC_MEAT_SERV, FATOIL_SERV, ADDEDSUGAR_SERV, SODIUM_SERV){
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

  ##DASHI calculation
  SERV_DATA %>%
    dplyr::mutate(
      RESPONDENTID = RESPONDENTID,
      
      DASHI_VEG = DASHI_HEALTHY(VEG_SERV, DASHI_MIN_VEG_SERV, DASHI_MAX_VEG_SERV),
      DASHI_FRT = DASHI_HEALTHY(FRT_FRTJ_SERV, DASHI_MIN_FRT_FRTJ_SERV, DASHI_MAX_FRT_FRTJ_SERV),
      DASHI_NUTSLEG = DASHI_HEALTHY(NUTSLEG_SERV, DASHI_MIN_NUTSLEG_SERV, DASHI_MAX_NUTSLEG_SERV),
      DASHI_LOWFATDAIRY = DASHI_HEALTHY(LOWF_DAIRY_SERV, DASHI_MIN_LOWF_DAIRY_SERV, DASHI_MAX_LOWF_DAIRY_SERV),
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
#' @param FRT_FRTJ_SERV The serving size of All fruits and 100\% fruit juices, unit=servings/day (0.5 c of berries; 1 cup=236.59 g; 1 med fruit (1 cup = 236.59 g); 1 cup fruit juice
#' @param VEG_SERV The serving size of All vegetables except potatoes and legumes, unit=0.5 c of vege; 1 cup of green leafy
#' @param WGRAIN_SERV The serving size of whole grains, including Whole-grain ready-to-eat cereals, cooked cereals, crackers, dark breads, brown rice, other grains, wheat germ, bran, popcorn, unit=1oz
#' @param LEGUMES_SERV The serving size of legumes, including Tofu, string beans, peas, beans, unit=*oz, 1 cup legume = 4 oz
#' @param NUTS_SERV The serving size of nuts, including Nuts, peanut butter, unit=1oz
#' @param FISH_SERV The serving size of all fish, including Fish and shrimp, breaded fish, unit=4oz
#' @param REDPROC_MEAT_SERV The serving size of red and processed meats, including Beef, pork, lamb, goat, veal, sausages, bacon, salami, ham, hot dog, deli meat, unit=serving (4 oz. unprocessed meat; 1.5 oz. processed meat)
#' @param MONSATFAT_SERV The serving size of the ratio of monounsaturated fat to saturated fat, unit=ratio
#' @param ALCOHOL_SERV The serving size of alcohol, unit=13g
#' @return The MED index/score
#' @examples
#' MED(SERV_DATA, SERV_DATA$RESPONDENTID, SERV_DATA$FRT_FRTJ_SERV, SERV_DATA$VEG_SERV, SERV_DATA$WGRAIN_SERV, SERV_DATA$LEGUMES_SERV, SERV_DATA$NUTS_SERV,FISH_SERV, SERV_DATA$REDPROC_MEAT_SERV, SERV_DATA$MONSATFAT_SERV, SERV_DATA$ALCOHOL_SERV)
#' @export

#Score calculation for MED
MED = function(SERV_DATA, RESPONDENTID, FRT_FRTJ_SERV, VEG_SERV, WGRAIN_SERV, LEGUMES_SERV, NUTS_SERV,
               FISH_SERV, REDPROC_MEAT_SERV, MONSATFAT_SERV, ALCOHOL_SERV){
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

  SERV_DATA %>%
    dplyr::mutate(
      RESPONDENTID = RESPONDENTID,
      
      MED_FRT = median_healthy(FRT_FRTJ_SERV),
      MED_VEG = median_healthy(VEG_SERV),
      MED_WGRAIN = median_healthy(WGRAIN_SERV),
      MED_LEGUMES = median_healthy(LEGUMES_SERV),
      MED_NUTS = median_healthy(NUTS_SERV),
      MED_FISH = median_healthy(FISH_SERV),
      MED_REDPROC_MEAT = median_unhealthy(REDPROC_MEAT_SERV),
      MED_MONSATFAT = median_healthy(MONSATFAT_SERV),
      MED_ALCOHOL = ifelse(ALCOHOL_SERV <=25 & ALCOHOL_SERV >= 10, 1, 0),

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
#' @param FRT_FRTJ_SERV The serving size of All fruits and 100\% fruit juices, unit=servings/day (0.5 c of berries; 1 cup=236.59 g; 1 med fruit (1 cup = 236.59 g); 1 cup fruit juice
#' @param VEG_SERV The serving size of All raw and cooked vegetables, unit=0.5 c of vege; 1 cup of green leafy
#' @param LEGUMES_SERV The serving size of legumes, including Dried beans, lentils, peas, soups (split pea), tofu, soymilk, unit=1oz (28.35 g) of nuts and legume or 1 TBLSP peanut butter (15 mL), 1 cup legume = 4 oz
#' @param WGRAIN_SERV The serving size of whole grains, including Whole wheat bread and flour, whole wheat pasta, brown rice, rusks,  whole grain breakfast cereals, couscous, semolina, unit=1 oz
#' @param FISH_SERV The serving size of all fish, including Fresh-water and sea-water fish; preserved fish such as salted fish, canned fish; shellfish (squid, prawns, mollusks), unit=serving (4 oz)
#' @param DAIRY_SERV The serving size of all dairy, including Milk, yogurt, cheese, custard, milk puddings, other milk products, unit=1 serving = 1 cup milk, 1 cup yogurt, 1 ½ ounces hard cheese (cheddar, mozzarella, Swiss, Parmesan), 1/3 cup shredded cheese, 2 ounces processed cheese (American), ½ cup ricotta cheese, 2 cups cottage cheese, 1 cup pudding made with milk, 1 cup frozen yogurt, 1 ½ cups ice cream
#' @param REDPROC_MEAT_SERV The serving size of red and processed meats, including Beef, pork, lamb, goat, veal, sausages, bacon, salami, ham, hot dog, deli meat, unit=serving (4 oz. unprocessed meat; 1.5 oz. processed meat)
#' @param NUTS_SERV The serving size of nuts, including Peanuts, almonds, sunflower seeds, cashews, walnuts, unit=1oz
#' @param MONSATFAT_SERV The serving size of the ratio of monounsaturated fat to saturated fat, unit=ratio
#' @param ALCOHOL_SERV The serving size of alcohol, including Wine, beer, "light" beer, liquor, unit=13g
#' @return The MEDI index/score
#' @examples
#' MED(SERV_DATA, SERV_DATA$RESPONDENTID, SERV_DATA$FRT_FRTJ_SERV, SERV_DATA$VEG_SERV, SERV_DATA$WGRAIN_SERV, SERV_DATA$LEGUMES_SERV, SERV_DATA$NUTS_SERV,FISH_SERV, SERV_DATA$REDPROC_MEAT_SERV, SERV_DATA$MONSATFAT_SERV, SERV_DATA$ALCOHOL_SERV)
#' @export

#Score calculation for MEDI
MEDI = function(SERV_DATA, RESPONDENTID, FRT_FRTJ_SERV, VEG_SERV, LEGUMES_SERV, WGRAIN_SERV, FISH_SERV, DAIRY_SERV, REDPROC_MEAT_SERV,
                NUTS_SERV, MONSATFAT_SERV, ALCOHOL_SERV){
  SERV_DATA %>%
    dplyr::mutate(
      RESPONDENTID = RESPONDENTID,
      
      MEDI_FRT = ifelse(FRT_FRTJ_SERV >=3, 1, 0),
      MEDI_VEG = ifelse(VEG_SERV >= 3, 1, 0),
      MEDI_LEGUMES = ifelse(LEGUMES_SERV*7 >= 1.5, 1, 0),
      MEDI_WGRAIN = ifelse(WGRAIN_SERV >= 3, 1, 0),
      MEDI_FISH = ifelse(FISH_SERV*7 >= 2, 1, 0),
      MEDI_DAIRY = ifelse(DAIRY_SERV >= 2, 1, 0),
      MEDI_REDPROC_MEAT = ifelse(REDPROC_MEAT_SERV*7 < 4.5, 1, 0),
      MEDI_NUTS = ifelse(NUTS_SERV*7 >= 2, 1, 0),
      MEDI_MONSATFAT = ifelse(MONSATFAT_SERV >= 1.6, 1, 0),
      MEDI_ALCOHOL = ifelse(ALCOHOL_SERV <=25 & ALCOHOL_SERV >= 10, 1, 0),

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
#' @param VEG_SERV The serving size of All vegetable except potatoes and legume, unit=servings/day (0.5 c of vege; 1 cup of green leafy (1 cup = 236.59 g)
#' @param FRT_SERV The serving size of All whole fruits and no fruit juice, unit=servings/day (0.5 c of berries; 1 cup=236.59 g; 1 med fruit (1 cup = 236.59 g)
#' @param WHITERED_RT_SERV The serving size of the ratio of white and red meats, White meat = poultry + all fish, red meat = pork + beef + lamb + all organ meats + processed meat, unit=servings/day; 1 srv= 4 oz. unprocessed meat; 1.5 oz. processed meat (1 oz. = 28.35 g)
#' @param FIBER_SERV The serving size of fibers, unit=grams/day
#' @param TRANS_SERV The serving size of trans fat, unit='\% of energy'
#' @param POLYSAT_RT The serving size of polyunsaturated/saturated fats, unit=ratio
#' @param CALCIUM_SERV The serving size of calcium, unit=mg/day
#' @param FOLATE_SERV The serving size of folate, unit=mcg/day
#' @param IRON_SERV The serving size of iron, unit=mg/day
#' @return The AHEIP index/score
#' @examples
#' AHEIP(SERV_DATA, SERV_DATA$RESPONDENTID, SERV_DATA$VEG_SERV, SERV_DATA$FRT_SERV, SERV_DATA$WHITERED_RT_SERV, SERV_DATA$FIBER_SERV, SERV_DATA$TRANS_SERV, SERV_DATA$POLYSAT_RT, SERV_DATA$CALCIUM_SERV, SERV_DATA$FOLATE_SERV, SERV_DATA$IRON_SERV)
#' @export


#Score calculation for AHEIP
AHEIP = function(SERV_DATA, RESPONDENTID, VEG_SERV, FRT_SERV, WHITERED_RT_SERV, FIBER_SERV, TRANS_SERV, POLYSAT_RT,
                 CALCIUM_SERV, FOLATE_SERV, IRON_SERV) {

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
#' @param TOTALKCAL The total calorie from all foods and drinks 
#' @param TOTALFRT_SERV The serving size of total fruits including fruit juice, unit= cup eq.
#' @param FRT_SERV The serving size of Citrus, Melons, Berries + Other Intact Fruits, unit= cup eq.
#' @param VEG_SERV The serving size of vegetables Total Vegetables + Legumes (Beans and Peas) in cup equivalents, unit= cup eq.
#' @param GREENNBEAN_SERV The serving size of Dark Green Vegetables + Legumes (Beans and Peas) in cup equivalents, unit= cup eq.
#' @param TOTALPRO_SERV The serving size of Total Meat, Poultry, and Seafood (including organ meats and cured meats) + Eggs + Nuts and Seeds + Soy + Legumes (Beans and Peas) in oz equivalents, unit=oz. eq., 1 cup legume = 4 oz
#' @param SEAPLANTPRO_SERV The serving size of Seafood (high in n-3) + Seafood (low in n-3) + Soy + Nuts and Seeds + Legumes (Beans and Peas) in oz equivalents, unit=oz. eq., 1 cup legume = 4 oz
#' @param WHOLEGRAIN_SERV The serving size of whole grains, unit=oz. eq.
#' @param DAIRY_SERV The serving size of all dairy, unit=cup eq.
#' @param FATTYACID_SERV The serving size of (Total Monounsaturated Fatty Acids + Total Polyunsaturated Fatty Acids)/Total Saturated Fatty Acids, unit=g
#' @param REFINEDGRAIN_SERV The serving size of refined grains, unit=oz. eq.
#' @param SODIUM_SERV The serving size of sodium, unit=g
#' @param ADDEDSUGAR_SERV The serving size of added sugar, unit=\% of total energy, 1 tsp = 4g, 1g = 4kcal
#' @param SATFAT_SERV The serving size of Total Saturated Fatty Acids, unit=\% of energy, 1g = 9 kcal
#' @return The HEI2015 index/score
#' @examples
#' HEI2015(SERV_DATA, SERV_DATA$RESPONDENTID, SERV_DATA$TOTALKCAL, SERV_DATA$TOTALFRT_SERV, SERV_DATA$FRT_SERV, SERV_DATA$VEG_SERV, SERV_DATA$GREENNBEAN_SERV, SERV_DATA$TOTALPRO_SERV,  SERV_DATA$SEAPLANTPRO_SERV, SERV_DATA$WHOLEGRAIN_SERV, SERV_DATA$DAIRY_SERV, SERV_DATA$FATTYACID_SERV, SERV_DATA$REFINEDGRAIN_SERV,  SERV_DATA$SODIUM_SERV, SERV_DATA$ADDEDSUGAR_SERV, SERV_DATA$SATFAT_SERV)
#' @export

#Score calculation for HEI2015
HEI2015 = function(SERV_DATA, RESPONDENTID, TOTALKCAL, TOTALFRT_SERV, FRT_SERV, VEG_SERV, GREENNBEAN_SERV, TOTALPRO_SERV,
                   SEAPLANTPRO_SERV, WHOLEGRAIN_SERV, DAIRY_SERV, FATTYACID_SERV, REFINEDGRAIN_SERV,
                   SODIUM_SERV, ADDEDSUGAR_SERV, SATFAT_SERV){
  
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
      TOTALKCAL = TOTALKCAL,
      
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
    if (SERV_DATA$TOTALKCAL[i] == 0){
      SERV_DATA$HEI2015_TOTALFRT[i] = 0
      SERV_DATA$HEI2015_FRT[i] = 0
      SERV_DATA$HEI2015_VEG[i] = 0
      SERV_DATA$HEI2015_GREENNBEAN[i] = 0
      SERV_DATA$HEI2015_TOTALPRO[i] = 0
      SERV_DATA$HEI2015_SEAPLANTPRO[i] = 0
      SERV_DATA$HEI2015_WHOLEGRAIN[i] = 0
      SERV_DATA$ HEI2015_DAIRY[i] = 0
      SERV_DATA$HEI2015_FATTYACID[i] = 0
      SERV_DATA$HEI2015_REFINEDGRAIN[i] = 0
      SERV_DATA$HEI2015_ADDEDSUGAR[i] = 0
      SERV_DATA$HEI2015_ALL[i] = 0
    }
  }
  
  SERV_DATA %>%
    dplyr::select(RESPONDENTID, TOTALKCAL, HEI2015_ALL, HEI2015_TOTALFRT, HEI2015_FRT, HEI2015_VEG, HEI2015_GREENNBEAN,
                  HEI2015_TOTALPRO, HEI2015_SEAPLANTPRO, HEI2015_WHOLEGRAIN, HEI2015_DAIRY,
                  HEI2015_FATTYACID, HEI2015_REFINEDGRAIN, HEI2015_SODIUM, HEI2015_ADDEDSUGAR,
                  HEI2015_SATFAT)
}

#' AHEI_BLOCK Calculation
#'
#' Calculate the AHEI dietary index for the Block FFQ (2013) per 1 day
#' @import dplyr
#' @import readr
#' @import haven
#' @param RAW_DATA The raw data file that includes results and raw responses of the dietary assessment
#' @return The AHEI and its component scores
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
      REDPROC_MEAT_SERV = (M_FRANK /1.5) + (M_MEAT/4),
      TRANS_SERV = ((DT_TRFAT * 9) / DT_KCAL)*100,
      ALCOHOL_SERV=A_BEV,
      SODIUM_SERV = DT_SODI
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
  
  SODIUM_DECILE = quantile(SERV_DATA$SODIUM_SERV, probs=seq(0, 1, by=0.1))
  
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
        SODIUM_SERV <= SODIUM_DECILE[11] & SODIUM_SERV >= SODIUM_DECILE[10] ~ 0,
        SODIUM_SERV < SODIUM_DECILE[10] & SODIUM_SERV >= SODIUM_DECILE[9] ~ 10/9,
        SODIUM_SERV < SODIUM_DECILE[9] & SODIUM_SERV >= SODIUM_DECILE[8] ~ 20/9,
        SODIUM_SERV < SODIUM_DECILE[8] & SODIUM_SERV >= SODIUM_DECILE[7] ~ 30/9,
        SODIUM_SERV < SODIUM_DECILE[7] & SODIUM_SERV >= SODIUM_DECILE[6] ~ 40/9,
        SODIUM_SERV < SODIUM_DECILE[6] & SODIUM_SERV >= SODIUM_DECILE[5] ~ 50/9,
        SODIUM_SERV < SODIUM_DECILE[5] & SODIUM_SERV >= SODIUM_DECILE[4] ~ 60/9,
        SODIUM_SERV < SODIUM_DECILE[4] & SODIUM_SERV >= SODIUM_DECILE[3] ~ 70/9,
        SODIUM_SERV < SODIUM_DECILE[3] & SODIUM_SERV >= SODIUM_DECILE[2] ~ 80/9,
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
                  AHEI_PUFA, AHEI_SSB_FRTJ, AHEI_REDPROC_MEAT, AHEI_TRANS, AHEI_SODIUM, AHEI_ALCOHOL)
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
      LOWF_MILK_SERV = ifelse(MILKTYPE==4,
                              foodfreq(MILKFREQ) * MILKQUAN, 
                              0),
      YOGURT_SERV = (foodfreq(YOGURTONLYFREQ) * 
                       foodport(YOGURTONLYQUAN, ref=YOGURT_PORT_DF)) +
        (foodfreq(BUTTERMILKFREQ) * 
           foodport(BUTTERMILKQUAN, ref=BUTTERMILK_PORT_DF)),
      LOWF_ICECREAMFROYO_SERV = ifelse(ICECREAMFROYOTYPE == 2, 
                                       foodfreq(ICECREAMFROYOFREQ) * 
                                         foodport(ICECREAMFROYOQUAN)*2,
                                       0),
      LOWF_CHEESE_SERV = ifelse(CHEESETYPE == 1, 
                                foodfreq(CHEESEFREQ) * CHEESEQUAN, 
                                0),
      LOWF_DAIRY_SERV = LOWF_MILK_SERV+YOGURT_SERV+LOWF_ICECREAMFROYO_SERV+LOWF_CHEESE_SERV,
      SODIUM_SERV = DT_SODI,
      REDPROC_MEAT_SERV = (M_FRANK /1.5) + (M_MEAT/4),
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
           DASH_SODIUM, DASH_REDPROC_MEAT, DASH_SSB_FRTJ)
  
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
      LOWF_MILK_SERV = ifelse(MILKTYPE==2 | MILKTYPE==3 | MILKTYPE==4,
                              foodfreq(MILKFREQ) * MILKQUAN, 
                              0),
      YOGURT_SERV = (foodfreq(YOGURTONLYFREQ) * foodport(YOGURTONLYQUAN, ref=YOGURT_PORT_DF)) +
        (foodfreq(BUTTERMILKFREQ) * foodport(BUTTERMILKQUAN, ref=BUTTERMILK_PORT_DF)),
      LOWF_ICECREAMFROYO_SERV = ifelse(ICECREAMFROYOTYPE == 2, 
                                       foodfreq(ICECREAMFROYOFREQ) * foodport(ICECREAMFROYOQUAN)*2,
                                       0),
      LOWF_CHEESE_SERV = ifelse(CHEESETYPE == 1, 
                                foodfreq(CHEESEFREQ) * CHEESEQUAN, 
                                0),
      LOWFATDAIRY_SERV = LOWF_MILK_SERV+YOGURT_SERV+LOWF_ICECREAMFROYO_SERV+LOWF_CHEESE_SERV,
      WGRAIN_SERV = G_WHL,
      ALLMEAT_SERV = M_MPF,
      REDPROC_MEAT_SERV = (M_FRANK /1.5) + (M_MEAT/4),
      FATOIL_SERV = (DFAT_OIL+DFAT_SOL)/14,
      ADDEDSUGAR_SERV = ((ADD_SUG*4*4) / DT_KCAL)*100,
      SODIUM_SERV = DT_SODI
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
           DASHI_ALLMEAT, DASHI_REDPROC_MEAT, DASHI_FATOIL, DASHI_ADDEDSUGAR, DASHI_SODIUM)
  
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
      REDPROC_MEAT_SERV = (M_FRANK/1.5) + (M_MEAT/4),
      MONSATFAT_SERV = DT_MFAT/DT_SFAT,
      ALCOHOL_SERV=A_BEV
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
      MED_ALCOHOL = ifelse(ALCOHOL_SERV <=25 & ALCOHOL_SERV >= 10, 1, 0),
      
      MED_ALL = MED_FRT+MED_VEG+MED_WGRAIN+MED_LEGUMES+MED_NUTS+MED_FISH+MED_REDPROC_MEAT+MED_MONSATFAT+MED_ALCOHOL,
      MED_NOETOH = MED_FRT+MED_VEG+MED_WGRAIN+MED_LEGUMES+MED_NUTS+MED_FISH+MED_REDPROC_MEAT+MED_MONSATFAT
    )%>%
    dplyr::select(RESPONDENTID, MED_ALL, MED_NOETOH, MED_FRT, MED_VEG, MED_WGRAIN, MED_LEGUMES, MED_NUTS,
           MED_FISH, MED_REDPROC_MEAT, MED_MONSATFAT, MED_ALCOHOL)
  
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
      REDPROC_MEAT_SERV = (M_FRANK/1.5) + (M_MEAT/4),
      NUTS_SERV = M_NUTSD,
      MONSATFAT_SERV = DT_MFAT/DT_SFAT,
      ALCOHOL_SERV=A_BEV
    ) 
  
  SERV_DATA %>%
    dplyr::mutate(
      MEDI_FRT = ifelse(FRT_FRTJ_SERV >=3, 1, 0),
      MEDI_VEG = ifelse(VEG_SERV >= 3, 1, 0),
      MEDI_LEGUMES = ifelse(LEGUMES_SERV*7 >= 1.5, 1, 0),
      MEDI_WGRAIN = ifelse(WGRAIN_SERV >= 3, 1, 0),
      MEDI_FISH = ifelse(FISH_SERV*7 >= 2, 1, 0),
      MEDI_DAIRY = ifelse(DAIRY_SERV >= 2, 1, 0),
      MEDI_REDPROC_MEAT = ifelse(REDPROC_MEAT_SERV*7 < 4.5, 1, 0),
      MEDI_NUTS = ifelse(NUTS_SERV*7 >= 2, 1, 0),
      MEDI_MONSATFAT = ifelse(MONSATFAT_SERV >= 1.6, 1, 0),
      MEDI_ALCOHOL = ifelse(ALCOHOL_SERV <=25 & ALCOHOL_SERV >= 10, 1, 0),
      
      MEDI_ALL = MEDI_FRT+MEDI_VEG+MEDI_LEGUMES+MEDI_WGRAIN+MEDI_FISH+MEDI_DAIRY+MEDI_REDPROC_MEAT+
        MEDI_NUTS+MEDI_MONSATFAT+MEDI_ALCOHOL,
      MEDI_NOETOH = MEDI_FRT+MEDI_VEG+MEDI_LEGUMES+MEDI_WGRAIN+MEDI_FISH+MEDI_DAIRY+MEDI_REDPROC_MEAT
      +MEDI_NUTS+MEDI_MONSATFAT
    )%>%
    dplyr::select(RESPONDENTID, MEDI_ALL, MEDI_NOETOH, MEDI_FRT, MEDI_VEG, MEDI_LEGUMES, MEDI_WGRAIN, MEDI_FISH,
           MEDI_DAIRY, MEDI_REDPROC_MEAT, MEDI_NUTS, MEDI_MONSATFAT, MEDI_ALCOHOL)
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
  
  #Match participant response food frequency to the standard food frequency response code
  SERV_DATA=RAW_DATA %>%
    dplyr::mutate(
      F_BERRIES = foodfreq(STRAWBERRIESFREQ)*foodport(STRAWBERRIESQUAN),
      F_WHOLE = F_SOLID - F_BERRIES + F_BERRIES*2,
      VEG_SERV = V_DPYEL + 0.5*V_DRKGR + V_OTHER + V_STARCY + V_TOMATO,
      FRT_SERV = F_WHOLE,
      WHITERED_RT_SERV = ((M_POULT+M_FISH_HI+M_FISH_LO)/4) /  ((M_FRANK /1.5) + (M_MEAT/4)),
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
           AHEIP_POLYSAT, AHEIP_CALCIUM, AHEIP_FOLATE, AHEIP_IRON)
  
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
      FATTYACID_SERV = ifelse(DT_SFAT == 0, 0, (DT_MFAT + DT_PFAT)/DT_SFAT),
      
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
           HEI2015_SATFAT)
  
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
  
  #Serving size calculation for DII
  COHORT = RAW_DATA %>%
    dplyr::mutate(
      ALCOHOL = A_BEV,
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
  
  COHORT2 = COHORT %>%
    dplyr::select(RESPONDENTID, VITB12, VITB6, BCAROTENE,CAFFEINE,CARB,CHOLES,KCAL,TOTALFAT,FIBER,FOLICACID,
           IRON,MG,MUFA,NIACIN,N3FAT,N6FAT,PROTEIN,PUFA,RIBOFLAVIN,SATFAT,SE,THIAMIN,TRANSFAT,
           VITA,VITC,VITD,VITE,ZN,TEA,ISOFLAVONES)
  
  COHORT1 = COHORT1 %>%
    tidyr::pivot_longer(-RESPONDENTID, names_to="Variable", values_to="Value")
  
  COHORT2 = COHORT2 %>%
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
  
  DII_ALL_df = COHORT1 %>%
    inner_join(DII_STD, by=c("Variable")) %>%
    dplyr::mutate(
      Z_SCORE = (Value - Global_mean)/SD,
      PERCENTILE = pnorm(Z_SCORE)*2 - 1,
      IND_DII_SCORE = PERCENTILE*Overall_inflammatory_score) %>%
    group_by(RESPONDENTID) %>%
    summarize(DII_ALL = sum(IND_DII_SCORE))
  
  DII_ALL_NOETOH_df = COHORT2 %>%
    inner_join(DII_STD, by=c("Variable")) %>%
    dplyr::mutate(
      Z_SCORE = (Value - Global_mean)/SD,
      PERCENTILE = pnorm(Z_SCORE)*2 - 1,
      IND_DII_SCORE = PERCENTILE*Overall_inflammatory_score) %>%
    group_by(RESPONDENTID) %>%
    summarize(DII_ALL_NOETOH = sum(IND_DII_SCORE))
  
  inner_join(DII_ALL_df, DII_ALL_NOETOH_df, by="RESPONDENTID")
  
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
      FATTYACID_SERV = ifelse(fatsaturated == 0, 0, (fatmono+fatpoly)/fatsaturated),
      
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
           HEI2015_SATFAT)
  
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
#' @return The AHEI and its component scores
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
      FATTYACID_SERV = ifelse(DR1TSFAT == 0, 0, (DR1TMFAT+DR1TPFAT)/DR1TSFAT),
      
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
           HEI2015_SATFAT)
}

#' AHEI_NHANES_FPED
#'
#' Calculate the AHEI for the NHANES_FPED data (after 2005) within 1 step  
#' @import dplyr
#' @import readr
#' @import haven
#' @param FPED_PATH The file path for the FPED data. The file name should be like: fped_dr1tot_1112.sas7bdat
#' @param NUTRIENT_PATH The file path for the NUTRIENT data. The file name should be like: DR1TOT_J.XPT
#' @param DEMO_PATH The file path for the DEMOGRAPHIC data. The file name should be like: DEMO_J.XPT
#' @return The AHEI and its component scores
#' @examples
#' FPED_PATH = "/Users/james/Desktop/data/fped_dr1tot_1112.sas7bdat"
#' NUTRIENT_PATH = "/Users/james/Desktop/data/DR1TOT_J.XPT"
#' DEMO_PATH = "/Users/james/Desktop/data/DEMO_J.XPT"
#' AHEI_NHANES_FPED(FPED_PATH, NUTRIENT_PATH, DEMO_PATH)
#' @export

AHEI_NHANES_FPED = function(FPED_PATH, NUTRIENT_PATH, DEMO_PATH){
  
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
  
  COHORT = COHORT %>%
    filter(DR1TKCAL > 0) %>%
    dplyr::mutate(
      VEG_SERV = DR1T_V_REDOR_TOTAL + 0.5*DR1T_V_DRKGR + DR1T_V_OTHER + DR1T_V_STARCHY_OTHER,
      FRT_SERV = DR1T_F_TOTAL - DR1T_F_JUICE,
      WGRAIN_SERV = DR1T_G_WHOLE/0.035274,
      NUTSLEG_SERV = (DR1T_V_LEGUMES*4) + DR1T_PF_NUTSDS + DR1T_PF_SOY,
      N3FAT_SERV = (DR1TP205 + DR1TP226)*1000,
      PUFA_SERV = (((DR1TPFAT - DR1TP205 - DR1TP226)*9)/ DR1TKCAL)*100,
      SSB_FRTJ_SERV = ((DR1T_ADD_SUGARS*4) / 240),
      REDPROC_MEAT_SERV = (DR1T_PF_CUREDMEAT /1.5) + ((DR1T_PF_MEAT+DR1T_PF_ORGAN+DR1T_PF_POULT)/4),
      TRANS_SERV = 0,
      ALCOHOL_SERV=DR1T_A_DRINKS,
      SODIUM_SERV = DR1TSODI
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
  
  SODIUM_DECILE = quantile(COHORT$SODIUM_SERV, probs=seq(0, 1, by=0.1))
  
  ##AHEI calculation
  COHORT %>%
    dplyr::mutate(
      AHEI_VEG = SCORE_HEALTHY(VEG_SERV, AHEI_MIN_VEG_SERV, AHEI_MAX_VEG_SERV, AHEI_MIN, AHEI_MAX),
      AHEI_FRT = SCORE_HEALTHY(FRT_SERV, AHEI_MIN_FRT_SERV, AHEI_MAX_FRT_SERV, AHEI_MIN, AHEI_MAX),
      AHEI_WGRAIN = case_when(
        RIAGENDR.x == 1 ~ SCORE_HEALTHY(WGRAIN_SERV, AHEI_MIN_WGRAIN_F_SERV, AHEI_MAX_WGRAIN_F_SERV, AHEI_MIN, AHEI_MAX),
        RIAGENDR.x == 2 ~ SCORE_HEALTHY(WGRAIN_SERV, AHEI_MIN_WGRAIN_M_SERV, AHEI_MAX_WGRAIN_M_SERV, AHEI_MIN, AHEI_MAX)
      ),
      AHEI_NUTSLEG = SCORE_HEALTHY(NUTSLEG_SERV, AHEI_MIN_NUTSLEG_SERV, AHEI_MAX_NUTSLEG_SERV, AHEI_MIN, AHEI_MAX),
      AHEI_N3FAT = SCORE_HEALTHY(N3FAT_SERV, AHEI_MIN_N3FAT_SERV, AHEI_MAX_N3FAT_SERV, AHEI_MIN, AHEI_MAX),
      AHEI_PUFA = SCORE_HEALTHY(PUFA_SERV, AHEI_MIN_PUFA_SERV, AHEI_MAX_PUFA_SERV, AHEI_MIN, AHEI_MAX),
      
      AHEI_SSB_FRTJ = SCORE_UNHEALTHY(SSB_FRTJ_SERV, AHEI_MIN_SSB_FRTJ_SERV, AHEI_MAX_SSB_FRTJ_SERV, AHEI_MIN, AHEI_MAX),
      AHEI_REDPROC_MEAT = SCORE_UNHEALTHY(REDPROC_MEAT_SERV, AHEI_MIN_REDPROC_MEAT_SERV, AHEI_MAX_REDPROC_MEAT_SERV, AHEI_MIN, AHEI_MAX),
      AHEI_TRANS = SCORE_UNHEALTHY(TRANS_SERV, AHEI_MIN_TRANS_SERV, AHEI_MAX_TRANS_SERV, AHEI_MIN, AHEI_MAX),
      
      AHEI_SODIUM = case_when(
        SODIUM_SERV <= SODIUM_DECILE[11] & SODIUM_SERV >= SODIUM_DECILE[10] ~ 0,
        SODIUM_SERV < SODIUM_DECILE[10] & SODIUM_SERV >= SODIUM_DECILE[9] ~ 10/9,
        SODIUM_SERV < SODIUM_DECILE[9] & SODIUM_SERV >= SODIUM_DECILE[8] ~ 20/9,
        SODIUM_SERV < SODIUM_DECILE[8] & SODIUM_SERV >= SODIUM_DECILE[7] ~ 30/9,
        SODIUM_SERV < SODIUM_DECILE[7] & SODIUM_SERV >= SODIUM_DECILE[6] ~ 40/9,
        SODIUM_SERV < SODIUM_DECILE[6] & SODIUM_SERV >= SODIUM_DECILE[5] ~ 50/9,
        SODIUM_SERV < SODIUM_DECILE[5] & SODIUM_SERV >= SODIUM_DECILE[4] ~ 60/9,
        SODIUM_SERV < SODIUM_DECILE[4] & SODIUM_SERV >= SODIUM_DECILE[3] ~ 70/9,
        SODIUM_SERV < SODIUM_DECILE[3] & SODIUM_SERV >= SODIUM_DECILE[2] ~ 80/9,
        SODIUM_SERV < SODIUM_DECILE[2] & SODIUM_SERV >= SODIUM_DECILE[1] ~ 10
      ),
      AHEI_ALCOHOL = 
        case_when(
          RIAGENDR.x == 2 & ALCOHOL_SERV >= 2.5 ~ 0,
          RIAGENDR.x == 2 & ALCOHOL_SERV < 2.5 & ALCOHOL_SERV > 1.5 ~ 0 + (ALCOHOL_SERV-2.5)*10/(1.5-2.5),
          RIAGENDR.x == 2 & ALCOHOL_SERV <= 1.5 & ALCOHOL_SERV >= 0.5 ~ 10,
          RIAGENDR.x == 2 & ALCOHOL_SERV < 0.5 ~  0 + (ALCOHOL_SERV-0)*10/(0.5-0),
          RIAGENDR.x == 2 & ALCOHOL_SERV <= 0.125 ~ 2.5,
          RIAGENDR.x == 1 & ALCOHOL_SERV >= 3.5 ~ 0,
          RIAGENDR.x == 1 & ALCOHOL_SERV < 3.5 & ALCOHOL_SERV > 2 ~ 0 + (ALCOHOL_SERV-2.5)*10/(1.5-2.5),
          RIAGENDR.x == 1 & ALCOHOL_SERV <= 2 & ALCOHOL_SERV >= 0.5 ~ 10,
          RIAGENDR.x == 1 & ALCOHOL_SERV < 0.5 ~ (ALCOHOL_SERV-0)*10/(0.5-0),
          RIAGENDR.x == 1 & ALCOHOL_SERV <= 0.125 ~ 2.5
        )
      ,
      AHEI_ALL = AHEI_VEG + AHEI_FRT + AHEI_WGRAIN + AHEI_NUTSLEG + AHEI_N3FAT +
        AHEI_PUFA + AHEI_SSB_FRTJ + AHEI_REDPROC_MEAT + AHEI_TRANS + AHEI_SODIUM + AHEI_ALCOHOL,
      
      AHEI_NOETOH = AHEI_VEG + AHEI_FRT + AHEI_WGRAIN + AHEI_NUTSLEG + AHEI_N3FAT +
        AHEI_PUFA + AHEI_SSB_FRTJ + AHEI_REDPROC_MEAT + AHEI_TRANS + AHEI_SODIUM
    ) %>%
    dplyr::select(SEQN, AHEI_ALL, AHEI_NOETOH, AHEI_VEG, AHEI_FRT, AHEI_WGRAIN, AHEI_NUTSLEG, AHEI_N3FAT,
           AHEI_PUFA, AHEI_SSB_FRTJ, AHEI_REDPROC_MEAT, AHEI_TRANS, AHEI_SODIUM, AHEI_ALCOHOL)
}

#' DASH_NHANES_FPED
#'
#' Calculate the DASH for the NHANES_FPED data (after 2005) within 1 step  
#' @import dplyr
#' @import readr
#' @import haven
#' @param FPED_PATH The file path for the FPED data. The file name should be like: fpre_dr1tot_1718.sas7bdat
#' @param NUTRIENT_PATH The file path for the NUTRIENT data. The file name should be like: DR1TOT_J.XPT
#' @param DEMO_PATH The file path for the DEMOGRAPHIC data. The file name should be like: DEMO_J.XPT
#' @param DBQ_PATH The file path for the Diet Behavior & Nutrition data. The file name should be like: DBQ_J.XPT
#' @return The DASH and its component scores
#' @examples
#' FPED_PATH = "/Users/james/Desktop/data/fpre_dr1tot_1718.sas7bdat"
#' NUTRIENT_PATH = "/Users/james/Desktop/data/DR1TOT_J.XPT"
#' DEMO_PATH = "/Users/james/Desktop/data/DEMO_J.XPT"
#' DBQ_PATH = "/Users/james/Desktop/data/DBQ_J.XPT"
#' DASH_NHANES_FPED(FPED_PATH, NUTRIENT_PATH, DEMO_PATH, DBQ_PATH)
#' @export


DASH_NHANES_FPED = function(FPED_PATH, NUTRIENT_PATH, DEMO_PATH, DBQ_PATH){

  if (is.character(FPED_PATH) == TRUE){
    FPED = read_sas(FPED_PATH)
  } else {
    FPED = FPED_PATH
  }
<<<<<<< HEAD

=======
<<<<<<< HEAD
  
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
  
=======
  
>>>>>>> jamesjiadazhan-patch6
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
  
<<<<<<< HEAD
=======
>>>>>>> jamesjiadazhan-patch6
>>>>>>> jamesjiadazhan-patch6
  if (is.character(DBQ_PATH) == TRUE){
    DBQ = read_xpt(DBQ_PATH)
  } else {
    DBQ = DBQ_PATH
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
    left_join(FPED, by = c("SEQN" = "SEQN")) %>%
    left_join(DBQ, by = c("SEQN" = "SEQN"))
  
  
  #Match participant response food frequency to the standard food frequency response code
  COHORT = COHORT %>%
    filter(DR1TKCAL > 0) %>%
    dplyr::mutate(
      FRT_FRTJ_SERV = DR1T_F_TOTAL,
      VEG_SERV = DR1T_V_REDOR_TOTAL + 0.5*DR1T_V_DRKGR + DR1T_V_OTHER + DR1T_V_STARCHY_OTHER,
      NUTSLEG_SERV = (DR1T_V_LEGUMES*4) + DR1T_PF_NUTSDS + DR1T_PF_SOY,
      WGRAIN_SERV = DR1T_G_WHOLE,
      LOWF_DAIRY_SERV = case_when(
        DBQ223C==12 | DBQ223D==13 ~ DR1T_D_MILK + DR1T_D_YOGURT + (2/40.2)*DR1T_D_CHEESE,
        TRUE ~ DR1T_D_YOGURT + (2/40.2)*DR1T_D_CHEESE
      ),
      SODIUM_SERV = DR1TSODI,
      REDPROC_MEAT_SERV = (DR1T_PF_CUREDMEAT /1.5) + ((DR1T_PF_MEAT+DR1T_PF_ORGAN+DR1T_PF_POULT)/4),
      SSB_FRTJ_SERV = ((DR1T_ADD_SUGARS*4) / 240)
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
           DASH_SODIUM, DASH_REDPROC_MEAT, DASH_SSB_FRTJ)
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
      ALCOHOL_SERV = DR1T_A_DRINKS
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
      MED_ALCOHOL = ifelse(ALCOHOL_SERV <=25 & ALCOHOL_SERV >= 10, 1, 0),
      
      MED_ALL = MED_FRT+MED_VEG+MED_WGRAIN+MED_LEGUMES+MED_NUTS+MED_FISH+MED_REDPROC_MEAT+MED_MONSATFAT+MED_ALCOHOL,
      MED_NOETOH = MED_FRT+MED_VEG+MED_WGRAIN+MED_LEGUMES+MED_NUTS+MED_FISH+MED_REDPROC_MEAT+MED_MONSATFAT
    )%>%
    dplyr::select(SEQN, MED_ALL, MED_NOETOH, MED_FRT, MED_VEG, MED_WGRAIN, MED_LEGUMES, MED_NUTS,
           MED_FISH, MED_REDPROC_MEAT, MED_MONSATFAT, MED_ALCOHOL)
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
      ZN = DR1TZINC) 
  
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
  
  DII_STD = data.frame(Variable, Overall_inflammatory_score, Global_mean, SD)
  
  #Score calculation for DII  
  
  COHORT %>%
    inner_join(DII_STD, by=c("Variable")) %>%
    dplyr::mutate(
      Z_SCORE = (Value - Global_mean)/SD,
      PERCENTILE = pnorm(Z_SCORE)*2 - 1,
      IND_DII_SCORE = PERCENTILE*Overall_inflammatory_score) %>%
    group_by(SEQN)%>%
    summarize(DII_ALL = sum(IND_DII_SCORE))
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
      FATTYACID_SERV = ifelse(SFAT == 0, 0, (MFAT+PFAT)/SFAT),
      
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
           HEI2015_SATFAT)
}


#' AHEI_F_ASA24
#'
#' Calculate the AHEI (female only) for the ASA24 data within 1 step  
#' @import dplyr
#' @import readr
#' @import haven
#' @param DATA_PATH The file path for the data. The file name should be like: Totals.csv.
#' @return The AHEI and its component scores
#' @examples
#' DATA_PATH = "/Users/james/Desktop/data/Totals.csv"
#' AHEI_F_ASA24(DATA_PATH)
#' @export


AHEI_F_ASA24 = function(DATA_PATH){
  
  if (is.character(DATA_PATH) == TRUE){
    COHORT = read_csv(DATA_PATH)
  } else {
    COHORT = DATA_PATH
  }
  
  COHORT = COHORT %>%
    dplyr::mutate(
      VEG_SERV = V_REDOR_TOTAL + V_DRKGR*0.5 + V_STARCHY_OTHER + V_OTHER,
      FRT_SERV = F_CITMLB+F_OTHER,
      WGRAIN_F_SERV = G_WHOLE/0.035274,
      NUTSLEG_SERV = PF_NUTSDS+PF_SOY+PF_LEGUMES,
      PUFA_SERV = ((PFAT-P205-P226)*9/KCAL)*100,
      N3FAT_SERV = (P205+P226)*1000,
      SSB_FRTJ_SERV = (ADD_SUGARS*4 / 240),
      REDPROC_MEAT_SERV = (PF_CUREDMEAT/1.5) + (PF_MEAT/4),
      TRANS_SERV = 0,
      SODIUM_SERV = SODI,
      ALCOHOL_SERV = A_DRINKS
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
  
  SODIUM_DECILE = quantile(COHORT$SODIUM_SERV, probs=seq(0, 1, by=0.1))
  
  COHORT=COHORT %>%
    dplyr::mutate(
      AHEI_VEG = SCORE_HEALTHY(VEG_SERV, AHEI_MIN_VEG_SERV, AHEI_MAX_VEG_SERV, AHEI_MIN, AHEI_MAX),
      AHEI_FRT = SCORE_HEALTHY(FRT_SERV, AHEI_MIN_FRT_SERV, AHEI_MAX_FRT_SERV, AHEI_MIN, AHEI_MAX),
      AHEI_WGRAIN_F = SCORE_HEALTHY(WGRAIN_F_SERV, AHEI_MIN_WGRAIN_F_SERV, AHEI_MAX_WGRAIN_F_SERV, AHEI_MIN, AHEI_MAX),
      AHEI_NUTSLEG = SCORE_HEALTHY(NUTSLEG_SERV, AHEI_MIN_NUTSLEG_SERV, AHEI_MAX_NUTSLEG_SERV, AHEI_MIN, AHEI_MAX),
      AHEI_N3FAT = SCORE_HEALTHY(N3FAT_SERV, AHEI_MIN_N3FAT_SERV, AHEI_MAX_N3FAT_SERV, AHEI_MIN, AHEI_MAX),
      AHEI_PUFA = SCORE_HEALTHY(PUFA_SERV, AHEI_MIN_PUFA_SERV, AHEI_MAX_PUFA_SERV, AHEI_MIN, AHEI_MAX),
      
      AHEI_SSB_FRTJ = SCORE_UNHEALTHY(SSB_FRTJ_SERV, AHEI_MIN_SSB_FRTJ_SERV, AHEI_MAX_SSB_FRTJ_SERV, AHEI_MIN, AHEI_MAX),
      AHEI_REDPROC_MEAT = SCORE_UNHEALTHY(REDPROC_MEAT_SERV, AHEI_MIN_REDPROC_MEAT_SERV, AHEI_MAX_REDPROC_MEAT_SERV, AHEI_MIN, AHEI_MAX),
      AHEI_TRANS = SCORE_UNHEALTHY(TRANS_SERV, AHEI_MIN_TRANS_SERV, AHEI_MAX_TRANS_SERV, AHEI_MIN, AHEI_MAX),
      
      AHEI_SODIUM = case_when(
        SODIUM_SERV <= SODIUM_DECILE[11] & SODIUM_SERV >= SODIUM_DECILE[10] ~ 0,
        SODIUM_SERV < SODIUM_DECILE[10] & SODIUM_SERV >= SODIUM_DECILE[9] ~ 10/9,
        SODIUM_SERV < SODIUM_DECILE[9] & SODIUM_SERV >= SODIUM_DECILE[8] ~ 20/9,
        SODIUM_SERV < SODIUM_DECILE[8] & SODIUM_SERV >= SODIUM_DECILE[7] ~ 30/9,
        SODIUM_SERV < SODIUM_DECILE[7] & SODIUM_SERV >= SODIUM_DECILE[6] ~ 40/9,
        SODIUM_SERV < SODIUM_DECILE[6] & SODIUM_SERV >= SODIUM_DECILE[5] ~ 50/9,
        SODIUM_SERV < SODIUM_DECILE[5] & SODIUM_SERV >= SODIUM_DECILE[4] ~ 60/9,
        SODIUM_SERV < SODIUM_DECILE[4] & SODIUM_SERV >= SODIUM_DECILE[3] ~ 70/9,
        SODIUM_SERV < SODIUM_DECILE[3] & SODIUM_SERV >= SODIUM_DECILE[2] ~ 80/9,
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
           AHEI_PUFA, AHEI_SSB_FRTJ, AHEI_REDPROC_MEAT, AHEI_TRANS, AHEI_SODIUM, AHEI_ALCOHOL_F)
  
}

#' AHEI_M_ASA24
#'
#' Calculate the AHEI (male only) for the ASA24 data within 1 step  
#' @import dplyr
#' @import readr
#' @import haven
#' @param DATA_PATH The file path for the data. The file name should be like: Totals.csv.
#' @return The AHEI and its component scores
#' @examples
#' DATA_PATH = "/Users/james/Desktop/data/Totals.csv"
#' AHEI_M_ASA24(DATA_PATH)
#' @export


AHEI_M_ASA24 = function(DATA_PATH){
  
  if (is.character(DATA_PATH) == TRUE){
    COHORT = read_csv(DATA_PATH)
  } else {
    COHORT = DATA_PATH
  }
  
  COHORT = COHORT %>%
    dplyr::mutate(
      VEG_SERV = V_REDOR_TOTAL + V_DRKGR*0.5 + V_STARCHY_OTHER + V_OTHER,
      FRT_SERV = F_CITMLB+F_OTHER,
      WGRAIN_M_SERV = G_WHOLE/0.035274,
      NUTSLEG_SERV = PF_NUTSDS+PF_SOY+PF_LEGUMES,
      N3FAT_SERV = (P205 + P226)*1000,
      PUFA_SERV = ((PFAT-P205-P226)*9/KCAL)*100,
      
      SSB_FRTJ_SERV = (ADD_SUGARS*4 / 240),
      REDPROC_MEAT_SERV = (PF_CUREDMEAT/1.5) + (PF_MEAT/4),
      TRANS_SERV = 0,
      SODIUM_SERV = SODI,
      ALCOHOL_SERV = A_DRINKS
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
  
  SODIUM_DECILE = quantile(COHORT$SODIUM_SERV, probs=seq(0, 1, by=0.1))
  
  COHORT=COHORT %>%
    dplyr::mutate(
      AHEI_VEG = SCORE_HEALTHY(VEG_SERV, AHEI_MIN_VEG_SERV, AHEI_MAX_VEG_SERV, AHEI_MIN, AHEI_MAX),
      AHEI_FRT = SCORE_HEALTHY(FRT_SERV, AHEI_MIN_FRT_SERV, AHEI_MAX_FRT_SERV, AHEI_MIN, AHEI_MAX),
      AHEI_WGRAIN_M = SCORE_HEALTHY(WGRAIN_M_SERV, AHEI_MIN_WGRAIN_M_SERV, AHEI_MAX_WGRAIN_M_SERV, AHEI_MIN, AHEI_MAX),
      AHEI_NUTSLEG = SCORE_HEALTHY(NUTSLEG_SERV, AHEI_MIN_NUTSLEG_SERV, AHEI_MAX_NUTSLEG_SERV, AHEI_MIN, AHEI_MAX),
      AHEI_N3FAT = SCORE_HEALTHY(N3FAT_SERV, AHEI_MIN_N3FAT_SERV, AHEI_MAX_N3FAT_SERV, AHEI_MIN, AHEI_MAX),
      AHEI_PUFA = SCORE_HEALTHY(PUFA_SERV, AHEI_MIN_PUFA_SERV, AHEI_MAX_PUFA_SERV, AHEI_MIN, AHEI_MAX),
      
      AHEI_SSB_FRTJ = SCORE_UNHEALTHY(SSB_FRTJ_SERV, AHEI_MIN_SSB_FRTJ_SERV, AHEI_MAX_SSB_FRTJ_SERV, AHEI_MIN, AHEI_MAX),
      AHEI_REDPROC_MEAT = SCORE_UNHEALTHY(REDPROC_MEAT_SERV, AHEI_MIN_REDPROC_MEAT_SERV, AHEI_MAX_REDPROC_MEAT_SERV, AHEI_MIN, AHEI_MAX),
      AHEI_TRANS = SCORE_UNHEALTHY(TRANS_SERV, AHEI_MIN_TRANS_SERV, AHEI_MAX_TRANS_SERV, AHEI_MIN, AHEI_MAX),
      
      AHEI_SODIUM = case_when(
        SODIUM_SERV <= SODIUM_DECILE[11] & SODIUM_SERV >= SODIUM_DECILE[10] ~ 0,
        SODIUM_SERV < SODIUM_DECILE[10] & SODIUM_SERV >= SODIUM_DECILE[9] ~ 10/9,
        SODIUM_SERV < SODIUM_DECILE[9] & SODIUM_SERV >= SODIUM_DECILE[8] ~ 20/9,
        SODIUM_SERV < SODIUM_DECILE[8] & SODIUM_SERV >= SODIUM_DECILE[7] ~ 30/9,
        SODIUM_SERV < SODIUM_DECILE[7] & SODIUM_SERV >= SODIUM_DECILE[6] ~ 40/9,
        SODIUM_SERV < SODIUM_DECILE[6] & SODIUM_SERV >= SODIUM_DECILE[5] ~ 50/9,
        SODIUM_SERV < SODIUM_DECILE[5] & SODIUM_SERV >= SODIUM_DECILE[4] ~ 60/9,
        SODIUM_SERV < SODIUM_DECILE[4] & SODIUM_SERV >= SODIUM_DECILE[3] ~ 70/9,
        SODIUM_SERV < SODIUM_DECILE[3] & SODIUM_SERV >= SODIUM_DECILE[2] ~ 80/9,
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
           AHEI_PUFA, AHEI_SSB_FRTJ, AHEI_REDPROC_MEAT, AHEI_TRANS, AHEI_SODIUM, AHEI_ALCOHOL_M)
  
}

#' DASH_ASA24
#'
#' Calculate the DASH for the ASA24 data within 1 step  
#' @import dplyr
#' @import readr
#' @import haven
#' @param DATA_PATH The file path for the data. The file name should be like: Totals.csv.
#' @return The DASH and its component scores
#' @examples
#' DATA_PATH = "/Users/james/Desktop/data/Totals.csv"
#' DASH_ASA24(DATA_PATH)
#' @export


DASH_ASA24 = function(DATA_PATH){
  
  if (is.character(DATA_PATH) == TRUE){
    COHORT = read_csv(DATA_PATH)
  } else {
    COHORT = DATA_PATH
  }
  
  #Match participant response food frequency to the standard food frequency response code
  COHORT = COHORT %>%
    dplyr::mutate(
      FRT_FRTJ_SERV = F_TOTAL,
      VEG_SERV = V_REDOR_TOTAL + V_DRKGR*0.5 + V_STARCHY_OTHER + V_OTHER,
      NUTSLEG_SERV = PF_NUTSDS+PF_SOY+PF_LEGUMES,
      WGRAIN_SERV = G_WHOLE,
      LOWF_DAIRY_SERV = 0.1738*D_MILK + D_YOGURT + (2/40.2)*D_CHEESE,
      SODIUM_SERV = SODI,
      REDPROC_MEAT_SERV = (PF_CUREDMEAT/1.5) + (PF_MEAT/4),
      SSB_FRTJ_SERV = (ADD_SUGARS / 240)
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
           DASH_SODIUM, DASH_REDPROC_MEAT, DASH_SSB_FRTJ)
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
  
  #Match participant response food frequency to the standard food frequency response code
  
  COHORT = COHORT %>%
    dplyr::mutate(
      FRT_FRTJ_SERV = F_TOTAL,
      VEG_SERV = V_REDOR_TOTAL + V_DRKGR*0.5 + V_STARCHY_OTHER + V_OTHER,
      WGRAIN_SERV = G_WHOLE,
      LEGUMES_SERV = PF_SOY+PF_LEGUMES,
      NUTS_SERV = PF_NUTSDS,
      FISH_SERV = PF_SEAFD_HI+PF_SEAFD_LOW,
      REDPROC_MEAT_SERV = (PF_CUREDMEAT/1.5) + (PF_MEAT/4),
      MONSATFAT_SERV = case_when(
        SFAT == 0 ~ 0, 
        TRUE ~ MFAT/SFAT
      ),
      ALCOHOL_SERV = A_DRINKS
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
      MED_ALCOHOL = ifelse(ALCOHOL_SERV <=25 & ALCOHOL_SERV >= 10, 1, 0),
      
      MED_ALL = MED_FRT+MED_VEG+MED_WGRAIN+MED_LEGUMES+MED_NUTS+MED_FISH+MED_REDPROC_MEAT+MED_MONSATFAT+MED_ALCOHOL,
      MED_NOETOH = MED_FRT+MED_VEG+MED_WGRAIN+MED_LEGUMES+MED_NUTS+MED_FISH+MED_REDPROC_MEAT+MED_MONSATFAT
    )%>%
    dplyr::select(UserName, MED_ALL, MED_NOETOH, MED_FRT, MED_VEG, MED_WGRAIN, MED_LEGUMES, MED_NUTS,
           MED_FISH, MED_REDPROC_MEAT, MED_MONSATFAT, MED_ALCOHOL)
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
  
  #Serving size calculation for DII
  COHORT = COHORT %>%
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
    dplyr::select(UserName, ALCOHOL, VITB12, VITB6, BCAROTENE, CAFFEINE, CARB, CHOLES, KCAL, TOTALFAT, FIBER, FOLICACID,
           IRON, MG, MUFA, NIACIN, N3FAT, N6FAT, PROTEIN, PUFA, RIBOFLAVIN, SATFAT, SE, THIAMIN, VITA,
           VITC, VITD, VITE, ZN)
  
  COHORT = COHORT %>%
    tidyr::pivot_longer(-UserID, names_to="Variable", values_to="Value")
  
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
  
  COHORT %>%
    inner_join(DII_STD, by=c("Variable")) %>%
    dplyr::mutate(
      Z_SCORE = (Value - Global_mean)/SD,
      PERCENTILE = pnorm(Z_SCORE)*2 - 1,
      IND_DII_SCORE = PERCENTILE*Overall_inflammatory_score) %>%
    group_by(UserID) %>%
    summarize(DII_ALL = sum(IND_DII_SCORE))
  
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
                  HEI2015_SATFAT) 
}

#' AHEI_DHQ3
#'
#' Calculate the AHEI for the DHQ3 data within 1 step  
#' @import dplyr
#' @import readr
#' @import haven
#' @param DATA_PATH The file path for the data. The data is Total Daily Results file, ending with results.csv
#' @return The AHEI and its component scores
#' @examples
#' DATA_PATH = "/Users/james/Desktop/data/results.csv"
#' AHEI_DHQ3(DATA_PATH)
#' @export

AHEI_DHQ3 = function(DATA_PATH){
  
  if (is.character(DATA_PATH) == TRUE){
    COHORT = read_csv(DATA_PATH)
  } else {
    COHORT = DATA_PATH
  }
  
  
  COHORT = COHORT %>%
    dplyr::mutate(
      VEG_SERV = `Total red/orange vegetable (cups)` + `Dark-green vegetable (cups)`*0.5 + `Other starchy vegetable (cups)` + `Other vegetable (cups)`,
      FRT_SERV = `Total fruit (cups)`-`Juice fruit (cups)`,
      WGRAIN_SERV = `Whole grain (oz)`/0.035274,
      NUTSLEG_SERV = `Nuts, seeds, soy, and legumes (oz)`,
      N3FAT_SERV = (`PFA 20:5 (Eicosapentaenoic) (g)`+`PFA 22:6 (Docosahexaenoic) (g)`)*1000,
      PUFA_SERV = (((`Total polyunsaturated fatty acids (g)`-`PFA 20:5 (Eicosapentaenoic) (g)`-`PFA 22:6 (Docosahexaenoic) (g)`)*9)/`Energy (kcal)`)*100,
      SSB_FRTJ_SERV = (`*Added sugars (g)` / 240),
      REDPROC_MEAT_SERV = (`Cured meat protein foods (oz)`/1.5) + (`Meat from beef, pork, veal, lamb, and game protein foods (oz)`/4),
      TRANS_SERV = ((`*Total trans fatty acitds (g)`*9)/`Energy (kcal)`)*100,
      SODIUM_SERV = `Sodium (mg)`,
      ALCOHOL_SERV = `Alcohol (drink(s))`
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
  
  SODIUM_DECILE = quantile(COHORT$SODIUM_SERV, probs=seq(0, 1, by=0.1))
  
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
        SODIUM_SERV <= SODIUM_DECILE[11] & SODIUM_SERV >= SODIUM_DECILE[10] ~ 0,
        SODIUM_SERV < SODIUM_DECILE[10] & SODIUM_SERV >= SODIUM_DECILE[9] ~ 10/9,
        SODIUM_SERV < SODIUM_DECILE[9] & SODIUM_SERV >= SODIUM_DECILE[8] ~ 20/9,
        SODIUM_SERV < SODIUM_DECILE[8] & SODIUM_SERV >= SODIUM_DECILE[7] ~ 30/9,
        SODIUM_SERV < SODIUM_DECILE[7] & SODIUM_SERV >= SODIUM_DECILE[6] ~ 40/9,
        SODIUM_SERV < SODIUM_DECILE[6] & SODIUM_SERV >= SODIUM_DECILE[5] ~ 50/9,
        SODIUM_SERV < SODIUM_DECILE[5] & SODIUM_SERV >= SODIUM_DECILE[4] ~ 60/9,
        SODIUM_SERV < SODIUM_DECILE[4] & SODIUM_SERV >= SODIUM_DECILE[3] ~ 70/9,
        SODIUM_SERV < SODIUM_DECILE[3] & SODIUM_SERV >= SODIUM_DECILE[2] ~ 80/9,
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
                  AHEI_PUFA, AHEI_SSB_FRTJ, AHEI_REDPROC_MEAT, AHEI_TRANS, AHEI_SODIUM, AHEI_ALCOHOL)
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
      REDPROC_MEAT_SERV = (`Cured meat protein foods (oz)`/1.5) + (`Meat from beef, pork, veal, lamb, and game protein foods (oz)`/4),
      MONSATFAT_SERV = case_when(
        `Total saturated fatty acids (g)` == 0 ~ 0, 
        TRUE ~ `Total monounsaturated fatty acids (g)`/`Total saturated fatty acids (g)`
      ),
      ALCOHOL_SERV = `Alcohol (drink(s))`
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
      MED_ALCOHOL = ifelse(ALCOHOL_SERV <=25 & ALCOHOL_SERV >= 10, 1, 0),
      
      MED_ALL = MED_FRT+MED_VEG+MED_WGRAIN+MED_LEGUMES+MED_NUTS+MED_FISH+MED_REDPROC_MEAT+MED_MONSATFAT+MED_ALCOHOL,
      MED_NOETOH = MED_FRT+MED_VEG+MED_WGRAIN+MED_LEGUMES+MED_NUTS+MED_FISH+MED_REDPROC_MEAT+MED_MONSATFAT
    )%>%
    dplyr::select(RESPONDENTID, MED_ALL, MED_NOETOH, MED_FRT, MED_VEG, MED_WGRAIN, MED_LEGUMES, MED_NUTS,
                  MED_FISH, MED_REDPROC_MEAT, MED_MONSATFAT, MED_ALCOHOL)
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
  
  COHORT = COHORT %>%
    dplyr::mutate(
      LOWF_MILK_SERV = case_when(
        `Food ID` == 5.3 | `Food ID` == 5.4 | `Food ID` == 6.3 | `Food ID` == 6.4 | `Food ID` == 10.3 | `Food ID` == 10.4 ~ `Milk (cups)`,
        TRUE ~ 0
      ),
      LOWF_CHEESECREAM_SERV = case_when(
        `Food ID` == 50.2 | `Food ID` == 64.2 | `Food ID` == 75.2 | `Food ID` == 80.1 ~ `Total dairy (cups)`,
        TRUE ~ 0
      ),
    ) %>%
    dplyr::group_by(`Respondent ID`) %>%
    dplyr::summarize(
      FRT_FRTJ_SERV = sum(`Total fruit (cups)`),
      VEG_SERV = sum(`Total red/orange vegetable (cups)` + `Dark-green vegetable (cups)`*0.5 + `Other starchy vegetable (cups)` + `Other vegetable (cups)`),
      NUTSLEG_SERV = sum(`Nuts, seeds, soy, and legumes (oz)`),
      WGRAIN_SERV = sum(`Whole grain (oz)`),
      LOWF_DAIRY_SERV = sum(LOWF_MILK_SERV+LOWF_CHEESECREAM_SERV+`Yogurt (cups)`),
      SODIUM_SERV = sum(`Sodium (mg)`),
      REDPROC_MEAT_SERV = sum((`Cured meat protein foods (oz)`/1.5) + (`Meat from beef, pork, veal, lamb, and game protein foods (oz)`/4)),
      SSB_FRTJ_SERV = sum((`*Added sugars (g)` / 240))
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
                  DASH_SODIUM, DASH_REDPROC_MEAT, DASH_SSB_FRTJ)
}


