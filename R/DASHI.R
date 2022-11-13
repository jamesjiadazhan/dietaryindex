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
