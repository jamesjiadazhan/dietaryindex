#' DASHI
#'
#' Calculate the DASHI dietary index (serving size-based), Dietary Approaches to Stop Hypertension, using given the serving sizes of foods and nutrients consumed per 1 day. All serving sizes should be divided by (total energy/2000 kcal) to adjust energy intake.
#' @import dplyr
#' @import readr
#' @import haven
#' @param SERV_DATA The raw data file that includes all the serving sizes of foods and nutrients
#' @param RESPONDENTID The unique participant ID for each participant
#' @param TOTALKCAL_DASHI The total energy intake, unit = kcal/day
#' @param VEG_SERV_DASHI The serving size of All vegetable except potatoes and legume, unit=servings per 2000 kcal/day (1 serving = 1 cup of green leafy, 0.5 cup other vegetables (cooked or raw), 3/4 cup vegetable juice) 
#' @param FRT_FRTJ_SERV_DASHI The serving size of All whole fruits + 100\% juice,  unit=servings per 2000 kcal/day (1 serving = 1 cup fruit, 1 cup fruit juice, 1/2 cup dried fruit)
#' @param NUTSLEG_SERV_DASHI The serving size of Nuts, legumes, and vegetable protein (e.g., tofu), unit=servings per 2000 kcal/day (1 serving =  1/3 cups of nuts, 2 tablespoons peanut butter, 1/2 cup cooked beans, 1/2 cup tofu)
#' @param LOWF_DAIRY_SERV_DASHI The serving size of low fat dairy, unit=servings per 2000 kcal/day (1 serving = 1 cup milk or yogurt, 1 1/2 oz natural cheese (e.g. Cheddar), 2 oz processed cheese (e.g. American))
#' @param WGRAIN_SERV_DASHI The serving size of whole grains, unit=servings per 2000 kcal/day (1 serving = 1oz)
#' @param WHITEMEAT_SERV_DASHI The serving size of poultry and fish, unit=servings per 2000 kcal/day (1 serving = 2.5oz)
#' @param REDPROC_MEAT_SERV_DASHI The serving size of red and processed meats, including Beef, pork, lamb, goat, veal, sausages, bacon, salami, ham, hot dog, deli meat, unit=servings per 2000 kcal/day (1 serving = 2.5oz)
#' @param FATOIL_SERV_DASHI The serving size of discretionary fats and oils, including added Plant oil + Animal fat, unit=servings per 2000 kcal/day (1 serving = 1 tablespoon or 14 g)
#' @param SNACKS_SWEETS_SERV_DASHI The serving size of sweets, unit=servings per 2000 kcal/day (1 serving = about 23 g)
#' @param SODIUM_SERV_DASHI The serving size of sodium, unit=mg per 2000 kcal/day
#' @return The DASHI index/score
#' @examples
#' data("DASH_trial")
#' DASHI(SERV_DATA = DASH_trial, RESPONDENTID = DASH_trial$Diet_Type,TOTALKCAL_DASHI = DASH_trial$Kcal,VEG_SERV_DASHI = DASH_trial$Vegetables, FRT_FRTJ_SERV_DASHI = DASH_trial$Fruits_Juices, NUTSLEG_SERV_DASHI = DASH_trial$Nuts_Seeds_Legumes, LOWF_DAIRY_SERV_DASHI = DASH_trial$Lowfat_Dairy,WGRAIN_SERV_DASHI = DASH_trial$Wholegrains, WHITEMEAT_SERV_DASHI = DASH_trial$Whitemeat, REDPROC_MEAT_SERV_DASHI = DASH_trial$Beef_Pork_Ham, FATOIL_SERV_DASHI = DASH_trial$Fat_Oils_salad_dressing, SNACKS_SWEETS_SERV_DASHI = DASH_trial$Snacks_Sweets,SODIUM_SERV_DASHI = DASH_trial$Sodium)
#' @export

#Score calculation for DASHI
DASHI = function(SERV_DATA, RESPONDENTID, TOTALKCAL_DASHI, VEG_SERV_DASHI, FRT_FRTJ_SERV_DASHI, NUTSLEG_SERV_DASHI, LOWF_DAIRY_SERV_DASHI, WGRAIN_SERV_DASHI,
                 WHITEMEAT_SERV_DASHI, REDPROC_MEAT_SERV_DASHI, FATOIL_SERV_DASHI, SNACKS_SWEETS_SERV_DASHI, SODIUM_SERV_DASHI){
  ##Create variables and functions needed for DASHI calculation
  DASHI_MIN = 0
  DASHI_MAX = 5
  
  DASHI_MIN_VEG_SERV = 0
  DASHI_MAX_VEG_SERV = 4
  DASHI_MIN_FRT_FRTJ_SERV = 0
  DASHI_MAX_FRT_FRTJ_SERV = 5
  DASHI_MIN_NUTSLEG_SERV = 0
  DASHI_MAX_NUTSLEG_SERV = 4/7
  DASHI_MIN_LOWF_DAIRY_SERV = 0
  DASHI_MAX_LOWF_DAIRY_SERV = 2
  DASHI_MIN_WGRAIN_SERV = 0
  DASHI_MAX_WGRAIN_SERV = 4
  DASHI_MIN_WHITEMEAT_SERV = 0
  DASHI_MAX_WHITEMEAT_SERV= 1.1

  DASHI_MIN_REDPROC_MEAT_SERV = 1.5
  DASHI_MAX_REDPROC_MEAT_SERV = 0.5
  DASHI_MIN_FATOIL_SERV = 6
  DASHI_MAX_FATOIL_SERV = 2.5
  DASHI_MIN_SNACKS_SWEETS_SERV = 4
  DASHI_MAX_SNACKS_SWEETS_SERV = 5/7
  DASHI_MIN_SODIUM_SERV = 3000
  DASHI_MAX_SODIUM_SERV = 1150
  
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
      DASHI_TOTALKCAL = TOTALKCAL_DASHI,
      VEG_SERV_DASHI = VEG_SERV_DASHI/(DASHI_TOTALKCAL/2000),
      FRT_FRTJ_SERV_DASHI = FRT_FRTJ_SERV_DASHI/(DASHI_TOTALKCAL/2000),
      NUTSLEG_SERV_DASHI = NUTSLEG_SERV_DASHI/(DASHI_TOTALKCAL/2000),
      LOWF_DAIRY_SERV_DASHI = LOWF_DAIRY_SERV_DASHI/(DASHI_TOTALKCAL/2000),
      WGRAIN_SERV_DASHI = WGRAIN_SERV_DASHI/(DASHI_TOTALKCAL/2000),
      WHITEMEAT_SERV_DASHI = WHITEMEAT_SERV_DASHI/(DASHI_TOTALKCAL/2000),

      REDPROC_MEAT_SERV_DASHI = REDPROC_MEAT_SERV_DASHI/(DASHI_TOTALKCAL/2000),
      FATOIL_SERV_DASHI = FATOIL_SERV_DASHI/(DASHI_TOTALKCAL/2000),
      SNACKS_SWEETS_SERV_DASHI = SNACKS_SWEETS_SERV_DASHI/(DASHI_TOTALKCAL/2000),
      SODIUM_SERV_DASHI = SODIUM_SERV_DASHI/(DASHI_TOTALKCAL/2000),

      DASHI_VEG = DASHI_HEALTHY(VEG_SERV_DASHI, DASHI_MIN_VEG_SERV, DASHI_MAX_VEG_SERV),
      DASHI_FRT = DASHI_HEALTHY(FRT_FRTJ_SERV_DASHI, DASHI_MIN_FRT_FRTJ_SERV, DASHI_MAX_FRT_FRTJ_SERV),
      DASHI_NUTSLEG = DASHI_HEALTHY(NUTSLEG_SERV_DASHI, DASHI_MIN_NUTSLEG_SERV, DASHI_MAX_NUTSLEG_SERV),
      DASHI_LOWFATDAIRY = DASHI_HEALTHY(LOWF_DAIRY_SERV_DASHI, DASHI_MIN_LOWF_DAIRY_SERV, DASHI_MAX_LOWF_DAIRY_SERV),
      DASHI_WGRAIN = DASHI_HEALTHY(WGRAIN_SERV_DASHI, DASHI_MIN_WGRAIN_SERV, DASHI_MAX_WGRAIN_SERV),
      DASHI_WHITEMEAT = DASHI_HEALTHY(WHITEMEAT_SERV_DASHI, DASHI_MIN_WHITEMEAT_SERV, DASHI_MAX_WHITEMEAT_SERV),

      DASHI_REDPROC_MEAT = DASHI_UNHEALTHY(REDPROC_MEAT_SERV_DASHI, DASHI_MIN_REDPROC_MEAT_SERV, DASHI_MAX_REDPROC_MEAT_SERV),
      DASHI_FATOIL = DASHI_UNHEALTHY(FATOIL_SERV_DASHI, DASHI_MIN_FATOIL_SERV, DASHI_MAX_FATOIL_SERV),
      DASHI_SNACKS_SWEETS = DASHI_UNHEALTHY(SNACKS_SWEETS_SERV_DASHI, DASHI_MIN_SNACKS_SWEETS_SERV, DASHI_MAX_SNACKS_SWEETS_SERV),
      DASHI_SODIUM = DASHI_UNHEALTHY(SODIUM_SERV_DASHI, DASHI_MIN_SODIUM_SERV, DASHI_MAX_SODIUM_SERV),

      DASHI_ALL= DASHI_VEG + DASHI_FRT + DASHI_NUTSLEG + DASHI_LOWFATDAIRY +
        DASHI_WGRAIN + DASHI_WHITEMEAT + DASHI_REDPROC_MEAT + DASHI_FATOIL + DASHI_SNACKS_SWEETS + DASHI_SODIUM
    )%>%
    dplyr::select(RESPONDENTID, DASHI_ALL, DASHI_TOTALKCAL, DASHI_VEG, DASHI_FRT, DASHI_NUTSLEG, DASHI_LOWFATDAIRY, DASHI_WGRAIN,
                  DASHI_WHITEMEAT, DASHI_REDPROC_MEAT, DASHI_FATOIL, DASHI_SNACKS_SWEETS, DASHI_SODIUM)
}
