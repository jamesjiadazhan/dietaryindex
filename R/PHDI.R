#' PHD Calculation
#'
#' Calculate the PHDI, The Planetary Health Diet Index that quantifies adherence to the reference diet included in the EAT-Lancet report Willett et al (2019), using the serving sizes of foods and NUTSrients consumed per 1 day
#' @import dplyr
#' @import readr
#' @import haven
#' @param SERV_DATA The raw data file that includes all the serving sizes of foods and NUTSrients
#' @param RESPONDENTID The unique participant ID for each participant
#' @param GENDER The gender of the participant. 2 is female and 1 is male.
#' @param TOTALKCAL_PHDI The total kcal
#' @param WGRAIN_SERV_PHDI The serving size of whole grains, unit=grams/day
#' @param STARCHY_VEG_SERV_PHDI The serving size of starchy vegetables, such as potatos, cassava, unit=grams/day
#' @param VEG_SERV_PHDI The serving size of All vegetable except potatoes, starchy vegetables, and LEGUMES, unit=grams/day
#' @param FRT_SERV_PHDI The serving size of All whole fruits and no fruit juice, unit=grams/day
#' @param DAIRY_SERV_PHDI The serving size of All dairy products, unit=grams/day
#' @param REDPROC_MEAT_SERV_PHDI The serving size of red and processed meats, including Beef, pork, lamb, goat, veal, sausages, bacon, salami, ham, hot dog, deli meat, unit=grams/day
#' @param POULTRY_SERV_PHDI The serving size of Poultry, including Chicken, turkey, duck, goose, ostrich, unit=grams/day
#' @param EGG_SERV_PHDI The serving size of Eggs with shell, unit=grams/day
#' @param FISH_SERV_PHDI The serving size of Fish and shellfish, unit=grams/day
#' @param NUTS_SERV_PHDI The serving size of nuts and seeds, including peanuts, tree nuts- - walnuts, almond, hazelnuts, pecan, cashew, pistachio, unit=grams/day
#' @param LEGUMES_SERV_PHDI The serving size of Nonsoy LEGUMES, including Beans, peas, lentils, chickpeas, unit=grams/day
#' @param SOY_SERV_PHDI The serving size of Soy products, including Tofu, tempeh, soy milk, soy yogurt, soy cheese, unit=grams/day
#' @param ADDED_FAT_UNSAT_SERV_PHDI The serving size of Added unsaturated fat, e.g. olive soybean, rapeseed, sunflower, peanuts oil, excluding transfat, unit=% of total kcal/day
#' @param ADDED_FAT_SAT_TRANS_SERV_PHDI The serving size of Added saturated fats and trans fat, e.g. butter, lard, coconuts oil, palm oil, unit=% of total kcal/day
#' @param ADDED_SUGAR_SERV_PHDI The serving size of Added sugar, including the added sugar from all sweeteners and fruit juice, unit=% of total kcal/day
#' @return The PHDI index/score, PHDI and its component scores
#' @examples
#' data("PHDI_VALIDATION")
#' PHDI_VALIDATION_RESULT = PHDI(SERV_DATA=PHDI_VALIDATION, PHDI_VALIDATION$id, PHDI_VALIDATION$gender, PHDI_VALIDATION$TOTALKCAL_PHDI, PHDI_VALIDATION$WGRAIN_SERV_PHDI, PHDI_VALIDATION$STARCHY_VEG_SERV_PHDI, PHDI_VALIDATION$VEG_SERV_PHDI, PHDI_VALIDATION$FRT_SERV_PHDI, PHDI_VALIDATION$DAIRY_SERV_PHDI, PHDI_VALIDATION$REDPROC_MEAT_SERV_PHDI, PHDI_VALIDATION$POULTRY_SERV_PHDI, PHDI_VALIDATION$EGG_SERV_PHDI, PHDI_VALIDATION$FISH_SERV_PHDI, PHDI_VALIDATION$NUTS_SERV_PHDI, PHDI_VALIDATION$LEGUMES_SERV_PHDI, PHDI_VALIDATION$SOY_SERV_PHDI, PHDI_VALIDATION$ADDED_FAT_UNSAT_SERV_PHDI, PHDI_VALIDATION$ADDED_FAT_SAT_TRANS_SERV_PHDI, PHDI_VALIDATION$ADDED_SUGAR_SERV_PHDI)
#' @export

#Score calculation for PHDI
PHDI = function(SERV_DATA, RESPONDENTID, GENDER, TOTALKCAL_PHDI, WGRAIN_SERV_PHDI, STARCHY_VEG_SERV_PHDI, VEG_SERV_PHDI, FRT_SERV_PHDI, DAIRY_SERV_PHDI, REDPROC_MEAT_SERV_PHDI, POULTRY_SERV_PHDI, EGG_SERV_PHDI, FISH_SERV_PHDI, NUTS_SERV_PHDI, LEGUMES_SERV_PHDI, SOY_SERV_PHDI, ADDED_FAT_UNSAT_SERV_PHDI, ADDED_FAT_SAT_TRANS_SERV_PHDI, ADDED_SUGAR_SERV_PHDI){
  ##Create variables and functions needed for PHDI calculation
  PHDI_MIN = 0
  PHDI_MAX = 10
  PHDI_MAX_2 = 5
  PHDI_MIN_WGRAIN_F_SERV = 0
  PHDI_MAX_WGRAIN_F_SERV = 75
  PHDI_MIN_WGRAIN_M_SERV = 0
  PHDI_MAX_WGRAIN_M_SERV = 90
  PHDI_MIN_STARCHY_VEG_SERV = 200
  PHDI_MAX_STARCHY_VEG_SERV = 50
  PHDI_MIN_VEG_SERV = 0
  PHDI_MAX_VEG_SERV = 300
  PHDI_MIN_FRT_SERV = 0
  PHDI_MAX_FRT_SERV = 200
  PHDI_MIN_DAIRY_SERV = 1000
  PHDI_MAX_DAIRY_SERV = 250
  PHDI_MIN_REDPROC_MEAT_SERV = 100
  PHDI_MAX_REDPROC_MEAT_SERV = 14
  PHDI_MIN_POULTRY_SERV = 100
  PHDI_MAX_POULTRY_SERV = 29
  PHDI_MIN_EGG_SERV = 120
  PHDI_MAX_EGG_SERV = 13
  PHDI_MIN_FISH_SERV = 0
  PHDI_MAX_FISH_SERV = 28
  PHDI_MIN_NUTS_SERV = 0
  PHDI_MAX_NUTS_SERV = 50
  PHDI_MIN_LEGUMES_SERV = 0
  PHDI_MAX_LEGUMES_SERV = 100
  PHDI_MIN_SOY_SERV = 0
  PHDI_MAX_SOY_SERV = 50
  PHDI_MIN_ADDED_FAT_UNSAT_SERV = 3.5
  PHDI_MAX_ADDED_FAT_UNSAT_SERV = 21
  PHDI_MIN_ADDED_FAT_SAT_TRANS_SERV = 10
  PHDI_MAX_ADDED_FAT_SAT_TRANS_SERV = 0
  PHDI_MIN_ADDED_SUGAR_SERV = 25
  PHDI_MAX_ADDED_SUGAR_SERV = 5

  
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
  
  ##PHDI calculation
  SERV_DATA %>%
    dplyr::mutate(
      RESPONDENTID = RESPONDENTID,
      GENDER = GENDER,
      TOTALKCAL_PHDI = TOTALKCAL_PHDI,

      PHDI_WGRAIN = case_when(
        #GENDER = 2 is female
        GENDER == 2 & WGRAIN_SERV_PHDI >= PHDI_MAX_WGRAIN_F_SERV ~ PHDI_MAX,
        GENDER == 2 & WGRAIN_SERV_PHDI <= PHDI_MIN_WGRAIN_F_SERV ~ PHDI_MIN,
        GENDER == 2 & WGRAIN_SERV_PHDI > PHDI_MIN_WGRAIN_F_SERV & WGRAIN_SERV_PHDI < PHDI_MAX_WGRAIN_F_SERV ~ PHDI_MIN+(WGRAIN_SERV_PHDI-PHDI_MIN_WGRAIN_F_SERV)*PHDI_MAX/(PHDI_MAX_WGRAIN_F_SERV-PHDI_MIN_WGRAIN_F_SERV),
        
        GENDER == 1 & WGRAIN_SERV_PHDI >= PHDI_MAX_WGRAIN_M_SERV ~ PHDI_MAX,
        GENDER == 1 & WGRAIN_SERV_PHDI <= PHDI_MIN_WGRAIN_M_SERV ~ PHDI_MIN,
        GENDER == 1 & WGRAIN_SERV_PHDI > PHDI_MIN_WGRAIN_M_SERV & WGRAIN_SERV_PHDI < PHDI_MAX_WGRAIN_M_SERV ~ PHDI_MIN+(WGRAIN_SERV_PHDI-PHDI_MIN_WGRAIN_M_SERV)*PHDI_MAX/(PHDI_MAX_WGRAIN_M_SERV-PHDI_MIN_WGRAIN_M_SERV),
      ),
      
      PHDI_STARCHY_VEG = SCORE_UNHEALTHY(STARCHY_VEG_SERV_PHDI, PHDI_MIN_STARCHY_VEG_SERV, PHDI_MAX_STARCHY_VEG_SERV, PHDI_MIN, PHDI_MAX),
      PHDI_VEG = SCORE_HEALTHY(VEG_SERV_PHDI, PHDI_MIN_VEG_SERV, PHDI_MAX_VEG_SERV, PHDI_MIN, PHDI_MAX),
      PHDI_FRT = SCORE_HEALTHY(FRT_SERV_PHDI, PHDI_MIN_FRT_SERV, PHDI_MAX_FRT_SERV, PHDI_MIN, PHDI_MAX),
      PHDI_DAIRY = SCORE_UNHEALTHY(DAIRY_SERV_PHDI, PHDI_MIN_DAIRY_SERV, PHDI_MAX_DAIRY_SERV, PHDI_MIN, PHDI_MAX),
      PHDI_REDPROC_MEAT = SCORE_UNHEALTHY(REDPROC_MEAT_SERV_PHDI, PHDI_MIN_REDPROC_MEAT_SERV, PHDI_MAX_REDPROC_MEAT_SERV, PHDI_MIN, PHDI_MAX),
      PHDI_POULTRY = SCORE_UNHEALTHY(POULTRY_SERV_PHDI, PHDI_MIN_POULTRY_SERV, PHDI_MAX_POULTRY_SERV, PHDI_MIN, PHDI_MAX),
      PHDI_EGG = SCORE_UNHEALTHY(EGG_SERV_PHDI, PHDI_MIN_EGG_SERV, PHDI_MAX_EGG_SERV, PHDI_MIN, PHDI_MAX),
      PHDI_FISH = SCORE_HEALTHY(FISH_SERV_PHDI, PHDI_MIN_FISH_SERV, PHDI_MAX_FISH_SERV, PHDI_MIN, PHDI_MAX),
      PHDI_NUTS = SCORE_HEALTHY(NUTS_SERV_PHDI, PHDI_MIN_NUTS_SERV, PHDI_MAX_NUTS_SERV, PHDI_MIN, PHDI_MAX),
      PHDI_LEGUMES = SCORE_HEALTHY(LEGUMES_SERV_PHDI, PHDI_MIN_LEGUMES_SERV, PHDI_MAX_LEGUMES_SERV, PHDI_MIN, PHDI_MAX_2),
      PHDI_SOY = SCORE_HEALTHY(SOY_SERV_PHDI, PHDI_MIN_SOY_SERV, PHDI_MAX_SOY_SERV, PHDI_MIN, PHDI_MAX_2),
      PHDI_ADDED_FAT_UNSAT = SCORE_HEALTHY(ADDED_FAT_UNSAT_SERV_PHDI, PHDI_MIN_ADDED_FAT_UNSAT_SERV, PHDI_MAX_ADDED_FAT_UNSAT_SERV, PHDI_MIN, PHDI_MAX),
      PHDI_ADDED_FAT_SAT = SCORE_UNHEALTHY(ADDED_FAT_SAT_TRANS_SERV_PHDI, PHDI_MIN_ADDED_FAT_SAT_TRANS_SERV, PHDI_MAX_ADDED_FAT_SAT_TRANS_SERV, PHDI_MIN, PHDI_MAX),
      PHDI_ADDED_SUGAR = SCORE_UNHEALTHY(ADDED_SUGAR_SERV_PHDI, PHDI_MIN_ADDED_SUGAR_SERV, PHDI_MAX_ADDED_SUGAR_SERV, PHDI_MIN, PHDI_MAX),

      PHDI_ALL = PHDI_WGRAIN + PHDI_STARCHY_VEG + PHDI_VEG + PHDI_FRT + PHDI_DAIRY + PHDI_REDPROC_MEAT + PHDI_POULTRY + PHDI_EGG + PHDI_FISH + PHDI_NUTS + PHDI_LEGUMES + PHDI_SOY + PHDI_ADDED_FAT_UNSAT + PHDI_ADDED_FAT_SAT + PHDI_ADDED_SUGAR

    ) %>%
    dplyr::select(RESPONDENTID, GENDER, PHDI_ALL, TOTALKCAL_PHDI, PHDI_WGRAIN, PHDI_STARCHY_VEG, PHDI_VEG, PHDI_FRT, PHDI_DAIRY, PHDI_REDPROC_MEAT, PHDI_POULTRY, PHDI_EGG, PHDI_FISH, PHDI_NUTS, PHDI_LEGUMES, PHDI_SOY, PHDI_ADDED_FAT_UNSAT, PHDI_ADDED_FAT_SAT, PHDI_ADDED_SUGAR)
  
}