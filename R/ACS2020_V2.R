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
#' @param REDPROC_MEAT_SERV_ACS2020 The serving size of red and processed meats, including Beef, pork, lamb, goat, veal, sausages, bacon, salami, ham, hot dog, deli meat, unit=servings/day; 1 srv= 4 oz. unprocessed meat; 1.5 oz. processed meat (1 oz. = 28.35 g)
#' @param HPFRG_SERV_ACS2020 The daily servings of highly processed foods and refined grains per 1000 kcal, note: the ultra-processed variable for the score should not double count foods included in other parts of the score, for example, sugar-sweetened beverages or processed meats
#' @param SSB_FRTJ_SERV_ACS2020 The serving size of sugar-sweetened beverages and non-100\% fruit juice, unit=servings/day = 1 ser= 8oz (1 oz. = 28.35 g)
#' @return The ACS2020_V2 index/score and its component scores 
#' @examples
#' data("ACS2020_VALIDATION")
#' ACS2020_V1(SERV_DATA = ACS2020_VALIDATION, RESPONDENTID = ACS2020_VALIDATION$id, GENDER = ACS2020_VALIDATION$gender, VEG_SERV_ACS2020 = ACS2020_VALIDATION$vegetable, VEG_ITEMS_SERV_ACS2020 = ACS2020_VALIDATION$vegetable_unique, FRT_SERV_ACS2020 = ACS2020_VALIDATION$fruit, FRT_ITEMS_SERV_ACS2020 = ACS2020_VALIDATION$fruit_unique, WGRAIN_SERV_ACS2020 = ACS2020_VALIDATION$whole_grain, REDPROC_MEAT_SERV_ACS2020 = ACS2020_VALIDATION$red_meat, HPFRG_RATIO_SERV_ACS2020 = ACS2020_VALIDATION$process_food, SSB_FRTJ_SERV_ACS2020 = ACS2020_VALIDATION$ssb)
#' @export


#Score calculation for ACS2020_V2
ACS2020_V2 = function(SERV_DATA, RESPONDENTID, GENDER, TOTALKCAL_ACS2020, VEG_SERV_ACS2020, VEG_ITEMS_SERV_ACS2020, 
                      FRT_SERV_ACS2020, FRT_ITEMS_SERV_ACS2020, WGRAIN_SERV_ACS2020, REDPROC_MEAT_SERV_ACS2020, HPFRG_SERV_ACS2020,
                      SSB_FRTJ_SERV_ACS2020){
  
  ##Create variables and functions needed for ACS2020_V1 calculation
  quintile_healthy1 = function(actual){
    quintile= quantile(actual, probs=seq(0, 1, by=0.25), na.rm=TRUE)
    case_when(
      actual <= quintile[5] & actual > quintile[4] ~ 0.75,
      actual <= quintile[4] & actual > quintile[3] ~ 0.5,
      actual <= quintile[3] & actual > quintile[2] ~ 0.25,
      actual <= quintile[2] & actual >= quintile[1] ~ 0
    )
  }
  
  quintile_healthy4 = function(actual){
    quintile = quantile(actual, probs=seq(0, 1, by=0.25), na.rm=TRUE)
    case_when(
      actual <= quintile[5] & actual > quintile[4] ~ 3,
      actual <= quintile[4] & actual > quintile[3] ~ 2,
      actual <= quintile[3] & actual > quintile[2] ~ 1,
      actual <= quintile[2] & actual >= quintile[1] ~ 0
    )
  }
  
  quintile_unhealthy2 = function(actual){
    quintile = quantile(actual, probs=seq(0, 1, by=0.25), na.rm=TRUE)
    case_when(
      actual <= quintile[5] & actual > quintile[4] ~ 0,
      actual <= quintile[4] & actual > quintile[3] ~ 0.5,
      actual <= quintile[3] & actual > quintile[2] ~ 1,
      actual <= quintile[2] & actual >= quintile[1] ~ 1.5
    )
  }
  
  quintile_unhealthy4 = function(actual){
    quintile = quantile(actual, probs=seq(0, 1, by=0.25), na.rm=TRUE)
    case_when(
      actual <= quintile[5] & actual > quintile[4] ~ 0,
      actual <= quintile[4] & actual > quintile[3] ~ 1,
      actual <= quintile[3] & actual > quintile[2] ~ 2,
      actual <= quintile[2] & actual >= quintile[1] ~ 3
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
        SSB_FRTJ_SERV_ACS2020 < 1 & SSB_FRTJ_SERV_ACS2020 >= 0.428 ~ 0.5,
        SSB_FRTJ_SERV_ACS2020 < 0.428 & SSB_FRTJ_SERV_ACS2020 > 0 ~ 1,
        SSB_FRTJ_SERV_ACS2020 <= 0 ~ 1.5,
      ),
      ACS2020_V2_ALL = ACS2020_VEG+ACS2020_VEG_ITEMS+ACS2020_FRT+ACS2020_FRT_ITEMS+ACS2020_WGRAIN+
        ACS2020_SSB_FRTJ+ACS2020_REDPROC_MEAT+ACS2020_HPFRG
    ) %>%
    dplyr::select(RESPONDENTID, GENDER, ACS2020_V2_ALL, TOTALKCAL_ACS2020, ACS2020_VEG, ACS2020_VEG_ITEMS, ACS2020_FRT, ACS2020_FRT_ITEMS, ACS2020_WGRAIN, ACS2020_REDPROC_MEAT, ACS2020_HPFRG,
                  ACS2020_SSB_FRTJ)
}