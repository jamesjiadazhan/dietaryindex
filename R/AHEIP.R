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