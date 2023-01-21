#' AHEIP_BLOCK Calculation
#'
#' Calculate the AHEIP dietary index (serving size based) per 1 day
#' @import dplyr
#' @import readr
#' @import haven
#' @param RAW_DATA The raw data file that includes results and raw data of the dietary assessment
#' @return The AHEIP index/score and its components
#' @examples
#' data("BLOCK_exp")
#' AHEIP_BLOCK(BLOCK_exp)
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
      VEG_SERV = V_DRKGR + (V_DPYEL + V_OTHER + V_STARCY + V_TOMATO)/0.5,
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