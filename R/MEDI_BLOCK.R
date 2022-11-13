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