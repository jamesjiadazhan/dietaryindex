#' AHEI_BLOCK Calculation
#'
#' Calculate the AHEI dietary index for the Block FFQ (2013) per 1 day
#' @import dplyr
#' @import readr
#' @import haven
#' @param RAW_DATA The raw data file that includes results and raw responses of the dietary assessment
#' @return The AHEI and its component scores. Sodium is energy adjusted: sodium_serv/(total kcal / 1000)
#' @examples
#' data("BLOCK_exp")
#' AHEI_BLOCK(BLOCK_exp)
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
      VEG_SERV = V_DRKGR + (V_DPYEL + V_OTHER + V_STARCY + V_TOMATO)/0.5,
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
      SODIUM_SERV = DT_SODI/(DT_KCAL/2000)
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
        GENDER == 1  & ALCOHOL_SERV < 3.5 & ALCOHOL_SERV > 2 ~ 0 + (ALCOHOL_SERV-3.5)*10/(2-3.5),
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