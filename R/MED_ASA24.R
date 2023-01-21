#' MED_ASA24
#'
#' Calculate the MED for the ASA24 data within 1 step  
#' @import dplyr
#' @import readr
#' @import haven
#' @param DATA_PATH The file path for the data. The file name should be like: Totals.csv.
#' @return The MED and its component scores
#' @examples
#' data("ASA24_exp")
#' MED_ASA24(ASA24_exp)
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
      VEG_SERV = V_DRKGR + (V_REDOR_TOTAL + V_STARCHY_OTHER + V_OTHER)/0.5,
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
