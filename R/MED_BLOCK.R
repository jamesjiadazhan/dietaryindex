#' MED_BLOCK Calculation
#'
#' Calculate the MED dietary index (serving size based) per 1 day
#' @import dplyr
#' @import readr
#' @import haven
#' @param RAW_DATA The raw data file that includes results and raw data of the dietary assessment
#' @return The MED index/score and its components
#' @examples
#' data("BLOCK_exp")
#' MED_BLOCK(BLOCK_exp)
#' @export

MED_BLOCK = function(RAW_DATA) {
    if (is.character(RAW_DATA) == TRUE) {
        RAW_DATA = read_csv(RAW_DATA)
    } else {
        RAW_DATA = RAW_DATA
    }

    STD_FOOD_FREQ = c(1, 2, 3, 4, 5, 6, 7, 8, 9)
    STD_FREQ_SERV = c(0, 1 / 90, 1 / 30, 2.5 / 30, 1 / 7, 2 / 7, 3.5 / 7, 5.5 / 7, 1)
    STD_FOOD_PORT = c(1, 2, 3, 4)
    STD_PORT_SERV = c(0.25, 0.5, 1, 2)
    STD_FOOD_FREQ_DF = data.frame(STD_FOOD_FREQ, STD_FREQ_SERV, stringsAsFactors = FALSE)
    STD_FOOD_PORT_DF = data.frame(STD_FOOD_PORT, STD_PORT_SERV, stringsAsFactors = FALSE)

    # Functions to match actual food frequency and portion to the standards
    foodfreq = function(actual, reference = STD_FOOD_FREQ_DF) {
        reference[match(actual, reference[, 1]), 2]
    }

    foodport = function(actual, reference = STD_FOOD_PORT_DF) {
        reference[match(actual, reference[, 1]), 2]
    }

    # Match participant response food frequency to the standard food frequency response code
    SERV_DATA = RAW_DATA %>%
        dplyr::mutate(
            F_BERRIES = foodfreq(STRAWBERRIESFREQ) * foodport(STRAWBERRIESQUAN),
            F_WHOLE = F_SOLID - F_BERRIES + F_BERRIES * 2,
            FRT_FRTJ_SERV = F_WHOLE + JUICE100,
            VEG_SERV = V_DRKGR + (V_DPYEL + V_OTHER + V_STARCY + V_TOMATO) / 0.5,
            WGRAIN_SERV = G_WHL,
            LEGUMES_SERV = (LEGUMES * 4) + M_SOY,
            NUTS_SERV = M_NUTSD,
            FISH_SERV = (M_FISH_HI + M_FISH_LO) / 4,
            REDPROC_MEAT_SERV = (M_FRANK / 1.5) + ((M_MEAT + M_ORGAN) / 4),
            MONSATFAT_SERV = DT_MFAT / DT_SFAT,
            ALCOHOL_SERV = DT_ALCO
        )

    ## Create variables and functions needed for MED
    median_healthy = function(actual) {
        median_score = median(actual, na.rm = TRUE)
        case_when(
            actual < median_score | actual == 0 ~ 0,
            actual >= median_score ~ 1
        )
    }

    median_unhealthy = function(actual) {
        median_score = median(actual, na.rm = TRUE)
        case_when(
            actual < median_score | actual == 0 ~ 1,
            actual >= median_score ~ 0
        )
    }

    print("Reminder: this MED index uses medians to rank participants' food/drink serving sizes and then calculate MED component scores, which may generate results that are specific to your study population but not comparable to other populations.")

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
            MED_ALCOHOL = case_when(
                ALCOHOL_SERV <= 25 & ALCOHOL_SERV >= 10 ~ 1,
                TRUE ~ 0
            ),
            MED_ALL = MED_FRT + MED_VEG + MED_WGRAIN + MED_LEGUMES + MED_NUTS + MED_FISH + MED_REDPROC_MEAT + MED_MONSATFAT + MED_ALCOHOL,
            MED_NOETOH = MED_FRT + MED_VEG + MED_WGRAIN + MED_LEGUMES + MED_NUTS + MED_FISH + MED_REDPROC_MEAT + MED_MONSATFAT
        ) %>%
        dplyr::select(
            RESPONDENTID, MED_ALL, MED_NOETOH, MED_FRT, MED_VEG, MED_WGRAIN, MED_LEGUMES, MED_NUTS,
            MED_FISH, MED_REDPROC_MEAT, MED_MONSATFAT, MED_ALCOHOL
        )
}
