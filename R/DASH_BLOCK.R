#' DASH_BLOCK Calculation
#'
#' Calculate the DASH dietary index for Block FFQ (2013) per 1 day
#' @import dplyr
#' @import readr
#' @import haven
#' @param RAW_DATA The raw data file that includes results and raw data of the dietary assessment
#' @return The DASH and its component scores
#' @examples
#' data("BLOCK_exp")
#' DASH_BLOCK(BLOCK_exp)
#' @export

DASH_BLOCK = function(RAW_DATA) {
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
    YOGURT_FOOD_PORT = c(2, 3)
    YOGURT_PORT_SERV = c(0.5, 1)
    YOGURT_PORT_DF = data.frame(STD_FOOD_PORT, STD_PORT_SERV)

    BUTTERMILK_FOOD_PORT = c(1, 2, 3, 4)
    BUTTERMILK_PORT_SERV = c(0.25, 0.5, 1, 2)
    BUTTERMILK_PORT_DF = data.frame(STD_FOOD_PORT, STD_PORT_SERV)

    SERV_DATA = RAW_DATA %>%
        dplyr::mutate(
            F_BERRIES = foodfreq(STRAWBERRIESFREQ) * foodport(STRAWBERRIESQUAN),
            F_WHOLE = F_SOLID - F_BERRIES + F_BERRIES * 2,
            FRT_FRTJ_SERV = F_WHOLE + JUICE100,
            VEG_SERV = V_DRKGR + (V_DPYEL + V_OTHER + V_STARCY + V_TOMATO) / 0.5,
            NUTSLEG_SERV = (LEGUMES * 4) + M_NUTSD + M_SOY,
            WGRAIN_SERV = G_WHL,
            LOWF_MILK_SERV = case_when(
                MILKTYPE == 4 ~ foodfreq(MILKFREQ) * MILKQUAN,
                TRUE ~ 0
            ),
            YOGURT_SERV = (foodfreq(YOGURTONLYFREQ) *
                foodport(YOGURTONLYQUAN, ref = YOGURT_PORT_DF)) +
                (foodfreq(BUTTERMILKFREQ) *
                    foodport(BUTTERMILKQUAN, ref = BUTTERMILK_PORT_DF)),
            LOWF_ICECREAMFROYO_SERV = case_when(
                ICECREAMFROYOTYPE == 2 ~ foodfreq(ICECREAMFROYOFREQ) * foodport(ICECREAMFROYOQUAN) * 2,
                TRUE ~ 0
            ),
            LOWF_CHEESE_SERV = case_when(
                CHEESETYPE == 1 ~ foodfreq(CHEESEFREQ) * CHEESEQUAN,
                TRUE ~ 0
            ),
            LOWF_DAIRY_SERV = LOWF_MILK_SERV + YOGURT_SERV + LOWF_ICECREAMFROYO_SERV + LOWF_CHEESE_SERV,
            SODIUM_SERV = DT_SODI / (DT_KCAL / 2000),
            REDPROC_MEAT_SERV = (M_FRANK / 1.5) + ((M_MEAT + M_ORGAN) / 4),
            SSB_FRTJ_SERV = GROUP_SUGARYBEVG_TOTAL_GRAMS / 240
        )

    ## Create variables and functions needed for AHEI calculation
    quintile_healthy = function(actual) {
        quintile = quantile(actual, probs = seq(0, 1, by = 0.2))
        case_when(
            actual <= quintile[6] & actual >= quintile[5] ~ 5,
            actual < quintile[5] & actual >= quintile[4] ~ 4,
            actual < quintile[4] & actual >= quintile[3] ~ 3,
            actual < quintile[3] & actual >= quintile[2] ~ 2,
            actual < quintile[2] & actual >= quintile[1] ~ 1
        )
    }

    quintile_unhealthy = function(actual) {
        quintile = quantile(actual, probs = seq(0, 1, by = 0.2))
        case_when(
            actual <= quintile[6] & actual >= quintile[5] ~ 1,
            actual < quintile[5] & actual >= quintile[4] ~ 2,
            actual < quintile[4] & actual >= quintile[3] ~ 3,
            actual < quintile[3] & actual >= quintile[2] ~ 4,
            actual < quintile[2] & actual >= quintile[1] ~ 5
        )
    }

    message("Reminder: this DASH index uses quintiles to rank participants' food/drink serving sizes and then calculate DASH component scores, which may generate results that are specific to your study population but not comparable to other populations.")

    ## DASH calculation
    SERV_DATA %>%
        dplyr::mutate(
            DASH_FRT = quintile_healthy(FRT_FRTJ_SERV),
            DASH_VEG = quintile_healthy(VEG_SERV),
            DASH_NUTSLEG = quintile_healthy(NUTSLEG_SERV),
            DASH_WGRAIN = quintile_healthy(WGRAIN_SERV),
            DASH_LOWF_DAIRY = quintile_healthy(LOWF_DAIRY_SERV),
            DASH_SODIUM = quintile_unhealthy(SODIUM_SERV),
            DASH_REDPROC_MEAT = quintile_unhealthy(REDPROC_MEAT_SERV),
            DASH_SSB_FRTJ = quintile_unhealthy(SSB_FRTJ_SERV),
            DASH_ALL = DASH_FRT + DASH_VEG + DASH_NUTSLEG + DASH_WGRAIN + DASH_LOWF_DAIRY +
                DASH_SODIUM + DASH_REDPROC_MEAT + DASH_SSB_FRTJ
        ) %>%
        dplyr::select(
            RESPONDENTID, DASH_ALL, DASH_FRT, DASH_VEG, DASH_NUTSLEG, DASH_WGRAIN, DASH_LOWF_DAIRY,
            DASH_SODIUM, DASH_REDPROC_MEAT, DASH_SSB_FRTJ,
            DT_KCAL, F_BERRIES, F_WHOLE, FRT_FRTJ_SERV, VEG_SERV, NUTSLEG_SERV, WGRAIN_SERV, LOWF_MILK_SERV, YOGURT_SERV, LOWF_ICECREAMFROYO_SERV, LOWF_CHEESE_SERV, LOWF_DAIRY_SERV, SODIUM_SERV, SODIUM_SERV, REDPROC_MEAT_SERV, SSB_FRTJ_SERV
        )
}
