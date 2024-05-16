#' MED Calculation
#'
#' Calculate the MED dietary index, Mediterranean, using given the serving sizes of foods and nutrients consumed per 1 day
#' @import dplyr
#' @import readr
#' @import haven
#' @param SERV_DATA The raw data file that includes all the serving sizes of foods and nutrients
#' @param RESPONDENTID The unique participant ID for each participant
#' @param FRT_FRTJ_SERV_MED The serving size of All fruits and 100\% fruit juices, unit=servings/day (0.5 c of berries; 1 cup=236.59 g; 1 med fruit (1 cup = 236.59 g); 1 cup fruit juice
#' @param VEG_SERV_MED The serving size of All vegetables except potatoes and legumes, unit=0.5 c of vege; 1 cup of green leafy
#' @param WGRAIN_SERV_MED The serving size of whole grains, including Whole-grain ready-to-eat cereals, cooked cereals, crackers, dark breads, brown rice, other grains, wheat germ, bran, popcorn, unit=1oz
#' @param LEGUMES_SERV_MED The serving size of legumes, including Tofu, string beans, peas, beans, unit=*oz, 1 cup legume = 4 oz
#' @param NUTS_SERV_MED The serving size of nuts, including Nuts, peanut butter, unit=1oz
#' @param FISH_SERV_MED The serving size of all fish, including Fish and shrimp, breaded fish, unit=4oz
#' @param REDPROC_MEAT_SERV_MED The serving size of red and processed meats, including Beef, pork, lamb, goat, veal, sausages, bacon, salami, ham, hot dog, deli meat, unit=serving (4 oz. unprocessed meat; 1.5 oz. processed meat)
#' @param MONSATFAT_SERV_MED The serving size of the ratio of monounsaturated fat to saturated fat, unit=ratio
#' @param ALCOHOL_SERV_MED The serving size of alcohol, unit=13g
#' @return The MED index/score
#' @examples
#' data("MED_VALIDATION")
#' MED(MED_VALIDATION, RESPONDENTID = MED_VALIDATION$id, FRT_FRTJ_SERV_MED = MED_VALIDATION$fruit, VEG_SERV_MED = MED_VALIDATION$vegetable, WGRAIN_SERV_MED = MED_VALIDATION$whole_grain, LEGUMES_SERV_MED = MED_VALIDATION$legume, NUTS_SERV_MED = MED_VALIDATION$nut, FISH_SERV_MED = MED_VALIDATION$fish, REDPROC_MEAT_SERV_MED = MED_VALIDATION$red_processed_meat, MONSATFAT_SERV_MED = MED_VALIDATION$monofat_satfat, ALCOHOL_SERV_MED = MED_VALIDATION$alcohol)
#' @export

# Score calculation for MED
MED = function(SERV_DATA, RESPONDENTID, FRT_FRTJ_SERV_MED, VEG_SERV_MED, WGRAIN_SERV_MED, LEGUMES_SERV_MED, NUTS_SERV_MED,
               FISH_SERV_MED, REDPROC_MEAT_SERV_MED, MONSATFAT_SERV_MED, ALCOHOL_SERV_MED) {
  
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
            RESPONDENTID = RESPONDENTID,
            MED_FRT = median_healthy(FRT_FRTJ_SERV_MED),
            MED_VEG = median_healthy(VEG_SERV_MED),
            MED_WGRAIN = median_healthy(WGRAIN_SERV_MED),
            MED_LEGUMES = median_healthy(LEGUMES_SERV_MED),
            MED_NUTS = median_healthy(NUTS_SERV_MED),
            MED_FISH = median_healthy(FISH_SERV_MED),
            MED_REDPROC_MEAT = median_unhealthy(REDPROC_MEAT_SERV_MED),
            MED_MONSATFAT = median_healthy(MONSATFAT_SERV_MED),
            MED_ALCOHOL = case_when(
                ALCOHOL_SERV_MED <= 25 & ALCOHOL_SERV_MED >= 10 ~ 1,
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
