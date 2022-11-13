#' MEDI Calculation
#'
#' Calculate the MEDI dietary index (serving size-based), Mediterranean, using given the serving sizes of foods and nutrients consumed per 1 day
#' @import dplyr
#' @import readr
#' @import haven
#' @param SERV_DATA The raw data file that includes all the serving sizes of foods and nutrients
#' @param RESPONDENTID The unique participant ID for each participant
#' @param FRT_FRTJ_SERV_MEDI The serving size of All fruits and 100\% fruit juices, unit=servings/day (0.5 c of berries; 1 cup=236.59 g; 1 med fruit (1 cup = 236.59 g); 1 cup fruit juice
#' @param VEG_SERV_MEDI The serving size of All raw and cooked vegetables, unit=0.5 c of vege; 1 cup of green leafy
#' @param LEGUMES_SERV_MEDI The serving size of legumes, including Dried beans, lentils, peas, soups (split pea), tofu, soymilk, unit=1oz (28.35 g) of nuts and legume or 1 TBLSP peanut butter (15 mL), 1 cup legume = 4 oz
#' @param WGRAIN_SERV_MEDI The serving size of whole grains, including Whole wheat bread and flour, whole wheat pasta, brown rice, rusks,  whole grain breakfast cereals, couscous, semolina, unit=1 oz
#' @param FISH_SERV_MEDI The serving size of all fish, including Fresh-water and sea-water fish; preserved fish such as salted fish, canned fish; shellfish (squid, prawns, mollusks), unit=serving (4 oz)
#' @param DAIRY_SERV_MEDI The serving size of all dairy, including Milk, yogurt, cheese, custard, milk puddings, other milk products, unit=1 serving = 1 cup milk, 1 cup yogurt, 1 ½ ounces hard cheese (cheddar, mozzarella, Swiss, Parmesan), 1/3 cup shredded cheese, 2 ounces processed cheese (American), ½ cup ricotta cheese, 2 cups cottage cheese, 1 cup pudding made with milk, 1 cup frozen yogurt, 1 ½ cups ice cream
#' @param REDPROC_MEAT_SERV_MEDI The serving size of red and processed meats, including Beef, pork, lamb, goat, veal, sausages, bacon, salami, ham, hot dog, deli meat, unit=serving (4 oz. unprocessed meat; 1.5 oz. processed meat)
#' @param NUTS_SERV_MEDI The serving size of nuts, including Peanuts, almonds, sunflower seeds, cashews, walnuts, unit=1oz
#' @param MONSATFAT_SERV_MEDI The serving size of the ratio of monounsaturated fat to saturated fat, unit=ratio
#' @param ALCOHOL_SERV_MEDI The serving size of alcohol, including Wine, beer, "light" beer, liquor, unit=13g
#' @return The MEDI index/score
#' @examples
#' MED(SERV_DATA, SERV_DATA$RESPONDENTID, SERV_DATA$FRT_FRTJ_SERV_MEDI, SERV_DATA$VEG_SERV_MEDI, SERV_DATA$WGRAIN_SERV_MEDI, SERV_DATA$LEGUMES_SERV_MEDI, SERV_DATA$NUTS_SERV_MEDI,FISH_SERV_MEDI, SERV_DATA$REDPROC_MEAT_SERV_MEDI, SERV_DATA$MONSATFAT_SERV_MEDI, SERV_DATA$ALCOHOL_SERV_MEDI)
#' @export

#Score calculation for MEDI
MEDI = function(SERV_DATA, RESPONDENTID, FRT_FRTJ_SERV_MEDI, VEG_SERV_MEDI, LEGUMES_SERV_MEDI, WGRAIN_SERV_MEDI, FISH_SERV_MEDI, DAIRY_SERV_MEDI, REDPROC_MEAT_SERV_MEDI,
                NUTS_SERV_MEDI, MONSATFAT_SERV_MEDI, ALCOHOL_SERV_MEDI){
  SERV_DATA %>%
    dplyr::mutate(
      RESPONDENTID = RESPONDENTID,
      
      MEDI_FRT = case_when(FRT_FRTJ_SERV_MEDI >=3 ~ 1, TRUE ~ 0),
      MEDI_VEG = case_when(VEG_SERV_MEDI >= 3 ~ 1, TRUE ~ 0),
      MEDI_LEGUMES = case_when(LEGUMES_SERV_MEDI >= 1.5 ~ 1, TRUE ~ 0),
      MEDI_WGRAIN = case_when(WGRAIN_SERV_MEDI >= 3 ~ 1, TRUE ~ 0),
      MEDI_FISH = case_when(FISH_SERV_MEDI >= 2 ~ 1, TRUE ~ 0),
      MEDI_DAIRY = case_when(DAIRY_SERV_MEDI >= 2 ~ 1, TRUE ~ 0),
      MEDI_REDPROC_MEAT = case_when(REDPROC_MEAT_SERV_MEDI < 4.5 ~ 1, TRUE ~ 0),
      MEDI_NUTS = case_when(NUTS_SERV_MEDI >= 2 ~ 1, TRUE ~ 0),
      MEDI_MONSATFAT = case_when(MONSATFAT_SERV_MEDI >= 1.6 ~ 1, TRUE ~ 0),
      MEDI_ALCOHOL = case_when(ALCOHOL_SERV_MEDI <=25 & ALCOHOL_SERV_MEDI >= 10 ~ 1, TRUE ~ 0),
      
      MEDI_ALL = MEDI_FRT+MEDI_VEG+MEDI_LEGUMES+MEDI_WGRAIN+MEDI_FISH+MEDI_DAIRY+MEDI_REDPROC_MEAT+
        MEDI_NUTS+MEDI_MONSATFAT+MEDI_ALCOHOL,
      MEDI_NOETOH = MEDI_FRT+MEDI_VEG+MEDI_LEGUMES+MEDI_WGRAIN+MEDI_FISH+MEDI_DAIRY+MEDI_REDPROC_MEAT
      +MEDI_NUTS+MEDI_MONSATFAT
    )%>%
    dplyr::select(RESPONDENTID, MEDI_ALL, MEDI_NOETOH, MEDI_FRT, MEDI_VEG, MEDI_LEGUMES, MEDI_WGRAIN, MEDI_FISH,
                  MEDI_DAIRY, MEDI_REDPROC_MEAT, MEDI_NUTS, MEDI_MONSATFAT, MEDI_ALCOHOL)
}
