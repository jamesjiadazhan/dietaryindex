#' MEDI Calculation
#'
#' Calculate the MEDI dietary index (serving size-based), Mediterranean, using given the serving sizes of foods and nutrients consumed per 1 day
#' @import dplyr
#' @import readr
#' @import haven
#' @param SERV_DATA The raw data file that includes all the serving sizes of foods and nutrients
#' @param RESPONDENTID The unique participant ID for each participant
#' @param OLIVE_OIL_SERV_MEDI The serving size of olive oil, unit= 10 g or 0.8 tbsp = 1 serving
#' @param FRT_SERV_MEDI The serving size of All whole fruits, unit= 125 g = 1 serving  (approximately 1 c)
#' @param VEG_SERV_MEDI The serving size of All vegetables except potatoes and legumes, unit= 125 g = 1 serving (approximately 1 c of non green leafy vege; 2 cup of green leafy vege)
#' @param LEGUMES_SERV_MEDI The serving size of legumes, including Dried beans, lentils, peas, soups (split pea), tofu, soymilk, unit= 40 g = 1 serving (about 1.5 oz)
#' @param NUTS_SERV_MEDI The serving size of nuts and seeds, including Peanuts, almonds, sunflower seeds, cashews, walnuts, unit= 25 g = 1 serving (about 1 oz)
#' @param FISH_SEAFOOD_SERV_MEDI The serving size of all fishes and seafoods, including Fresh-water and sea-water fish; preserved fish such as salted fish, canned fish; shellfish (squid, prawns, mollusks), unit=125 g = 1 serving (about 4 oz)
#' @param ALCOHOL_SERV_MEDI The serving size of alcohol, including Wine, beer, "light" beer, liquor, unit= 14 g pure alcohol = 1 serving (12 ounces of beer, 5 ounces of wine, or 1.5 ounces of hard liquor, such as vodka or whiskey)
#' @param SSB_SERV_MEDI The serving size of all dairy, including Milk, yogurt, cheese, custard, milk puddings, other milk products, unit=1 serving = 1 cup milk, 1 cup yogurt, 1 ½ ounces hard cheese (cheddar, mozzarella, Swiss, Parmesan), 1/3 cup shredded cheese, 2 ounces processed cheese (American), ½ cup ricotta cheese, 2 cups cottage cheese, 1 cup pudding made with milk, 1 cup frozen yogurt, 1 ½ cups ice cream
#' @param SWEETS_SERV_MEDI The serving size of all sweets, including Candy, chocolate, ice cream, cookies, cakes, pies, pastries, unit= 50 g = 1 serving (about 1.5 oz ) = 1 piece of candy, 1 chocolate bar, 1 scoop ice cream, 1 cookie, 1 cake slice, 1 pie slice, 1 pastry
#' @param DISCRET_FAT_SERV_MEDI The serving size of all discretionary fats, including Butter, margarine, mayonnaise, salad dressing, unit= 10 g = 1 serving (about 0.8 tbsp)
#' @param REDPROC_MEAT_SERV_MEDI The serving size of red and processed meats, including Beef, pork, lamb, goat, veal, sausages, bacon, salami, ham, hot dog, deli meat, unit= 150 g = 1 serving (about 5 oz)
#' @return The MEDI index/score and the component scores
#' @examples
#' data("PREDIMED_trial")
#' MEDI(SERV_DATA = PREDIMED_trial,RESPONDENTID = PREDIMED_trial$Diet_Type,OLIVE_OIL_SERV_MEDI = PREDIMED_trial$Virgin_Oliveoil,FRT_SERV_MEDI = PREDIMED_trial$Fruits, VEG_SERV_MEDI = PREDIMED_trial$Vegetables,LEGUMES_SERV_MEDI = PREDIMED_trial$Legumes,NUTS_SERV_MEDI = PREDIMED_trial$Total_nuts,FISH_SEAFOOD_SERV_MEDI = PREDIMED_trial$Fish_Seafood,ALCOHOL_SERV_MEDI = PREDIMED_trial$Alcohol,SSB_SERV_MEDI = PREDIMED_trial$Soda_Drinks,SWEETS_SERV_MEDI = PREDIMED_trial$Sweets,DISCRET_FAT_SERV_MEDI = PREDIMED_trial$Refined_Oliveoil,REDPROC_MEAT_SERV_MEDI = PREDIMED_trial$Meat)
#' @export

#Score calculation for MEDI
MEDI = function(SERV_DATA, RESPONDENTID, OLIVE_OIL_SERV_MEDI, FRT_SERV_MEDI, VEG_SERV_MEDI, LEGUMES_SERV_MEDI, NUTS_SERV_MEDI, FISH_SEAFOOD_SERV_MEDI, ALCOHOL_SERV_MEDI, SSB_SERV_MEDI, SWEETS_SERV_MEDI, DISCRET_FAT_SERV_MEDI, REDPROC_MEAT_SERV_MEDI){
  SERV_DATA %>%
    dplyr::mutate(
      RESPONDENTID = RESPONDENTID,
      
      MEDI_OLIVE_OIL = case_when(OLIVE_OIL_SERV_MEDI >= 5 ~ 1, TRUE ~ 0),
      MEDI_FRT = case_when(FRT_SERV_MEDI >= 3 ~ 1, TRUE ~ 0),
      MEDI_VEG = case_when(VEG_SERV_MEDI >= 2 ~ 1, TRUE ~ 0),
      MEDI_LEGUMES = case_when(LEGUMES_SERV_MEDI >= 3/7 ~ 1, TRUE ~ 0),
      MEDI_NUTS = case_when(NUTS_SERV_MEDI >= 3/7 ~ 1, TRUE ~ 0),
      MEDI_FISH = case_when(FISH_SEAFOOD_SERV_MEDI >= 3/7 ~ 1, TRUE ~ 0),
      MEDI_ALCOHOL = case_when(ALCOHOL_SERV_MEDI >= 1 ~ 1, TRUE ~ 0),

      MEDI_SSB = case_when(SSB_SERV_MEDI < 1 ~ 1, TRUE ~ 0),
      MEDI_SWEETS = case_when(SWEETS_SERV_MEDI < 2/7 ~ 1, TRUE ~ 0),
      MEDI_DISCRET_FAT = case_when(DISCRET_FAT_SERV_MEDI < 1 ~ 1, TRUE ~ 0),
      MEDI_REDPROC_MEAT = case_when(REDPROC_MEAT_SERV_MEDI < 1 ~ 1, TRUE ~ 0),

      
      MEDI_ALL = MEDI_OLIVE_OIL+MEDI_FRT+MEDI_VEG+MEDI_LEGUMES+MEDI_NUTS+MEDI_FISH+MEDI_ALCOHOL+MEDI_SSB+MEDI_SWEETS+MEDI_DISCRET_FAT+MEDI_REDPROC_MEAT,
      MEDI_NOETOH = MEDI_FRT+MEDI_VEG+MEDI_LEGUMES+MEDI_NUTS+MEDI_FISH+MEDI_SSB+MEDI_SWEETS+MEDI_DISCRET_FAT+MEDI_REDPROC_MEAT
    )%>%
    dplyr::select(RESPONDENTID, MEDI_ALL, MEDI_NOETOH, MEDI_OLIVE_OIL, MEDI_FRT, MEDI_VEG, MEDI_LEGUMES, MEDI_NUTS, MEDI_FISH, MEDI_ALCOHOL, MEDI_SSB, MEDI_SWEETS, MEDI_DISCRET_FAT, MEDI_REDPROC_MEAT)
}
