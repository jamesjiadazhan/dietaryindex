#' MEDI_V2 Calculation
#'
#' Calculate the Mediterranean dietary index in serving size-based and 5 point scoring criteria, using given the serving sizes of foods and nutrients consumed per 1 day
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
#' MEDI_V2(SERV_DATA = PREDIMED_trial,RESPONDENTID = PREDIMED_trial$Diet_Type,OLIVE_OIL_SERV_MEDI = PREDIMED_trial$Virgin_Oliveoil,FRT_SERV_MEDI = PREDIMED_trial$Fruits, VEG_SERV_MEDI = PREDIMED_trial$Vegetables,LEGUMES_SERV_MEDI = PREDIMED_trial$Legumes,NUTS_SERV_MEDI = PREDIMED_trial$Total_nuts,FISH_SEAFOOD_SERV_MEDI = PREDIMED_trial$Fish_Seafood,ALCOHOL_SERV_MEDI = PREDIMED_trial$Alcohol,SSB_SERV_MEDI = PREDIMED_trial$Soda_Drinks,SWEETS_SERV_MEDI = PREDIMED_trial$Sweets,DISCRET_FAT_SERV_MEDI = PREDIMED_trial$Refined_Oliveoil,REDPROC_MEAT_SERV_MEDI = PREDIMED_trial$Meat)
#' @export

#Score calculation for MEDI_V2
MEDI_V2 = function(SERV_DATA, RESPONDENTID, OLIVE_OIL_SERV_MEDI, FRT_SERV_MEDI, VEG_SERV_MEDI, LEGUMES_SERV_MEDI, NUTS_SERV_MEDI, FISH_SEAFOOD_SERV_MEDI, ALCOHOL_SERV_MEDI, SSB_SERV_MEDI, SWEETS_SERV_MEDI, DISCRET_FAT_SERV_MEDI, REDPROC_MEAT_SERV_MEDI){
  
  MEDI_V2_MIN = 0
  MEDI_V2_MAX = 5
  
  MEDI_V2_MIN_OLIVE_OIL_SERV = 0
  MEDI_V2_MAX_OLIVE_OIL_SERV = 5
  MEDI_V2_MIN_FRT_SERV = 0
  MEDI_V2_MAX_FRT_SERV = 3
  MEDI_V2_MIN_VEG_SERV = 0
  MEDI_V2_MAX_VEG_SERV = 2
  MEDI_V2_MIN_LEGUMES_SERV = 0
  MEDI_V2_MAX_LEGUMES_SERV = 3/7
  MEDI_V2_MIN_NUTS_SERV = 0
  MEDI_V2_MAX_NUTS_SERV = 3/7
  MEDI_V2_MIN_FISH_SEAFOOD_SERV = 0
  MEDI_V2_MAX_FISH_SEAFOOD_SERV = 3/7
  MEDI_V2_MIN_ALCOHOL_SERV = 0
  MEDI_V2_MAX_ALCOHOL_SERV = 1

  MEDI_V2_MIN_SSB_SERV = 1
  MEDI_V2_MAX_SSB_SERV = 0
  MEDI_V2_MIN_SWEETS_SERV = 2/7
  MEDI_V2_MAX_SWEETS_SERV = 0
  MEDI_V2_MIN_DISCRET_FAT_SERV = 1
  MEDI_V2_MAX_DISCRET_FAT_SERV = 0
  MEDI_V2_MIN_REDPROC_MEAT_SERV = 1
  MEDI_V2_MAX_REDPROC_MEAT_SERV = 0

  MEDI_HEALTHY = function(actual, min, max){
    case_when(
      actual >= max ~ MEDI_V2_MAX,
      actual <= min ~ MEDI_V2_MIN,
      TRUE ~ MEDI_V2_MIN+(actual-min)*MEDI_V2_MAX/(max-min)
    )
  }
  
  MEDI_UNHEALTHY = function(actual, min, max){
    case_when(
      actual >= min ~ MEDI_V2_MIN ,
      actual <= max ~ MEDI_V2_MAX,
      TRUE ~ MEDI_V2_MIN+(actual-min)*MEDI_V2_MAX/(max-min)
    )
  }
  
  SERV_DATA %>%
    dplyr::mutate(
      RESPONDENTID = RESPONDENTID,
      
      MEDI_OLIVE_OIL = MEDI_HEALTHY(OLIVE_OIL_SERV_MEDI, MEDI_V2_MIN_OLIVE_OIL_SERV, MEDI_V2_MAX_OLIVE_OIL_SERV),
      MEDI_FRT = MEDI_HEALTHY(FRT_SERV_MEDI, MEDI_V2_MIN_FRT_SERV, MEDI_V2_MAX_FRT_SERV),
      MEDI_VEG = MEDI_HEALTHY(VEG_SERV_MEDI, MEDI_V2_MIN_VEG_SERV, MEDI_V2_MAX_VEG_SERV),
      MEDI_LEGUMES = MEDI_HEALTHY(LEGUMES_SERV_MEDI, MEDI_V2_MIN_LEGUMES_SERV, MEDI_V2_MAX_LEGUMES_SERV),
      MEDI_NUTS = MEDI_HEALTHY(NUTS_SERV_MEDI, MEDI_V2_MIN_NUTS_SERV, MEDI_V2_MAX_NUTS_SERV),
      MEDI_FISH = MEDI_HEALTHY(FISH_SEAFOOD_SERV_MEDI, MEDI_V2_MIN_FISH_SEAFOOD_SERV, MEDI_V2_MAX_FISH_SEAFOOD_SERV),
      MEDI_ALCOHOL = MEDI_HEALTHY(ALCOHOL_SERV_MEDI, MEDI_V2_MIN_ALCOHOL_SERV, MEDI_V2_MAX_ALCOHOL_SERV),

      MEDI_SSB = MEDI_UNHEALTHY(SSB_SERV_MEDI, MEDI_V2_MIN_SSB_SERV, MEDI_V2_MAX_SSB_SERV),
      MEDI_SWEETS = MEDI_UNHEALTHY(SWEETS_SERV_MEDI, MEDI_V2_MIN_SWEETS_SERV, MEDI_V2_MAX_SWEETS_SERV),
      MEDI_DISCRET_FAT = MEDI_UNHEALTHY(DISCRET_FAT_SERV_MEDI, MEDI_V2_MIN_DISCRET_FAT_SERV, MEDI_V2_MAX_DISCRET_FAT_SERV),
      MEDI_REDPROC_MEAT = MEDI_UNHEALTHY(REDPROC_MEAT_SERV_MEDI, MEDI_V2_MIN_REDPROC_MEAT_SERV, MEDI_V2_MAX_REDPROC_MEAT_SERV),

      
      MEDI_V2_ALL = MEDI_OLIVE_OIL+MEDI_FRT+MEDI_VEG+MEDI_LEGUMES+MEDI_NUTS+MEDI_FISH+MEDI_ALCOHOL+MEDI_SSB+MEDI_SWEETS+MEDI_DISCRET_FAT+MEDI_REDPROC_MEAT,
      MEDI_V2_NOETOH = MEDI_FRT+MEDI_VEG+MEDI_LEGUMES+MEDI_NUTS+MEDI_FISH+MEDI_SSB+MEDI_SWEETS+MEDI_DISCRET_FAT+MEDI_REDPROC_MEAT
    )%>%
    dplyr::select(RESPONDENTID, MEDI_V2_ALL, MEDI_V2_NOETOH, MEDI_OLIVE_OIL, MEDI_FRT, MEDI_VEG, MEDI_LEGUMES, MEDI_NUTS, MEDI_FISH, MEDI_ALCOHOL, MEDI_SSB, MEDI_SWEETS, MEDI_DISCRET_FAT, MEDI_REDPROC_MEAT)
}
