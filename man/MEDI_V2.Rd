% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/MEDI_V2.R
\name{MEDI_V2}
\alias{MEDI_V2}
\title{MEDI_V2 Calculation}
\usage{
MEDI_V2(
  SERV_DATA,
  RESPONDENTID,
  OLIVE_OIL_SERV_MEDI,
  FRT_SERV_MEDI,
  VEG_SERV_MEDI,
  LEGUMES_SERV_MEDI,
  NUTS_SERV_MEDI,
  FISH_SEAFOOD_SERV_MEDI,
  ALCOHOL_SERV_MEDI,
  SSB_SERV_MEDI,
  SWEETS_SERV_MEDI,
  DISCRET_FAT_SERV_MEDI,
  REDPROC_MEAT_SERV_MEDI
)
}
\arguments{
\item{SERV_DATA}{The raw data file that includes all the serving sizes of foods and nutrients}

\item{RESPONDENTID}{The unique participant ID for each participant}

\item{OLIVE_OIL_SERV_MEDI}{The serving size of olive oil, unit= 10 g or 0.8 tbsp = 1 serving}

\item{FRT_SERV_MEDI}{The serving size of All whole fruits, unit= 125 g = 1 serving  (approximately 1 c)}

\item{VEG_SERV_MEDI}{The serving size of All vegetables except potatoes and legumes, unit= 125 g = 1 serving (approximately 1 c of non green leafy vege; 2 cup of green leafy vege)}

\item{LEGUMES_SERV_MEDI}{The serving size of legumes, including Dried beans, lentils, peas, soups (split pea), tofu, soymilk, unit= 40 g = 1 serving (about 1.5 oz)}

\item{NUTS_SERV_MEDI}{The serving size of nuts and seeds, including Peanuts, almonds, sunflower seeds, cashews, walnuts, unit= 25 g = 1 serving (about 1 oz)}

\item{FISH_SEAFOOD_SERV_MEDI}{The serving size of all fishes and seafoods, including Fresh-water and sea-water fish; preserved fish such as salted fish, canned fish; shellfish (squid, prawns, mollusks), unit=125 g = 1 serving (about 4 oz)}

\item{ALCOHOL_SERV_MEDI}{The serving size of alcohol, including Wine, beer, "light" beer, liquor, unit= 14 g pure alcohol = 1 serving (12 ounces of beer, 5 ounces of wine, or 1.5 ounces of hard liquor, such as vodka or whiskey)}

\item{SSB_SERV_MEDI}{The serving size of all dairy, including Milk, yogurt, cheese, custard, milk puddings, other milk products, unit=1 serving = 1 cup milk, 1 cup yogurt, 1 ½ ounces hard cheese (cheddar, mozzarella, Swiss, Parmesan), 1/3 cup shredded cheese, 2 ounces processed cheese (American), ½ cup ricotta cheese, 2 cups cottage cheese, 1 cup pudding made with milk, 1 cup frozen yogurt, 1 ½ cups ice cream}

\item{SWEETS_SERV_MEDI}{The serving size of all sweets, including Candy, chocolate, ice cream, cookies, cakes, pies, pastries, unit= 50 g = 1 serving (about 1.5 oz ) = 1 piece of candy, 1 chocolate bar, 1 scoop ice cream, 1 cookie, 1 cake slice, 1 pie slice, 1 pastry}

\item{DISCRET_FAT_SERV_MEDI}{The serving size of all discretionary fats, including Butter, margarine, mayonnaise, salad dressing, unit= 10 g = 1 serving (about 0.8 tbsp)}

\item{REDPROC_MEAT_SERV_MEDI}{The serving size of red and processed meats, including Beef, pork, lamb, goat, veal, sausages, bacon, salami, ham, hot dog, deli meat, unit= 150 g = 1 serving (about 5 oz)}
}
\value{
The MEDI index/score and the component scores
}
\description{
Calculate the Mediterranean dietary index in serving size-based and 5 point scoring criteria, using given the serving sizes of foods and nutrients consumed per 1 day
}
\examples{
data("PREDIMED_trial")
MEDI_V2(SERV_DATA = PREDIMED_trial, RESPONDENTID = PREDIMED_trial$Diet_Type, OLIVE_OIL_SERV_MEDI = PREDIMED_trial$Virgin_Oliveoil, FRT_SERV_MEDI = PREDIMED_trial$Fruits, VEG_SERV_MEDI = PREDIMED_trial$Vegetables, LEGUMES_SERV_MEDI = PREDIMED_trial$Legumes, NUTS_SERV_MEDI = PREDIMED_trial$Total_nuts, FISH_SEAFOOD_SERV_MEDI = PREDIMED_trial$Fish_Seafood, ALCOHOL_SERV_MEDI = PREDIMED_trial$Alcohol, SSB_SERV_MEDI = PREDIMED_trial$Soda_Drinks, SWEETS_SERV_MEDI = PREDIMED_trial$Sweets, DISCRET_FAT_SERV_MEDI = PREDIMED_trial$Refined_Oliveoil, REDPROC_MEAT_SERV_MEDI = PREDIMED_trial$Meat)
}
