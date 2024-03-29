% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/AHEIP.R
\name{AHEIP}
\alias{AHEIP}
\title{AHEIP Calculation}
\usage{
AHEIP(
  SERV_DATA,
  RESPONDENTID,
  VEG_SERV_AHEIP,
  FRT_SERV_AHEIP,
  WHITERED_RT_SERV_AHEIP,
  FIBER_SERV_AHEIP,
  TRANS_SERV_AHEIP,
  POLYSAT_RT_SERV_AHEIP,
  CALCIUM_SERV_AHEIP,
  FOLATE_SERV_AHEIP,
  IRON_SERV_AHEIP
)
}
\arguments{
\item{SERV_DATA}{The raw data file that includes all the serving sizes of foods and nutrients}

\item{RESPONDENTID}{The unique participant ID for each participant}

\item{VEG_SERV_AHEIP}{The serving size of All vegetable except potatoes and legume, unit=servings/day (0.5 c of vege; 1 cup of green leafy (1 cup = 236.59 g)}

\item{FRT_SERV_AHEIP}{The serving size of All whole fruits and no fruit juice, unit=servings/day (0.5 c of berries; 1 cup=236.59 g; 1 med fruit (1 cup = 236.59 g)}

\item{WHITERED_RT_SERV_AHEIP}{The serving size of the ratio of white and red meats, White meat = poultry + all fish, red meat = pork + beef + lamb + all organ meats + processed meat, unit=servings/day; 1 srv= 4 oz. unprocessed meat; 1.5 oz. processed meat (1 oz. = 28.35 g)}

\item{FIBER_SERV_AHEIP}{The serving size of fibers, unit=grams/day}

\item{TRANS_SERV_AHEIP}{The serving size of trans fat, unit='\% of energy'}

\item{POLYSAT_RT_SERV_AHEIP}{The serving size of polyunsaturated/saturated fats, unit=ratio}

\item{CALCIUM_SERV_AHEIP}{The serving size of calcium, unit=mg/day}

\item{FOLATE_SERV_AHEIP}{The serving size of folate, unit=mcg/day}

\item{IRON_SERV_AHEIP}{The serving size of iron, unit=mg/day}
}
\value{
The AHEIP index/score
}
\description{
Calculate the AHEIP dietary index (serving size-based), Mediterranean, using given the serving sizes of foods and nutrients consumed per 1 day
}
\examples{
data("AHEIP_VALIDATION")
AHEIP(SERV_DATA = AHEIP_VALIDATION, RESPONDENTID = AHEIP_VALIDATION$id, VEG_SERV_AHEIP = AHEIP_VALIDATION$vegetable, FRT_SERV_AHEIP = AHEIP_VALIDATION$whole_fruit, WHITERED_RT_SERV_AHEIP = AHEIP_VALIDATION$white_meat_red_meat, FIBER_SERV_AHEIP = AHEIP_VALIDATION$fiber, TRANS_SERV_AHEIP = AHEIP_VALIDATION$trans_fat, POLYSAT_RT_SERV_AHEIP = AHEIP_VALIDATION$poly_fat_sat_fat, CALCIUM_SERV_AHEIP = AHEIP_VALIDATION$calcium, FOLATE_SERV_AHEIP = AHEIP_VALIDATION$folate, IRON_SERV_AHEIP = AHEIP_VALIDATION$iron)
}
