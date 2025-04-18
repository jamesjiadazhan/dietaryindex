% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/AHEI_M_ASA24.R
\name{AHEI_M_ASA24}
\alias{AHEI_M_ASA24}
\title{AHEI_M_ASA24}
\usage{
AHEI_M_ASA24(DATA_PATH, SSB_code = NULL, RECALL_SUMMARIZE = TRUE)
}
\arguments{
\item{DATA_PATH}{The file path for the data. The file name should be like: Items.csv}

\item{SSB_code}{The food code for sugar sweetened beverage, default is the SSB code from 17-18 FNDDS file}

\item{RECALL_SUMMARIZE}{Whether to summarize the recalls to the participant level, default is TRUE}
}
\value{
The AHEI and its component scores
}
\description{
Calculate the AHEI (male only) for the ASA24 data within 1 step
}
\examples{
data("ASA24_exp_detailed")
AHEI_M_ASA24(ASA24_exp_detailed, SSB_code = NULL, RECALL_SUMMARIZE = TRUE) # for participants who are all male
}
