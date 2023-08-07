#' HEI2020_NHANES_MPED
#'
#' Calculate the HEI2020 for the NHANES_MPED data (after 2005) within 1 step for day 1, day 2, or day 1 and 2 combined, including HEI2020 and HEI-Toddlers-2020
#' @import dplyr
#' @import readr
#' @import haven
#' @param MPED_PER_100_GRAM_PATH The file path for the MPED per 100 gram data for the day 1 and day 2 data. The file name should be like: pyr_tot_d1.sas7bdat
#' @param WJFRT The file path for the WJFRT data for the day 1 and day2 data. The file name should be like: wjfrt.sas7bdat
#' @param NUTRIENT_PATH The file path for the NUTRIENT data for the day 1 data. The file name should be like: DR1TOT_J.XPT or DRXTOT_B.XPT
#' @param NUTRIENT_IND_PATH The file path for the NUTRIENT_IND data for the day 1 data. The file name should be like: DR1IFF_J.XPT
#' @param DEMO_PATH The file path for the DEMOGRAPHIC data. The file name should be like: DEMO_J.XPT
#' @param NUTRIENT_PATH2 The file path for the NUTRIENT2 data for the day 2 data. The file name should be like: DR2TOT_J.XPT
#' @param NUTRIENT_IND_PATH2 The file path for the NUTRIENT_IND2 data for the day 2 data The file name should be like: DR2IFF_J.XPT
#' @return The HEI2020 and its component scores and serving sizes
#' @examples
#' data("NHANES_20032004")
#' HEI2020_NHANES_MPED(MPED_PER_100_GRAM_PATH = NHANES_20032004$MPED_PER_100_GRAM, WJFRT = NHANES_20032004$WJFRT, NUTRIENT_PATH = NHANES_20032004$NUTRIENT, NUTRIENT_IND_PATH = NHANES_20032004$NUTRIENT_IND, DEMO_PATH = NHANES_20032004$DEMO, NUTRIENT_PATH2 = NHANES_20032004$NUTRIENT2, NUTRIENT_IND_PATH2 = NHANES_20032004$NUTRIENT_IND2)
#' @export

HEI2020_NHANES_MPED = function(MPED_PER_100_GRAM_PATH = NULL, WJFRT = NULL, NUTRIENT_PATH = NULL, NUTRIENT_IND_PATH = NULL, DEMO_PATH, NUTRIENT_PATH2 = NULL, NUTRIENT_IND_PATH2 = NULL) {
    ## Create variables needed for HEI2020 calculation
    HEI2020_MIN = 0
    HEI2020_MAX1 = 5
    HEI2020_MAX2 = 10

    HEI2020_MIN_TOTALFRT_SERV = 0
    HEI2020_MAX_TOTALFRT_SERV = 0.8
    HEI2020_MIN_FRT_SERV = 0
    HEI2020_MAX_FRT_SERV = 0.4
    HEI2020_MIN_VEG_SERV = 0
    HEI2020_MAX_VEG_SERV = 1.1
    HEI2020_MIN_GREENNBEAN_SERV = 0
    HEI2020_MAX_GREENNBEAN_SERV = 0.2
    HEI2020_MIN_TOTALPRO_SERV = 0
    HEI2020_MAX_TOTALPRO_SERV = 2.5
    HEI2020_MIN_SEAPLANTPRO_SERV = 0
    HEI2020_MAX_SEAPLANTPRO_SERV = 0.8
    HEI2020_MIN_WHOLEGRAIN_SERV = 0
    HEI2020_MAX_WHOLEGRAIN_SERV = 1.5
    HEI2020_MIN_DAIRY_SERV = 0
    HEI2020_MAX_DAIRY_SERV = 1.3
    HEI2020_MIN_FATTYACID_SERV = 1.2
    HEI2020_MAX_FATTYACID_SERV = 2.5

    HEI2020_MIN_REFINEDGRAIN_SERV = 4.3
    HEI2020_MAX_REFINEDGRAIN_SERV = 1.8
    HEI2020_MIN_SODIUM_SERV = 2.0
    HEI2020_MAX_SODIUM_SERV = 1.1
    HEI2020_MIN_ADDEDSUGAR_SERV = 26
    HEI2020_MAX_ADDEDSUGAR_SERV = 6.5
    HEI2020_MIN_SATFAT_SERV = 16
    HEI2020_MAX_SATFAT_SERV = 8

    ## Create variables needed for HEI-Toddlers-2020 calculation
    HEI2020_TODDLERS_MIN_TOTALFRT_SERV = 0
    HEI2020_TODDLERS_MAX_TOTALFRT_SERV = 0.7
    HEI2020_TODDLERS_MIN_FRT_SERV = 0
    HEI2020_TODDLERS_MAX_FRT_SERV = 0.3
    HEI2020_TODDLERS_MIN_VEG_SERV = 0
    HEI2020_TODDLERS_MAX_VEG_SERV = 0.9
    HEI2020_TODDLERS_MIN_GREENNBEAN_SERV = 0
    HEI2020_TODDLERS_MAX_GREENNBEAN_SERV = 0.1
    HEI2020_TODDLERS_MIN_TOTALPRO_SERV = 0
    HEI2020_TODDLERS_MAX_TOTALPRO_SERV = 2.0
    HEI2020_TODDLERS_MIN_SEAPLANTPRO_SERV = 0
    HEI2020_TODDLERS_MAX_SEAPLANTPRO_SERV = 0.5
    HEI2020_TODDLERS_MIN_WHOLEGRAIN_SERV = 0
    HEI2020_TODDLERS_MAX_WHOLEGRAIN_SERV = 1.5
    HEI2020_TODDLERS_MIN_DAIRY_SERV = 0
    HEI2020_TODDLERS_MAX_DAIRY_SERV = 2.0
    HEI2020_TODDLERS_MIN_FATTYACID_SERV = 0.9
    HEI2020_TODDLERS_MAX_FATTYACID_SERV = 1.5

    HEI2020_TODDLERS_MIN_REFINEDGRAIN_SERV = 3.4
    HEI2020_TODDLERS_MAX_REFINEDGRAIN_SERV = 1.5
    HEI2020_TODDLERS_MIN_SODIUM_SERV = 1.7
    HEI2020_TODDLERS_MAX_SODIUM_SERV = 1.1
    HEI2020_TODDLERS_MIN_ADDEDSUGAR_SERV = 13.8
    HEI2020_TODDLERS_MAX_ADDEDSUGAR_SERV = 0
    HEI2020_TODDLERS_MIN_SATFAT_SERV = 18.2
    HEI2020_TODDLERS_MAX_SATFAT_SERV = 12.2

    HEI2020_HEALTHY1 = function(actual, min, max) {
        case_when(
            actual >= max ~ HEI2020_MAX1,
            actual <= min ~ HEI2020_MIN,
            TRUE ~ HEI2020_MIN + (actual - min) * HEI2020_MAX1 / (max - min)
        )
    }

    HEI2020_HEALTHY2 = function(actual, min, max) {
        case_when(
            actual >= max ~ HEI2020_MAX2,
            actual <= min ~ HEI2020_MIN,
            TRUE ~ HEI2020_MIN + (actual - min) * HEI2020_MAX2 / (max - min)
        )
    }

    HEI2020_UNHEALTHY = function(actual, min, max) {
        case_when(
            actual >= min ~ HEI2020_MIN,
            actual <= max ~ HEI2020_MAX2,
            TRUE ~ HEI2020_MIN + (actual - min) * HEI2020_MAX2 / (max - min)
        )
    }

    # stop if the input data is not provided for any day
    if (is.null(NUTRIENT_PATH) & is.null(NUTRIENT_PATH2)) {
        stop("Please provide the file path for the MPED and NUTRIENT data, day 1 or day 2 or day 1 and day 2.")
    }

    # load the MPED per 100 gram data
    ## if MPED_PER_100_GRAM_PATH is a character
    if (is.character(MPED_PER_100_GRAM_PATH) == TRUE) {
        # contains "sas"
        if (grepl("sas", MPED_PER_100_GRAM_PATH) == TRUE){
            MPED = read_sas(MPED_PER_100_GRAM_PATH)
        }
        # does not contain "sas"
        else {
            # Column widths - derived from the character positions
            widths <- c(8, 1, 6, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8)

            # Column names
            col_names <- c("DRDIFDCD", "EQUIVFLAG", "MODCODE", "G_TOTAL", "G_WHL", "G_NWHL",
                        "V_TOTAL", "V_DRKGR", "V_DPYEL", "V_POTATO", "V_STARCY", "V_TOMATO",
                        "V_OTHER", "F_TOTAL", "F_CITMLB", "F_OTHER", "D_TOTAL", "D_MILK",
                        "D_YOGURT", "D_CHEESE", "M_MPF", "M_MEAT", "M_ORGAN", "M_FRANK",
                        "M_POULT", "M_FISH_HI", "M_FISH_LO", "M_EGG", "M_SOY", "M_NUTSD",
                        "LEGUMES", "DISCFAT_OIL", "DISCFAT_SOL", "ADD_SUG", "A_BEV")

            # Read the file with fixed width format
            MPED <- read.fwf(MPED_PER_100_GRAM_PATH, widths = widths, header = FALSE)
            # Assign the column names
            names(MPED) <- col_names
        }
    } else  {
        MPED = MPED_PER_100_GRAM_PATH
    }

    # load the WJFRT data (whole fruit and fruit juice per 100 gram data)
    if (is.character(WJFRT) == TRUE) {
        WJFRT = read_sas(WJFRT)
    } else {
        WJFRT = WJFRT
    }

    # load the DEMOGRAPHIC data
    if (is.character(DEMO_PATH) == TRUE) {
        DEMO = read_xpt(DEMO_PATH)
    } else {
        DEMO = DEMO_PATH
    }

    ################## processing the MPED and WJFRT data to make them ready for analysis ##################
    # Step 1: locate the required datasets and variables and make necessary edits to the datasets

    ## part a: get MPED per 100 grams of food and perform edits and corrections

    # Rename variables
    MPED_PER_100_GRAM = MPED_PER_100_GRAM %>%
        mutate(
            FOODCODE=1*DRDIFDCD,
            MODCODE=1*DRDIMC
        )

    # Move soy beverages out of soybean products into dairy
    MPED_PER_100_GRAM_2 <- MPED_PER_100_GRAM %>%
    mutate(
        M_SOY = case_when(
        FOODCODE == 11310000 ~ 0,
        FOODCODE == 11320000 ~ 0,
        FOODCODE == 11321000 ~ 0,
        FOODCODE == 11330000 ~ 0,
        TRUE ~ M_SOY
        ),
        D_TOTAL = case_when(
        FOODCODE == 11310000 ~ round(100*(1/244), 3),
        FOODCODE == 11320000 ~ round(100*(1/245), 3),
        FOODCODE == 11321000 ~ round(100*(1/240), 3),
        FOODCODE == 11330000 ~ round(100*(1/245), 3),
        TRUE ~ D_TOTAL
        )
    )

    # Replace some food codes with values to correct for previously identified errors in the database
    ## pizza (n=3) values from FPED 11/12 data
    MPED_PER_100_GRAM_3 <- MPED_PER_100_GRAM_2 %>%
        mutate(
            G_TOTAL = case_when(
                FOODCODE == 58106210 ~ 1.88,
                FOODCODE == 58106220 ~ 1.75,
                FOODCODE == 58106230 ~ 1.88,
                TRUE ~ G_TOTAL
            ),
            G_WHL = case_when(
                FOODCODE %in% c(58106210, 58106220, 58106230) ~ 0,
                TRUE ~ G_WHL
            ),
            G_NWHL = case_when(
                FOODCODE == 58106210 ~ 1.88,
                FOODCODE == 58106220 ~ 1.75,
                FOODCODE == 58106230 ~ 1.88,
                TRUE ~ G_NWHL
            ),
            V_TOTAL = case_when(
                FOODCODE %in% c(58106210, 58106220, 58106230) ~ 0.12,
                TRUE ~ V_TOTAL
            ),
            V_TOMATO = case_when(
                FOODCODE %in% c(58106210, 58106220, 58106230) ~ 0.12,
                TRUE ~ V_TOMATO
            ),
            D_TOTAL = case_when(
                FOODCODE == 58106210 ~ 0.70,
                FOODCODE == 58106220 ~ 0.66,
                FOODCODE == 58106230 ~ 0.66,
                TRUE ~ D_TOTAL
            ),
            D_CHEESE = case_when(
                FOODCODE == 58106210 ~ 0.70,
                FOODCODE == 58106220 ~ 0.66,
                FOODCODE == 58106230 ~ 0.66,
                TRUE ~ D_CHEESE
            ),
            DISCFAT_OIL = case_when(
                FOODCODE %in% c(58106210, 58106220, 58106230) ~ 0.44,
                TRUE ~ DISCFAT_OIL
            ),
            DISCFAT_SOL = case_when(
                FOODCODE == 58106210 ~ 8.00,
                FOODCODE == 58106220 ~ 10.62,
                FOODCODE == 58106230 ~ 8.82,
                TRUE ~ DISCFAT_SOL
            ),
            ADD_SUG = case_when(
                FOODCODE %in% c(58106210, 58106220, 58106230) ~ 0.19,
                TRUE ~ ADD_SUG
            )
    )

    # part b: get juice data per 100 grams of food;
    ## rename variable
    ### if the column name contains "ModCode"
    if (("ModCode" %in% colnames(WJFRT)) == TRUE) {
        # rename the column names
        colnames(WJFRT)[1] <- c("FOODCODE")
        colnames(WJFRT)[2] <- c("MODCODE")

        # Merge the data frames using inner join
        MPED_PER_100_GRAM_4 <- inner_join(MPED_PER_100_GRAM_3, WJFRT, by = c("FOODCODE", "MODCODE"))
    } 
    ### if the column name does not contain "ModCode"
    else {
        # rename the column names
        colnames(WJFRT)[1] <- c("FOODCODE")

        # Merge the data frames using inner join
        MPED_PER_100_GRAM_4 <- inner_join(MPED_PER_100_GRAM_3, WJFRT, by = c("FOODCODE"))
    }
    
    # get demographic data for persons aged two and older
    DEMO_2 <- DEMO %>%
        filter(RIDAGEYR >= 1) %>% # persons aged 1 and older
        select(SEQN, RIDAGEYR, RIAGENDR, SDDSRVYR, SDMVPSU, SDMVSTRA) %>%
        arrange(SEQN)

    # start with the first day data calculation
    if (!is.null(NUTRIENT_PATH) & !is.null(NUTRIENT_IND_PATH)) {
        
        # load the NUTRIENT data
        if (is.character(NUTRIENT_PATH) == TRUE) {
            NUTRIENT = read_xpt(NUTRIENT_PATH)
        } else {
            NUTRIENT = NUTRIENT_PATH
        }

        # load the NUTRIENT_IND data
        if (is.character(NUTRIENT_IND_PATH) == TRUE) {
            NUTRIENT_IND = read_xpt(NUTRIENT_IND_PATH)
        } else {
            NUTRIENT_IND = NUTRIENT_IND_PATH
        }

        # stop if the user provides the individual-level data
        if ("DR1ILINE" %in% colnames(NUTRIENT) | "DRXILINE" %in% colnames(NUTRIENT)) {
            stop("Please use the population-level data for the first day data. The file name should contain: TOT")
        }

        # if NHANES 2001-2002 data is used as evidenced by the presence of DRDDRSTZ
        if ("DRDDRSTZ" %in% colnames(NUTRIENT)) {
            # rename the variables in NUTRIENT
            NUTRIENT = NUTRIENT %>%
                mutate(
                    DR1DRSTZ = DRDDRSTZ,
                    DR1TKCAL = DRXTKCAL,
                    DR1TSFAT = DRXTSFAT,
                    DR1TALCO = DRXTALCO,
                    DR1TSODI = DRDTSODI,
                    DR1TMFAT = DRXTMFAT,
                    DR1TPFAT = DRXTPFAT
                )
            
            # rename the variables in NUTRIENT_IND
            NUTRIENT_IND = NUTRIENT_IND %>%
                mutate(
                    DR1DRSTZ = DRDDRSTZ
                )
        } 
        # if NHANES 1999-2000 data is used as evidenced by the presence of DRXDRSTZ
        else if ("DRDDRSTS" %in% colnames(NUTRIENT)) {
            # rename the variables in NUTRIENT
            NUTRIENT = NUTRIENT %>%
                mutate(
                    DR1DRSTZ = DRDDRSTS,
                    DR1TKCAL = DRXTKCAL,
                    DR1TSFAT = DRXTSFAT,
                    DR1TALCO = DRXTALCO,
                    DR1TSODI = DRDTSODI,
                    DR1TMFAT = DRXTMFAT,
                    DR1TPFAT = DRXTPFAT
                )
            
            # rename the variables in NUTRIENT_IND
            NUTRIENT_IND = NUTRIENT_IND %>%
                mutate(
                    DR1DRSTZ = DRDDRSTS
                )
        }
        # check if DR1IFDCD is in the NUTRIENT_IND colnames
        if ("DR1IFDCD" %in% colnames(NUTRIENT_IND)){
            # get individual food intake data for people with reliable dietary recall status
            NUTRIENT_IND <- NUTRIENT_IND %>%
                mutate(
                    FOODCODE = 1*DR1IFDCD, # convert variable name and type
                    MODCODE = 1*DR1MC
                ) %>%
                filter(DR1DRSTZ == 1) # reliable dietary recall status

            
            # get individual total food intake for people with reliable recall status
            NUTRIENT_2 <- NUTRIENT %>%
                filter(DR1DRSTZ == 1) %>% # reliable dietary recall status
                arrange(SEQN)

            ## merge the two datasets
            # combine food intake and MPED plus WHOLE FRUIT data on a food level
            MPED_PER_100_GRAM_5 <- inner_join(NUTRIENT_IND, MPED_PER_100_GRAM_4, by = c("FOODCODE", "MODCODE"))

        } else if ("DRDIFDCD" %in% colnames(NUTRIENT_IND)){
            # get individual food intake data for people with reliable dietary recall status
            NUTRIENT_IND <- NUTRIENT_IND %>%
                mutate(
                    FOODCODE = 1*DRDIFDCD # convert variable name and type
                ) %>%
                filter(DR1DRSTZ == 1) # reliable dietary recall status
            

            # get individual total food intake for people with reliable recall status
            NUTRIENT_2 <- NUTRIENT %>%
                filter(DR1DRSTZ == 1) %>% # reliable dietary recall status
                arrange(SEQN)

            ## merge the two datasets
            # combine food intake and MPED plus WHOLE FRUIT data on a food level
            MPED_PER_100_GRAM_5 <- inner_join(NUTRIENT_IND, MPED_PER_100_GRAM_4, by = c("FOODCODE"), relationship = "many-to-many")
        }


        # convert individuals' food intake amounts from grams to MyPyramid equivalents
        # Get the column names
        column_names <- colnames(MPED_PER_100_GRAM_5)

        # Identify the columns between G_TOTAL and A_BEV
        start_index <- which(column_names == "G_TOTAL")
        end_index <- which(column_names == "A_BEV")
        selected_columns <- column_names[start_index:end_index]

        # Add WHOLEFRT and FRTJUICE to the list
        selected_columns <- c(selected_columns, "WHOLEFRT", "FRTJUICE")

        # Apply the conversion
        ## check if DR1IGRMS is in the MPED_PER_100_GRAM_5 colnames
        if ("DR1IGRMS" %in% colnames(MPED_PER_100_GRAM_5)){
            MPED_IND <- MPED_PER_100_GRAM_5 %>%
                mutate(across(all_of(selected_columns), ~ .x * (DR1IGRMS / 100)))
        } 
        ## check if DRXIGRMS is in the MPED_PER_100_GRAM_5 colnames
        else if ("DRXIGRMS" %in% colnames(MPED_PER_100_GRAM_5)){
            MPED_IND <- MPED_PER_100_GRAM_5 %>%
                mutate(across(all_of(selected_columns), ~ .x * (DRXIGRMS / 100)))
        }

        # arrange the data by SEQN
        MPED_IND_2 <- MPED_IND %>%
        arrange(SEQN)

        # calculate the sum of each food group for each individual
        MPED <- MPED_IND_2 %>%
        group_by(SEQN) %>%
        summarise_at(vars(selected_columns), sum, na.rm = TRUE)

        # combine nutrient and demographic data on a person level;
        COHORT = inner_join(NUTRIENT_2, DEMO_2, by = "SEQN")

        # combine all data on a person level;
        COHORT_2 = left_join(COHORT, MPED, by = "SEQN")

        # calculate the HEI2020 food group serving size / 1000 kcal
        COHORT_3 = COHORT_2 %>%
            dplyr::mutate(
                RIDAGEYR = RIDAGEYR,
                TOTALFRT_SERV = F_TOTAL / (DR1TKCAL / 1000),
                FRT_SERV = (F_CITMLB + F_OTHER) / (DR1TKCAL / 1000),
                VEG_SERV = (V_TOTAL + LEGUMES) / (DR1TKCAL / 1000),
                GREENNBEAN_SERV = (V_DRKGR + LEGUMES) / (DR1TKCAL / 1000),
                M_LEGUMES = LEGUMES*4,
                TOTALPRO_SERV = (M_MPF + M_EGG + M_NUTSD + M_SOY + M_LEGUMES) / (DR1TKCAL / 1000),
                SEAPLANTPRO_SERV = (M_FISH_HI + M_FISH_LO + M_NUTSD + M_SOY + M_LEGUMES) / (DR1TKCAL / 1000),
                WHOLEGRAIN_SERV = G_WHL / (DR1TKCAL / 1000),
                DAIRY_SERV = D_TOTAL / (DR1TKCAL / 1000),
                FATTYACID_SERV = case_when(
                    DR1TSFAT == 0 ~ 0,
                    TRUE ~ (DR1TMFAT + DR1TPFAT) / DR1TSFAT
                ),
                REFINEDGRAIN_SERV = G_NWHL / (DR1TKCAL / 1000),
                SODIUM_SERV = (DR1TSODI / 1000) / (DR1TKCAL / 1000),
                ADDEDSUGAR_SERV = ((ADD_SUG * 4 * 4) / DR1TKCAL) * 100,
                SATFAT_SERV = ((DR1TSFAT * 9) / DR1TKCAL) * 100,
                TOTALKCAL = DR1TKCAL
            )

        # use the HEI2020 generic function to calculate the HEI2020 total and component scores
        COHORT_4 = HEI2020(
            SERV_DATA = COHORT_3,
            RESPONDENTID = COHORT_3$SEQN,
            AGE = COHORT_3$RIDAGEYR,
            TOTALKCAL_HEI2020 = COHORT_3$TOTALKCAL,
            TOTALFRT_SERV_HEI2020 = COHORT_3$TOTALFRT_SERV,
            FRT_SERV_HEI2020 = COHORT_3$FRT_SERV,
            VEG_SERV_HEI2020 = COHORT_3$VEG_SERV,
            GREENNBEAN_SERV_HEI2020 = COHORT_3$GREENNBEAN_SERV,
            TOTALPRO_SERV_HEI2020 = COHORT_3$TOTALPRO_SERV,
            SEAPLANTPRO_SERV_HEI2020 = COHORT_3$SEAPLANTPRO_SERV,
            WHOLEGRAIN_SERV_HEI2020 = COHORT_3$WHOLEGRAIN_SERV,
            DAIRY_SERV_HEI2020 = COHORT_3$DAIRY_SERV,
            FATTYACID_SERV_HEI2020 = COHORT_3$FATTYACID_SERV,
            REFINEDGRAIN_SERV_HEI2020 = COHORT_3$REFINEDGRAIN_SERV,
            SODIUM_SERV_HEI2020 = COHORT_3$SODIUM_SERV,
            ADDEDSUGAR_SERV_HEI2020 = COHORT_3$ADDEDSUGAR_SERV,
            SATFAT_SERV_HEI2020 = COHORT_3$SATFAT_SERV
        )

        COHORT_4 = COHORT_4 %>%
            mutate(
                SEQN = RESPONDENTID,
                RIDAGEYR = AGE
            ) %>%
            select(SEQN, TOTALKCAL_HEI2020, RIDAGEYR, HEI2020_ALL:HEI2020_SATFAT)
    }

    # start with the second day data calculation
    if (!is.null(NUTRIENT_PATH2) & !is.null(NUTRIENT_IND_PATH2)) {

        # load the NUTRIENT data
        if (is.character(NUTRIENT_PATH2) == TRUE) {
            NUTRIENT2 = read_xpt(NUTRIENT_PATH2)
        } else {
            NUTRIENT2 = NUTRIENT_PATH2
        }


        # load the NUTRIENT_IND data
        if (is.character(NUTRIENT_IND_PATH2) == TRUE) {
            NUTRIENT_IND2 = read_xpt(NUTRIENT_IND_PATH2)
        } else {
            NUTRIENT_IND2 = NUTRIENT_IND_PATH2
        }


        if ("DR2ILINE" %in% colnames(NUTRIENT2) | "DRXILINE" %in% colnames(NUTRIENT2)) {
            stop("Please use the population-level data for the second day data. The file name should contain: TOT")
        }

        # if NHANES 2001-2002 data is used as evidenced by the presence of DRDDRSTZ
        if ("DRDDRSTZ" %in% colnames(NUTRIENT2)) {
            NUTRIENT2 = NUTRIENT2 %>%
                mutate(
                    DR2DRSTZ = DRDDRSTZ,
                    DR2TKCAL = DRXTKCAL,
                    DR2TSFAT = DRXTSFAT,
                    DR2TALCO = DRXTALCO,
                    DR2TSODI = DRDTSODI,
                    DR2TMFAT = DRXTMFAT,
                    DR2TPFAT = DRXTPFAT
                )
        }
        # if NHANES 1999-2000 data is used as evidenced by the presence of DRXDRSTZ
        else if ("DRDDRSTS" %in% colnames(NUTRIENT2)) {
            NUTRIENT2 = NUTRIENT2 %>%
                mutate(
                    DR2DRSTZ = DRDDRSTS,
                    DR2TKCAL = DRXTKCAL,
                    DR2TSFAT = DRXTSFAT,
                    DR2TALCO = DRXTALCO,
                    DR2TSODI = DRDTSODI,
                    DR2TMFAT = DRXTMFAT,
                    DR2TPFAT = DRXTPFAT
                )
        }


        # get individual food intake data for people with reliable dietary recall status
        NUTRIENT_IND2 <- NUTRIENT_IND2 %>%
            mutate(
                FOODCODE = 1*DR2IFDCD, # convert variable name and type
                MODCODE = 1*DR2MC
            ) %>%
            filter(DR2DRSTZ == 1) # reliable dietary recall status

        # get individual total food intake for people with reliable recall status
        NUTRIENT2_2 <- NUTRIENT2 %>%
            filter(DR2DRSTZ == 1) %>% # reliable dietary recall status
            arrange(SEQN)

        ## merge the two datasets
        # combine food intake and MPED plus WHOLE FRUIT data on a food level
        MPED_PER_100_GRAM2_5 <- inner_join(NUTRIENT_IND2, MPED_PER_100_GRAM_4, by = c("FOODCODE", "MODCODE"))

        # convert individuals' food intake amounts from grams to MyPyramid equivalents
        # Get the column names
        column_names <- colnames(MPED_PER_100_GRAM2_5)

        # Identify the columns between G_TOTAL and A_BEV
        start_index <- which(column_names == "G_TOTAL")
        end_index <- which(column_names == "A_BEV")
        selected_columns <- column_names[start_index:end_index]

        # Add WHOLEFRT and FRTJUICE to the list
        selected_columns <- c(selected_columns, "WHOLEFRT", "FRTJUICE")

        # Apply the conversion
        MPED_IND2 <- MPED_PER_100_GRAM2_5 %>%
            mutate(across(all_of(selected_columns), ~ .x * (DR2IGRMS / 100)))

        # arrange the data by SEQN
        MPED_IND2_2 <- MPED_IND2 %>%
            arrange(SEQN)

        # calculate the sum of each food group for each individual
        MPED2 <- MPED_IND2_2 %>%
            group_by(SEQN) %>%
            summarise_at(vars(selected_columns), sum, na.rm = TRUE)

        # combine NUTRIENT2 and demographic data on a person level;
        COHORT2 = inner_join(NUTRIENT2_2, DEMO_2, by = "SEQN")

        # combine all data on a person level;
        COHORT2_2 = left_join(COHORT2, MPED2, by = "SEQN")

        # calculate the HEI2020 food group serving size / 1000 kcal
        COHORT2_3 = COHORT2_2 %>%
            dplyr::mutate(
                RIDAGEYR = RIDAGEYR,
                TOTALFRT_SERV = F_TOTAL / (DR2TKCAL / 1000),
                FRT_SERV = (F_CITMLB + F_OTHER) / (DR2TKCAL / 1000),
                VEG_SERV = (V_TOTAL + LEGUMES) / (DR2TKCAL / 1000),
                GREENNBEAN_SERV = (V_DRKGR + LEGUMES) / (DR2TKCAL / 1000),
                M_LEGUMES = LEGUMES*4,
                TOTALPRO_SERV = (M_MPF + M_EGG + M_NUTSD + M_SOY + M_LEGUMES) / (DR2TKCAL / 1000),
                SEAPLANTPRO_SERV = (M_FISH_HI + M_FISH_LO + M_NUTSD + M_SOY + M_LEGUMES) / (DR2TKCAL / 1000),
                WHOLEGRAIN_SERV = G_WHL / (DR2TKCAL / 1000),
                DAIRY_SERV = D_TOTAL / (DR2TKCAL / 1000),
                FATTYACID_SERV = case_when(
                    DR2TSFAT == 0 ~ 0,
                    TRUE ~ (DR2TMFAT + DR2TPFAT) / DR2TSFAT
                ),
                REFINEDGRAIN_SERV = G_NWHL / (DR2TKCAL / 1000),
                SODIUM_SERV = (DR2TSODI / 1000) / (DR2TKCAL / 1000),
                ADDEDSUGAR_SERV = ((ADD_SUG * 4 * 4) / DR2TKCAL) * 100,
                SATFAT_SERV = ((DR2TSFAT * 9) / DR2TKCAL) * 100,
                TOTALKCAL = DR2TKCAL
            )

        # use the HEI2020 generic function to calculate the HEI2020 total and component scores
        COHORT2_4 = HEI2020(
            SERV_DATA = COHORT2_3,
            RESPONDENTID = COHORT2_3$SEQN,
            AGE = COHORT2_3$RIDAGEYR,
            TOTALKCAL_HEI2020 = COHORT2_3$TOTALKCAL,
            TOTALFRT_SERV_HEI2020 = COHORT2_3$TOTALFRT_SERV,
            FRT_SERV_HEI2020 = COHORT2_3$FRT_SERV,
            VEG_SERV_HEI2020 = COHORT2_3$VEG_SERV,
            GREENNBEAN_SERV_HEI2020 = COHORT2_3$GREENNBEAN_SERV,
            TOTALPRO_SERV_HEI2020 = COHORT2_3$TOTALPRO_SERV,
            SEAPLANTPRO_SERV_HEI2020 = COHORT2_3$SEAPLANTPRO_SERV,
            WHOLEGRAIN_SERV_HEI2020 = COHORT2_3$WHOLEGRAIN_SERV,
            DAIRY_SERV_HEI2020 = COHORT2_3$DAIRY_SERV,
            FATTYACID_SERV_HEI2020 = COHORT2_3$FATTYACID_SERV,
            REFINEDGRAIN_SERV_HEI2020 = COHORT2_3$REFINEDGRAIN_SERV,
            SODIUM_SERV_HEI2020 = COHORT2_3$SODIUM_SERV,
            ADDEDSUGAR_SERV_HEI2020 = COHORT2_3$ADDEDSUGAR_SERV,
            SATFAT_SERV_HEI2020 = COHORT2_3$SATFAT_SERV
        )

        COHORT2_4 = COHORT2_4 %>%
            mutate(
                SEQN = RESPONDENTID,
                RIDAGEYR = AGE
            ) %>%
            select(SEQN, TOTALKCAL_HEI2020, RIDAGEYR, HEI2020_ALL:HEI2020_SATFAT)

    }

    if (!is.null(NUTRIENT_PATH) & is.null(NUTRIENT_PATH2)) {
        return(COHORT_4)
    } 
    else if (is.null(NUTRIENT_PATH) & !is.null(NUTRIENT_PATH2)) {
        return(COHORT2_4)
    }  
    # merge two days data if they both exist
    else if (!is.null(NUTRIENT_PATH) & !is.null(NUTRIENT_PATH2)) {
        COHORT12 = inner_join(COHORT_4, COHORT2_4, by = "SEQN") %>%
            mutate(
                SEQN = SEQN,
                RIDAGEYR = RIDAGEYR.x,
                TOTALKCAL_HEI2020 = (TOTALKCAL_HEI2020.x + TOTALKCAL_HEI2020.y) / 2,
                HEI2020_ALL = (HEI2020_ALL.x + HEI2020_ALL.y) / 2,
                HEI2020_TOTALFRT = (HEI2020_TOTALFRT.x + HEI2020_TOTALFRT.y) / 2,
                HEI2020_FRT = (HEI2020_FRT.x + HEI2020_FRT.y) / 2,
                HEI2020_VEG = (HEI2020_VEG.x + HEI2020_VEG.y) / 2,
                HEI2020_GREENNBEAN = (HEI2020_GREENNBEAN.x + HEI2020_GREENNBEAN.y) / 2,
                HEI2020_TOTALPRO = (HEI2020_TOTALPRO.x + HEI2020_TOTALPRO.y) / 2,
                HEI2020_SEAPLANTPRO = (HEI2020_SEAPLANTPRO.x + HEI2020_SEAPLANTPRO.y) / 2,
                HEI2020_WHOLEGRAIN = (HEI2020_WHOLEGRAIN.x + HEI2020_WHOLEGRAIN.y) / 2,
                HEI2020_DAIRY = (HEI2020_DAIRY.x + HEI2020_DAIRY.y) / 2,
                HEI2020_FATTYACID = (HEI2020_FATTYACID.x + HEI2020_FATTYACID.y) / 2,
                HEI2020_REFINEDGRAIN = (HEI2020_REFINEDGRAIN.x + HEI2020_REFINEDGRAIN.y) / 2,
                HEI2020_SODIUM = (HEI2020_SODIUM.x + HEI2020_SODIUM.y) / 2,
                HEI2020_ADDEDSUGAR = (HEI2020_ADDEDSUGAR.x + HEI2020_ADDEDSUGAR.y) / 2,
                HEI2020_SATFAT = (HEI2020_SATFAT.x + HEI2020_SATFAT.y) / 2
            ) %>%
            dplyr::select(
                SEQN, TOTALKCAL_HEI2020, RIDAGEYR, HEI2020_ALL, HEI2020_TOTALFRT, HEI2020_FRT, HEI2020_VEG, HEI2020_GREENNBEAN,
                HEI2020_TOTALPRO, HEI2020_SEAPLANTPRO, HEI2020_WHOLEGRAIN, HEI2020_DAIRY,
                HEI2020_FATTYACID, HEI2020_REFINEDGRAIN, HEI2020_SODIUM, HEI2020_ADDEDSUGAR,
                HEI2020_SATFAT
            )
        return(COHORT12)
    }
    
}
