#' HEI2015_NHANES_MPED
#'
#' Calculate the HEI2015 for the NHANES_MPED data (before 2005, 1999-2004) within 1 step for day 1, day 2, or day 1 and 2 combined (age >= 2 only)
#' @import dplyr
#' @import readr
#' @import haven
#' @param MPED_PER_100_GRAM_PATH The file path for the MPED per 100 gram data for the day 1 and day 2 data. The file name should be like: equiv0304.sas7bdat
#' @param WJFRT The file path for the WJFRT data for the day 1 and day2 data. The file name should be like: wjfrt.sas7bdat
#' @param NUTRIENT_PATH The file path for the NUTRIENT data for the day 1 data. The file name should be like: DR1TOT_J.XPT or DRXTOT_B.XPT
#' @param NUTRIENT_IND_PATH The file path for the NUTRIENT_IND data for the day 1 data. The file name should be like: DR1IFF_J.XPT
#' @param DEMO_PATH The file path for the DEMOGRAPHIC data. The file name should be like: DEMO_J.XPT
#' @param NUTRIENT_PATH2 The file path for the NUTRIENT2 data for the day 2 data. The file name should be like: DR2TOT_J.XPT
#' @param NUTRIENT_IND_PATH2 The file path for the NUTRIENT_IND2 data for the day 2 data The file name should be like: DR2IFF_J.XPT
#' @return The HEI2015 and its component scores and serving sizes
#' @examples
#' data("NHANES_20032004")
#' HEI2015_NHANES_MPED(MPED_PER_100_GRAM_PATH = NHANES_20032004$MPED_PER_100_GRAM, WJFRT = NHANES_20032004$WJFRT, NUTRIENT_PATH = NHANES_20032004$NUTRIENT, NUTRIENT_IND_PATH = NHANES_20032004$NUTRIENT_IND, DEMO_PATH = NHANES_20032004$DEMO, NUTRIENT_PATH2 = NHANES_20032004$NUTRIENT2, NUTRIENT_IND_PATH2 = NHANES_20032004$NUTRIENT_IND2)
#' @export

HEI2015_NHANES_MPED = function(MPED_PER_100_GRAM_PATH = NULL, WJFRT = NULL, NUTRIENT_PATH = NULL, NUTRIENT_IND_PATH = NULL, DEMO_PATH, NUTRIENT_PATH2 = NULL, NUTRIENT_IND_PATH2 = NULL) {
    ## Create variables needed for HEI2015 calculation
    HEI2015_MIN = 0
    HEI2015_MAX1 = 5
    HEI2015_MAX2 = 10

    HEI2015_HEALTHY1 = function(actual, min, max) {
        case_when(
            actual >= max ~ HEI2015_MAX1,
            actual <= min ~ HEI2015_MIN,
            TRUE ~ HEI2015_MIN + (actual - min) * HEI2015_MAX1 / (max - min)
        )
    }

    HEI2015_HEALTHY2 = function(actual, min, max) {
        case_when(
            actual >= max ~ HEI2015_MAX2,
            actual <= min ~ HEI2015_MIN,
            TRUE ~ HEI2015_MIN + (actual - min) * HEI2015_MAX2 / (max - min)
        )
    }

    HEI2015_UNHEALTHY = function(actual, min, max) {
        case_when(
            actual >= min ~ HEI2015_MIN,
            actual <= max ~ HEI2015_MAX2,
            TRUE ~ HEI2015_MIN + (actual - min) * HEI2015_MAX2 / (max - min)
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
        if (grepl("sas", MPED_PER_100_GRAM_PATH) == TRUE) {
            MPED_PER_100_GRAM = read_sas(MPED_PER_100_GRAM_PATH)
        }
        # does not contain "sas"
        else {
            # Column widths - derived from the character positions
            widths <- c(8, 1, 6, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8)

            # Column names
            col_names <- c(
                "DRDIFDCD", "EQUIVFLAG", "MODCODE", "G_TOTAL", "G_WHL", "G_NWHL",
                "V_TOTAL", "V_DRKGR", "V_DPYEL", "V_POTATO", "V_STARCY", "V_TOMATO",
                "V_OTHER", "F_TOTAL", "F_CITMLB", "F_OTHER", "D_TOTAL", "D_MILK",
                "D_YOGURT", "D_CHEESE", "M_MPF", "M_MEAT", "M_ORGAN", "M_FRANK",
                "M_POULT", "M_FISH_HI", "M_FISH_LO", "M_EGG", "M_SOY", "M_NUTSD",
                "LEGUMES", "DISCFAT_OIL", "DISCFAT_SOL", "ADD_SUG", "A_BEV"
            )

            # Read the file with fixed width format
            MPED_PER_100_GRAM <- read.fwf(MPED_PER_100_GRAM_PATH, widths = widths, header = FALSE)
            # Assign the column names
            names(MPED_PER_100_GRAM) <- col_names
        }
    } else {
        MPED_PER_100_GRAM = MPED_PER_100_GRAM_PATH
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

    if (("DRDIMC" %in% colnames(MPED_PER_100_GRAM)) == TRUE) {
        # Rename variables
        MPED_PER_100_GRAM = MPED_PER_100_GRAM %>%
            mutate(
                FOODCODE = 1 * DRDIFDCD,
                MODCODE = 1 * DRDIMC
            )
    } else {
        # Rename variables
        MPED_PER_100_GRAM = MPED_PER_100_GRAM %>%
            mutate(
                FOODCODE = 1 * DRDIFDCD
            )
    }

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
                FOODCODE == 11310000 ~ round(100 * (1 / 244), 3),
                FOODCODE == 11320000 ~ round(100 * (1 / 245), 3),
                FOODCODE == 11321000 ~ round(100 * (1 / 240), 3),
                FOODCODE == 11330000 ~ round(100 * (1 / 245), 3),
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

        # update the column name from "frtjuice" to "FRTJUICE" and "wholefrt" to "WHOLEFRT"
        if ("frtjuice" %in% colnames(WJFRT) & "wholefrt" %in% colnames(WJFRT)) {
            colnames(WJFRT)[2] <- c("FRTJUICE")
            colnames(WJFRT)[3] <- c("WHOLEFRT")
        }

        # Merge the data frames using inner join
        MPED_PER_100_GRAM_4 <- inner_join(MPED_PER_100_GRAM_3, WJFRT, by = c("FOODCODE"))
    }

    # get demographic data for persons aged two and older
    DEMO_2 <- DEMO %>%
        filter(RIDAGEYR >= 2) %>% # persons aged 2 and older
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
        if ("DR1IFDCD" %in% colnames(NUTRIENT_IND)) {
            # get individual food intake data for people with reliable dietary recall status
            NUTRIENT_IND <- NUTRIENT_IND %>%
                mutate(
                    FOODCODE = 1 * DR1IFDCD, # convert variable name and type
                    MODCODE = 1 * DR1MC
                ) %>%
                filter(DR1DRSTZ == 1) # reliable dietary recall status


            # get individual total food intake for people with reliable recall status
            NUTRIENT_2 <- NUTRIENT %>%
                filter(DR1DRSTZ == 1) %>% # reliable dietary recall status
                arrange(SEQN)

            ## merge the two datasets
            # combine food intake and MPED plus WHOLE FRUIT data on a food level
            MPED_PER_100_GRAM_5 <- inner_join(NUTRIENT_IND, MPED_PER_100_GRAM_4, by = c("FOODCODE", "MODCODE"))
        } else if ("DRDIFDCD" %in% colnames(NUTRIENT_IND)) {
            # get individual food intake data for people with reliable dietary recall status
            NUTRIENT_IND <- NUTRIENT_IND %>%
                mutate(
                    FOODCODE = 1 * DRDIFDCD # convert variable name and type
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
        if ("DR1IGRMS" %in% colnames(MPED_PER_100_GRAM_5)) {
            MPED_IND <- MPED_PER_100_GRAM_5 %>%
                mutate(across(all_of(selected_columns), ~ .x * (DR1IGRMS / 100)))
        }
        ## check if DRXIGRMS is in the MPED_PER_100_GRAM_5 colnames
        else if ("DRXIGRMS" %in% colnames(MPED_PER_100_GRAM_5)) {
            MPED_IND <- MPED_PER_100_GRAM_5 %>%
                mutate(across(all_of(selected_columns), ~ .x * (DRXIGRMS / 100)))
        }

        # arrange the data by SEQN
        MPED_IND_2 <- MPED_IND %>%
            arrange(SEQN)

        # calculate the sum of each food group for each individual
        MPED <- MPED_IND_2 %>%
            group_by(SEQN) %>%
            summarise(across(all_of(selected_columns), sum, .names = "{.col}"), na.rm = TRUE)

        # combine nutrient and demographic data on a person level;
        COHORT = inner_join(NUTRIENT_2, DEMO_2, by = "SEQN")

        # combine all data on a person level;
        COHORT_2 = left_join(COHORT, MPED, by = "SEQN")

        # calculate the HEI2015 food group serving size / 1000 kcal
        COHORT_3 = COHORT_2 %>%
            dplyr::mutate(
                RIDAGEYR = RIDAGEYR,
                TOTALFRT_SERV = F_TOTAL,
                FRT_SERV = WHOLEFRT,
                VEG_SERV = V_TOTAL + LEGUMES,
                GREENNBEAN_SERV = V_DRKGR + LEGUMES,
                M_LEGUMES = LEGUMES * 4,
                TOTALPRO_SERV = M_MPF + M_EGG + M_NUTSD + M_SOY + M_LEGUMES,
                SEAPLANTPRO_SERV = M_FISH_HI + M_FISH_LO + M_NUTSD + M_SOY + M_LEGUMES,
                WHOLEGRAIN_SERV = G_WHL,
                DAIRY_SERV = D_TOTAL,
                FATTYACID_SERV = case_when(
                    DR1TSFAT == 0 ~ 0,
                    TRUE ~ (DR1TMFAT + DR1TPFAT) / DR1TSFAT
                ),
                REFINEDGRAIN_SERV = G_NWHL,
                SODIUM_SERV = DR1TSODI / 1000,
                ADDEDSUGAR_SERV = ((ADD_SUG * 4 * 4) / DR1TKCAL) * 100,
                SATFAT_SERV = ((DR1TSFAT * 9) / DR1TKCAL) * 100,
                TOTALKCAL = DR1TKCAL
            )

        # use the HEI2015 generic function to calculate the HEI2015 total and component scores
        COHORT_4 = HEI2015(
            SERV_DATA = COHORT_3,
            RESPONDENTID = COHORT_3$SEQN,
            TOTALKCAL_HEI2015 = COHORT_3$TOTALKCAL,
            TOTALFRT_SERV_HEI2015 = COHORT_3$TOTALFRT_SERV,
            FRT_SERV_HEI2015 = COHORT_3$FRT_SERV,
            VEG_SERV_HEI2015 = COHORT_3$VEG_SERV,
            GREENNBEAN_SERV_HEI2015 = COHORT_3$GREENNBEAN_SERV,
            TOTALPRO_SERV_HEI2015 = COHORT_3$TOTALPRO_SERV,
            SEAPLANTPRO_SERV_HEI2015 = COHORT_3$SEAPLANTPRO_SERV,
            WHOLEGRAIN_SERV_HEI2015 = COHORT_3$WHOLEGRAIN_SERV,
            DAIRY_SERV_HEI2015 = COHORT_3$DAIRY_SERV,
            FATTYACID_SERV_HEI2015 = COHORT_3$FATTYACID_SERV,
            REFINEDGRAIN_SERV_HEI2015 = COHORT_3$REFINEDGRAIN_SERV,
            SODIUM_SERV_HEI2015 = COHORT_3$SODIUM_SERV,
            ADDEDSUGAR_SERV_HEI2015 = COHORT_3$ADDEDSUGAR_SERV,
            SATFAT_SERV_HEI2015 = COHORT_3$SATFAT_SERV
        )

        COHORT_4 = COHORT_4 %>%
            mutate(
                SEQN = RESPONDENTID
            ) %>%
            select(SEQN, TOTALKCAL_HEI2015, HEI2015_ALL:HEI2015_SATFAT)
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
                FOODCODE = 1 * DR2IFDCD, # convert variable name and type
                MODCODE = 1 * DR2MC
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

        # calculate the HEI2015 food group serving size / 1000 kcal
        COHORT2_3 = COHORT2_2 %>%
            dplyr::mutate(
                RIDAGEYR = RIDAGEYR,
                TOTALFRT_SERV = F_TOTAL,
                FRT_SERV = WHOLEFRT,
                VEG_SERV = V_TOTAL + LEGUMES,
                GREENNBEAN_SERV = V_DRKGR + LEGUMES,
                M_LEGUMES = LEGUMES * 4,
                TOTALPRO_SERV = M_MPF + M_EGG + M_NUTSD + M_SOY + M_LEGUMES,
                SEAPLANTPRO_SERV = M_FISH_HI + M_FISH_LO + M_NUTSD + M_SOY + M_LEGUMES,
                WHOLEGRAIN_SERV = G_WHL,
                DAIRY_SERV = D_TOTAL,
                FATTYACID_SERV = case_when(
                    DR2TSFAT == 0 ~ 0,
                    TRUE ~ (DR2TMFAT + DR2TPFAT) / DR2TSFAT
                ),
                REFINEDGRAIN_SERV = G_NWHL,
                SODIUM_SERV = DR2TSODI / 1000,
                ADDEDSUGAR_SERV = ((ADD_SUG * 4 * 4) / DR2TKCAL) * 100,
                SATFAT_SERV = ((DR2TSFAT * 9) / DR2TKCAL) * 100,
                TOTALKCAL = DR2TKCAL
            )

        # use the HEI2015 generic function to calculate the HEI2015 total and component scores
        COHORT2_4 = HEI2015(
            SERV_DATA = COHORT2_3,
            RESPONDENTID = COHORT2_3$SEQN,
            TOTALKCAL_HEI2015 = COHORT2_3$TOTALKCAL,
            TOTALFRT_SERV_HEI2015 = COHORT2_3$TOTALFRT_SERV,
            FRT_SERV_HEI2015 = COHORT2_3$FRT_SERV,
            VEG_SERV_HEI2015 = COHORT2_3$VEG_SERV,
            GREENNBEAN_SERV_HEI2015 = COHORT2_3$GREENNBEAN_SERV,
            TOTALPRO_SERV_HEI2015 = COHORT2_3$TOTALPRO_SERV,
            SEAPLANTPRO_SERV_HEI2015 = COHORT2_3$SEAPLANTPRO_SERV,
            WHOLEGRAIN_SERV_HEI2015 = COHORT2_3$WHOLEGRAIN_SERV,
            DAIRY_SERV_HEI2015 = COHORT2_3$DAIRY_SERV,
            FATTYACID_SERV_HEI2015 = COHORT2_3$FATTYACID_SERV,
            REFINEDGRAIN_SERV_HEI2015 = COHORT2_3$REFINEDGRAIN_SERV,
            SODIUM_SERV_HEI2015 = COHORT2_3$SODIUM_SERV,
            ADDEDSUGAR_SERV_HEI2015 = COHORT2_3$ADDEDSUGAR_SERV,
            SATFAT_SERV_HEI2015 = COHORT2_3$SATFAT_SERV
        )

        COHORT2_4 = COHORT2_4 %>%
            mutate(
                SEQN = RESPONDENTID
            ) %>%
            select(SEQN, TOTALKCAL_HEI2015, HEI2015_ALL:HEI2015_SATFAT)
    }

    if (!is.null(NUTRIENT_PATH) & is.null(NUTRIENT_PATH2)) {
        return(COHORT_4)
    } else if (is.null(NUTRIENT_PATH) & !is.null(NUTRIENT_PATH2)) {
        return(COHORT2_4)
    }
    # merge two days data if they both exist
    else if (!is.null(NUTRIENT_PATH) & !is.null(NUTRIENT_PATH2)) {
        COHORT12 = inner_join(COHORT_4, COHORT2_4, by = "SEQN") %>%
            mutate(
                SEQN = SEQN,
                TOTALKCAL_HEI2015 = (TOTALKCAL_HEI2015.x + TOTALKCAL_HEI2015.y) / 2,
                HEI2015_ALL = (HEI2015_ALL.x + HEI2015_ALL.y) / 2,
                HEI2015_TOTALFRT = (HEI2015_TOTALFRT.x + HEI2015_TOTALFRT.y) / 2,
                HEI2015_FRT = (HEI2015_FRT.x + HEI2015_FRT.y) / 2,
                HEI2015_VEG = (HEI2015_VEG.x + HEI2015_VEG.y) / 2,
                HEI2015_GREENNBEAN = (HEI2015_GREENNBEAN.x + HEI2015_GREENNBEAN.y) / 2,
                HEI2015_TOTALPRO = (HEI2015_TOTALPRO.x + HEI2015_TOTALPRO.y) / 2,
                HEI2015_SEAPLANTPRO = (HEI2015_SEAPLANTPRO.x + HEI2015_SEAPLANTPRO.y) / 2,
                HEI2015_WHOLEGRAIN = (HEI2015_WHOLEGRAIN.x + HEI2015_WHOLEGRAIN.y) / 2,
                HEI2015_DAIRY = (HEI2015_DAIRY.x + HEI2015_DAIRY.y) / 2,
                HEI2015_FATTYACID = (HEI2015_FATTYACID.x + HEI2015_FATTYACID.y) / 2,
                HEI2015_REFINEDGRAIN = (HEI2015_REFINEDGRAIN.x + HEI2015_REFINEDGRAIN.y) / 2,
                HEI2015_SODIUM = (HEI2015_SODIUM.x + HEI2015_SODIUM.y) / 2,
                HEI2015_ADDEDSUGAR = (HEI2015_ADDEDSUGAR.x + HEI2015_ADDEDSUGAR.y) / 2,
                HEI2015_SATFAT = (HEI2015_SATFAT.x + HEI2015_SATFAT.y) / 2
            ) %>%
            dplyr::select(
                SEQN, TOTALKCAL_HEI2015, HEI2015_ALL, HEI2015_TOTALFRT, HEI2015_FRT, HEI2015_VEG, HEI2015_GREENNBEAN,
                HEI2015_TOTALPRO, HEI2015_SEAPLANTPRO, HEI2015_WHOLEGRAIN, HEI2015_DAIRY,
                HEI2015_FATTYACID, HEI2015_REFINEDGRAIN, HEI2015_SODIUM, HEI2015_ADDEDSUGAR,
                HEI2015_SATFAT
            )
        return(COHORT12)
    }
}
