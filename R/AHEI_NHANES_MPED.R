#' AHEI_NHANES_MPED
#'
#' Calculate the AHEI for the NHANES_MPED data (before 2005, 1999-2004) within 1 step for day 1, day 2, or day 1 and 2 combined (age >= 2 only)
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
#' @param SSB_code The food code for sugar sweetened beverage, default is the SSB code from 17-18 FNDDS file.
#' @return The AHEI and its component scores and serving sizes
#' @examples
#' data("NHANES_20032004")
#' AHEI_NHANES_MPED(MPED_PER_100_GRAM_PATH = NHANES_20032004$MPED_PER_100_GRAM, WJFRT = NHANES_20032004$WJFRT, NUTRIENT_PATH = NHANES_20032004$NUTRIENT, NUTRIENT_IND_PATH = NHANES_20032004$NUTRIENT_IND, DEMO_PATH = NHANES_20032004$DEMO, NUTRIENT_PATH2 = NHANES_20032004$NUTRIENT2, NUTRIENT_IND_PATH2 = NHANES_20032004$NUTRIENT_IND2)
#' @export

AHEI_NHANES_MPED = function(MPED_PER_100_GRAM_PATH = NULL, WJFRT = NULL, NUTRIENT_PATH = NULL, NUTRIENT_IND_PATH = NULL, DEMO_PATH, NUTRIENT_PATH2 = NULL, NUTRIENT_IND_PATH2 = NULL, SSB_code = NULL) {

    # stop if the input data is not provided for any day
    if (is.null(NUTRIENT_PATH) & is.null(NUTRIENT_IND_PATH) & is.null(NUTRIENT_PATH2) & is.null(NUTRIENT_IND_PATH2)) {
        stop("Please provide the file path for the NUTRIENT data, day 1 or day 2 or day 1 and day 2.")
    }

    if (is.null(SSB_code)) {
        # load the SSB codes from 17-18 FNDDS file as default
        data("SSB_FNDDS_1718")
        SSB = unique(SSB_FNDDS_1718$`Food code`)
        print("Since no SSB code is provided, the default SSB code from 17-18 FNDDS file is used.")
    } else {
        # use the provided SSB code
        SSB = SSB_code
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
                    DR1TPFAT = DRXTPFAT,
                    DR1TP182 = DRXTP182,
                    DR1TP183 = DRXTP183,
                    DR1TP184 = DRXTP184,
                    DR1TP204 = DRXTP204,
                    DR1TP225 = DRXTP225,
                    DR1TP205 = DRXTP205,
                    DR1TP226 = DRXTP226
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
                    DR1TPFAT = DRXTPFAT,
                    DR1TP182 = DRXTP182,
                    DR1TP183 = DRXTP183,
                    DR1TP184 = DRXTP184,
                    DR1TP204 = DRXTP204,
                    DR1TP205 = DRXTP205,
                    DR1TP225 = DRXTP225,
                    DR1TP226 = DRXTP226
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
        
            # rename V_DPYEL to V_ORANGE
            colnames(MPED_PER_100_GRAM_5)[colnames(MPED_PER_100_GRAM_5) == "V_DPYEL"] <- "V_ORANGE"
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
        
        # Create the variable for added sugars from SSB
        MPED_IND_2 = MPED_IND_2 %>%
            dplyr::mutate(
                # create the variable for added sugars from SSB
                ADDED_SUGAR_SSB_SERV = case_when(
                    FOODCODE %in% SSB ~ ADD_SUG,
                    TRUE ~ 0
                )
            )
        
        # Add ADDED_SUGAR_SSB_SERV to the list
        selected_columns <- c(selected_columns, "ADDED_SUGAR_SSB_SERV")

        # calculate the sum of each food group for each individual
        MPED <- MPED_IND_2 %>%
            group_by(SEQN) %>%
            summarise(across(all_of(selected_columns), ~ sum(.x, na.rm = TRUE)))

        # combine nutrient and demographic data on a person level;
        COHORT = inner_join(NUTRIENT_2, DEMO_2, by = "SEQN")

        # combine all data on a person level;
        COHORT_2 = left_join(COHORT, MPED, by = "SEQN")

        # calculate the AHEI food group serving size / 1000 kcal
        COHORT_3 = COHORT_2 %>%
            dplyr::mutate(
                VEG_SERV = V_DRKGR + ((V_ORANGE + V_TOMATO + V_OTHER + V_STARCY) / 0.5),
                FRT_SERV = WHOLEFRT,
                WGRAIN_SERV = G_WHL / 0.035274,
                NUTSLEG_SERV = M_NUTSD + M_SOY + LEGUMES*4,
                PUFA_SERV = (((DR1TP182 + DR1TP183 + DR1TP184 + DR1TP204 + DR1TP225) * 9) / DR1TKCAL) * 100,
                N3FAT_SERV = (DR1TP205 + DR1TP226) * 1000,
                SSB_FRTJ_SERV = (ADDED_SUGAR_SSB_SERV * 4 / 26),
                REDPROC_MEAT_SERV = (M_FRANK / 1.5) + ((M_MEAT + M_ORGAN) / 4),
                SODIUM_SERV = DR1TSODI / (DR1TKCAL / 2000),
                ALCOHOL_SERV = A_BEV
            )

        # use the AHEI generic function to calculate the AHEI total and component scores
        COHORT_4 = AHEI(
            SERV_DATA = COHORT_3, 
            RESPONDENTID = COHORT_3$SEQN, 
            GENDER = COHORT_3$RIAGENDR, 
            TOTALKCAL_AHEI = COHORT_3$DR1TKCAL, 
            VEG_SERV_AHEI = COHORT_3$VEG_SERV, 
            FRT_SERV_AHEI = COHORT_3$FRT_SERV, 
            WGRAIN_SERV_AHEI = COHORT_3$WGRAIN_SERV, 
            NUTSLEG_SERV_AHEI = COHORT_3$NUTSLEG_SERV, 
            N3FAT_SERV_AHEI = COHORT_3$N3FAT_SERV, 
            PUFA_SERV_AHEI = COHORT_3$PUFA_SERV, 
            SSB_FRTJ_SERV_AHEI = COHORT_3$SSB_FRTJ_SERV, 
            REDPROC_MEAT_SERV_AHEI = COHORT_3$REDPROC_MEAT_SERV, 
            TRANS_SERV_AHEI = NA, 
            SODIUM_SERV_AHEI = COHORT_3$SODIUM_SERV, 
            ALCOHOL_SERV_AHEI = COHORT_3$ALCOHOL_SERV
        )

        COHORT_4 = COHORT_4 %>%
            dplyr::rename(
                SEQN = RESPONDENTID
            ) %>%
            dplyr::select(
                SEQN, AHEI_ALL, AHEI_NOETOH, AHEI_VEG, AHEI_FRT, AHEI_WGRAIN, AHEI_NUTSLEG, AHEI_N3FAT,
                AHEI_PUFA, AHEI_SSB_FRTJ, AHEI_REDPROC_MEAT, AHEI_SODIUM, AHEI_ALCOHOL
            )
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
        
        # Create the variable for added sugars from SSB
        MPED_IND2_2 = MPED_IND2_2 %>%
            dplyr::mutate(
                # create the variable for added sugars from SSB
                ADDED_SUGAR_SSB_SERV = case_when(
                    DR2IFDCD %in% SSB ~ ADD_SUG,
                    TRUE ~ 0
                )
            )

        # Add ADDED_SUGAR_SSB_SERV to the list
        selected_columns <- c(selected_columns, "ADDED_SUGAR_SSB_SERV")

        # calculate the sum of each food group for each individual
        MPED2 <- MPED_IND2_2 %>%
            group_by(SEQN) %>%
            summarise(across(all_of(selected_columns), ~ sum(.x, na.rm = TRUE)))

        # combine NUTRIENT2 and demographic data on a person level;
        COHORT2 = inner_join(NUTRIENT2_2, DEMO_2, by = "SEQN")

        # combine all data on a person level;
        COHORT2_2 = left_join(COHORT2, MPED2, by = "SEQN")

        # calculate the AHEI food group serving size / 1000 kcal
        COHORT2_3 = COHORT2_2 %>%
            dplyr::mutate(
                VEG_SERV = V_DRKGR + ((V_ORANGE + V_TOMATO + V_OTHER + V_STARCY) / 0.5),
                FRT_SERV = WHOLEFRT,
                WGRAIN_SERV = G_WHL / 0.035274,
                NUTSLEG_SERV = M_NUTSD + M_SOY + LEGUMES*4,
                PUFA_SERV = (((DR2TP182 + DR2TP183 + DR2TP184 + DR2TP204 + DR2TP225) * 9) / DR2TKCAL) * 100,
                N3FAT_SERV = (DR2TP205 + DR2TP226) * 1000,
                SSB_FRTJ_SERV = (ADDED_SUGAR_SSB_SERV * 4 / 26),
                REDPROC_MEAT_SERV = (M_FRANK / 1.5) + ((M_MEAT + M_ORGAN) / 4),
                SODIUM_SERV = DR2TSODI / (DR2TKCAL / 2000),
                ALCOHOL_SERV = A_BEV
            )

        # use the AHEI generic function to calculate the AHEI total and component scores
        COHORT2_4 = AHEI(
            SERV_DATA = COHORT2_3,
            RESPONDENTID = COHORT2_3$SEQN,
            GENDER = COHORT2_3$RIAGENDR,
            TOTALKCAL_AHEI = COHORT2_3$DR2TKCAL,
            VEG_SERV_AHEI = COHORT2_3$VEG_SERV,
            FRT_SERV_AHEI = COHORT2_3$FRT_SERV,
            WGRAIN_SERV_AHEI = COHORT2_3$WGRAIN_SERV,
            NUTSLEG_SERV_AHEI = COHORT2_3$NUTSLEG_SERV,
            N3FAT_SERV_AHEI = COHORT2_3$N3FAT_SERV,
            PUFA_SERV_AHEI = COHORT2_3$PUFA_SERV,
            SSB_FRTJ_SERV_AHEI = COHORT2_3$SSB_FRTJ_SERV,
            REDPROC_MEAT_SERV_AHEI = COHORT2_3$REDPROC_MEAT_SERV,
            TRANS_SERV_AHEI = NA,
            SODIUM_SERV_AHEI = COHORT2_3$SODIUM_SERV,
            ALCOHOL_SERV_AHEI = COHORT2_3$ALCOHOL_SERV
        )

        COHORT2_4 = COHORT2_4 %>%
            dplyr::rename(
                SEQN = RESPONDENTID
            ) %>%
            dplyr::select(
                SEQN, AHEI_ALL, AHEI_NOETOH, AHEI_VEG, AHEI_FRT, AHEI_WGRAIN, AHEI_NUTSLEG, AHEI_N3FAT,
                AHEI_PUFA, AHEI_SSB_FRTJ, AHEI_REDPROC_MEAT, AHEI_SODIUM, AHEI_ALCOHOL
            )
    }

    if (!is.null(NUTRIENT_PATH) & is.null(NUTRIENT_PATH2)) {
        return(COHORT_4)
    } else if (is.null(NUTRIENT_PATH) & !is.null(NUTRIENT_PATH2)) {
        return(COHORT2_4)
    }
    # merge two days data if they both exist
    else if (!is.null(NUTRIENT_PATH) & !is.null(NUTRIENT_PATH2)) {
        COHORT12 = inner_join(COHORT_4, COHORT2_4, by = "SEQN") %>%
            dplyr::mutate(
                AHEI_ALL = (AHEI_ALL.x + AHEI_ALL.y) / 2,
                AHEI_NOETOH = (AHEI_NOETOH.x + AHEI_NOETOH.y) / 2,
                AHEI_VEG = (AHEI_VEG.x + AHEI_VEG.y) / 2,
                AHEI_FRT = (AHEI_FRT.x + AHEI_FRT.y) / 2,
                AHEI_WGRAIN = (AHEI_WGRAIN.x + AHEI_WGRAIN.y) / 2,
                AHEI_NUTSLEG = (AHEI_NUTSLEG.x + AHEI_NUTSLEG.y) / 2,
                AHEI_N3FAT = (AHEI_N3FAT.x + AHEI_N3FAT.y) / 2,
                AHEI_PUFA = (AHEI_PUFA.x + AHEI_PUFA.y) / 2,
                AHEI_SSB_FRTJ = (AHEI_SSB_FRTJ.x + AHEI_SSB_FRTJ.y) / 2,
                AHEI_REDPROC_MEAT = (AHEI_REDPROC_MEAT.x + AHEI_REDPROC_MEAT.y) / 2,
                AHEI_SODIUM = (AHEI_SODIUM.x + AHEI_SODIUM.y) / 2,
                AHEI_ALCOHOL = (AHEI_ALCOHOL.x + AHEI_ALCOHOL.y) / 2
            ) %>%
            dplyr::select(
                SEQN, AHEI_ALL, AHEI_NOETOH, AHEI_VEG, AHEI_FRT, AHEI_WGRAIN, AHEI_NUTSLEG, AHEI_N3FAT,
                AHEI_PUFA, AHEI_SSB_FRTJ, AHEI_REDPROC_MEAT, AHEI_SODIUM, AHEI_ALCOHOL
            )
        return(COHORT12)
    }
}
