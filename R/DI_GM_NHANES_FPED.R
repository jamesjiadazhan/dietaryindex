#' DI_GM_NHANES_FPED
#'
#' Calculate the Dietary Index for Gut Microbiota using a binary cutoff for the NHANES_FPED data (after 2005) within 1 step
#' @import dplyr
#' @import readr
#' @import haven
#' @param FPED_IND_PATH The file path for the FPED IND data. The file name should be like: fped_dr1iff.sas7bdat
#' @param NUTRIENT_IND_PATH The file path for the NUTRIENT IND data. The file name should be like: DR1IFF_J
#' @param FPED_IND_PATH2 The file path for the FPED IND data for day 2. The file name should be like: fped_dr1iff.sas7bdat
#' @param NUTRIENT_IND_PATH2 The file path for the NUTRIENT IND data for day 2. The file name should be like: DR2IFF_J
#' @param AVOCADO_CODE The code for avocado in the FPED data. The default food codes are from the FPED data for NHANES 2017-2018
#' @param BROCCOLI_CODE The code for broccoli in the FPED data. The default food codes are from the FPED data for NHANES 2017-2018
#' @param CHICKPEA_CODE The code for chickpea in the FPED data. The default food codes are from the FPED data for NHANES 2017-2018
#' @param COFFEE_CODE The code for coffee in the FPED data. The default food codes are from the FPED data for NHANES 2017-2018
#' @param CRANBERRY_CODE The code for cranberry in the FPED data. The default food codes are from the FPED data for NHANES 2017-2018
#' @param FERMENT_DAIRY_CODE The code for fermented dairy in the FPED data. The default food codes are from the FPED data for NHANES 2017-2018
#' @param GREEN_TEA_CODE The code for green tea in the FPED data. The default food codes are from the FPED data for NHANES 2017-2018
#' @param SOYBEAN_CODE The code for soybean foods in the FPED data. The default food codes are from the FPED data for NHANES 2017-2018
#' @return The DI_GM and its component scores and serving sizes
#' @examples
#' data("NHANES_20172018")
#' DI_GM_NHANES_FPED(FPED_IND_PATH = NHANES_20172018$FPED_IND, NUTRIENT_IND_PATH = NHANES_20172018$NUTRIENT_IND, FPED_IND_PATH2 = NHANES_20172018$FPED_IND2, NUTRIENT_IND_PATH2 = NHANES_20172018$NUTRIENT_IND2)
#' @export

DI_GM_NHANES_FPED = function(FPED_IND_PATH = NULL, NUTRIENT_IND_PATH = NULL, FPED_IND_PATH2 = NULL, NUTRIENT_IND_PATH2 = NULL, AVOCADO_CODE = NULL, BROCCOLI_CODE = NULL, CHICKPEA_CODE = NULL, COFFEE_CODE = NULL, CRANBERRY_CODE = NULL, FERMENTED_DAIRY_CODE = NULL, GREEN_TEA_CODE = NULL, SOYBEAN_CODE = NULL) {
    # stop if the input data is not provided for any day
    if (is.null(FPED_IND_PATH) & is.null(NUTRIENT_IND_PATH) & is.null(FPED_IND_PATH2) & is.null(NUTRIENT_IND_PATH2)) {
        stop("Please provide the file path for the FPED and NUTRIENT data, day 1 or day 2 or day 1 and day 2.")
    }

    if (is.null(AVOCADO_CODE)) {
        # load the default AVOCADO codes 
        AVOCADO = c(63105010)
        message("The default food codes for avocado from 17-18 FNDDS file is used.")
    } else {
        # use the provided AVOCADO code
        AVOCADO = AVOCADO_CODE
    }

    # if no BROCCOLI code is provided, use the default BROCCOLI codes
    if (is.null(BROCCOLI_CODE)) {
        # load the 
        BROCCOLI = c(72103000, 72103030, 72201100, 72201190, 72201211, 72201212, 72201220, 72201221, 72201222, 72201223, 72201224, 72201226, 72201227, 72203000, 72203070)  
        message("The default food codes for broccoli from 17-18 FNDDS file is used.")
    } else {
        # use the provided BROCCOLI code
        BROCCOLI = BROCCOLI_CODE
    }

    if (is.null(CHICKPEA_CODE)) {
        CHICKPEA = c(41301990, 41302010, 41302020, 41302040, 41302080, 41302110)
        message("The default food codes for chickpea from 17-18 FNDDS file is used.")
    } else {
        # use the provided CHICKPEA code
        CHICKPEA = CHICKPEA_CODE
    }

    if (is.null(COFFEE_CODE)) {
        COFFEE = c(92100000, 92100500, 92101000, 92101500, 92101600, 92101610, 92101630, 92101700, 92101800, 92101810, 
                92101820, 92101850, 92101851, 92101900, 92101901, 92101903, 92101904, 92101905, 92101906, 92101910, 
                92101911, 92101913, 92101917, 92101918, 92101919, 92101920, 92101921, 92101923, 92101925, 92101926, 
                92101928, 92101930, 92101931, 92101933, 92101935, 92101936, 92101938, 92101950, 92101955, 92101960, 
                92101965, 92101970, 92101975, 92102000, 92102010, 92102020, 92102030, 92102040, 92102050, 92102060, 
                92102070, 92102080, 92102090, 92102100, 92102110, 92102400, 92102401, 92102450, 92102500, 92102501, 
                92102502, 92102503, 92102504, 92102505, 92102510, 92102511, 92102512, 92102513, 92102514, 92102515, 
                92102600, 92102601, 92102602, 92102610, 92102611, 92102612, 92103000, 92104000, 92111000, 92111010, 
                92114000, 92121000, 92121001, 92121010, 92121020, 92121030, 92121040, 92121041, 92121050, 92130000, 
                92130001, 92130005, 92130006, 92130010, 92130011, 92130020, 92130021, 92130030, 92130031, 92152000, 
                92152010, 92161000, 92161001, 92161002, 92162000, 92162001, 92162002, 92171000, 92171010, 92201010)
        message("The default food codes for coffee from 17-18 FNDDS file is used.")
    } else {
        # use the provided COFFEE code
        COFFEE = COFFEE_CODE
    }

    if (is.null(CRANBERRY_CODE)) {
        CRANBERRY = c(62109100, 63207010, 63207110)
        message("The default food codes for cranberry from 17-18 FNDDS file is used.")
    } else {
        # use the provided CRANBERRY code
        CRANBERRY = CRANBERRY_CODE
    }

    if (is.null(FERMENTED_DAIRY_CODE)) {
        FERMENTED_DAIRY = c(11400000, 11400010, 11410000, 11411010, 11411100, 11411200, 11411300, 11411390, 11411400, 11411410,
                        11411420, 11430000, 11431000, 11432000, 11433000, 11433990, 11434000, 11434010, 11434020, 11434090,
                        11434100, 11434200, 11434300, 11435000, 11435010, 11435020, 11435030, 11435100, 11436000, 11446000,
                        11480010, 11480020, 11480030, 11480040, 42401100, 67250100, 67250150, 67404070, 67404300, 67404500,
                        67408500, 67413700, 67430500, 14010000, 14101010, 14102010, 14103010, 14103020, 14104100, 14104110,
                        14104115, 14104200, 14104250, 14104400, 14104600, 14104700, 14105010, 14105200, 14106010, 14106200,
                        14106500, 14107010, 14107030, 14107040, 14107060, 14107200, 14107250, 14108010, 14108015, 14108020,
                        14108060, 14108200, 14108400, 14108420, 14109010, 14109020, 14109030, 14109040, 14110010, 14120010,
                        14120020, 14131000, 14200100, 14201010, 14201200, 14201500, 14202010, 14202020, 14203010, 14203020,
                        14203510, 14204010, 14204020, 14206010, 14207010, 14410100, 14410110, 14410120, 14410130, 14410210,
                        14410330, 14410500, 14410600, 14410620, 14420100, 14420160, 14420300, 14502000, 14610200, 14610210,
                        14610250, 14610520, 11115400, 12310100, 12310350, 12310370, 12320100, 11115000, 11115100, 11115200,
                        11115300)
        message("The default food codes for fermented dairy from 17-18 FNDDS file is used.")
    } else {
        # use the provided FERMENTED_DAIRY code
        FERMENTED_DAIRY = FERMENTED_DAIRY_CODE
    }


    if (is.null(GREEN_TEA_CODE)) {
        GREEN_TEA = c(92303010, 92303100, 92305900, 92305910, 92305920, 92308500, 92308510, 92308520, 92308530, 92308540, 92308550, 92309500, 92309510, 92309520)
        message("The default food codes for green tea from 17-18 FNDDS file is used.")
    } else {
        # use the provided GREEN_TEA code
        GREEN_TEA = GREEN_TEA_CODE
    }

    if (is.null(SOYBEAN_CODE)) {
        SOYBEAN = c(11320000, 11320100, 11320200, 11321000, 11321100, 11321200, 41107010, 41410010)
        message("The default food codes for soybean from 17-18 FNDDS file is used.")
    } else {
        # use the provided SOYBEAN code
        SOYBEAN = SOYBEAN_CODE
    }

    # if only day 1 data is provided
    if (!is.null(FPED_IND_PATH) & !is.null(NUTRIENT_IND_PATH)) {
        message("Calculating DI_GM index using day 1 data...")

        if (is.character(FPED_IND_PATH) == TRUE) {
            FPED_IND = read_sas(FPED_IND_PATH)
        } else {
            FPED_IND = FPED_IND_PATH
        }

        if (is.character(NUTRIENT_IND_PATH) == TRUE) {
            NUTRIENT_IND = read_xpt(NUTRIENT_IND_PATH)
        } else {
            NUTRIENT_IND = NUTRIENT_IND_PATH
        }

        if (!("DR1ILINE" %in% colnames(FPED_IND)) | !("DR1ILINE" %in% colnames(NUTRIENT_IND))) {
            stop("Please use individual-level first day data. Individual-level nutrient data should be like DR1IFF_J.XPT. Individual-level FPED data should be like fped_dr1iff_1718.sas7bdat")
        }

        # Select the only the high quality dietary intake data
        NUTRIENT_IND = NUTRIENT_IND %>%
            filter(DR1DRSTZ == 1) %>%
            arrange(SEQN)

        # select only participants with more than 0 kcal intake
        NUTRIENT_IND = NUTRIENT_IND %>%
            filter(DR1IKCAL > 0)

        FPED_IND = FPED_IND %>%
            arrange(SEQN)

        # Merge the nutrient and food pattern data
        COHORT = NUTRIENT_IND %>%
            left_join(FPED_IND, by = c("SEQN", "DR1ILINE"))
        
        # Create the serving size for DI_GM calculation
        COHORT = COHORT %>%
            dplyr::mutate(
                AVOCADO_SERV = case_when(
                    DR1IFDCD.x %in% AVOCADO ~ DR1IGRMS.x,
                    TRUE ~ 0
                ),
                BROCCOLI_SERV = case_when(
                    DR1IFDCD.x %in% BROCCOLI ~ DR1IGRMS.x,
                    TRUE ~ 0
                ),
                CHICKPEA_SERV = case_when(
                    DR1IFDCD.x %in% CHICKPEA ~ DR1IGRMS.x,
                    TRUE ~ 0
                ),
                COFFEE_SERV = case_when(
                    DR1IFDCD.x %in% COFFEE ~ DR1IGRMS.x,
                    TRUE ~ 0
                ),
                CRANBERRY_SERV = case_when(
                    DR1IFDCD.x %in% CRANBERRY ~ DR1IGRMS.x,
                    TRUE ~ 0
                ),
                FERMENTED_DAIRY_SERV = case_when(
                    DR1IFDCD.x %in% FERMENTED_DAIRY ~ DR1IGRMS.x,
                    TRUE ~ 0
                ),
                GREEN_TEA_SERV = case_when(
                    DR1IFDCD.x %in% GREEN_TEA ~ DR1IGRMS.x,
                    TRUE ~ 0
                ),
                SOYBEAN_SERV = case_when(
                    DR1IFDCD.x %in% SOYBEAN ~ DR1IGRMS.x,
                    TRUE ~ 0
                )
            ) 
        
        COHORT = COHORT %>%
            dplyr::group_by(SEQN) %>%
            dplyr::summarize(
                RIAGENDR = first(RIAGENDR),
                AVOCADO_SERV_DI_GM = sum(AVOCADO_SERV),
                BROCCOLI_SERV_DI_GM = sum(BROCCOLI_SERV),
                CHICKPEA_SERV_DI_GM = sum(CHICKPEA_SERV),
                COFFEE_SERV_DI_GM = sum(COFFEE_SERV),
                CRANBERRY_SERV_DI_GM = sum(CRANBERRY_SERV),
                FERMENTED_DAIRY_SERV_DI_GM = sum(FERMENTED_DAIRY_SERV),
                FIBER_SERV_DI_GM = sum(DR1IFIBE),
                GREEN_TEA_SERV_DI_GM = sum(GREEN_TEA_SERV),
                SOYBEAN_SERV_DI_GM = sum(SOYBEAN_SERV),
                WHOLE_GRAIN_SERV_DI_GM = sum(DR1I_G_WHOLE),
                TOTAL_FAT_PERCENTAGE_SERV_DI_GM = sum((DR1ITFAT * 9/DR1IKCAL) * 100),
                PROCESSED_MEAT_SERV_DI_GM = sum(DR1I_PF_CUREDMEAT),
                RED_MEAT_SERV_DI_GM = sum(DR1I_PF_MEAT),
                REFINED_GRAIN_SERV_DI_GM = sum(DR1I_G_REFINED)
            )

        # Calculate the DI_GM component and total scores using the DI_GM function
        COHORT_DI_GM_1 = DI_GM(
            SERV_DATA = COHORT, 
            RESPONDENTID = COHORT$SEQN, 
            GENDER = COHORT$RIAGENDR, 
            AVOCADO = COHORT$AVOCADO_SERV_DI_GM, 
            BROCCOLI = COHORT$BROCCOLI_SERV_DI_GM, 
            CHICKPEA = COHORT$CHICKPEA_SERV_DI_GM, 
            COFFEE = COHORT$COFFEE_SERV_DI_GM, 
            CRANBERRY = COHORT$CRANBERRY_SERV_DI_GM, 
            FERMENTED_DAIRY = COHORT$FERMENTED_DAIRY_SERV_DI_GM, 
            FIBER = COHORT$FIBER_SERV_DI_GM, 
            GREEN_TEA = COHORT$GREEN_TEA_SERV_DI_GM, 
            SOYBEAN = COHORT$SOYBEAN_SERV_DI_GM, 
            WHOLE_GRAIN = COHORT$WHOLE_GRAIN_SERV_DI_GM, 
            TOTAL_FAT_PERCENTAGE = COHORT$TOTAL_FAT_PERCENTAGE_SERV_DI_GM, 
            PROCESSED_MEAT = COHORT$PROCESSED_MEAT_SERV_DI_GM, 
            RED_MEAT = COHORT$RED_MEAT_SERV_DI_GM, 
            REFINED_GRAIN = COHORT$REFINED_GRAIN_SERV_DI_GM
            )
    
        # rename the columns: RESPONDENTID to SEQN, GENDER to RIAGENDR
        COHORT_DI_GM_1 = COHORT_DI_GM_1 %>%
            dplyr::rename(
                SEQN = RESPONDENTID,
                RIAGENDR = GENDER
            )
    }

    # if only day 2 data is provided
    if (!is.null(FPED_IND_PATH2) & !is.null(NUTRIENT_IND_PATH2)) {
        message("Calculating DI_GM index using day 2 data...")

        if (is.character(FPED_IND_PATH2) == TRUE) {
            FPED_IND2 = read_sas(FPED_IND_PATH2)
        } else {
            FPED_IND2 = FPED_IND_PATH2
        }

        if (is.character(NUTRIENT_IND_PATH2) == TRUE) {
            NUTRIENT_IND2 = read_xpt(NUTRIENT_IND_PATH2)
        } else {
            NUTRIENT_IND2 = NUTRIENT_IND_PATH2
        }

        if (!("DR2ILINE" %in% colnames(FPED_IND2)) | !("DR2ILINE" %in% colnames(NUTRIENT_IND2))) {
            stop("Please use individual-level second day data. Individual-level nutrient data should be like DR2IFF_J.XPT. Individual-level FPED data should be like fped_DR2iff_1718.sas7bdat")
        }

        NUTRIENT_IND2 = NUTRIENT_IND2 %>%
            filter(DR2DRSTZ == 1) %>%
            arrange(SEQN)

        # select only participants with more than 0 kcal intake
        NUTRIENT_IND2 = NUTRIENT_IND2 %>%
            filter(DR2IKCAL > 0)

        FPED_IND2 = FPED_IND2 %>%
            arrange(SEQN)

        COHORT2 = NUTRIENT_IND2 %>%
            left_join(FPED_IND2, by = c("SEQN", "DR2ILINE"))

        # Match participant response food frequency to the standard food frequency response code
        COHORT2 = COHORT2 %>%
            dplyr::mutate(
                AVOCADO_SERV = case_when(
                    DR2IFDCD.x %in% AVOCADO ~ DR2IGRMS.x,
                    TRUE ~ 0
                ),
                BROCCOLI_SERV = case_when(
                    DR2IFDCD.x %in% BROCCOLI ~ DR2IGRMS.x,
                    TRUE ~ 0
                ),
                CHICKPEA_SERV = case_when(
                    DR2IFDCD.x %in% CHICKPEA ~ DR2IGRMS.x,
                    TRUE ~ 0
                ),
                COFFEE_SERV = case_when(
                    DR2IFDCD.x %in% COFFEE ~ DR2IGRMS.x,
                    TRUE ~ 0
                ),
                CRANBERRY_SERV = case_when(
                    DR2IFDCD.x %in% CRANBERRY ~ DR2IGRMS.x,
                    TRUE ~ 0
                ),
                FERMENTED_DAIRY_SERV = case_when(
                    DR2IFDCD.x %in% FERMENTED_DAIRY ~ DR2IGRMS.x,
                    TRUE ~ 0
                ),
                GREEN_TEA_SERV = case_when(
                    DR2IFDCD.x %in% GREEN_TEA ~ DR2IGRMS.x,
                    TRUE ~ 0
                ),
                SOYBEAN_SERV = case_when(
                    DR2IFDCD.x %in% SOYBEAN ~ DR2IGRMS.x,
                    TRUE ~ 0
                )
            ) %>%
            dplyr::group_by(SEQN) %>%
            dplyr::summarize(
                RIAGENDR = first(RIAGENDR),
                AVOCADO_SERV_DI_GM = sum(AVOCADO_SERV),
                BROCCOLI_SERV_DI_GM = sum(BROCCOLI_SERV),
                CHICKPEA_SERV_DI_GM = sum(CHICKPEA_SERV),
                COFFEE_SERV_DI_GM = sum(COFFEE_SERV),
                CRANBERRY_SERV_DI_GM = sum(CRANBERRY_SERV),
                FERMENTED_DAIRY_SERV_DI_GM = sum(FERMENTED_DAIRY_SERV),
                FIBER_SERV_DI_GM = sum(DR2IFIBE),
                GREEN_TEA_SERV_DI_GM = sum(GREEN_TEA_SERV),
                SOYBEAN_SERV_DI_GM = sum(SOYBEAN_SERV),
                WHOLE_GRAIN_SERV_DI_GM = sum(DR2I_G_WHOLE),
                TOTAL_FAT_PERCENTAGE_SERV_DI_GM = sum((DR2ITFAT * 9/DR2IKCAL) * 100),
                PROCESSED_MEAT_SERV_DI_GM = sum(DR2I_PF_CUREDMEAT),
                RED_MEAT_SERV_DI_GM = sum(DR2I_PF_MEAT),
                REFINED_GRAIN_SERV_DI_GM = sum(DR2I_G_REFINED)
            )

        # Calculate the DI_GM component and total scores using the DI_GM function
        COHORT_DI_GM_2 = DI_GM(SERV_DATA = COHORT2, RESPONDENTID = COHORT2$SEQN, GENDER = COHORT2$RIAGENDR, AVOCADO = COHORT2$AVOCADO_SERV_DI_GM, BROCCOLI = COHORT2$BROCCOLI_SERV_DI_GM, CHICKPEA = COHORT2$CHICKPEA_SERV_DI_GM, COFFEE = COHORT2$COFFEE_SERV_DI_GM, CRANBERRY = COHORT2$CRANBERRY_SERV_DI_GM, FERMENTED_DAIRY = COHORT2$FERMENTED_DAIRY_SERV_DI_GM, FIBER = COHORT2$FIBER_SERV_DI_GM, GREEN_TEA = COHORT2$GREEN_TEA_SERV_DI_GM, SOYBEAN = COHORT2$SOYBEAN_SERV_DI_GM, WHOLE_GRAIN = COHORT2$WHOLE_GRAIN_SERV_DI_GM, TOTAL_FAT_PERCENTAGE = COHORT2$TOTAL_FAT_PERCENTAGE_SERV_DI_GM, PROCESSED_MEAT = COHORT2$PROCESSED_MEAT_SERV_DI_GM, RED_MEAT = COHORT2$RED_MEAT_SERV_DI_GM, REFINED_GRAIN = COHORT2$REFINED_GRAIN_SERV_DI_GM)

        # rename the columns: RESPONDENTID to SEQN, GENDER to RIAGENDR
        COHORT_DI_GM_2 = COHORT_DI_GM_2 %>%
            dplyr::rename(
                SEQN = RESPONDENTID,
                RIAGENDR = GENDER
            )
    }

    if (!is.null(FPED_IND_PATH) & !is.null(NUTRIENT_IND_PATH) & is.null(FPED_IND_PATH2) & is.null(NUTRIENT_IND_PATH2)) {
        return(COHORT_DI_GM_1)
    }

    if (is.null(FPED_IND_PATH) & is.null(NUTRIENT_IND_PATH) & !is.null(FPED_IND_PATH2) & !is.null(NUTRIENT_IND_PATH2)) {
        return(COHORT_DI_GM_2)
    }

    # merge two days data if they both exist by creating columns with the same name but taking average of the original column
    if (!is.null(FPED_IND_PATH) & !is.null(NUTRIENT_IND_PATH) & !is.null(FPED_IND_PATH2) & !is.null(NUTRIENT_IND_PATH2)) {
        COHORT_1_2_DI_GM <- inner_join(COHORT_DI_GM_1, COHORT_DI_GM_2, by = "SEQN") %>%
            dplyr::mutate(
                RIAGENDR = RIAGENDR.x,
                DI_GM_TOTAL = (DI_GM_TOTAL.x + DI_GM_TOTAL.y) / 2,
                DI_GM_AVOCADO = (DI_GM_AVOCADO.x + DI_GM_AVOCADO.y) / 2, 
                DI_GM_BROCCOLI = (DI_GM_BROCCOLI.x + DI_GM_BROCCOLI.y) / 2,
                DI_GM_CHICKPEA = (DI_GM_CHICKPEA.x + DI_GM_CHICKPEA.y) / 2,
                DI_GM_COFFEE = (DI_GM_COFFEE.x + DI_GM_COFFEE.y) / 2,
                DI_GM_CRANBERRY = (DI_GM_CRANBERRY.x + DI_GM_CRANBERRY.y) / 2,
                DI_GM_FERMENTED_DAIRY = (DI_GM_FERMENTED_DAIRY.x + DI_GM_FERMENTED_DAIRY.y) / 2,
                DI_GM_FIBER = (DI_GM_FIBER.x + DI_GM_FIBER.y) / 2,
                DI_GM_GREEN_TEA = (DI_GM_GREEN_TEA.x + DI_GM_GREEN_TEA.y) / 2,
                DI_GM_SOYBEAN = (DI_GM_SOYBEAN.x + DI_GM_SOYBEAN.y) / 2,
                DI_GM_WHOLE_GRAIN = (DI_GM_WHOLE_GRAIN.x + DI_GM_WHOLE_GRAIN.y) / 2,
                DI_GM_TOTAL_FAT_PERCENTAGE = (DI_GM_TOTAL_FAT_PERCENTAGE.x + DI_GM_TOTAL_FAT_PERCENTAGE.y) / 2,
                DI_GM_PROCESSED_MEAT = (DI_GM_PROCESSED_MEAT.x + DI_GM_PROCESSED_MEAT.y) / 2,
                DI_GM_RED_MEAT = (DI_GM_RED_MEAT.x + DI_GM_RED_MEAT.y) / 2,
                DI_GM_REFINED_GRAIN = (DI_GM_REFINED_GRAIN.x + DI_GM_REFINED_GRAIN.y) / 2
            ) %>%
            dplyr::select(
                SEQN, 
                RIAGENDR, 
                DI_GM_TOTAL, 
                DI_GM_AVOCADO, 
                DI_GM_BROCCOLI, 
                DI_GM_CHICKPEA, 
                DI_GM_COFFEE, 
                DI_GM_CRANBERRY, 
                DI_GM_FERMENTED_DAIRY, 
                DI_GM_FIBER, 
                DI_GM_GREEN_TEA, 
                DI_GM_SOYBEAN, 
                DI_GM_WHOLE_GRAIN, 
                DI_GM_TOTAL_FAT_PERCENTAGE, 
                DI_GM_PROCESSED_MEAT, 
                DI_GM_RED_MEAT, 
                DI_GM_REFINED_GRAIN
            )

        return(COHORT_1_2_DI_GM)
    }
}
