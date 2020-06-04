#' National Cancer Database Recode Project
#'
#' This package serves to recode the standard NCDB excel or stata .sav file for ease of survival analysis
#' @export
NCDBClean <- function(df){
  #Remove cases with no survival info
  df <- subset(df, !(is.na(df$PUF_VITAL_STATUS)))
  df
}
NCDBRecode <- function(df) {
  #PATIENT DEMOGRAPHICS####

  # PUF_CASE_ID - Unique case identification number assigned to the case in the PUF.
  # NCDB assigned value that uniquely identifies each case included in the PUF. The value
  # assigned to each case is selected at random, and the value assigned to each case will
  # change with each issued PUF. The PUF Case IDs are not the same across cancer sites, and
  # cases cannot be linked across cancer sites.
  # Note that the length of this key was expanded from 10 to 37 in January 2014.

  df$PUF_CASE_ID

  # PUF_FACULTY_ID - The facility reporting the case to the NCDB. Codes are anonymized. The facility
  # random IDs are assigned regardless of cancer site, so you may identify the same
  # facilities across cancer sites.

  df$PUF_FACILITY_ID <- NA

  # FACILITY_TYPE_CD
  # Each facility reporting cases to the NCDB is assigned a category classification by
  # the Commission on Cancer Accreditation program. This item provides a general
  # classification of the structural characteristics of each reporting facility.
  # Analytic Note: For additional information about CoC accreditation categories
  # see https://www.facs.org/quality-programs/cancer/accredited/about/categories.
  # Please note that for hospitals who are categorized as Integrated Network Cancer
  # Programs, there is no information in the PUF data as to when these facilities
  # became part of a Network. Some facilities may have been in a Network throughout
  # the time period in the PUF, whereas others may have only recently become part of
  # a Network. Additionally, facilities designated in the Network category could
  # previously been assigned a different category before joining their Network, such as
  # Community, Academic, etc. Keep this in mind when analyzing facility type data, as
  # Networks are comprised of several different types of facilities, but are only
  # classified as Integrated Network Cancer Program in the PUF data. The hospital
  # category in the PUF only represents the current designation of the facility.
  # See Data De-identification and Confidentiality for a description of the handling of
  # some categories. Please note that VA/DoD facilities are not included in the PUF
  # files, and therefore are not identifiable as a type of cancer program

  df$FACILITY_TYPE_CD <-
    factor(
      df$FACILITY_TYPE_CD,
      levels = c(1, 2, 3, 4),
      labels = c(
        "Community Cancer Program",
        "Comprehensive Community Cancer Program",
        "Academic/Research Program (includes NCI-designated comprehensive cancer centers)",
        "Integrated Network Cancer Program"
      )
    )

  var_label(df$FACILITY_TYPE_CD) <- "Facility Type"

  #FACILITY_LOCATION_CD
  # The US Census Division of the reporting facility
  df$FACILITY_LOCATION_CD <-
    factor(
      df$FACILITY_LOCATION_CD,
      levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9),
      labels = c(
        "New England",
        "Middle Atlantic",
        "South Atlantic",
        "East North Central",
        "East South Central",
        "West North Central",
        "West South Central",
        "Mountain",
        "Pacific"
      )
    )

  var_label(df$FACILITY_LOCATION_CD) <- "Facility Location"

  #PUF_MULTI_SOURCE
  # Identifies whether there was more than one CoC facility that submitted a report for this case to NCDB.

  df$PUF_MULT_SOURCE <-
    factor(
      df$PUF_MULT_SOURCE,
      levels = c(0, 1),
      labels = c(
        'Only one CoC facility reported this case to NCDB',
        'Records pertaining to this case submitted to NCDB by more than one CoC facility'
      )
    )

  var_label(df$PUF_MULT_SOURCE) <- "Patient Treated in More than One CoC Facility"

  #REFERENCE_DATE_FLAG
  # Identifies whether a report for a case has a diagnosis date before or after the facility's reference date.
  # NOTE:
  # Every facility has a reference date, from which they are accountable for the completeness of the data for cases
  # diagnosed in that year through the present. Since a facility may request to move their reference date forward,
  # there are some instances where a case’s diagnosis year falls before the facility’s reference date. This item is
  # coded 0 in cases where this occurs. A 1 signifies cases where the diagnosis year is on or after the reference
  # date year. Reports for cases whose diagnosis date is prior to the reference date cannot be changed or updated by
  # the facility. For this reason, PUF researchers may choose to omit cases where the diagnosis date precedes the
  # reference date, depending on the nature of the study.

  df$REFERENCE_DATE_FLAG <-
    factor(
      df$REFERENCE_DATE_FLAG,
      levels = c(0, 1),
      labels = c(
        'Diagnosis date before reference date',
        'Diagnosis date on or after reference date'
      )
    )

  var_label(df$REFERENCE_DATE_FLAG) <- "Reference Date Flag"

  #AGE
  # Records the age of the patient at his or her last birthday before diagnosis.

  df$AGE

  #AGE_GROUP
  # declare a categorical variable to use with NCDBGroupAge
  df$AGE_GROUP <- NA
  NCDBGroupAge(df$AGE)

  #SEX
  # Identifies the sex of the patient

  df$SEX <-
    factor(df$SEX,
           levels = c(1, 2),
           labels = c("Male", "Female"))

  var_label(df$SEX) <- "Sex"

  #RACE
  # Identifies the primary race of the person
  # Race is analyzed with Spanish/Hispanic Origin (NAACCR Item #190).
  # Both items must be recorded. All tumors for the same patient should have the same race code.

  df$RACE <-
    factor(
      df$RACE,
      levels = c(1, 2, 3, 4, 5, 6, 7, 8, 10, 11, 12, 13, 14, 15, 16, 17, 20, 21, 22, 25, 26, 27, 28, 30, 31, 32, 96, 97, 98, 99),
      labels = c(
        "White",
        "Black",
        "American Indian, Aleutian, or Eskimo",
        "Chinese",
        "Japanese",
        "Filipino",
        "Hawaiian",
        "Korean",
        "Vietnamese",
        "Laotian",
        "Hmong",
        "Kampuchean (including Khmer and Cambodian)",
        "Thai",
        "Asian Indian or Pakistani, NOS (formerly code 09)",
        "Asian Indian",
        "Pakistani",
        "Micronesian, NOS",
        "Chamorran",
        "Guamanian, NOS",
        "Polynesian, NOS",
        "Tahitian",
        "Samoan",
        "Tongan",
        "Melanesian, NOS",
        "Fiji Islander",
        "New Guinean",
        "Other Asian, including Asian, NOS and Oriental, NOS",
        "Pacific Islander, NOS",
        "Other",
        "Unknown"
      )
    )
  var_label(df$RACE) <- "Race"

  #SPANISH_HISPANIC_ORIGIN
  # Persons of Spanish or Hispanic origin may be of any race, but these categories are generally not
  # used for Native Americans, Filipinos, or others who may have Spanish names.
  # Code 0 (Non-Spanish; non-Hispanic) for Portuguese and Brazilian persons. If the patient has multiple tumors,
  # all records should have the same code.

  df$SPANISH_HISPANIC_ORIGIN <-
    factor(
      df$SPANISH_HISPANIC_ORIGIN,
      levels = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9),
      labels = c(
        "Non-Spanish; non-Hispanic",
        "Mexican (includes Chicano)",
        "Puerto Rican",
        "Cuban",
        "South or Central America (except Brazil)",
        "Other specified Spanish/Hispanic origin (includes European)",
        "Spanish, NOS; Hispanic, NOS; Latino, NOS (There is evidence other than surname or maiden name that the person is Hispanic, but he/she cannot be assigned to any category of 1-5)",
        "Spanish surname only (The only evidence of the person's Hispanic origin is surname or maiden name, and there is no contrary evidence that the person is not Hispanic)",
        "Dominican Republic (for use with patients who were diagnosed with cancer on January 1, 2005, or later)",
        "Unknown whether Spanish or not; not stated in patient record"
      )
    )

  var_label(df$SPANISH_HISPANIC_ORIGIN) <- "Spanish Origin"

  #INSURANCE_STATUS
  # Identifies the patient's primary insurance carrier at the time of initial diagnosis
  # and/or treatment.
  df$INSURANCE_STATUS <-
    factor(
      df$INSURANCE_STATUS,
      levels = c(0, 1, 2, 3, 4, 9),
      labels = c(
        "Not Insured",
        "Private Insurance/Managed Care",
        "Medicaid",
        "Medicare",
        "Other Government",
        "Insurance Status unknown"
      )
    )

  var_label(df$INSURANCE_STATUS) <- "Primary Payer"

  #MEDICAID_EXPN_CODE
  # Reference: https://www.medicaid.gov/medicaid/program- information/downloads/december-2015-enrollment-report.pdf

  df$MEDICAID_EXPN_CODE <-
    factor(
      df$MEDICAID_EXPN_CODE,
      levels = c(0, 1, 2, 3, 9),
      labels = c("Non-Expansion States",
                 "January 2014 Expansion States",
                 "Early Expansion States (2010 - 2013)",
                 "Late Expansion States (after Jan 2014)",
                 "Suppressed for Ages 0-39")
    )

  var_label(df$MEDICAID_EXPN_CODE) <- "Medicaid Expansion Status State Group"


  #MED_INC_QUAR_00
  # Median household income for each patient's area of residence is estimated by
  # matching the zip code of the patient recorded at the time of diagnosis against files
  # derived from year 2000 US Census data. Household income is categorized as
  # quartiles based on equally proportioned income ranges among all US zip codes.
  df$MED_INC_QUAR_00 <-
    factor(
      df$MED_INC_QUAR_00,
      levels = c(1, 2, 3, 4),
      labels = c(
        "Less than $30,000",
        "$30,000-$34,000",
        "$35,000-$45,999",
        "$46,000 or more"
      )
    )

  var_label(df$MED_INC_QUAR_00) <- "Income 2000"

  ### NO_HSD_QUAR_00
  # This measure of educational attainment for each patient's area of residence is
  # estimated by matching the zip code of the patient recorded at the time of diagnosis
  # against files derived from year 2000 US Census data. This item provides a
  # measure of the number of adults in the patient's zip code who did not graduate
  # from high school, and is categorized as equally proportioned quartiles among all
  # US zip codes

  df$NO_HSD_QUAR_00 <-
    factor(
      df$NO_HSD_QUAR_00,
      levels = c(1, 2, 3, 4),
      labels = c("29% or more",
                 "20% - 28.9%",
                 "14% - 19.9%",
                 "Less than 14%")
    )

  var_label(df$NO_HSD_QUAR_00) <- "Education 2000"


  #UR_CD_2003
  # Area-based measure of rurality and urban influence, using the typology published
  # by the USDA Economic Research Service.
  # Analytic Note:
  #   This item was estimated by matching the state and county FIPS code of the patient
  # recorded at the time of diagnosis against 2003 files published by the United States
  # Department of Agriculture Economic Research Service
  # (http://www.ers.usda.gov/data-products/rural-urban-continuum-codes).
  # Rural-Urban continuum codes form a classification scheme that distinguishes
  # metropolitan (metro) counties by the population size of their metro area, and
  # nonmetropolitan (nonmetro) counties by degree of urbanization and adjacency to a
  # metro area or areas. The metro and nonmetro categories have been subdivided
  # into three metro and six nonmetro groupings, resulting in a nine-part county
  # codification. The codes allow researchers working with data to break such data into
  # finer residential groups beyond a simple metro-nonmetro dichotomy, particularly for
  # the analysis of trends in nonmetro areas that may be related to degree of rurality
  # and metro proximity.

  df$UR_CD_03 <-
    factor(
      df$UR_CD_03,
      levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9),
      labels = c(
        # Metro counties
        "Counties in metro areas of 1 million population or more",
        "Counties in metro areas of 250,000 to 1 million population",
        "Counties in metro areas of fewer than 250,000 population",
        # Urban Counties
        "Urban population of 20,000 or more, adjacent to a metro area.",
        "Urban population of 20,000 or more, not adjacent to a metro area.",
        "Urban population of 2,500 to 19,999, adjacent to a metro area.",
        "Urban population of 2,500 to 19,999, not adjacent to a metro area.",
        # Rural Counties
        "Completely rural or less than 2,500 urban population, adjacent to a metro area",
        "Completely rural or less than 2,500 urban population, not adjacent to a metro area"

      )
    )

  var_label(df$UR_CD_03) <- "Urban/Rural 2003"

  #MED_INC_QUAR_12
  # Median household income for each patient's area of residence is estimated by
  # matching the zip code of the patient recorded at the time of diagnosis against files
  # derived from the 2012 American Community Survey data, spanning years 2008
  # 2012 and adjusted for 2012 inflation. Household income is categorized as quartiles
  # based on equally proportioned income ranges among all US zip codes. Due to
  # differences in collection methodology, comparisons with Census 2000 income data
  # should be done with caution. See
  # https://www.census.gov/acs/www/guidance_for_data_users/comparing_2012/ for
  # more information.

  df$MED_INC_QUAR_12 <-
    factor(
      df$MED_INC_QUAR_12,
      levels = c(1, 2, 3, 4),
      labels = c(
        "Less than $38,000",
        "$38,000-$47,999",
        "$48,000-$62,999",
        "$63,000 or more"
      )
    )

  var_label(df$MED_INC_QUAR_12) <- "Income 2008-2012"


  #NO_HSD_QUAR_12
  # This measure of educational attainment for each patient's area of residence is
  # estimated by matching the zip code of the patient recorded at the time of diagnosis
  # against files derived from the 2012 American Community Survey data, spanning
  # years 2008-2012. This item provides a measure of the number of adults in the
  # patient's zip code who did not graduate from high school, and is categorized as
  # equally proportioned quartiles among all US zip codes. Comparisons with Census
  # 2000 education data may be done. See
  # https://www.census.gov/acs/www/guidance_for_data_users/comparing_2012/ for
  # more information.
  df$NO_HSD_QUAR_12 <-
    factor(
      df$NO_HSD_QUAR_12,
      levels = c(1, 2, 3, 4),
      labels = c("21% or more",
                 "13%-20.9%",
                 "7%-12.9%",
                 "Less than 7%")
    )
  var_label(df$NO_HSD_QUAR_12) <- "Education 2008-2012"

  #MED_INC_QUAR_2016
  # Median household income for each patient's area of residence is estimated by matching
  # the zip code of the patient recorded at the time of diagnosis against files derived from
  # the 2016 American Community Survey data, spanning years 2012-2016 and adjusted for 2016 inflation.
  # Household income is categorized as quartiles based on equally proportioned income ranges
  # among all US zip codes.

  df$MED_INC_QUAR_16 <-
    factor(
      df$MED_INC_QUAR_16,
      levels = c(1, 2, 3, 4),
      labels = c(
        "Less than $40,227",
        "$40,227 - $50,353",
        "$50,354 - $63,332",
        "63,333+")
    )
  var_label(df$MED_INC_QUAR_16) <- "Income 2012-2016"


  #UR_CD_03
  # Area-based measure of rurality and urban influence, using the typology published
  # by the USDA Economic Research Service.
  # Analytic Note:
  #   This item was estimated by matching the state and county FIPS code of the patient
  # recorded at the time of diagnosis against 2013 files published by the United States
  # Department of Agriculture Economic Research Service
  # (http://www.ers.usda.gov/data-products/rural-urban-continuum-codes).
  # Rural-Urban continuum codes form a classification scheme that distinguishes
  # metropolitan (metro) counties by the population size of their metro area, and
  # nonmetropolitan (nonmetro) counties by degree of urbanization and adjacency to a
  # metro area or areas. The metro and nonmetro categories have been subdivided
  # into three metro and six nonmetro groupings, resulting in a nine-part county
  # codification. The codes allow researchers working with data to break such data into
  # finer residential groups beyond a simple metro-nonmetro dichotomy, particularly for
  # the analysis of trends in nonmetro areas that may be related to degree of rurality
  # and metro proximity.
  # Since labels for the 2013 classification codes are the same as the 2003 labels, a
  # direct comparison with the 2003 Urban/Rural codes may be made.

  #This data is newer and slightly more complete than the 03 variable

  df$UR_CD_13 <-
    factor(
      df$UR_CD_13,
      levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9),
      labels = c(
        # Metro counties
        "Metro areas >= 1 million",
        "Metro areas of 250,000 to 1 million",
        "Metro areas of fewer than 250,000",
        # Urban Counties
        "Urban areas of 20,000 or more, adjacent to a metro area",
        "Urban population of 20,000 or more, not adjacent to a metro area",
        "Urban population of 2,500 to 19,999, adjacent to a metro area",
        "Urban population of 2,500 to 19,999, not adjacent to a metro area",
        # Rural Counties
        "Completely rural or less than 2,500 urban population, adjacent to a metro area",
        "Completely rural or less than 2,500 urban population, not adjacent to a metro area"

      )
    )

  var_label(df$UR_CD_03) <- "Urban/Rural 2013"

  #URBAN_RURAL
  # Calculated column to group the UR_CD_13 values by Metro/Urban/Rural
  df$URBAN_RURAL <- NA
  df$URBAN_RURAL[df$UR_CD_13 %in% c(
    "Metro areas >= 1 million",
    "Metro areas of 250,000 to 1 million",
    "Metro areas of fewer than 250,000"
  )] <- 0
  df$URBAN_RURAL[df$UR_CD_13 %in% c(
    "Urban areas of 20,000 or more, adjacent to a metro area.",
    "Urban population of 20,000 or more, not adjacent to a metro area.",
    "Urban population of 2,500 to 19,999, adjacent to a metro area.",
    "Urban population of 2,500 to 19,999, not adjacent to a metro area."
  )] <- 1
  df$URBAN_RURAL[df$UR_CD_13 %in% c(
    "Completely rural or less than 2,500 urban population, adjacent to a metro area",
    "Completely rural or less than 2,500 urban population, not adjacent to a metro area"
  )] <- 2

  df$URBAN_RURAL <-
    factor(
      df$URBAN_RURAL,
      levels = c(0, 1, 2),
      labels = c("Metro",
                 "Urban",
                 "Rural")
    )

  var_label(df$URBAN_RURAL) <- "Rurality"

  #NO_HSD_QUAR_2016
  # This measure of educational attainment for each patient's area of residence is estimated
  # by matching the zip code of the patient recorded at the time of diagnosis against files derived
  # from the 2016 American Community Survey data, spanning years 2012-2016. This item provides a
  # measure of the number of adults age 25 or older in the patient's zip code who did not graduate
  # from high school, and is categorized as equally proportioned quartiles among all US zip codes.

  df$NO_HSD_QUAR_16 <-
    factor(
      df$NO_HSD_QUAR_16,
      levels = c(1, 2, 3, 4),
      labels = c("17.6% or more",
                 "10.9% - 17.5%",
                 "6.3% - 10.8%",
                 "Less than 6.3%")
    )
  var_label(df$NO_HSD_QUAR_16) <- "Education 2012-2016"

  #CROWFLY
  # The "great circle" distance in miles between the patient's residence and the hospital that reported the case.
  # Analytic Note:  Residential latitude and longitude are based on the patient's zip code centroid or on
  # the city if the zip code was not available. Hospital locations are based on the street address for the
  # facility. The great circle distance is calculated between those two points. In some instances, the
  # residential city is outside of the United States, so the upper bound of distance may be quite large.
  # A distance of 0 can result when the patient lives in the same zip code where the facility is located.

  df$CROWFLY

  var_label(df$CROWFLY) <- "Great Circle Distance"


  #CDCC_TOTAL_BEST
  # Comorbid conditions as described by Charlson/Deyo (1992) [1] are mapped from as many as ten reported
  # ICD-9-CM or ICD-10 secondary diagnosis codes. The Charlson/Deyo value is a weighted score derived from
  # the sum of the scores for each of the comorbid conditions listed in the Charlson Comorbidity Score Mapping Table
  # (source: http://mchp-appserv.cpe.umanitoba.ca/viewConcept.php?conceptID=109 ). The range for this value
  # is between 0 and 25. Starting with the 2015 PUF released in the Fall of 2017, ICD-10 codes are incorporated
  # into the score calculation for cases diagnosed in 2006-2015. Registries were able to submit ICD-10 codes
  # starting in 2006. However, very few ICD-10 codes were submitted until 2015.
  # The 2015 Charlson-Deyo Score is derived from the highest score that is calculated from using either
  # the ICD-9 codes or the ICD-10 codes. The allowable values have also been extended to now include values
  # up to 3 or more.
  # Analytic note: Because of the small proportion of cases with a Charlson Comorbidity score exceeding 3,
  # the data have been truncated to 0, 1, 2, 3 (greater than or equal to 3). A score of 0 indicates
  # "no comorbid conditions recorded", or none of the values shown below. Patients with a score of 0
  # could still have comorbidities if they are conditions that are not included in the mapping table below.
  # Note that the patient's cancer is not directly reflected in the recorded score. Two examples illustrating
  # how the Charlson Score is summarized for the PUF data: If a patient had a myocardial infarction, diabetes,
  # and renal disease, the cumulative score would be 4, and the value shown in the PUF would be 3. If a patient
  # had severe liver disease, the value in the PUF would also be 3, since the Charlson Score of severe liver
  # disease is 3.

  df$CDCC_TOTAL_BEST <-
    factor(
      df$CDCC_TOTAL_BEST,
      levels = c(0, 1, 2, 3),
      labels = c(
        "Total Charlson Score of 0",
        "Total Charlson score of 1",
        "Total Charlson score of 2",
        "Total Charlson score of 3 or more"
      )
    )

  var_label(df$CDCC_TOTAL_BEST) <- "Charlson/Deyo Score"

  #CANCER IDENTIFICATION####

  #SEQUENCE_NUMBER - NOT DONE - Unsure how to manage mix of continuous and categorical
  #Indicates the sequence of malignant and non-malignant neoplasms over the lifetime of the patient.

  df$SEQUENCE_NUMBER <-
    factor(
      as.numeric(df$SEQUENCE_NUMBER),
      levels = c(0, 1, 2, 3, 4),
      labels = c(
        "In Situ or invasive Malignant Behavior",
        "Benign or borderline, non-malignant behavior",
        "Unknown",
        "Single non-malignant primary",
        "Sequence of benign or borderline tumor unknown"
      )
    )

  var_label(df$SEQUENCE_NUMBER) <- "Sequence Number"


  #CLASS_OF_CASE
  # Classifies cases recorded in the database.

  df$CLASS_OF_CASE <-
    factor(
      df$CLASS_OF_CASE,
      levels = c(00, 10, 11, 12, 13, 14, 20, 21, 22),
      labels = c(
        "Diagnosis at the reporting facility and all treatment or a decision not to treat was done elsewhere.",
        "Initial diagnosis at the reporting facility, and part or all of first course treatment or a decision not to treat was at the reporting facility, NOS.",
        "Initial diagnosis in a staff physician's office and part of first course treatment was done at the reporting facility.",
        "Initial diagnosis in a staff physician's office and all of first course treatment or a decision not to treate was done at the reporting facility.",
        "Initial diagnosis at the reporting facility and part of first course treatment was done at the reporting facility; part of first course treatment was done elsewhere.",
        "Initial diagnosis at the reporting facility and all of first course treatment or a decision not to treat was done at the reporting facility.",
        "Initial diagnosis elsewhere and all or part of first course treatment or a decision not to treat was done at the reporting facility, NOS.",
        "Initial diagnosis elsewhere and part of first course treatment was done at the reporting facility; part of first course treatment was done elsewhere.",
        "Initial diagnosis elsewhere and all of first course treatment or a decision not to treat was done at the reporting facility."
      )
    )

  var_label(df$CLASS_OF_CASE) <- "Class of Case"

  #YEAR_OF_DIAGNOSIS
  # Records the year of initial diagnosis by a physician for the tumor being reported.

  df$YEAR_OF_DIAGNOSIS

  var_label(df$YEAR_OF_DIAGNOSIS) <- "Year of Diagnosis"


  #PRIMARY_SITE - NOT DONE
  # Identifies the primary site, that is, the anatomic site of origin for the cancer.
  # Record the ICD-O-3 (International Classification of Diseases for Oncology, Third
  # Edition) topography code for the site of origin.

  df$PRIMARY_SITE <-
    factor(
      df$PRIMARY_SITE,
      levels = c("C300", "C310", "C311", "C312", "C313", "C318", "C319", "C301"),
      labels = c(
        "Nasal Cavity",
        "Maxillary",
        "Ethmoid",
        "Frontal",
        "Sphenoid",
        "Overlapping lesion of accessory sinuses",
        "Accessory sinus, NOS",
        "Middle Ear"
      )
    )

  var_label(df$PRIMARY_SITE) <- "Primary Site"

  #LATERALITY
  # Identifies the side of a paired organ or the side of the body on which the reportable
  # tumor originated. This applies to the primary site only
  df$LATERALITY <-
    factor(
      df$LATERALITY,
      levels = c(0, 1, 2, 3, 4, 5, 9),
      labels = c(
        "Organ is not considered to be a paired site.",
        "Origin of primary is right.",
        "Origin of primary is left.",
        "Only one side involved, right or left origin not specified.",
        "Bilateral involvement, side of origin unknown, stated to be a single primary. This includes: Both ovaries simultaneously involved with a single histology, Bilateral retinoblastomas, Bilateral Wilms tumors",
        "Midline of a paired site tumors",
        "Paired site, but lateral origin unknown; midline tumor."
      )
    )

  var_label(df$LATERALITY) <- "Laterality"

  #HISTOLOGY
  # Records the tumor histology of all cases reported to the NCDB in International Classification of Disease
  # for Oncology, Third Edition (ICD-O-3) terms.
  # Analytic Note:
  #   This item is the product of the application of the conversion rules expressed in
  # ICDO2-3_SEER.xls (http://seer.cancer.gov/tools/conversion/index.html) for cases
  # diagnosed prior to 2001, which were originally coded according to ICD-O-2, and the
  # ICD-O-3 codes reported by registries for cases diagnosed in 2001 and
  # subsequently. In addition, beginning with 2010 diagnoses, malignant hematopoietic
  # and lymphoid histology codes not yet printed in the ICD-O-3 were added. For a list
  # of the added codes, consult http://seer.cancer.gov/tools/heme/; the codes are in
  # Appendix D of the Hematopoietic and Lymphoid Manual which can be accessed
  # from the online or downloadable database files on that site. Hematopoietic and
  # lymphatic cancers diagnosed prior to 2010 retain the earlier ICD-O-3 values.
  # A list of histologies and labels may be found on the online ICD-O-3 site:
  #   (http://codes.iarc.fr/home).

  var_label(df$HISTOLOGY) <- "Histology"

  #BEHAVIOR
  # Records the behavior of all cases reported to the NCDB. The fifth digit of the
  # morphology code is the behavior code

  df$BEHAVIOR <-
    factor(
      df$BEHAVIOR,
      levels = c(0, 1, 2, 3),
      labels = c(
        "Benign Benign",
        "Borderline",
        "In situ",
        "Invasive or microinvasive"
      )
    )

  var_label(df$BEHAVIOR) <- "Behavior"


  #GRADE
  # Describes the tumor's resemblance to normal tissue. Well differentiated (Grade I) is
  # the most like normal tissue, and undifferentiated (Grade IV) is the least like normal
  # tissue. Code the grade or differentiation as stated in the final pathologic diagnosis.
  # NCDBRecode note: Raw GRADE data is saved as GRADE_DESC, which is then broken out to GRADE_RECODE
  # to simplify to low, high, other grade. GRADE_SHOT contains the same information as the raw GRADE
  # but shorter

  df$GRADE_DESC <-
    factor(
      df$GRADE,
      levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9),
      labels = c(
        "Grade I,1,i Well differentiated; differentiated, NOS",
        "Grade II,2,ii,I/III, or 1/3 (Moderately differentiated; moderately well differentiated; intermediate differentiation)",
        "Grade III,3,iii, II/III, or 2/3 (Poorly differentiated)",
        "Grade IV,4,iv,III/III, or 3/3 (Undifferentiated; anaplastic)",
        "T cell; T-precursor",
        "B cell; pre-B; B-precursor",
        "Null cell; non T-non B",
        "NK (natural killer) cell (effective with diagnosis 1/1/95 and after)",
        "Cell type not determined, not stated or not applicable;unknown primaries; high grade dysplasia (adenocarcinoma in situ)"
      )
    )

  # Declare GRADE_RECODE and sort GRADE into three groups
  df$GRADE_RECODE <- NA
  df$GRADE_RECODE[df$GRADE %in% c(1, 2)] <- 0
  df$GRADE_RECODE[df$GRADE %in% c(3, 4)] <- 1
  df$GRADE_RECODE[df$GRADE %in% c(5, 6, 7, 8, 9)] <- 2

  # Factor GRADE_RECODE
  df$GRADE_RECODE <-
    factor(
      df$GRADE_RECODE,
      levels = c(0, 1, 2),
      labels = c("Low (I/II)",
                 "High (III/IV)",
                 "Other")
    )

  # Declare GRADE_SHORT to maintain the detail of GRADE but more readable
  df$GRADE_SHORT <- df$GRADE
  df$GRADE_SHORT <-
    factor(
      df$GRADE_SHORT,
      levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9),
      labels = c( "Well differentiated",
                  "Moderately differentiated",
                  "Poorly differentiated",
                  "Undifferentiated",
                  "T cell; T-precursor",
                  "B cell; pre-B; B-precursor",
                  "Null cell; non T-non B",
                  "NK (natural killer) cell (effective with diagnosis 1/1/95 and after)",
                  "Cell type not determined, not stated or not applicable;unknown primaries; high grade dysplasia (adenocarcinoma in situ)"
      )
    )


  var_label(df$GRADE_SHORT) <- "Grade (short)"
  var_label(df$GRADE_RECODE) <- "Grade (recode)"


  #DIAGNOSTIC CONFIRMATION
  # Records the most definitive method of diagnostic confirmation of the cancer being
  # reported at any time in the patient's history.

  df$DIAGNOSTIC_CONFIRMATION <-
    factor(
      df$DIAGNOSTIC_CONFIRMATION,
      levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9),
      labels = c(
        "Positive histology", #Histologic confirmation (tissue microscopically examined)
        "Positive cytology", #	Cytologic confirmation (no tissue microscopically examined; fluid cells microscopically examined
        "Postive histology PLUS positive immunophenotyping and/or positive genetic studies", #Histology is positive for cancer, and there are also positive immunophenotyping and/or genetic test results. Use this code only for histology range 9590-9992 where the year of diagnosis is 2010 or later
        "Positive microscopic confirmation, method not specified", #Microscopic confirmation is all that is known. It is unknown if the cells were from histology or cytology
        "Positive laboratory test/marker study", #A clinical diagnosis of cancer is based on laboratory tests/marker studies which are clinically diagnostic for cancer. This includes alpha-fetoprotein for liver cancer and abnormal electrophoretic spike for multiple myeloma. Elevated PSA is nondiagnostic of cancer. If the physician uses the PSA as a basis for diagnosing prostate cancer with no other workup, record as code 5. (Adapted from SEER.)
        "Direct visualization without microscopic confirmation", #The tumor was visualized during a surgical/endoscopic procedure only with no tissue resected for microscopic examination
        "Radiography and other imaging techniques without microscopic confirmation", #The malignancy was reported by the physician from an imaging technique report only
        "Clinical diagnosis only", # (Other than 5,6,7) The malignancy was reported by the physician in the medical record
        "A statement of malignancy was reported in the medical record" #A statement of malignancy was reported in the medical record, but there is no statement of how the cancer was diagnosed (usually Class of Case 3)
      )
    )

  # NOT DONE - Separate out categorical from continuous, batch continuous into groups, factor all.
  ### REGIONAL_NODES_POSITIVE
  # Records the exact number of regional lymph nodes examined by the pathologist
  # and found to contain metastases.
  # df$REGIONAL_NODES_POSITIVE <-
  #   factor(
  #     df$REGIONAL_NODES_POSITIVE,
  #     levels = c(00,90,95,97,98,99),
  #     labels = c(
  #       "00-All nodes examined are negative.",
  #       "1-89 or more nodes are positive.",
  #       "90 - 90 or more nodes positive"
  #       "95-Positive aspiration of lymph node(s) was performed.",
  #       "97-Positive nodes are documented, but the number is unspecified.",
  #       "98-No nodes were examined.",
  #       "99-It is unknown whether nodes are positive; not applicable; not stated in patient record.",
  #       )
  #   )

  # df$NODES_POSITIVE <- NA
  # for (i in 1:length(df$REGIONAL_NODES_POSITIVE)) {
  #   tryCatch({
  #     df$NODES_POSITIVE[i] <-
  #       nodeCount(df$REGIONAL_NODES_POSITIVE[i])
  #   }, error = function(error_message) {
  #     return(NULL)
  #   })
  # }
  #
  #
  # df$NODES_POSITIVE <-
  #   factor(
  #     df$NODES_POSITIVE,
  #     levels = c(0, 1, 2, 3, 4, 5, 6, 8, 9, 10),
  #     labels = c(
  #       "0",
  #       "1",
  #       "2-5",
  #       "6-20",
  #       "21-89",
  #       "90 or more",
  #       "Positive aspiration was performed",
  #       "Positive nodes, # unspecified",
  #       "No nodes examined",
  #       "Unknown"
  #     )
  #   )


  # 1 = 1
  # 2 = 2-5
  # 3 = 6-94
  # 4 = >21
  # 5 = FNA performed
  #       - 0-89 = # of nodes examined
  #       - 90 = 90+ nodes examined
  #       - 95 = FNA performed
  #       - blank = unknown

  ### REGIONAL_NODES_EXAMINED
  # Records the total number of regional lymph nodes that were removed and
  # examined by the pathologist.
  # df$REGIONAL_NODES_EXAMINED <-
  #   factor(
  #     df$REGIONAL_NODES_EXAMINED,
  #     levels = c(00,90,95,97,98,99),
  #     labels = c(
  # "00-No nodes were examined.",
  # "01-89 -1-89 nodes were examined.",
  # "90-90 or more nodes were examined.",
  # "95-No regional nodes were removed, but aspiration of regional nodes was performed.",
  # "96-Regional lymph node removal was documented as a sampling, and the number of nodes is unknown/not stated.",
  # "97-Regional lymph node removal was documented as a dissection, and the number of nodes is unknown/not stated.",
  # "98-Regional lymph nodes surgically removed but number not documented, not documented as sampling or dissection.",
  # "99-Unknown if regional nodes"
  #       )
  #   )

  # for (i in 1:length(df$REGIONAL_NODES_EXAMINED)) {
  #   tryCatch({
  #     df$NODES_EXAMINED[i] <-
  #       nodeCount(df$REGIONAL_NODES_EXAMINED[i])
  #   }, error = function(error_message) {
  #     return(NULL)
  #   })
  # }
  #
  # df$NODES_EXAMINED <-
  #   factor(
  #     df$NODES_EXAMINED,
  #     levels = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
  #     labels = c(
  #       "0",
  #       "1",
  #       "2-5",
  #       "6-20",
  #       "21-89",
  #       "90 or more",
  #       "Aspiration was performed",
  #       "Nodes sampled, # unspecified",
  #       "Nodes dissected, # unspecified",
  #       "Nodes removed but # not documented",
  #       "Unknown"
  #     )
  #   )

  #STAGE OF DISEASE####


  #DX_STAGING_PROC_DAYS
  # The number of days between the date of diagnosis (NAACCR Item #390) and the
  # date the surgical diagnostic and/or staging procedure was performed (NAACCR
  # Item #1280). This item is only available for diagnosis years 2003 and later.

  var_label(df$DX_STAGING_PROC_DAYS) <- "Diagnostic and Staging procedure, days from DX"


  #RX_SUMM_DXSTG_PROC
  # Records the type of surgical diagnostic and/or staging procedure performed.

  df$RX_SUMM_DXSTG_PROC <-
    factor(
      df$RX_SUMM_DXSTG_PROC,
      levels = c(0, 1, 2, 3, 4, 5, 7, 9),
      labels = c(
        "No surgical diagnosis or staging performed",
        "Biopsy done to site other than primary",
        "Biopsy done to the primary done",
        "Surgical exploration only, no biopsy or treatment",
        "Surgical procedure with bypass, no biopsy",
        "Exploratory procedure with biopsy of primary or other site",
        "Procedure done, type unknown",
        "No information on diagnosis or staging"
      )
    )

  var_label(df$RX_SUMM_DXSTG_PROC) <- "Diagnostic and Staging Procedure"

  # RX_HOSP_DXSTG_PROC
  # Records the type of surgical diagnostic and/or staging procedure performed at the reporting facility.
  # This data item was added to the 2015 PUF (data released in Fall 2017),
  # and does not appear in prior versions of the PUF data.

  df$RX_HOSP_DXSTG_PROC <-
    factor(
      as.numeric(df$RX_HOSP_DXSTG_PROC),
      levels = c(0, 1, 2, 3, 4, 5, 6, 7, 9),
      labels = c(
        "No surgical diagnostic or staging procedure was performed",
        "A biopsy (incisional, needle, or aspiration) was done to a site other than the primary. No exploratory procedure was done",
        "A biopsy (incisional, needle, or aspiration) was done to the primary site",
        "A surgical exploration only. The patient was not biopsied or treated",
        "A surgical procedure with a bypass was performed, but no biopsy was done",
        "An exploratory procedure was performed, and a biopsy of either the primary site or another site was done",
        "A bypass procedure was performed, and a biopsy of either the primary site or another site was done",
        "A procedure was done, but the type of procedure is unknown",
        "No information of whether a diagnostic or staging procedure was performed"
      )
    )

  var_label(df$RX_HOSP_DXSTG_PROC) <- "Diagnostic and Staging Procedure at This Facility"

  #TNM_CLIN_T
  # Identifies the clinically determined size and/or extension of the primary tumor (cT)
  # as defined by the American Joint Committee on Cancer (AJCC).

  #Remove white space from TNM variables
  df$TNM_CLIN_T <- trimws(df$TNM_CLIN_T, which = c("both", "left", "right"), whitespace = "[ \t\r\n]")
  df$TNM_CLIN_N <- trimws(df$TNM_CLIN_N, which = c("both", "left", "right"), whitespace = "[ \t\r\n]")
  df$TNM_CLIN_M <- trimws(df$TNM_CLIN_M, which = c("both", "left", "right"), whitespace = "[ \t\r\n]")
  df$TNM_PATH_T <- trimws(df$TNM_PATH_T, which = c("both", "left", "right"), whitespace = "[ \t\r\n]")
  df$TNM_PATH_N <- trimws(df$TNM_PATH_N, which = c("both", "left", "right"), whitespace = "[ \t\r\n]")
  df$TNM_PATH_M <- trimws(df$TNM_PATH_M, which = c("both", "left", "right"), whitespace = "[ \t\r\n]")
  df$TNM_CLIN_STAGE_GROUP <- trimws(df$TNM_CLIN_STAGE_GROUP, which = c("both", "left", "right"), whitespace = "[ \t\r\n]")
  df$TNM_PATH_STAGE_GROUP <- trimws(df$TNM_PATH_STAGE_GROUP, which = c("both", "left", "right"), whitespace = "[ \t\r\n]")

  df$TNM_CLIN_T <-
    factor(
      df$TNM_CLIN_T,
      levels = c(
        "cX",
        "c0",
        "cA",
        "pIS",
        "pISPU",
        "pISPD",
        "c1MI",
        "c1",
        "c1A",
        "c1A1",
        "c1A2",
        "c1B",
        "c1B1",
        "c1B2",
        "c1C",
        "c1D",
        "c2",
        "c2A",
        "c2A1",
        "c2A2",
        "c2B",
        "c2C",
        "c2D",
        "c3",
        "c3A",
        "c3B",
        "c3C",
        "c3D",
        "c4",
        "c4A",
        "c4B",
        "c4C",
        "c4D",
        "c4E",
        "88"
      ),
      labels = c(
        "cTX",
        "cT0",
        "cTa",
        "pTis",
        "pTispu",
        "pTispd",
        "cT1mic",
        "cT1",
        "cT1a",
        "cT1a1",
        "cT1a2",
        "cT1b",
        "cT1b1",
        "cT1b2",
        "cT1c",
        "cT1d",
        "cT2",
        "cT2a",
        "cT2a1",
        "cT2a2",
        "cT2b",
        "cT2c",
        "cT2d",
        "cT3",
        "cT3a",
        "cT3b",
        "cT3c",
        "cT3d",
        "cT4",
        "cT4a",
        "cT4b",
        "c4c",
        "c4d",
        "c4e",
        "Not applicable"
      )
    )

  df$cT_RECODE <- NA
  df$cT_RECODE[df$TNM_CLIN_T %in% c("cTX")] <- 0
  df$cT_RECODE[df$TNM_CLIN_T %in% c("cT0", "cTa")] <- 1
  df$cT_RECODE[df$TNM_CLIN_T %in% c("cT1", "cT1a", "cT1a1", "cT1a2", "ct1b", "ct1b1", "ct1b2", "cT1c", "cT1d")] <- 2
  df$cT_RECODE[df$TNM_CLIN_T %in% c("cT2", "cT2a", "cT2a1", "cT2a2", "cT2b", "cT2c", "cT2d")] <- 3
  df$cT_RECODE[df$TNM_CLIN_T %in% c("cT3", "cT3a", "cT3b", "cT3c", "cT3d")] <- 4
  df$cT_RECODE[df$TNM_CLIN_T %in% c("cT4")] <- 5
  df$cT_RECODE[df$TNM_CLIN_T %in% c("cT4a")] <-6
  df$cT_RECODE[df$TNM_CLIN_T %in% c("cT4b")] <- 7
  df$cT_RECODE[df$TNM_CLIN_T %in% c("c4c")] <- 8
  df$cT_RECODE[df$TNM_CLIN_T %in% c("c4d")] <- 9
  df$cT_RECODE[df$TNM_CLIN_T %in% c("c4e")] <- 10
  df$cT_RECODE[df$TNM_CLIN_T %in% c("pTis", "pTispu", "pTispd")] <- 11 ## Look into how this is used/what to call these



  df$cT_RECODE <-
    factor(
      df$cT_RECODE,
      levels = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11),
      labels = c("Tx",
                 "T0",
                 "T1",
                 "T2",
                 "T3",
                 "T4",
                 "T4a",
                 "T4b",
                 "T4c",
                 "T4d",
                 "T4e",
                 "cIs or similar")
    )

  var_label(df$cT_RECODE) <- "Clinical T"

  #TNM_CLIN_N
  # Identifies the clinically determined absence or presence of regional lymph node
  # (cN) metastasis and describes the extent of the regional lymph node metastasis as
  # defined by the American Joint Committee on Cancer (AJCC).
  df$TNM_CLIN_N <-
    factor(
      df$TNM_CLIN_N,
      levels = c(
        "cX",
        "c0",
        "c0I-",
        "c0I+",
        "c0M-",
        "c0M+",
        "c1MI",
        "c0A",
        "c0B",
        "c1",
        "c1A",
        "c1B",
        "c1C",
        "c2",
        "c2A",
        "c2B",
        "c2C",
        "c3",
        "c3A",
        "c3B",
        "c3C",
        "c4",
        "88"
      ),
      labels = c(
        "cNX",
        "cN0",
        "cN0i-",
        "cN0i+",
        "cN0m-",
        "cN0m+",
        "cN1mi",
        "cN0a",
        "cN0b",
        "cN1",
        "cN1a",
        "cN1b",
        "cN1c",
        "cN2",
        "cN2a",
        "cN2b",
        "cN2c",
        "cN3",
        "cN3a",
        "cN3b",
        "cN3c",
        "cN4",
        "Not applicable"
      )
    )


  df$cN_RECODE <- NA
  df$cN_RECODE[df$TNM_CLIN_N %in% c("cNX")] <- 0
  df$cN_RECODE[df$TNM_CLIN_N %in% c("cN0", "cN0i-", "cN0i+", "cN0m-", "cN0m+", "cN0a", "cN0b")] <- 1
  df$cN_RECODE[df$TNM_CLIN_N %in% c("cN1mi", "cN1", "cN1a", "cN1b", "cN1c")] <- 2
  df$cN_RECODE[df$TNM_CLIN_N %in% c("cN2", "cN2a", "cN2b", "cN2c")] <- 3
  df$cN_RECODE[df$TNM_CLIN_N %in% c("cN2a")] <- 4
  df$cN_RECODE[df$TNM_CLIN_N %in% c("cN2b")] <- 5
  df$cN_RECODE[df$TNM_CLIN_N %in% c("cN2c")] <- 6
  df$cN_RECODE[df$TNM_CLIN_N %in% c("cN3", "cN3a", "cN3b", "cN3c")] <- 7
  df$cN_RECODE[df$TNM_CLIN_N %in% c("cN4")] <- 8


  df$cN_RECODE <-
    factor(
      df$cN_RECODE,
      levels = c(0, 1, 2, 3, 4, 5, 6, 7),
      labels = c("NX",
                 "N0",
                 "N1",
                 "N2A",
                 "N2B",
                 "N2C",
                 "N3",
                 "N4")
    )

  var_label(df$cN_RECODE) <- "Clinical N"

  #TNM_CLIN_M
  # # Identifies the clinically determined absence or presence of distant metastasis (cM)
  # # as defined by the American Joint Committee on Cancer (AJCC).
  df$TNM_CLIN_M <-
    factor(
      df$TNM_CLIN_M,
      levels = c("cX", "c0", "c0I+", "c1", "c1A", "c1B", "c1C", "c1D", "88"),
      labels = c(
        "cMX",
        "cM0",
        "cM0(i+)",
        "cM1",
        "cM1a",
        "cM1b",
        "cM1c",
        "cM1d",
        "Not applicable (not defined)"
      )
    )



  df$cM_RECODE <- NA
  df$cM_RECODE[df$TNM_CLIN_M %in% c("cMX")] <- 0
  df$cM_RECODE[df$TNM_CLIN_M %in% c("cM0", "cM0(i+)")] <- 1
  df$cM_RECODE[df$TNM_CLIN_M %in% c("cM1", "cM1a", "cM1b", "cM1c", "cM1d")] <- 2
  df$cM_RECODE[df$TNM_CLIN_M %in% c("Not applicable (not defined)")] <- ""


  df$cM_RECODE <-
    factor(
      df$cM_RECODE,
      levels = c(0, 1, 2),
      labels = c("Mx",
                 "M0",
                 "M1"
      )
    )

  var_label(df$cM_RECODE) <- "Clinical M"

  #TNM_CLIN_STAGE_GROUP - AJCC Clinical Stage Group
  # Identifies the applicable stage group based on the T, N, and M elements as defined
  # by the American Joint Committee on Cancer (AJCC).
  df$TNM_CLIN_STAGE_GROUP <-
    factor(
      df$TNM_CLIN_STAGE_GROUP,
      levels = c(
        "0",
        "0A",
        "0IS",
        "1",
        "1A",
        "1A1",
        "1A2",
        "1B",
        "1B1",
        "1B2",
        "1C",
        "1S",
        "2",
        "2A",
        "2A1",
        "2A2",
        "2B",
        "2C",
        "3",
        "3A",
        "3B",
        "3C",
        "3C1",
        "3C2",
        "4",
        "4A",
        "4A1",
        "4A2",
        "4B",
        "4C",
        "OC",
        "88",
        "99"
      ),
      labels = c(
        "cStage 0",
        "cStage 0A",
        "cStage 0is",
        "cStage I",
        "cStage IA",
        "cStage IA1",
        "cStage IA2",
        "cStage IB",
        "cStage IB1",
        "cStage IB2",
        "cStage IC",
        "cStage IS",
        "cStage II",
        "cStage IIA",
        "cStage IIA1",
        "cStage IIA2",
        "cStage IIB",
        "cStage IIC",
        "cStage III",
        "cStage IIIA",
        "cStage IIIB",
        "cStage IIIC",
        "cStage IIIC1",
        "cStage IIIC2",
        "cStage IV",
        "cStage IVA",
        "cStage IVA1",
        "cStage IVA2",
        "cStage IVB",
        "cStage IVC",
        "Occult",
        "Not applicable",
        "Unknown"
      )
    )

  var_label(df$TNM_CLIN_STAGE_GROUP) <- "AJCC Clinical Stage Group"

  df$cSTAGE_RECODE <- NA
  df$cSTAGE_RECODE[df$TNM_CLIN_STAGE_GROUP %in% c("cStage 0","cStage 0A","cStage 0is")] <- 0
  df$cSTAGE_RECODE[df$TNM_CLIN_STAGE_GROUP %in% c("cStage I","cStage IA","cStage IA1","cStage IA2","cStage IB","cStage IB1","cStage IB2","cStage IC","cStage IS")] <- 1
  df$cSTAGE_RECODE[df$TNM_CLIN_STAGE_GROUP %in% c("cStage II","cStage IIA","cStage IIA1","cStage IIA2","cStage IIB","cStage IIC")] <- 2
  df$cSTAGE_RECODE[df$TNM_CLIN_STAGE_GROUP %in% c("cStage III","cStage IIIA","cStage IIIB","cStage IIIC","cStage IIIC1","cStage IIIC2")] <- 3
  df$cSTAGE_RECODE[df$TNM_CLIN_STAGE_GROUP %in% c("cStage IV", "cStage IVA","cStage IVA1","cStage IVA2","cStage IVB","cStage IVC")] <- 4
  df$cSTAGE_RECODE[df$TNM_CLIN_STAGE_GROUP %in% c("Occult")] <- 5
  df$cSTAGE_RECODE[df$TNM_CLIN_STAGE_GROUP %in% c("Not applicable", "Unknown")] <- 6


  df$cSTAGE_RECODE <-
    factor(
      df$cSTAGE_RECODE,
      levels = c(0, 1, 2, 3, 4, 5, 6),
      labels = c("cStage 0",
                 "cStage I",
                 "cStage II",
                 "cStage III",
                 "cStage IV",
                 "Occult",
                 "Not applicable/Unknown")
    )

  var_label(df$cSTAGE_RECODE) <- "AJCC Clincal Stage Group"




  #TNM_PATH_T - AJCC Pathologic T
  # Identifies the pathologically-determined tumor size and/or extension (pT) as
  # defined by the American Joint Committee on Cancer (AJCC)

  df$TNM_PATH_T <-
    factor(
      df$TNM_PATH_T,
      levels = c(
        "pX",
        "p0",
        "pA",
        "pIS",
        "pISPU",
        "pISPD",
        "p1MI",
        "p1",
        "p1A",
        "p1A1",
        "p1A2",
        "p1B",
        "p1B1",
        "p1B2",
        "p1C",
        "p1D",
        "p2",
        "p2A1",
        "p2A2",
        "p2B",
        "p2C",
        "p2D",
        "p3",
        "p3A",
        "p3B",
        "p3C",
        "p3D",
        "p4",
        "p4A",
        "p4B",
        "p4C",
        "p4D",
        "p4E",
        "88"
        ),
      labels = c(
        "pTX",
        "pT0",
        "pTa",
        "pTis",
        "pTispu",
        "pTispd",
        "pT1mic",
        "pT1",
        "pT1a",
        "pT1a1",
        "pT1a2",
        "pT1b",
        "pT1b1",
        "pT1b2",
        "pT1c",
        "pT1d",
        "pT2",
        "pT2a1",
        "pT2a2",
        "pT2b",
        "pT2c",
        "pT2d",
        "pT3",
        "pT3a",
        "pT3b",
        "pT3c",
        "pT3d",
        "pT4",
        "pT4a",
        "pT4b",
        "p4c",
        "p4d",
        "p4e",
        "Not applicable"
      )
    )

  df$pT_RECODE <- NA
  df$pT_RECODE[df$TNM_PATH_T %in% c("pTX")] <- 0
  df$pT_RECODE[df$TNM_PATH_T %in% c("pT0", "pTa")] <- 1
  df$pT_RECODE[df$TNM_PATH_T %in% c("pT1", "pT1a", "pT1a1", "pT1a2", "pT1b", "pT1b1", "pT1b2", "pT1c", "pT1d")] <- 2
  df$pT_RECODE[df$TNM_PATH_T %in% c("pT2", "pT2a", "pT2a1", "pT2a2", "pT2b", "pT2c", "pT2d")] <- 3
  df$pT_RECODE[df$TNM_PATH_T %in% c("pT3", "pT3a", "pT3b", "pT3c", "pT3d")] <- 4
  df$pT_RECODE[df$TNM_PATH_T %in% c("pT4")] <- 5
  df$pT_RECODE[df$TNM_PATH_T %in% c("pT4a")] <-6
  df$pT_RECODE[df$TNM_PATH_T %in% c("pT4b")] <- 7
  df$pT_RECODE[df$TNM_PATH_T %in% c("c4c")] <- 8
  df$pT_RECODE[df$TNM_PATH_T %in% c("c4d")] <- 9
  df$pT_RECODE[df$TNM_PATH_T %in% c("c4e")] <- 10
  df$pT_RECODE[df$TNM_PATH_T %in% c("pTis", "pTispu", "pTispd")] <- 11 ## Look into how this is used/what to call these



  df$pT_RECODE <-
    factor(
      df$pT_RECODE,
      levels = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11),
      labels = c("Tx",
                 "T0",
                 "T1",
                 "T2",
                 "T3",
                 "T4",
                 "T4a",
                 "T4b",
                 "T4c",
                 "T4d",
                 "T4e",
                 "cIs or similar")
    )

  var_label(df$pT_RECODE) <- "Pathologic T"


  #TNM_PATH_N - AJCC Pathologic N
  # Identifies the pathologically-determined absence or presence or extent of regional
  # lymph node (pN) metastasis as defined by the American Joint Committee on
  # Cancer (AJCC)

  df$TNM_PATH_N <-
    factor(
      df$TNM_PATH_N,
      levels = c(
        "pX",
        "p0",
        "p0I-",
        "p0I+",
        "p0M-",
        "p0M+",
        "p1MI",
        "p0A",
        "p0B",
        "p1",
        "p1A",
        "p1B",
        "p1C",
        "p2",
        "p2A",
        "p2B",
        "p2C",
        "p3",
        "p3A",
        "p3B",
        "p3C",
        "p4",
        "88"
      ),
      labels = c(
        "pNX",
        "pN0",
        "pN0i-",
        "pN0i+",
        "pN0m-",
        "pN0m+",
        "pN1mi",
        "pN0a",
        "pN0b",
        "pN1",
        "pN1a",
        "pN1b",
        "pN1c",
        "pN2",
        "pN2a",
        "pN2b",
        "pN2c",
        "pN3",
        "pN3a",
        "pN3b",
        "pN3c",
        "pN4",
        "Not applicable"
      )
    )



  df$pN_RECODE <- NA
  df$pN_RECODE[df$TNM_PATH_N %in% c("pNX")] <- 0
  df$pN_RECODE[df$TNM_PATH_N %in% c("pN0", "pN0i-", "pN0i+", "pN0m-", "pN0m+", "pN0a", "pN0b")] <- 1
  df$pN_RECODE[df$TNM_PATH_N %in% c("pN1mi", "pN1", "pN1a", "pN1b", "pN1c")] <- 2
  df$pN_RECODE[df$TNM_PATH_N %in% c("pN2", "pN2a", "pN2b", "pN2c")] <- 3
  df$pN_RECODE[df$TNM_PATH_N %in% c("pN3", "pN3a", "pN3b", "pN3c")] <- 4
  df$pN_RECODE[df$TNM_PATH_N %in% c("pN4")] <- 5


  df$pN_RECODE <-
    factor(
      df$pN_RECODE,
      levels = c(0, 1, 2, 3, 4, 5),
      labels = c("NX",
                 "N0",
                 "N1",
                 "N2",
                 "N3",
                 "N4")
    )

  var_label(df$pN_RECODE) <- "Pathologic N"

  #TNM_PATH_M - AJCC Pathologic M
  # Identifies the pathologically determined tumor size and/or extension (pT) as
  # defined by the American Joint Committee on Cancer (AJCC)

  df$TNM_PATH_M <-
    factor(
      df$TNM_PATH_M,
      levels = c(
        "p0",
        "p0I+",
        "p1",
        "p1A",
        "p1B",
        "p1C",
        "p1D",
        "88"
      ),
      labels = c(
        "pM0",
        "pM0(i+)",
        "pM1",
        "pM1a",
        "pM1b",
        "pM1c",
        "pM1d",
        "Not appliable"
      )
    )


  df$pM_RECODE <- NA
  df$pM_RECODE[df$TNM_PATH_M %in% c("pMX")] <- 0
  df$pM_RECODE[df$TNM_PATH_M %in% c("pM0", "pM0(i+)")] <- 1
  df$pM_RECODE[df$TNM_PATH_M %in% c("pM1", "pM1a", "pM1b", "pM1c", "pM1d")] <- 2
  df$pM_RECODE[df$TNM_PATH_M %in% c("Not applicable (not defined)")] <- ""


  df$pM_RECODE <-
    factor(
      df$pM_RECODE,
      levels = c(0, 1, 2),
      labels = c("Mx",
                 "M0",
                 "M1"
      )
    )

  var_label(df$pM_RECODE) <- "Pathologic M"

  #TNM_PATH_STAGE_GROUP
  # Identifies the pathologically-determined anatomic extent of disease based on the T,
  # N, and M elements as defined by the American Joint Committee on Cancer (AJCC).

  df$TNM_PATH_STAGE_GROUP <-
    factor(
      df$TNM_PATH_STAGE_GROUP,
      levels = c(
        "0",
        "0A",
        "0IS",
        "1",
        "1A",
        "1A1",
        "1A2",
        "1B",
        "1B1",
        "1B2",
        "1C",
        "IS",
        "2",
        "2A",
        "2A1",
        "2A2",
        "2B",
        "2C",
        "3",
        "3A",
        "3B",
        "3C",
        "3C1",
        "3C2",
        "4",
        "4A",
        "4A1",
        "4A2",
        "4B",
        "4C",
        "OC",
        "88",
        "99"
      ),
      labels = c(
        "pStage 0",
        "pStage 0A",
        "pStage 0is",
        "pStage I",
        "pStage IA",
        "pStage IA1",
        "pStage IA2",
        "pStage IB",
        "pStage IB1",
        "pStage IB2",
        "pStage IC",
        "pStage IS",
        "pStage II",
        "pStage IIA",
        "pStage IIA1",
        "pStage IIA2",
        "pStage IIB",
        "pStage IIC",
        "pStage III",
        "pStage IIIA",
        "pStage IIIB",
        "pStage IIIC",
        "pStage IIIC1",
        "pStage IIIC2",
        "pStage IV",
        "pStage IVA",
        "pStage IVA1",
        "pStage IVA2",
        "pStage IVB",
        "pStage IVC",
        "Occult",
        "Not applicable",
        "Unknown"
        )
    )

  df$pSTAGE_RECODE <- NA
  df$pSTAGE_RECODE[df$TNM_PATH_STAGE_GROUP %in% c("pStage 0","pStage 0A","pStage 0is")] <- 0
  df$pSTAGE_RECODE[df$TNM_PATH_STAGE_GROUP %in% c("pStage I","pStage IA","pStage IA1","pStage IA2","pStage IB","pStage IB1","pStage IB2","pStage IC","pStage IS")] <- 1
  df$pSTAGE_RECODE[df$TNM_PATH_STAGE_GROUP %in% c("pStage II","pStage IIA","pStage IIA1","pStage IIA2","pStage IIB","pStage IIC")] <- 2
  df$pSTAGE_RECODE[df$TNM_PATH_STAGE_GROUP %in% c("pStage III","pStage IIIA","pStage IIIB","pStage IIIC","pStage IIIC1","pStage IIIC2")] <- 3
  df$pSTAGE_RECODE[df$TNM_PATH_STAGE_GROUP %in% c("pStage IV", "pStage IVA","pStage IVA1","pStage IVA2","pStage IVB","pStage IVC")] <- 4
  df$pSTAGE_RECODE[df$TNM_PATH_STAGE_GROUP %in% c("Occult")] <- 5
  df$pSTAGE_RECODE[df$TNM_PATH_STAGE_GROUP %in% c("Not applicable", "Unknown")] <- 6


  df$pSTAGE_RECODE <-
    factor(
      df$pSTAGE_RECODE,
      levels = c(0, 1, 2, 3, 4, 5, 6),
      labels = c("pStage 0",
                 "pStage I",
                 "pStage II",
                 "pStage III",
                 "pStage IV",
                 "Occult",
                 "Not applicable/Unknown")
    )

  var_label(df$pSTAGE_RECODE) <- "AJCC Pathologic Stage Group"


  #TNM_EDITION_NUMBER
  # Identifies the edition number of the AJCC Cancer Staging Manual used to stage the
  # case

  df$TNM_EDITION_NUMBER <-
    factor(
      df$TNM_EDITION_NUMBER,
      levels = c(
        "0",
        "5",
        "6",
        "7",
        "88",
        "99"
      ),
      labels = c(
        "Not staged", #cases that have AJCC staging scheme and staging was not done
        "5th Edition",
        "6th Edition",
        "7th Edition",
        "Not applicable", #cases that do not have an AJCC staging scheme
        "Staged, edition unknown" #prior to the 5th edition
      )
    )


  var_label(df$TNM_EDITION_NUMBER) <- "TNM Edition Number"

  #ANALYTIC_STAGE_GROUP
  # Analytic Stage Group is assigned the value of reported Pathologic Stage Group.
  # Clinical Stage Group is used if pathologic stage is not reported. Sub-stage groups
  # are collapsed into the corresponding general stage designation. The alphanumeric
  # representation of stage group is provided for ease of display

  df$ANALYTIC_STAGE_GROUP <-
    factor(
      df$ANALYTIC_STAGE_GROUP,
      levels = c(0, 1, 2, 3, 4, 5, 8, 9),
      labels = c(
        "Stage 0",
        "Stage I",
        "Stage II",
        "Stage III",
        "Stage IV",
        "Occult (lung only)",
        "AJCC Staging not applicable",
        "AJCC Stage group unknown"
      )
    )


  ### CS_METS_DX_BONE - CS Mets at DX-Bone
  # Identifies whether there is metastatic involvement of distant site(s) at the time of
  # diagnosis

  df$METS_AT_DX_BONE <-
    factor(
      df$CS_METS_DX_BONE,
      levels = c(0, 1, 8, 9),
      labels = c(
        "None", # no bone metastases
        "Yes",
        "Not applicable",
        "Unknown" # whether bone is involved; Not documented in patient record
      )
    )

  var_label(df$METS_AT_DX_BONE) <- "Bone metastases at diagnosis"


  #METS_AT_DX_BRAIN
  # Identifies the presence of distant metastatic involvement of the bone at the time of
  # diagnosis

  df$METS_AT_DX_BRAIN <-
    factor(
      df$METS_AT_DX_BRAIN,
      levels = c(0, 1, 8, 9),
      labels = c(
        "None", # no brain metastases"
        "Yes",
        "Not applicable",
        "Unknown" # unknown  whether brain is involved; Not documented in patient record
      )
    )

  var_label(df$CS_METS_DX_BRAIN) <- "Brain metastases as diagnosis"

  #METS_AT_DX_LIVER
  # Identifies the presence of distant metastatic involvement of the liver at the time of
  # diagnosis

  df$METS_AT_DX_LIVER <-
    factor(
      df$CS_METS_DX_LIVER,
      levels = c(0, 1, 8, 9),
      labels = c(
        "None", # no liver metastases"
        "Yes",
        "Not applicable",
        "Unknown" # whether liver is involved; Not documented in patient record
      )
    )

  var_label(df$METS_AT_DX_LIVER) <- "Liver metastases as diagnosis"


  #METS_AT_DX_LUNG
  # Identifies the presence of distant metastatic involvement of the lung at the time of
  # diagnosis

  df$METS_AT_DX_LUNG <-
    factor(
      df$METS_AT_DX_LUNG,
      levels = c(0, 1, 8, 9),
      labels = c(
        "None", # no lung metastases
        "Yes",
        "Not applicable",
        "Unknown" # whether lung is involved; Not documented in patient record
      )
    )

  var_label(df$METS_AT_DX_LUNG) <- "Lung metastases as diagnosis"

  #METS_AT_DX_DISTANT_LN
  # identifies whether distant lymph node(s) are an involved metastatic site.

  df$METS_AT_DX_DISTANT_LN <-
    factor(
      df$METS_AT_DX_DISTANT_LN,
      levels = c(0, 1, 8, 9),
      labels = c(
        "None", # no distant LN metastases
        "Yes",
        "Not applicable",
        "Unknown" # whether distant LN are involved; Not documented in patient record
      )
    )

  var_label(df$METS_AT_DX_DISTANT_LN) <- "Distant LN metastases as diagnosis"


  #METS_AT_DX_OTHER
  # This data item identifies whether other metastatic involvement, other than bone, brain,
  #liver, lung or distant lymph nodes exists. Some examples include but are not limited to
  #the adrenal gland, bone marrow, pleura, peritoneum, and skin. T

  df$METS_AT_DX_OTHER <-
    factor(
      df$METS_AT_DX_OTHER,
      levels = c(0, 1, 2, 8, 9),
      labels = c(
        "None", # no other metastases
        "Yes", # distant metastases in known site(s) other than bone, brain, liver, lung or distant lymph nodes.
        "Generalized metastases", #such as carcinomatosis
        "Not applicable",
        "Unknown" # whether other metastatic site is involved; Not documented in patient record
      )
    )

  var_label(df$METS_AT_DX_OTHER) <- "Other metastases as diagnosis"

  #CS_EXTENSION
  # Identifies contiguous growth (extension) of the primary tumor within the organ or origin or its direct
  # extension into neighboring organs. For some sites such as ovary, discontinuous metastasis is coded in CS Extension.
  # CS extension codes are found here: http://ncdbpuf.facs.org/?q=node/370
  # TODO: create function for CS values

  # separate function here to decode using this http://ncdbpuf.facs.org/sites/default/files/cs/cs_head_neck_replpgs01.02.00.pdf

  #CS_TUMOR_SIZEEXT_EVAL
  # Records how the codes for the two items, CS Tumor Size and CS Extension were determined,
  # based on the diagnostic methods employed.
  # TODO: pull info from here: http://web2.facs.org/cstage0205/mouthother/MouthOther_cpa.html

  ### LYMPH_VASCULAR_INVASION
  # Indicates the presence or absence of tumor cells in lymphatic channels (not lymph
  # nodes) or blood vessels within the primary tumor as noted microscopically by the
  # pathologist. This data item is separate from the CS data items but is included in this
  # manual because of its relationship to the Collaborative Stage Data Collection
  # System. Lymph-vascular invasion is an item of interest to both pathologists and
  # clinicians and is mentioned in many chapters of the AJCC Cancer Staging Manual,
  # seventh edition. This field is required for mapping of T in some sites, such as testis
  # and penis.

  df$LYMPH_VASCULAR_INVASION <-
    factor(
      df$LYMPH_VASCULAR_INVASION,
      levels = c(0, 1, 8, 9),
      labels = c(
        "None", #http://web2.facs.org/cstage0205/mouthother/MouthOther_cpa.html
        "Present", #Lymph-vascular invasion is present or identified",
        "Not applicable",
        "Unknown" # unknown if lymph-vascular invasion is present, or indeterminant"
      )
    )

  var_label(df$LYMPH_VASCULAR_INVASION) <- "Lympovascular Invasion"


  ### CS_METS_AT_DX - CS Mets at DX
  # Identifies whether there is metastatic involvement of distant site(s) at the time of
  # diagnosis.
  # codes found at http://web2.facs.org/205/nasalcavity/NasalCavity_hpb.html
  # TODO: large effort to make this applicable to all NCDB sites since each CS variable
  # has site-specific values.
  # df$CS_METS_AT_DX <-
  #   factor(
  #     as.numeric(df$CS_METS_AT_DX),
  #     levels = c(00, 10, 40, 50, 60, 99),
  #     labels = c(
  #       "No distant metastasis",
  #       "Distant lymph node(s)",
  #       "Distant metastases except distant lymph nodes",
  #       "Distant metastasis plus distant lymph nodes",
  #       "Distant metastasis, NOS",
  #       "Unknown"
  #     )
  #   )

  var_label(df$CS_METS_AT_DX) <- "(CS) Metastases at diagnosis"

  ### CS_METS_DX_BONE - CS Mets at DX-Bone
  # Identifies whether there is metastatic involvement of distant site(s) at the time of
  # diagnosis

  df$CS_METS_DX_BONE <-
    factor(
      df$CS_METS_DX_BONE,
      levels = c(0, 1, 8, 9),
      labels = c(
        "None", # no bone metastases
        "Yes",
        "Not applicable",
        "Unknown" # whether bone is involved; Not documented in patient record
      )
    )

  var_label(df$CS_METS_DX_BONE) <- "(CS) Bone metastases at diagnosis"

  ### CS_METS_DX_Brain - CS Mets at DX-Brain
  # Identifies the presence of distant metastatic involvement of the bone at the time of
  # diagnosis

  df$CS_METS_DX_BRAIN <-
    factor(
      df$CS_METS_DX_BRAIN,
      levels = c(0, 1, 8, 9),
      labels = c(
        "None", # no brain metastases"
        "Yes",
        "Not applicable",
        "Unknown" # unknown  whether brain is involved; Not documented in patient record
      )
    )

  var_label(df$CS_METS_DX_BRAIN) <- "(CS) Brain metastases as diagnosis"

  ### CS_METS_DX_Liver - CS Mets at DX-Liver
  # Identifies the presence of distant metastatic involvement of the liver at the time of
  # diagnosis

  df$CS_METS_DX_LIVER <-
    factor(
      df$CS_METS_DX_LIVER,
      levels = c(0, 1, 8, 9),
      labels = c(
        "None", # no liver metastases"
        "Yes",
        "Not applicable",
        "Unknown" # whether liver is involved; Not documented in patient record
      )
    )

  var_label(df$CS_METS_DX_LIVER) <- "(CS) Liver metastases as diagnosis"


  ### CS_METS_DX_LUNG - CS Mets at DX-LUNG
  # Identifies the presence of distant metastatic involvement of the lung at the time of
  # diagnosis

  df$CS_METS_DX_LUNG <-
    factor(
      df$CS_METS_DX_LUNG,
      levels = c(0, 1, 8, 9),
      labels = c(
        "None", # no lung metastases
        "Yes",
        "Not applicable",
        "Unknown" # whether lung is involved; Not documented in patient record
      )
    )

  var_label(df$CS_METS_DX_LUNG) <- "(CS) Lung metastases as diagnosis"

  #CS_SITESPECIFIC_FACTORs
  # TODO: http://ncdbpuf.facs.org/?q=node/370 find way to do this

  #CS_METS_EVAL
  # Records how the code for CS Mets at DX was determined based on the diagnostic methods employed.
  # TODO: do with other CS codes

  #TUMOR_SIZE
  # Describes the largest dimension of the diameter of the primary tumor in millimeters(mm).

  var_label(df$TUMOR_SIZE) <- "Size of Tumor (mm)"

  #CS_VERSION_LATEST
  #This is the version number of the most recent derivation of CS data items in the record.
  # TODO

  #TREATMENT####

  #RX_SUMM_TREATMENT_STATUS - Treatment Status
  # This item summarizes whether the patient received any treatment or was under
  # active surveillance.
  df$RX_SUMM_TREATMENT_STATUS <-
    factor(
      df$RX_SUMM_TREATMENT_STATUS,
      levels = c(0, 1, 2, 9),
      labels = c(
        "No treatment given",
        "Treatment given",
        "Active surveillance (watchful waiting)",
        "Unknown if treatment given"
      )
    )

  var_label(df$RX_SUMM_TREATMENT_STATUS) <- "Treatment status"

  #DX_RX_STARTED_DAYS
  # The number of days between the date of diagnosis (NAACCR Item #390) and the
  # date on which treatment [surgery, radiation, systemic, or other therapy] (NAACCR
  # Item #1270) of the patient began at any facility

  var_label(df$DX_RX_STARTED_DAYS) <- "Treatment started, days from diagnosis"

  #DX_SURG_STARTED_DAYS - First Surgical Procedure, days from dx
  # The number of days between the date of diagnosis (NAACCR Item #390) and the
  # date the first treatment surgery was performed (NAACCR Item #1200). The surgery
  # may be primary site surgery (NAACCR Item #1290), regional lymph node surgery
  # (NAACCR Item #1292) or other regional or distant surgery (NAACCR Item #1294).
  # Incisional biopsies are not coded as treatment surgery.

  var_label(df$DX_SURG_STARTED_DAYS) <- "First surgical procedure, days from diagnosis"

  #DX_DEFSURG_STARTED_DAYS - Definitive Surgical Procedure, days from dx
  # The number of days between the date of diagnosis (NAACCR Item #390) and the
  # date on which the most definitive surgical procedure was performed on the primary
  # site (NAACCR Item #3170).

  var_label(df$DX_DEFSURG_STARTED_DAYS) <- "Definitive Surgical Procedure, Days from Dx"

  #RX_SUMM_SURG_PRIM_SITE - Surgical procedure of the primary site
  # Records the surgical procedure performed to the primary site at any facility.
  # "00-None No surgical procedure of primary site. Diagnosed at autopsy."
  # "10--19 Site-specific codes; tumor destruction Tumor destruction, no pathologic specimen produced.  Refer to Surgery of the Primary Site Codes for the correct site-specific code for the procedure."
  # "20--80-Site-specific codes; resection Refer to Surgery of the Primary Site Codes for the correct site-specific code for the procedure. "
  # "90-Surgery, NOS A surgical procedure to the primary site was done, but no information on the type of surgical procedure is provided. "
  # "98-Site-specific codes; special Special code. Refer to Surgery of the Primary Site Codes for the correct site-specific code for the procedure."
  # "99-Unknown Patient record does not state whether a surgical procedure of the primary site was performed and no information is available. Death certificate only."
  # TODO

  df$RX_SUMM_SURG_PRIM_SITE <-
    factor(
      df$RX_SUMM_SURG_PRIM_SITE,
      levels = c(0,1),
      labels = c("Tumor destruction", "Tumor resection")
    )

  var_label(df$RX_SUMM_SURG_PRIM_SITE) <- "Surgical procedure of primary site"


  #RX_HOSP_SURG_PRIM_SITE
  # This item records the surgical procedure performed to the primary site at the facility
  # that submitted this record
  # Records the surgical procedure performed to the primary site at any facility.
  # "00-None No surgical procedure of primary site. Diagnosed at autopsy."
  # "10-19 Site-specific codes; tumor destruction Tumor destruction, no pathologic specimen produced.  efer to Surgery of the Primary Site Codes for the correct site-specific code for the procedure."
  # "20--80-Site-specific codes; resection Refer to Surgery of the Primary Site Codes for the correct site-specific code for the procedure. "
  # "90-Surgery, NOS A surgical procedure to the primary site was done, but no information on the type of surgical procedure is provided. "
  # "98-Site-specific codes; special Special code. Refer to Surgery of the Primary Site Codes for the correct site-specific code for the procedure."
  # "99-Unknown Patient record does not state whether a surgical procedure of the primary site was performed and no information is available. Death certificate only."

  var_label(df$RX_SUMM_SURG_PRIM_SITE) <- "Surgery at this Facility"

  #RX_HOSP_SURG_APPR_2010 - Surgical Approach ONLY USED AFTER 2010
  # This item is used to monitor patterns and trends in the adoption and utilization of
  # minimally-invasive surgical techniques.

  df$RX_HOSP_SURG_APPR_2010 <-
    factor(
      df$RX_HOSP_SURG_APPR_2010,
      levels = c(0, 1, 2, 3, 4, 5, 9),
      labels = c(
        "No surgical procedure of primary site at this facility.",
        "Robotic assisted.",
        "Robotic converted to open.",
        "Endoscopic or laparoscopic.",
        "Endoscopic or laparoscopic converted to open.",
        "Open or approach unspecified.",
        "Unknown whether surgery was performed at this facility"
      )
    )

  #RX_SUMM_SURGICAL_MARGINS - Surgical Margins
  # Records the final status of the surgical margins after resection of the primary
  # tumor.

    df$RX_SUMM_SURGICAL_MARGINS <-
    factor(
      df$RX_SUMM_SURGICAL_MARGINS,
      levels = c(0, 1, 2, 3, 7, 8, 9),
      labels = c(
        "No residual tumor", # All margins are grossly and microscopically negative
        "Residual tumor, NOS", #Involvement is indicated, but not otherwise specified
        "Microscopic residual tumor", # Cannot be seen by the naked eye
        "Macroscopic residual tumor", #Gross tumor of the primary site which is visible to the naked eye
        "Margins not evaluable", # Cannot be assessed (indeterminate)
        "No primary site surgery", # No surgical procedure of the primary site. Diagnosed at autopsy
        "Unknown or not applicable" # It is unknown whether a surgical procedure to the primary site was performed; death certificate-only; for lymphomas with a lymph node primary site; an unknown or ill-defined primary; or for hematopoietic, reticuloendothelial, immunoproliferative, or myeloproliferative disease"
      )
    )

  df$MARGINS_RECODE <- NA
  df$MARGINS_RECODE[df$RX_SUMM_SURGICAL_MARGINS %in% c("No residual tumor")] <-    0
  df$MARGINS_RECODE[df$RX_SUMM_SURGICAL_MARGINS %in% c(
    "Residual tumor, NOS",
    "Microscopic residual tumor",
    "Macroscopic residual tumor"  )] <- 1
  df$MARGINS_RECODE[df$RX_SUMM_SURGICAL_MARGINS %in% c(
    "MARGINS_RECODE not evaluable",
    "No primary site surgery",
    "Unknown or not applicable")] <- 2

  df$MARGINS_RECODE <-
    factor(
      df$MARGINS_RECODE,
      levels = c(0, 1, 2),
      labels = c("Negative margin",
                 "Positive margin",
                 "Indeterminate/NA")
    )

  var_label(df$MARGINS_RECODE) <- "MARGINS (recoded)"

  #RX_SUMM_SCOPE_REG_LN_SUR
  # Identifies the removal, biopsy, or aspiration of regional lymph node(s) at the time of
  # surgery of the primary site or during a separate surgical event.

  df$RX_SUMM_SCOPE_REG_LN_SUR <-
    factor(
      df$RX_SUMM_SCOPE_REG_LN_SUR,
      levels = c(0, 1, 9),
      labels = c(
        "No regional lymph node surgery",
        "Regional lymph node surgery",
        "Unknown if regional lymph node surgery performed"
      )
    )


  var_label(df$RX_SUMM_SCOPE_REG_LN_SUR) <- "Scope of regional lymph node surgery"

  #RX_SUMM_SCOPE_REG_LN_2012
  # Identifies the removal, biopsy, or aspiration of regional lymph node(s) at the time of
  # surgery of the primary site or during a separate surgical event.
  # TODO bad link on site, can't find values

  # df$RX_SUMM_SCOPE_REG_LN_2012 <-
  #   factor(
  #     df$RX_SUMM_SCOPE_REG_LN_2012,
  #     levels = c(0, 1, 2, 3, 4, 5, 6, 7, 9),
  #     labels = c(""
  #     )
  #   )


  #var_label(df$RX_SUMM_SCOPE_REG_LN_2012) <- "Scope of regional lymph node surgery"


  #RX_SUMM_SURG_OTH_REGDIS
  # Records the surgical removal of distant lymph nodes or other tissue(s)/organ(s)
  # beyond the primary site.

  df$RX_SUMM_SURG_OTH_REGDIS <-
    factor(
      df$RX_SUMM_SURG_OTH_REGDIS,
      levels = c(0, 1, 2, 3, 4, 5, 9),
      labels = c(
        "None", # No nonprimary surgical site resection was performed. Diagnosed at autopsy.",
        "Nonprimary surgical procedure performed", # Nonprimary surgical resection to other site(s), unknown if whether the site(s) is regional or distant.",
        "Nonprimary surgical procedure to other regional sites", # Resection of regional site.",
        "Nonprimary surgical procedure to distant lymph node(s)", # Resection of distant lymph node(s) .",
        "Nonprimary surgical procedure to distant site", # Resection of distant site.",
        "Combination of codes", # Any combination of surgical procedures 2, 3, or 4.",
        "Unknown" # It is unknown whether any surgical procedure of a non primary site was performed. Death certificate only."
      )
    )

  var_label(df$RX_SUMM_SURG_OTH_REGDIS) <- "Surgery at other sites"

  #SURG_DISCHARGE_DAYS
  # The number of days between the date the most definitive surgical procedure was
  # performed on the primary site (NAACCR Item #3170) and the date the patient was
  # discharged following primary site surgery (NAACCR Item #3180)

  var_label(df$SURG_DISCHARGE_DAYS) < "Surgical Inpatient Stay, Days from Surgery"

  #READM_HOSP_30_DAYS
  # Records a readmission to the same hospital, for the same illness, within 30 days of
  # discharge following hospitalization for surgical resection of the primary site

  df$READM_HOSP_30_DAYS <-
    factor(
      df$READM_HOSP_30_DAYS,
      levels = c(0, 1, 2, 3, 9),
      labels = c(
        "None", #surgical procedure of the primary site was performed, or the patient was not readmitted to the same hospital within 30 days of discharge.
        "Unplanned", # A patient was surgically treated and was readmitted to the same hospital within 30 days of being discharged. This readmission was unplanned.
        "Planned", # A patient was surgically treated and was then readmitted to the same hospital within 30 days of-being discharged. This readmission was planned (chemotherapy port insertion, revision of colostomy, etc.)
        "Planned + unplanned", # A patient was surgically treated and, within 30 days of being discharged, the patient had both a planned and an unplanned readmission to the same hospital
        "Unknown" # Unknown whether surgery of the primary site was recommended or performed. It is unknown whether the patient was readmitted to the same hospital within 30 days of discharge
      )
    )

  var_label(df$READM_HOSP_30_DAYS) <- "Readmissions within 30 days of surgical discharge"

  #REASON_FOR_NO_SURGERY
  # Records the reason that no surgery was performed on the primary site.
  df$REASON_FOR_NO_SURGERY <-
    factor(
      df$REASON_FOR_NO_SURGERY,
      levels = c(0, 1, 2, 5, 6, 7, 8, 9),
      labels = c(
        "Surgery performed", # surgery of the primary site was performed
        "Not part of planned treatment", # Surgery of the primary site was not performed because it was not part of the planned first course treatment
        "Contraindicated, risk factors", # Surgery of the primary site was not recommended/performed because it was contraindicated due to patient risk factors (comorbid conditions, advanced age, etc.)
        "Recommended, patient deceased", # Surgery of the primary site was not performed because the patient died prior to planned or recommended surgery.
        "Recommended, no reason provided", # Surgery of the primary site was not performed; it was recommended by the patient=s physician, but was not performed as part of the first course of therapy. No reason was noted in patient record.
        "Recommended, refused by patient", # Surgery of the primary site was not performed; it was recommended by the patient=s physician, but this treatment was refused by the patient, the patient=s family member, or the patient=s guardian. The refusal was noted in patient record.
        "Recommended, unknown if performed", # Surgery of the primary site was recommended, but it is unknown if it was performed. Further follow-up is recommended.
        "Unknown if recommended or performed" # It is unknown whether surgery of the primary site was recommended or performed. Diagnosed at autopsy or death certificate only."
      )
    )

  var_label(df$REASON_FOR_NO_SURGERY) <- "Reason for no surgery"


  # Calculated field to determine if the patient had surgery
  df$ANY_SURGERY <- NA
  df$ANY_SURGERY[df$REASON_FOR_NO_SURGERY %in% c("Surgery performed")] <-
    0

  df$ANY_SURGERY[df$REASON_FOR_NO_SURGERY %in% c(
    "Not part of planned treatment",
    "Contraindicated, risk factors",
    "Recommended, patient deceased",
    "Recommended, no reason provided",
    "Recommended, refused by patient",
    "Recommended, unknown if performed",
    "Unknown if recommended or performed"
  )] <- 1

  df$ANY_SURGERY <-
    factor(df$ANY_SURGERY,
           levels = c(0, 1),
           labels = c("Surgery",
                      "No Surgery"))


  # calculated variable to combine surgery and margins
  df$JOINT_SURG_MARGINS <- factor(
    paste(df$ANY_SURGERY, df$MARGINS_RECODE),
    levels = c(
      "No Surgery Indeterminate/NA",
      "Surgery Indeterminate/NA",
      "Surgery NA",
      "Surgery Negative margin",
      "Surgery Positive margin"
    )
  )


  df$SURGERY_MARGINS <- NA
  df$SURGERY_MARGINS[df$JOINT_SURG_MARGINS %in% c("No Surgery Indeterminate/NA")] <- 0
  df$SURGERY_MARGINS[df$JOINT_SURG_MARGINS %in% c("Surgery Negative margin")] <- 1
  df$SURGERY_MARGINS[df$JOINT_SURG_MARGINS %in% c("Surgery Positive margin")] <- 2
  df$SURGERY_MARGINS[df$JOINT_SURG_MARGINS %in% c("Surgery Indeterminate/NA",
                                                  "Surgery NA")] <- 3


  df$SURGERY_MARGINS <-
    factor(
      df$SURGERY_MARGINS,
      levels = c(0, 1, 2, 3),
      labels = c(
        "No Surgery",
        "Surgery - Margins",
        "Surgery + Margins",
        "Surgery Margins Unknown"
      )
    )

  var_label(df$SURGERY_MARGINS) <- "Margin Status"

  #DX_RAD_STARTED_DAYS
  # The number of days between the date of diagnosis (NAACCR Item #390) and the
  # date on which radiation therapy was started (NAACCR Item #1210)

  var_label(df$DX_RAD_STARTED_DAYS) <- "Radiation, Days from diagnosis"

  #RX_SUMM_RADIATION
  # Records the type of radiation administered to the primary site or any metastatic site
  # and includes all radiation therapy that is part of the first course of treatment,
  # whether delivered at the reporting institution or at other institutions.

  df$RX_SUMM_RADIATION <-
    factor(
      df$RX_SUMM_RADIATION,
      levels = c(0, 1, 2, 3, 4, 5, 9),
      labels = c(
        "No Radiation",
        "Beam Radiation",
        "Radioactive Implants",
        "Radioisotopes",
        "Combination Radiotherapy",
        "Radiation Therapy, NOS",
        "Unknown"
      )
    )

  #RAD_LOCATION_OF_RX
  # Identifies the location where radiation therapy was administered during the first
  # course of treatment, as "at the reporting facility" or "elsewhere".
  df$RAD_LOCATION_OF_RX <-
    factor(
      df$RAD_LOCATION_OF_RX,
      levels = c(0, 1, 2, 3, 4, 8, 9),
      labels = c(
        "None", # No radiation therapy was administered to the patient. Diagnosed at autopsy.",
        "All radiation treatment at this facility", #All radiation therapy was administered at the reporting facility.",
        "Regional treatment at this facility, boost elsewhere", # Regional treatment was administered at the reporting facility; a boost dose was administered elsewhere.",
        "Boost radiation at this facility, regional elsewhere", # Regional treatment was administered elsewhere; a boost dose was administered at the reporting facility.",
        "All radiation treatment elsewhere",
        "Other pattern", # Radiation therapy was administered, but the pattern does not fit the above categories.",
        "Unknown" # Radiation therapy was administered, but the location of the treatment facility is unknown or not stated in patient record; it is unknown whether radiation therapy was administered. Death certificate only."
      )
    )

  var_label(df$RAD_LOCATION_OF_RX) <- "Location of radiation therapy administration"

  #RAD_TREAT_VOL, Radiation Treatment Volume
  # Identifies the volume or anatomic target of the most clinically significant regional
  # radiation therapy delivered to the patient during the first course of treatment.
  df$RAD_TREAT_VOL <-
    factor(
      df$RAD_TREAT_VOL,
      levels = c(
        00,
        01,
        02,
        03,
        04,
        05,
        06,
        07,
        08,
        09,
        10,
        11,
        12,
        13,
        14,
        15,
        16,
        17,
        18,
        19,
        20,
        21,
        22,
        23,
        24,
        25,
        26,
        27,
        28,
        29,
        30,
        31,
        32,
        33,
        34,
        35,
        36,
        37,
        38,
        39,
        40,
        41,
        50,
        60,
        98,
        99
      ),
      labels = c(
        "No radiation treatment", # Radiation therapy was not administered to the patient. Diagnosed at autopsy. ",
        "Eye/orbit", # The radiation therapy target volume is limited to the eye and/or orbit.",
        "Pituitary", # The target volume is restricted to the pituitary gland and all adjacent volumes are irradiated incidentally.",
        "Brain (NOS)", # Treatment is directed at tumors lying within the substance of the brain, or its meninges.",
        "Brain (limited)", # The treatment volume encompasses less than the total brain, or less than all of the meninges.",
        "Head and neck (NOS)", # The treatment volume is directed at a primary tumor of the oropharyngeal complex, usually encompassing regional lymph nodes.",
        "Head and neck (limited)", # Limited volume treatment of a head and neck primary with the exception of glottis (code 7), sinuses (code 8), or parotid (code 9).",
        "Glottis", # Treatment is limited to a volume in the immediate neighborhood of the vocal cords.",
        "Sinuses", # The primary target is one or both of the maxillary sinuses or the ethmoidal frontal sinuses. In some cases, the adjacent lymph node regions my be irradiated.",
        "Parotid", # The primary target is one of the parotid glands. There may be secondary regional lymph node irradiation as well.",
        "Chest/lung (NOS)", # Radiation therapy is directed to some combination of hilar, mediastinal, and/or supraclavicular lymph nodes, and/or peripheral lung structures.",
        "Lung (limited)", # Radiation therapy is directed at one region of the lung without nodal  irradiation.",
        "Esophagus", # The primary target is some portion of the esophagus. Regional lymph nodes may or may not be included in the treatment. Include tumors of the gastroesophageal junction.",
        "Stomach", # The primary malignancy is in the stomach. Radiation is directed to the stomach and possibly adjacent lymph nodes.",
        "Liver", # The primary target is all or a portion of the liver, for either primary or metastatic disease.",
        "Pancreas", # The primary tumor is in the pancreas. The treatment field encompasses the pancreas and possibly adjacent lymph node regions.",
        "Kidney", # The target is primary or metastatic disease in the kidney or the kidney bed after resection of a primary kidney tumor. Adjacent lymph node regions may be included in the field.",
        "Abdomen (NOS)", # Include all treatment of abdominal contents that do not fit codes 12–16.",
        "Breast", # The primary target is the intact breast and no attempt has been made to irradiate the regional lymph nodes. Intact breast includes breast tissue that either was not surgically treated or received a lumpectomy or partial mastectomy (C50.0–C50.9, Surgical Procedure of Primary Site [NAACCR Item #1290] codes 0–24).",
        "Breast/lymph nodes", # A deliberate attempt has been made to include regional lymph nodes in the treatment of an intact breast. See definition of intact breast above.",
        "Chest wall", # Treatment encompasses the chest wall (following mastectomy).",
        "Chest wall/lymph nodes", # Treatment encompasses the chest wall (following mastectomy) plus fields directed at regional lymph nodes.",
        "Mantle, Mini-mantle", # Treatment consists of a large radiation field designed to encompass all of the regional lymph nodes above the diaphragm, including cervical supraclavicular, axillary, mediastinal, and hilar nodes (mantle), or most of them (minimantle).This code is used exclusively for patients with Hodgkin’s or non-Hodgkin’s lymphoma.",
        "Lower extended field", # The target zone includes lymph nodes below the diaphragm along the paraaortic chain. It may include extension to one side of the pelvis. This code includes the “hockey stick” field utilized to treat seminomas.",
        "Spine", # The primary target relates to the bones of the spine, including the sacrum. Spinal cord malignancies should be coded 40 (Spinal cord).",
        "Skull", # Treatment is directed at the bones of the skull. Any brain irradiation is a secondary consequence.",
        "Ribs", # Treatment is directed toward metastatic disease in one or more ribs. Fields may be tangential or direct.",
        "Hip", # The target includes the proximal femur for metastatic disease. In may cases there may beacetabular disease as well.",
        "Pelvic bones", # The target includes structures of the bones of the pelvis other than the hip or sacrum.",
        "Pelvis (NOS)", # Irradiation is directed at soft tissues within the pelvic region and codes 34–36 do not apply.",
        "Skin", # The primary malignancy originates in the skin and the skin is the primary target. So-called skin metastases are usually subcutaneous and should be coded 31 (Soft tissue).",
        "Soft tissue", # All treatment of primary or metastatic soft tissue malignancies not fitting other categories.",
        "Hemibody", # A single treatment volume encompassing either all structures above the diaphragm, or all structures below the diaphragm. This is almost always administered for palliation of widespread bone metastasis in patients with prostate or breast cancer.",
        "Whole body", # Entire body included in a single treatment.",
        "Bladder and pelvis", # The primary malignancy originated in the bladder, all or most of the pelvis is treated as part of the plan, typically with a boost to the bladder.",
        "Prostate and pelvis", # The primary malignancy originated in the prostate, all or most of the pelvis is treated as part of the plan, typically with a boost to the prostate.",
        "Uterus and cervix", # Treatment is confined to the uterus and cervix or vaginal cuff, usually by intracavitary or interstitial technique. If entire pelvis is included in a portion of the treatment, then code 29 (Pelvis, NOS).",
        "Shoulder", # Treatment is directed to the proxmal humerus, scapula, clavicle, or other components of the shoulder complex. This is usually administered for control of symptoms for metastases.",
        "Extremity bone, NOS", #Bones of the arms or legs. This excludes the proximal femur, code 27 (Hip). This excludes the proximal humerus, code 37 (Shoulder).",
        "Inverted Y", # Treatment has been given to a field that encompasses the paraaortic and bilateral inguinal or inguinofemoral lymph nodes in a single port.",
        "Spinal cord", # Treatment is directed at the spinal cord or its meninges.",
        "Prostate", # Treatment is directed at the prostate with or without the seminal vesicles, without regional lymph node treatment.",
        "Thyroid", # Treatment is directed at the thyroid gland.",
        "Lymph node region, NOS", # The target is a group of lymph nodes not listed above. Examples include isolated treatment of a cervical, supraclavicular, or inguinofemoral region.",
        "Other", # Radiation therapy administered, treatment volume other than those previously categorized.",
        "Unknown" # Radiation therapy administered, treatment volume unknown or not stated in patient record; it is unknown whether radiation therapy was administered. Death certificate only"
      )
    )

  var_label(df$RAD_TREAT_VOL) <- "Radiation treatment volume/location"

  #RAD_REGIONAL_RX_MODALITY
  # Records the dominant modality of radiation therapy used to deliver the most
  # clinically significant regional dose to the primary volume of interest during the
  # first course of treatment.
  # Analytic Note:
  #   This item is reported as an optional item per the ROADS manual for cases
  # diagnosed between January 1, 1996, and December 31, 2002, and required
  # thereafter. For cases diagnosed December 31, 2002 and earlier where this item
  # was not reported, an imputed value from the ROADS radiation therapy item
  # (NAACCR Item# 1350) has been used to assign this item

  df$RAD_REGIONAL_RX_MODALITY <-
    factor(
      df$RAD_REGIONAL_RX_MODALITY,
      levels = c(
        00,
        20,
        21,
        22,
        23,
        24,
        25,
        26,
        27,
        28,
        29,
        30,
        31,
        32,
        40,
        41,
        42,
        43,
        50,
        51,
        52,
        53,
        54,
        55,
        60,
        61,
        62,
        80,
        85,
        98,
        99
      ),
      labels = c(
        "No radiation treatment", # Radiation therapy was not administered to the patient. Diagnosed at autopsy.",
        "External beam, NOS", # The treatment is known to be by external beam, but there is insufficient information to determine the specific modality.",
        "Orthovoltage", # External beam therapy administered using equipment with a maximum energy of less than one (1) million volts (MV). Orthovoltage,energies are typically expressed in units of kilovolts (k",
        "Cobalt-60, Cesium-137", # External beam therapy using a machine containing either a Cobalt-60 or Cesium-137 source. Intracavitary use of these sources is coded either 50 or 51.",
        "Photons (2–5 MV)", # External beam therapy using a photon producing machine with beam energy in the range of 2–5 MV.",
        "Photons (6–10 MV)", # External beam therapy using a photon producing machine with beam energy in the range of 6–10 MV.",
        "Photons (11–19 MV)", # External beam therapy using a photon producing machine with beam energy in the range of 11–19 MV.",
        "Photons (>19 MV)", # External beam therapy using a photon producing machine with beam energy of more than 19 MV.",
        "Photons (mixed energies)", # External beam therapy using more than one energy over the course of treatment.",
        "Electrons", # Treatment delivered by electron beam. ",
        "Photons and electrons mixed", # Treatment delivered using a combination of photon and electron beams.",
        "Neutrons, with or withoutphotons/electrons", # Treatment delivered using neutron beam.",
        "IMRT", # Intensity modulated radiation therapy, an external beam technique that should be clearly stated in patient record.",
        "Conformal or 3-D therapy", # An external beam technique using multiple, fixed portals shaped to conform to a defined target volume. Should be clearly described as conformal or 3D therapy in patient rrd.",
        "Protons", # Treatment delivered using proton therapy.",
        "Stereotactic radiosurgery, NOS", # Treatment delivered using stereotactic radiosurgery, type not specified in patient record.",
        "Linac radiosurgery", # Treatment categorized as using stereotactic technique delivered with a linear accelerator.",
        "Gamma Knife", # Treatment categorized as using stereotactic technique delivered using a Gamma Knife machine.",
        "Brachytherapy, NOS", # Brachytherapy, interstitial implants, molds, seeds, needles, or intracavitary applicators of radioactive materials not otherwise specified.",
        "Brachytherapy, Intracavitary, LDR, Intracavitary", # (no direct insertion into tissues), radioisotope treatment using low dose rate applicators and isotopes (Cesium-137, Fletcher applicator).",
        "Brachytherapy, Intracavitary, HDR, Intracavitary", # (no direct insertion into tissues), radioisotope treatment using high dose rate, afterloading applicators and isotopes.",
        "Brachytherapy, Interstitial, LDR, Interstitial", # (direct insertion into tissues), radioisotope treatment using low dose rate sources.",
        "Brachytherapy, Interstitial, HDR, Interstitial", # (direct insertion into tissues), radioisotope treatment using high dose rate sources.",
        "Radium", # Infrequently used for low dose rate (LDR) interstitial and intracavitary therapy.",
        "Radioisotopes, NOS", # Iodine-131, Phosphorus-32, etc.",
        "Strontium-89", # Treatment primarily by intravenous routes for bone metastases.",
        "Strontium-90",
        "Combination modality", # specified Combination of external beam radiation and either radioactive implants or radioisotopes*",
        "Combination modality, NOS", #, Combination of radiation treatment modalities",
        "Other, NOS", # Radiation therapy administered, but the treatment modality is not specified or is unknown.",
        "Unknown Radiation therapy administered" #, treatment volume unknown or not stated in the patient record; it is unknown whether radiation therapy was administered. Death certificate only."
      )
    )

  var_label(df$RAD_REGIONAL_RX_MODALITY) <- "Regional radiation treatment modality"

  #RAD_REGIONAL_DOSE_CGY
  # Records the dominant or most clinically significant total dose of regional radiation
  # therapy delivered to the patient during the first course of treatment. The unit of
  # measure is centiGray (cGy).

  var_label(df$RAD_REGIONAL_DOSE_CGY) <- "Regional Dose (cGy)"

  #RAD_BOOST_RX_MODALITY
  # Records the dominant modality of radiation therapy used to deliver the most
  # clinically significant boost dose to the primary volume of interest during the first
  # course of treatment. This is accomplished with external beam fields of reduced
  # size (relative to the regional treatment fields), implants, stereotactic radiosurgery,
  # conformal therapy, or IMRT. External beam boosts may consist of two or more
  # successive phases with progressively smaller fields generally coded as a single
  # entity.

  df$RAD_BOOST_RX_MODALITY <-
    factor(
      df$RAD_BOOST_RX_MODALITY,
      levels = c(
        00,
        20,
        21,
        22,
        23,
        24,
        25,
        26,
        27,
        28,
        29,
        30,
        31,
        32,
        40,
        41,
        42,
        43,
        50,
        51,
        52,
        53,
        54,
        55,
        60,
        61,
        62,
        80,
        85,
        98,
        99
      ),
      labels = c(
        "No radiation treatment", # Radiation therapy was not administered to the patient. Diagnosed at autopsy.",
        "External beam, NOS", # The treatment is known to be by external beam, but there is insufficient information to determine the specific modality.",
        "Orthovoltage", # External beam therapy administered using equipment with a maximum energy of less than one (1) million volts (MV). Orthovoltage,energies are typically expressed in units of kilovolts (k",
        "Cobalt-60, Cesium-137", # External beam therapy using a machine containing either a Cobalt-60 or Cesium-137 source. Intracavitary use of these sources is coded either 50 or 51.",
        "Photons (2–5 MV)", # External beam therapy using a photon producing machine with beam energy in the range of 2–5 MV.",
        "Photons (6–10 MV)", # External beam therapy using a photon producing machine with beam energy in the range of 6–10 MV.",
        "Photons (11–19 MV)", # External beam therapy using a photon producing machine with beam energy in the range of 11–19 MV.",
        "Photons (>19 MV)", # External beam therapy using a photon producing machine with beam energy of more than 19 MV.",
        "Photons (mixed energies)", # External beam therapy using more than one energy over the course of treatment.",
        "Electrons", # Treatment delivered by electron beam. ",
        "Photons and electrons mixed", # Treatment delivered using a combination of photon and electron beams.",
        "Neutrons, with or withoutphotons/electrons", # Treatment delivered using neutron beam.",
        "IMRT", # Intensity modulated radiation therapy, an external beam technique that should be clearly stated in patient record.",
        "Conformal or 3-D therapy", # An external beam technique using multiple, fixed portals shaped to conform to a defined target volume. Should be clearly described as conformal or 3D therapy in patient rrd.",
        "Protons", # Treatment delivered using proton therapy.",
        "Stereotactic radiosurgery, NOS", # Treatment delivered using stereotactic radiosurgery, type not specified in patient record.",
        "Linac radiosurgery", # Treatment categorized as using stereotactic technique delivered with a linear accelerator.",
        "Gamma Knife", # Treatment categorized as using stereotactic technique delivered using a Gamma Knife machine.",
        "Brachytherapy, NOS", # Brachytherapy, interstitial implants, molds, seeds, needles, or intracavitary applicators of radioactive materials not otherwise specified.",
        "Brachytherapy, Intracavitary, LDR, Intracavitary", # (no direct insertion into tissues), radioisotope treatment using low dose rate applicators and isotopes (Cesium-137, Fletcher applicator).",
        "Brachytherapy, Intracavitary, HDR, Intracavitary", # (no direct insertion into tissues), radioisotope treatment using high dose rate, afterloading applicators and isotopes.",
        "Brachytherapy, Interstitial, LDR, Interstitial", # (direct insertion into tissues), radioisotope treatment using low dose rate sources.",
        "Brachytherapy, Interstitial, HDR, Interstitial", # (direct insertion into tissues), radioisotope treatment using high dose rate sources.",
        "Radium", # Infrequently used for low dose rate (LDR) interstitial and intracavitary therapy.",
        "Radioisotopes, NOS", # Iodine-131, Phosphorus-32, etc.",
        "Strontium-89", # Treatment primarily by intravenous routes for bone metastases.",
        "Strontium-90",
        "Combination modality", # specified Combination of external beam radiation and either radioactive implants or radioisotopes*",
        "Combination modality, NOS", #, Combination of radiation treatment modalities",
        "Other, NOS", # Radiation therapy administered, but the treatment modality is not specified or is unknown.",
        "Unknown Radiation therapy administered" #, treatment volume unknown or not stated in the patient record; it is unknown whether radiation therapy was administered. Death certificate only."
      )
    )
  var_label(df$RAD_BOOST_RX_MODALITY) <- "Radiation boost treatment modality"

  #RAD_BOOST_DOSE_CGY - Boost Dose
  # Records the additional dose delivered to that part of the treatment volume
  # encompassed by the boost fields or devices. The unit of measure is centiGray
  # (cGy).

  var_label(df$RAD_BOOST_RX_MODALITY) <- "Radiation boost treatment dose (cGy)"

  #RAD_NUM_TREAT_VOL
  # Records the total number of treatment sessions (fractions) administered during the
  # first course of treatment.

  var_label(df$RAD_NUM_TREAT_VOL) <- "Number of radiation treatments to this Volume"

  #RX_SUMM_SURGRAD_SEQ
  # Records the sequencing of radiation and surgical procedures given as part of the
  # first course of treatment.

  df$RX_SUMM_SURGRAD_SEQ <-
    factor(
      df$RX_SUMM_SURGRAD_SEQ,
      levels = c(0, 2, 3, 4, 5, 6, 9),
      labels = c(
        "No radiation therapy and/or surgical procedures",
        "Radiation before surgery",
        "Radiation after surgery",
        "Radiation before and after",
        "Intraoperative",
        "Intraoperative radiation eith other therapy administered before or after surgery",
        "Sequence unknown"
      )
    )

  var_label(df$RX_SUMM_SURGRAD_SEQ) <- "Sequence of radiation and surgery"


  #RAD_ELAPSED_RX_DAYS
  # For diagnosis years prior to 2003, this item uses a single ROADS item containing
  # the number of elapsed days between the start and end of radiation. For diagnosis
  # years 2003 and later, this item is calculated as the number of days between the
  # date radiation started (NAACCR Item #1210) and the date on which radiation
  # therapy ended (NAACCR Item #3220). 1 is added to the number of days elapsed.
  # This means that if radiation starts and ends on the same date, then 1 day has
  # elapsed, if radiation ends the day after it is started, then 2 days have elapsed, and
  # so on.

  var_label(df$RAD_ELAPSED_RX_DAYS) <- "Radiation ended, days from start of radiation"

  #REASON_FOR_NO_RADIATION
  # Records the reason that no regional radiation therapy was administered to the
  # patient.

  df$REASON_FOR_NO_RADIATION <-
    factor(
      df$REASON_FOR_NO_RADIATION,
      levels = c(0, 1, 2, 5, 6, 7, 8, 9),
      labels = c(
        "Administered",
        "Not planned", #Radiation therapy was not administered because it was not part of the planned first course treatment.",
        "Contraindicated, risk factors", #Radiation therapy was not recommended/administered because it was contraindicated due to other patient risk factors (comorbid conditions, advanced age, etc.).",
        "Recommended, patient deceased", #Radiation therapy was not administered because the patient died prior to planned or recommended therapy.",
        "Recommended, no reason", #Radiation therapy was not administered; it was recommended by the patient’s physician, but was not administered as part of first course treatment. No reason was noted in patient record.",
        "Recommended, refused", #Radiation therapy was not administered; it was recommended by the patient’s physician, but this treatment was refused by the patient, the patient’s family member, or the patient’s guardian. The refusal was noted in patient record.",
        "Recommended, unknown if administered", #Radiation therapy was recommended, but it is unknown whether it was administered.",
        "Unknown" #It is unknown if radiation therapy was recommended or administered. Death certificate and autopsy cases only."
      )
    )

  var_label(df$REASON_FOR_NO_RADIATION) <- "Reason for no radiation"


  #Calculated column ANY_RADIATION
  df$ANY_RADIATION <- NA
  df$ANY_RADIATION[df$REASON_FOR_NO_RADIATION %in% c("Administered")] <- 1
  df$ANY_RADIATION[df$REASON_FOR_NO_RADIATION %in% c(
    "Not planned",
    "Contraindicated, risk factors",
    "Recommended, patient deceased",
    "Recommended, no reason",
    "Recommended, refused",
    "Recommended, unknown if administered",
    "Unknown")] <- 0

  df$ANY_RADIATION <-
    factor(
      df$ANY_RADIATION,
      levels = c(0, 1),
      labels = c("No radiation",
                 "Radiation")
    )

  var_label(df$ANY_RADIATION) <- "Radiation"

  #DX_SYSTEMIC_STARTED_DAYS
  # The number of days between the date of diagnosis (NAACCR Item #390) and the
  # date on which any systemic therapy [chemotherapy, hormone therapy,
  # immunotherapy, or hematologic transplant and endocrine procedures] was started
  # (NAACCR Item #3230).

  var_label(df$DX_SYSTEMIC_STARTED_DAYS) <- "Systemic, days from diagnosis"

  #RX_SUMM_CHEMO
  # Records the type of chemotherapy administered as first course treatment at any
  # facility. If chemotherapy was not administered, then this item records the reason it
  # was not administered to the patient. Chemotherapy consists of a group of
  # anticancer drugs that inhibit the reproduction of cancer cells by interfering with DNA
  # synthesis and mitosis.

  df$RX_SUMM_CHEMO <-
    factor(
      df$RX_SUMM_CHEMO,
      levels = c(00, 01, 02, 03, 82, 85, 86, 87, 88, 99),
      labels = c(
        "Not planned", # chemotherapy was not part of the planned first course of therapy. Diagnosed at autopsy.",
        "Administered, unknown details", #Chemotherapy administered as first course therapy, but the type and number of agents is not documented in patient record.",
        "Single-agent chemotherapy", # administered as first course therapy.",
        "Multiagent chemotherapy", # administered as first course therapy.",
        "Contraindicated, risk factors", #Chemotherapy was not recommended/administered because it was contraindicated due to patient risk factors (ie, comorbid conditions, advanced age).",
        "Recommended, patient deceased", #Chemotherapy was not administered because the patient died prior to planned or recommended therapy.",
        "Recommended, no reason given", #Chemotherapy was not administered. It was recommended by the patient's physician, but was not administered as part of the first course of therapy. No reason was stated in patient record.",
        "Recommended, refused", #Chemotherapy was not administered. It was recommended by the patient's physician, but this treatment was refused by the patient, a patient's family member, or the patient's guardian. The refusal was noted in patient record.",
        "Recommended, unknown if administered", #Chemotherapy was recommended, but it is unknown if it was administered.",
        "Unknown" #It is unknown whether a chemotherapeutic agent(s) was recommended or administered because it is not stated in patient record. Death certificate only"
      )
    )

  var_label(df$RX_SUMM_CHEMO) <- "Chemotherapy"

  #calculated column to make chemotherapy admin binary
  df$ANY_CHEMO <- NA
  df$ANY_CHEMO[df$RX_SUMM_CHEMO %in% c(
    "Administered, unknown details",
    "Single-agent chemotherapy",
    "Multiagent chemotherapy")] <- 1

  df$ANY_CHEMO[df$RX_SUMM_CHEMO %in% c(
    "Not planned",
    "Contraindicated, risk factors",
    "Recommended, patient deceased",
    "Recommended, no reason given",
    "Recommended, refused",
    "Recommended, unknown if administered",
    "Unknown")] <- 0

  df$ANY_CHEMO <-
    factor(
      df$ANY_CHEMO,
      levels = c(0, 1),
      labels = c("No Chemo",
                 "Chemo")
    )
  var_label(df$ANY_CHEMO) <- "Chemotherapy"

  #RX_HOSP_CHEMO
  # Records the type of chemotherapy administered as first course treatment by the
  # facility that submitted this record. If chemotherapy was not administered, then this
  # item records the reason it was not administered to the patient.

  df$RX_HOSP_CHEMO <-
    factor(
      df$RX_HOSP_CHEMO,
      levels = c(00, 01, 02, 03, 82, 85, 86, 87, 88, 99),
      labels = c(
        "Not planned", # chemotherapy was not part of the planned first course of therapy. Diagnosed at autopsy.",
        "Administered, unknown details", #Chemotherapy administered as first course therapy, but the type and number of agents is not documented in patient record.",
        "Single-agent chemotherapy", # administered as first course therapy.",
        "Multiagent chemotherapy", # administered as first course therapy.",
        "Contraindicated, risk factors", #Chemotherapy was not recommended/administered because it was contraindicated due to patient risk factors (ie, comorbid conditions, advanced age).",
        "Recommended, patient deceased", #Chemotherapy was not administered because the patient died prior to planned or recommended therapy.",
        "Recommended, no reason given", #Chemotherapy was not administered. It was recommended by the patient's physician, but was not administered as part of the first course of therapy. No reason was stated in patient record.",
        "Recommended, refused", #Chemotherapy was not administered. It was recommended by the patient's physician, but this treatment was refused by the patient, a patient's family member, or the patient's guardian. The refusal was noted in patient record.",
        "Recommended, unknown if administered", #Chemotherapy was recommended, but it is unknown if it was administered.",
        "Unknown" #It is unknown whether a chemotherapeutic agent(s) was recommended or administered because it is not stated in patient record. Death certificate only"
      )
    )

  var_label(df$RX_HOSP_CHEMO) <- "Chemotherapy at this facility"

  #DX_CHEMO_STARTED_DAYS
  # The number of days between the date of diagnosis (NAACCR Item #390) and the
  # date on which chemotherapy at any facility was started (NAACCR Item #1220).

  var_label(df$DX_CHEMO_STARTED_DAYS) <- "Chemotherapy, days from diagnosis"

  #RX_SUMM_HORMONE
  # Records the type of hormone therapy administered as first course treatment at any
  # facility. If hormone therapy was not administered, then this item records the reason
  # it was not administered to the patient. Hormone therapy consists of a group of
  # drugs that may affect the long-term control of a cancer's growth.

  df$RX_SUMM_HORMONE <-
    factor(
      df$RX_SUMM_HORMONE,
      levels = c(00, 01, 82, 85, 86, 87, 88, 99),
      labels = c(
        "Not planned", #  was not part of the planned first course of therapy. Diagnosed at autopsy.",
        "Administered, first course", #hormone therapy administered as first course therapy, but the type and number of agents is not documented in patient record.",
        "Contraindicated, risk factors", #hormone therapy was not recommended/administered because it was contraindicated due to patient risk factors (ie, comorbid conditions, advanced age).",
        "Recommended, patient deceased", #hormone therapy was not administered because the patient died prior to planned or recommended therapy.",
        "Recommended, no reason given", #hormone therapy was not administered. It was recommended by the patient's physician, but was not administered as part of the first course of therapy. No reason was stated in patient record.",
        "Recommended, refused", #hormone therapy was not administered. It was recommended by the patient's physician, but this treatment was refused by the patient, a patient's family member, or the patient's guardian. The refusal was noted in patient record.",
        "Recommended, unknown if administered", #hormone therapy was recommended, but it is unknown if it was administered.",
        "Unknown" #It is unknown whether a chemotherapeutic agent(s) was recommended or administered because it is not stated in patient record. Death certificate only"
      )
    )

  var_label(df$RX_SUMM_HORMONE) <- "Hormone therapy"

  #calculated column to simplify hormone treatment to a binary
  df$ANY_HORMONE <- NA
  df$ANY_HORMONE[df$RX_SUMM_HORMONE %in% c("Administered, first course")] <-1
  df$ANY_HORMONE[df$RX_SUMM_HORMONE %in% c(
    "Not planned",
    "Contraindicated, risk factors",
    "Recommended, patient deceased",
    "Recommended, no reason given",
    "Recommended, refused",
    "Recommended, unknown if administered",
    "Unknown"
  )] <- 0


  df$ANY_HORMONE <-
    factor(
      df$ANY_HORMONE,
      levels = c(0, 1),
      labels = c("No Hormone",
                 "Hormone Therapy")
    )

  var_label(df$ANY_HORMONE) <- "Hormone Therapy"

  #RX_HOSP_HORMONE
  # This item records the type of hormone therapy administered as first course
  # treatment by the facility that submitted this record. If hormone therapy was not
  # administered, then this item records the reason it was not administered to the
  # patient. Hormone therapy consists of a group of drugs that may affect the longterm control of a cancer's growth.

  df$RX_HOSP_HORMONE <-
    factor(
      df$RX_HOSP_HORMONE,
      levels = c(0, 1, 82, 85, 86, 87, 88, 99),
      labels = c(
        "Not planned", #  was not part of the planned first course of therapy. Diagnosed at autopsy.",
        "Administered, first course", #hormone therapy administered as first course therapy, but the type and number of agents is not documented in patient record.",
        "Contraindicated, risk factors", #hormone therapy was not recommended/administered because it was contraindicated due to patient risk factors (ie, comorbid conditions, advanced age).",
        "Recommended, patient deceased", #hormone therapy was not administered because the patient died prior to planned or recommended therapy.",
        "Recommended, no reason given", #hormone therapy was not administered. It was recommended by the patient's physician, but was not administered as part of the first course of therapy. No reason was stated in patient record.",
        "Recommended, refused", #hormone therapy was not administered. It was recommended by the patient's physician, but this treatment was refused by the patient, a patient's family member, or the patient's guardian. The refusal was noted in patient record.",
        "Recommended, unknown if administered", #hormone therapy was recommended, but it is unknown if it was administered.",
        "Unknown" #It is unknown whether a chemotherapeutic agent(s) was recommended or administered because it is not stated in patient record. Death certificate only"
      )
    )

  var_label(df$RX_HOSP_HORMONE) <- "Hormone Therapy at this Facility"

  #DX_HORMONE_STARTED_DAYS
  # The number of days between the date of diagnosis (NAACCR Item #390) and the
  # date on which hormone therapy at any facility was started (NAACCR Item #1230).

  var_label(df$DX_HORMONE_STARTED_DAYS) <- "Hormone Therapy, Days from dx"

  #RX_SUMM_IMMUNOTHERAPY
  # Records the type of immunotherapy administered as first course treatment at this
  # and all other facilities. If immunotherapy was not administered, then this item
  # records the reason it was not administered to the patient. Immunotherapy consists
  # of biological or chemical agents that alter the immune system or change the host's
  # response to tumor cells

  df$RX_SUMM_IMMUNOTHERAPY <-
    factor(
      as.numeric(df$RX_SUMM_IMMUNOTHERAPY),
      levels = c(0, 1, 82, 85, 86, 87, 88, 99),
      labels = c(
        "Not planned", #  was not part of the planned first course of therapy. Diagnosed at autopsy.",
        "Administered, first course", #immunotherapy administered as first course therapy, but the type and number of agents is not documented in patient record.",
        "Contraindicated, risk factors", #immunotherapy was not recommended/administered because it was contraindicated due to patient risk factors (ie, comorbid conditions, advanced age).",
        "Recommended, patient deceased", #immunotherapy was not administered because the patient died prior to planned or recommended therapy.",
        "Recommended, no reason given", #immunotherapy was not administered. It was recommended by the patient's physician, but was not administered as part of the first course of therapy. No reason was stated in patient record.",
        "Recommended, refused", #immunotherapy was not administered. It was recommended by the patient's physician, but this treatment was refused by the patient, a patient's family member, or the patient's guardian. The refusal was noted in patient record.",
        "Recommended, unknown if administered", #immunotherapy was recommended, but it is unknown if it was administered.",
        "Unknown" #It is unknown whether a chemotherapeutic agent(s) was recommended or administered because it is not stated in patient record. Death certificate only"
      )
    )

  #calculated column to simplify immunotherapy to a binary
  df$ANY_IMMUNO <- NA
  df$ANY_IMMUNO[df$RX_SUMM_IMMUNOTHERAPY %in% c("Administered, first course")] <- 1
  df$ANY_IMMUNO[df$RX_SUMM_IMMUNOTHERAPY %in% c(
    "Not planned",
    "Contraindicated, risk factors",
    "Recommended, patient deceased",
    "Recommended, no reason given",
    "Recommended, refused",
    "Recommended, unknown if administered",
    "Unknown"
  )] <- 0


  df$ANY_IMMUNO <-
    factor(
      df$ANY_IMMUNO,
      levels = c(0, 1),
      labels = c("No Immunotherapy",
                 "Immunotherapy")
    )


  var_label(df$ANY_IMMUNO) <- "Immunotherapy"

  #RX_HOSP_IMMUNOTHERAPY
  # Records the type of immunotherapy administered as first course treatment at the
  # facility that submitted the record. If immunotherapy was not administered, then this
  # item records the reason it was not administered to the patient.

  df$RX_HOSP_IMMUNOTHERAPY <-
    factor(
      df$RX_HOSP_IMMUNOTHERAPY,
      levels = c(0, 1, 82, 85, 86, 87, 88, 99),
      labels = c(
        "Not planned", #  was not part of the planned first course of therapy. Diagnosed at autopsy.",
        "Administered, first course", #immunotherapy administered as first course therapy, but the type and number of agents is not documented in patient record.",
        "Contraindicated, risk factors", #immunotherapy was not recommended/administered because it was contraindicated due to patient risk factors (ie, comorbid conditions, advanced age).",
        "Recommended, patient deceased", #immunotherapy was not administered because the patient died prior to planned or recommended therapy.",
        "Recommended, no reason given", #immunotherapy was not administered. It was recommended by the patient's physician, but was not administered as part of the first course of therapy. No reason was stated in patient record.",
        "Recommended, refused", #immunotherapy was not administered. It was recommended by the patient's physician, but this treatment was refused by the patient, a patient's family member, or the patient's guardian. The refusal was noted in patient record.",
        "Recommended, unknown if administered", #immunotherapy was recommended, but it is unknown if it was administered.",
        "Unknown" #It is unknown whether a chemotherapeutic agent(s) was recommended or administered because it is not stated in patient record. Death certificate only"
      )
    )

  var_label(df$RX_HOSP_IMMUNOTHERAPY) <- "Immunotherapy at this Facility"

  #DX_IMMUNO_STARTED_DAYS
  # The number of days between the date of diagnosis (NAACCR Item #390) and the
  # date on which immunotherapy at any facility was started (NAACCR Item #1240).

  var_label(df$DX_IMMUNO_STARTED_DAYS) <- "Immunotherapy, days from diagnosis"

  ### RX_SUMM_TRNSPLNT_ENDO
  # identifies systemic therapeutic procedures performed as part of the first course of
  # treatment at this and all other facilities. If none of these procedures was performed,
  # then this item records the reason why not. These procedures include bone marrow
  # transplants, stem cell harvests, and surgical and/or radiation endocrine therapy

  df$RX_SUMM_TRNSPLNT_ENDO <-
    factor(
      df$RX_SUMM_TRNSPLNT_ENDO,
      levels = c(00, 10, 11, 12, 20, 30, 40, 82, 85, 86, 87, 88, 99),
      labels = c(
        "None", #No transplant procedure or endocrine therapy was administered as part of first course therapy. Diagnosed at autopsy.",
        "Bone marrow transplant, unspecified", #A bone marrow transplant procedure was administered, but the type was not specified.",
        "Bone marrow transplant autologous",
        "Bone marrow transplant allogeneic",
        "Stem cell harvest and infusion",
        "Endocrine surgery and/or endocrine radiation therapy",
        "Combination of endocrine surgery and/or radiation with a transplant procedure", # (Combination of codes 30 and 10, 11, 12, or 20.)",
        "Contraindicated, risk factors", #Hematologic transplant and/or endocrine surgery/radiation was not recommended/administered because it was contraindicated due to patient risk factors (ie, comorbid conditions, advanced age).",
        "Recommended, patient deceased", #Hematologic transplant and/or endocrine surgery/radiation was not administered because the patient died prior to planned or recommended therapy.",
        "Recommended, no reason", #Hematologic transplant and/or endocrine surgery/radiation was not administered. It was recommended by the patient's physician, but was not administered as part of the first course of therapy. No reason was stated in patient record.",
        "Recommended, refused", #Hematologic transplant and/or endocrine surgery/radiation was not administered. It was recommended by the patient's physician, but this treatment was refused by the patient, a patient's family member, or the patient's guardian. The refusal was noted in patient record.",
        "Recommended, unknown", #Hematologic transplant and/or endocrine surgery/radiation was recommended, but it is unknown if it was administered.",
        "Unknown" #It is unknown whether hematologic transplant and/or endocrine surgery/radiation was recommended or administered because it is not stated in patient record. Death certificate only."
      )
    )

  var_label(df$RX_SUMM_TRNSPLNT_ENDO) <- "Hematologic Transplant and endocrine procedures"

  #RX_SUMM_SYSTEMIC_SUR_SEQ
  # Records the sequencing of systemic treatment and surgical procedures given as
  # part of the first course of treatment.
  # Note: this is a bit of a tricky one. Be sure to read the descriptions carefully.

  df$RX_SUMM_SYSTEMIC_SUR_SEQ <-
    factor(
      df$RX_SUMM_SYSTEMIC_SUR_SEQ,
      levels = c(0, 2, 3, 4, 5, 6, 7, 9),
      labels = c(
        "No systemic or surgical treatment, or unknown",
        #No systemic therapy was given; and/or no surgical procedure of primary site; no scope of regional lymph node surgery; no surgery to other regional site(s), distant site(s), or distant lymph node(s); or no reconstructive surgery was performed. Or: It is unknown whether both surgery and systemic treatment were provided.",
        "Systemic treatment before surgery",
        #Systemic therapy was given before surgical procedure of primary site; scope of regional lymph node surgery; surgery to other regional site(s), distant site(s), or distant lymph node(s) was performed.",
        "Systemic treatment after surgery",
        #Systemic therapy was given after surgical procedure of primary site; scope of regional lymph node surgery; surgery to other regional site(s), distant site(s), or distant lymph node(s) was performed.",
        "At least two courses systemic treatment before surgery and two after",
        #At least two courses of systemic therapy were given before and at least two more after a surgical procedure of primary site; scope of regional lymph node sugery; surgery to other regional site(s), or distant site(s), or lymph node(s) was performed.",
        "Intraoperative systemic therapy",
        #Intraoperative systemic therapy was given during surgical procedure of primary site; scope of regional lymph node surgery; surgery to other regional site(s), distant site(s), or distant lymph node(s).",
        "Intraoperative systemic and systemic therapy before and/or after surgery",
        #Intraoperative systemic therapy was given during surgical procedure of primary site; scope of regional lymph node surgery; surgery to other regional site(s), distant site(s), or distant lymph node(s) with other systemic therapy administered before or after surgical procedure of primary site; scope of regional lymph node surgery; surgery to other regional site(s), distant site(s), or distant lymph node(s) was performed.",
        "Systemic therapy between two procedures",
        #Systemic therapy was administered between two separate surgical procedures to the primary site; regional lymph node surgery; surgery to other regional site(s), distant site(s), or distant lymph node(s) with other systemic therapy adminstered before or after surgical procedures to the primary site; regional lymph node surgery; surgery to other regional site(s), distant site(s), or distant lymph node(s) was performed.",
        "Surgery and systemic, sequence unknown"
        #Both surgery and systemic therapy were provided, but the sequence is unknown."

                 )
    )

  var_label(df$RX_SUMM_SYSTEMIC_SUR_SEQ) <- "Systemic treatment and surgery sequence"

  #RX_SUMM_OTHER
  # Identifies other treatment that cannot be defined as surgery, radiation, or systemic
  # therapy according to the defined data items in this manual.

  df$RX_SUMM_OTHER <-
    factor(
      df$RX_SUMM_OTHER,
      levels = c(0, 1, 2, 3, 6, 7, 8, 9),
      labels = c(
        "None", #All cancer treatment was coded in other treatment fields (surgery, radiation, systemic therapy). Patient received no cancer treatment. Diagnosed at autopsy.
        "Other", #Cancer treatment that cannot be appropriately assigned to specified treatment data items (surgery, radiation, systemic). Use this code for treatment unique to hematopoietic diseases.
        "Other-Experimental", #This code is not defined. It may be used to record participation in institution-based clinical trials.
        "Other-Double Blind", #A patient is involved in a double-blind clinical trial. Code the treatment actually administered when the double-blind trial code is broken.
        "Other-Unproven", #Cancer treatments administered by nonmedical personnel.
        "Refusal", #Other treatment was not administered. It was recommended by the patient's physician, but this treatment (which would have been coded 1, 2, or 3) was refused by the patient, a patient's family member, or the patient's guardian. The refusal was noted in the patient record.
        "Recommended; uknown if adminstered", #Other treatment was recommended, but it is unknown whether it was administered.
        "Unknown" #It is unknown whether other treatment was recommended or administered, and there is no information in the medical record to confirm the recommendation or administration of other treatment. Death certificate only.
      )
    )

  var_label(df$RX_SUMM_OTHER) <- "Other Treatment"

  #RX_HOSP_OTHER - Other treatment at this facility
  # Identifies other treatment given at the reporting facility that cannot be defined as
  # surgery, radiation, or systemic therapy.

  df$RX_HOSP_OTHER <-
    factor(
      df$RX_HOSP_OTHER,
      levels = c(0, 1, 2, 3, 6, 7, 8, 9),
      labels = c(
        "None", #All cancer treatment was coded in other treatment fields (surgery, radiation, systemic therapy). Patient received no cancer treatment. Diagnosed at autopsy.
        "Other", #Cancer treatment that cannot be appropriately assigned to specified treatment data items (surgery, radiation, systemic). Use this code for treatment unique to hematopoietic diseases.
        "Other-Experimental", #This code is not defined. It may be used to record participation in institution-based clinical trials.
        "Other-Double Blind", #A patient is involved in a double-blind clinical trial. Code the treatment actually administered when the double-blind trial code is broken.
        "Other-Unproven", #Cancer treatments administered by nonmedical personnel.
        "Refusal", #Other treatment was not administered. It was recommended by the patient's physician, but this treatment (which would have been coded 1, 2, or 3) was refused by the patient, a patient's family member, or the patient's guardian. The refusal was noted in the patient record.
        "Recommended; uknown if adminstered", #Other treatment was recommended, but it is unknown whether it was administered.
        "Unknown" #It is unknown whether other treatment was recommended or administered, and there is no information in the medical record to confirm the recommendation or administration of other treatment. Death certificate only.
      )
    )

  var_label(df$RX_HOSP_OTHER) <- "Other treatment at this facility"

  #DX_OTHER_STARTED_DAYS
  # The number of days between the date of diagnosis (NAACCR Item #390) and the
  # date on which Other Treatment at any facility was started (NAACCR Item
  # #1250).

  var_label(df$DX_OTHER_STARTED_DAYS) <- "Other Treatment, days from diagnosis"

  #PALLIATIVE_CARE
  # Identifies any care provided in an effort to palliate or alleviate symptoms. Palliative
  # care is performed to relieve symptoms and may include surgery, radiation therapy,
  # systemic therapy (chemotherapy, hormone therapy, or other systemic drugs),
  # and/or other pain management therapy

  df$PALLIATIVE_CARE <-
    factor(
      df$PALLIATIVE_CARE,
      levels = c(0, 1, 2, 3, 4, 5, 6, 7, 9),
      labels = c(
        "None", #No palliative care provided. Diagnosed at autopsy.",
        "Surgery", #(which may involve a bypass procedure) to alleviate symptoms, but no attempt to diagnose, stage, or treat the primary tumor is made.",
        "Radiation therapy", # to alleviate symptoms, but no attempt to diagnose, stage, or treat the primary tumor is made.",
        "Chemotherapy, hormone therapy, or other systemic", # drugs to alleviate symptoms, but no attempt to diagnose, stage, or treat the primary tumor is made.",
        "Pain management", #Patient received or was referred for pain management therapy with no other palliative care.",
        "Combination of surgery, systemic without pain management", #Any combination of codes 1, 2, and/or 3 without code 4.
        "Combination of surgery, systemic with pain management", #	Any combination of codes 1, 2, and/or 3 with code 4.
        "Recommended, no information available", #Palliative care was performed or referred, but no information on the type of procedure is available in the patient record. Palliative care was provided that does not fit the descriptions for codes 1–6.",
        "Unknown" #It is unknown if palliative care was performed or referred; not stated in patient record."
      )
    )

  var_label(df$PALLIATIVE_CARE) <- "Palliative care"

  #PALLIATIVE_CARE_HOSP
  # Identifies any care provided in an effort to palliate or alleviate symptoms at the reporting facility.
  # Palliative care is performed to relieve symptoms and may include surgery, radiation therapy, systemic
  #therapy (chemotherapy, hormone therapy, or other systemic drugs), and/or other pain management therapy.
  #This data item was added to the 2015 PUF (data released in Fall 2017), and does not appear in prior versions of the PUF data.

  df$PALLIATIVE_CARE_HOSP <-
    factor(
      df$PALLIATIVE_CARE_HOSP,
      levels = c(0, 1, 2, 3, 4, 5, 6, 7, 9),
      labels = c(
        "None", #No palliative care provided. Diagnosed at autopsy.",
        "Surgery", #(which may involve a bypass procedure) to alleviate symptoms, but no attempt to diagnose, stage, or treat the primary tumor is made.",
        "Radiation therapy", # to alleviate symptoms, but no attempt to diagnose, stage, or treat the primary tumor is made.",
        "Chemotherapy, hormone therapy, or other systemic", # drugs to alleviate symptoms, but no attempt to diagnose, stage, or treat the primary tumor is made.",
        "Pain management", #Patient received or was referred for pain management therapy with no other palliative care.",
        "Combination of surgery, systemic without pain management", #Any combination of codes 1, 2, and/or 3 without code 4.
        "Combination of surgery, systemic with pain management", #	Any combination of codes 1, 2, and/or 3 with code 4.
        "Recommended, no information available", #Palliative care was performed or referred, but no information on the type of procedure is available in the patient record. Palliative care was provided that does not fit the descriptions for codes 1–6.",
        "Unknown" #It is unknown if palliative care was performed or referred; not stated in patient record."
      )
    )

  var_label(df$PALLIATIVE_CARE_HOSP) <- "Palliative care at this facility"


  #Alternative to decide if systemic treatment was before or after surgery
  ### Adjuvant VS Neoadjuvant chemo
  # 0 - No surgery, Chemo
  # 1 - Neoadjuvant = DX_CHEMO_START_DAYS > DX_RX_STARTED_DAYS
  # 2 - Adjuvant = DX_CHEMO_START_DAYS !> DX_RX_STARTED_DAYS
  df$NEOADJUVANT <- NA
  df$NEOADJUVANT <-
    ifelse(
      df$ANY_CHEMO == "Chemo" &
        df$ANY_SURGERY == "No Surgery",
      0,
      ifelse(
        df$ANY_CHEMO == "Chemo" &
          df$DX_CHEMO_STARTED_DAYS > df$DX_RX_STARTED_DAYS,
        1,
        ifelse(
          df$ANY_CHEMO == "Chemo" &
            df$DX_CHEMO_STARTED_DAYS < df$DX_RX_STARTED_DAYS,
          2,
          3
        )
      )
    )

  df$NEOADJUVANT <-
    factor(
      df$NEOADJUVANT,
      levels = c(0, 1, 2, 3),
      labels = c("Chemo + No Surgery",
                 "Neoadjuvant Chemo",
                 "Adjuvant Chemo",
                 "NA")
    )

  var_label(df$NEOADJUVANT) <- "Neoadjuvant Chemotherapy Treatment"

  ### adjuvant vs neoadjuvant rads
  df$NEOADJUVANTRT <- NA
  df$NEOADJUVANTRT <-
    ifelse(
      df$ANY_RADIATION == "Radiation" &
        df$ANY_SURGERY == "No Surgery",
      0,
      ifelse(
        df$ANY_RADIATION == "Radiation" &
          df$DX_RAD_STARTED_DAYS > df$DX_RX_STARTED_DAYS,
        1,
        ifelse(
          df$ANY_RADIATION == "Radiation" &
            df$DX_RAD_STARTED_DAYS < df$DX_RX_STARTED_DAYS,
          2,
          3
        )
      )
    )

  df$NEOADJUVANTRT <-
    factor(
      df$NEOADJUVANTRT,
      levels = c(0, 1, 2, 3),
      labels = c("Radiation + No Surgery",
                 "Neoadjuvant Radiation",
                 "Adjuvant Radiation",
                 "NA")
    )

  var_label(df$NEOADJUVANTRT) <- "Neo/adjuvant Radiation Treatment"

#OUTCOMES####

  #PUF_30_DAY_MORT_CD - Thirty Day Mortality
  # This item indicates mortality within 30 days of the most definitive primary site
  # surgery

  df$PUF_30_DAY_MORT_CD <-
    factor(
      df$PUF_30_DAY_MORT_CD,
      levels = c(0, 1, 9),
      labels = c(
        "Alive", #or died more than 30 days after surgery performed",
        "Dead", #Patient died 30 or fewer days after surgery performed",
        "Alive, < 30 days FU" #Patient alive with fewer than 30 days of follow-up, surgery date missing, or last contact date missing"
      )
    )

  var_label(df$PUF_30_DAY_MORT_CD) <- "30-day Mortality"


  #PUF_90_DAY_MORT_CD - Nintey day mortality
  # This item indicates mortality within 90 days after the most definitive primary site
  # surgery

  df$PUF_90_DAY_MORT_CD <-
    factor(
      df$PUF_90_DAY_MORT_CD,
      levels = c(0, 1, 9),
      labels = c(
        "Alive", #Patient alive, or died more than 90 days after surgery performed",
        "Dead", #Patient died 90 or fewer days after surgery performed",
        "Alive, < 90 days FU" #or surgery date missing, or last contact date missing"
      )
    )

  var_label(df$PUF_90_DAY_MORT_CD) <- "90-day Mortality"


  #DX_LASTCONTACT_DEATH_MONTHS
  # The number of months between the date of diagnosis (NAACCR Item #390) and
  # the date on which the patient was last contacted or died (NAACCR Item #1750).

  var_label(df$DX_LASTCONTACT_DEATH_MONTHS) <- "Last contact or death, months from dx"

  #PUF_VITAL_STATUS - PUF Vital Status
  # Records the vital status of the patient as of the date entered in Date of Last Contact
  # or Death (NAACCR Item #1750), which is the status of the patient at the end of
  # Elapsed Months Date of Diagnosis to Date of Last Contact or Death in the PUF.

  df$PUF_VITAL_STATUS <-
    factor(
      df$PUF_VITAL_STATUS,
      levels = c(0, 1),
      labels = c("Dead",
                 "Alive")
    )

  # R Survival object defaults to 0 = Alive, 1 = dead, recode to  use


  #OUTPUT####
  #returns recoded dataframe
  df$RECODED_STATUS <- NA
  df$RECODED_STATUS[df$PUF_VITAL_STATUS == "Dead"] <-  1
  df$RECODED_STATUS[df$PUF_VITAL_STATUS == "Alive"] <- 0

  df

}
NCDBTableOne <- function(df){
# If a vector of variables is not passed, use this as default
  tableOne <-
    CreateTableOne(
      vars = c(
        "AGE_GROUP",
        "SEX",
        "RACE",
        "CDCC_TOTAL_BEST",
        "INSURANCE_STATUS",
        "MED_INC_QUAR_12",
        "NO_HSD_QUAR_12",
        "URBAN_RURAL",
        "FACILITY_TYPE_CD",
        "GRADE_RECODE",
        "pSTAGE_RECODE",
        "cSTAGE_RECODE",
        "PRIMARY_SITE",
        "SURGERY_MARGINS",
        "ANY_RADIATION",
        "ANY_CHEMO",
        "DX_RX_STARTED_DAYS"
      ),
      data = df
    )
  tableOne

}
NCDBOS <- function(df){
  # Make survival object
  survObject <-
    Surv(time = df$DX_LASTCONTACT_DEATH_MONTHS,
         event = df$RECODED_STATUS)

  # Plot overall survival curve
  fit <-
    survfit(survObject ~ 1, data = df, conf.type = "log-log")
  # plot <- ggsurvplot(
  #   fit,
  #   data = df,
  #   risk.table = FALSE,
  #   pval = FALSE,
  #   conf.int = TRUE,
  #   xlab = "Months Follow-up",
  #   ylab = "Percent Alive",
  #   censor = FALSE,
  #   legend.title = ,
  #   #legend.labs = strataLabels,
  #   legend = "top",
  #   #title = "Kaplan-Meier Overall Survival",
  #   font.title = 12,
  #   font.x = 10,
  #   font.y = 10,
  #   linetype = c(1:10),
  #   palette = gray(0:8 / 8))

    fit
}
NCDBGroupAge <- function(df){
  # break ages into bins
  #loop through rows and run the ageCalculation function
  # function to sort into age groups
  df$AGE_GROUP <- NA
  df$AGE_GROUP<-cut(
    as.numeric(df$AGE),
    breaks = c(-1,50,60,70, Inf),
    labels = c(
      "<50 years",
      "50-60 years",
      "60-70 years",
      ">70 years"
  ))

  var_label(df$AGE_GROUP) <- "Age (grouped)"

  df

}
NCDB_NodeGrouping <- function(df){
# group nodes for analysis as categorical variable
df$REGIONAL_NODES_EXAMINED_GROUPED <- NA
df$REGIONAL_NODES_POSITIVE_GROUPED <- NA

df$REGIONAL_NODES_EXAMINED_GROUPED<-cut(
  as.numeric(df$REGIONAL_NODES_EXAMINED),
  breaks = c(-1, 20, 50, 89, 90,95,96, 97,98,99),
  right = TRUE,
  labels = c(
     "0-20 nodes",
     "21-50 nodes",
     "51-89 nodes",
     ">=90 nodes",
     "No regional nodes removed, aspiration performed",
     "Documented as sampling, number unknown",
     "Documented as dissection, number unknown",
     "Nodes surgically removed, number not documented",
     "Unknown if regional nodes examined"

   )
)

df$REGIONAL_NODES_POSITIVE_GROUPED<-cut(
  as.numeric(df$REGIONAL_NODES_POSITIVE),
  breaks = c(-1, 0, 1, 5, 10, 20, 90,95,96, 97,98,99),
  right = TRUE,
  labels = c(
    "All examined nodes are negative",
    "1 node positive",
    "2-5 nodes positive",
    "6-10 nodes positive",
    "11-20 nodes positive",
    "21-90 nodes positive",
    "90+ nodes positive",
    "Positive aspiration of nodes performed",
    "Positive nodes documented, number unspecified",
    "No nodes examined",
    "Unknown whether nodes are positive"

  )
 )


}
NCDB_MissingData <- function(df){

}
