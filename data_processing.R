library(tidyverse)
library(fuzzyjoin)

# taken from https://www.waterboards.ca.gov/drinking_water/certlic/drinkingwater/documents/leadsamplinginschools/monthly_posting_final.xlsx
test_results_df <- read_csv("data/raw/monthly_posting_final.csv") %>%
  filter(
    SchoolType == "public"
  ) %>%
  select(
    PwsID, DISTRICT, SchoolName, SchoolAddress, SchoolCounty, `Action_Level_Exceedance?`
  ) %>%
  group_by(
    PwsID, DISTRICT, SchoolName, SchoolAddress, SchoolCounty
  ) %>%
  summarize(
    exceeded_action_level = "Yes" %in% `Action_Level_Exceedance?`
  )

# taken from https://www.cde.ca.gov/ds/ad/documents/frpm1718.xlsx
free_reduced_lunch_df <- read_csv("data/raw/free_reduced_lunch.csv") %>%
  select(
    `District Name`, `School Name`, `School Type`, `Enrollment \n(K-12)`, `Free Meal \nCount \n(K-12)`, `Percent (%) \nEligible Free \n(K-12)`, `FRPM Count \n(K-12)`, `Percent (%) \nEligible FRPM \n(K-12)`
  ) %>%
  mutate(
    fuzzyjoin_school = paste(`District Name`, `School Name`, sep = " ")
  )

# taken from https://www.cde.ca.gov/SchoolDirectory/report?rid=dl1&tp=xlsx&ict=Y
public_schools_directory_df <- read_csv("data/raw/public_schools_directory.csv") %>%
  select(District, School, Street, City, Zip) %>%
  filter(School != "No Data", Street != "No Data") %>%
  mutate(
    fuzzyjoin_school = paste(District, School, sep = " ")
  )

# taken from https://dq.cde.ca.gov/dataquest/dlfile/dlfile.aspx?cLevel=School&cYear=2017-18&cCat=Enrollment&cPage=filesenr.asp
public_schools_enrollment_df <- read_csv("data/raw/school_enrollment.csv") %>%
  select(DISTRICT, SCHOOL, ETHNIC, GENDER, ENR_TOTAL) %>%
  mutate(
    fuzzyjoin_school = paste(DISTRICT, SCHOOL, sep = " ")
  )

# taken from https://www.cde.ca.gov/SchoolDirectory/report?rid=dl1&tp=xlsx&ict=Y
public_schools_directory_df <- read_csv("data/raw/public_schools_directory.csv") %>%
  select(District, School, Street, City, Zip) %>%
  filter(School != "No Data", Street != "No Data") %>%
  mutate(
    fuzzyjoin_school = paste(District, School, sep = " ")
  )

public_schools_directory_df %>%
  inner_join(free_reduced_lunch_df, by = "fuzzyjoin_school") %>%
  mutate(fuzzyjoin_test_results = paste(Street, City, "CA", Zip, sep = " ")) %>%
  inner_join(test_results_df, by = c("fuzzyjoin_test_results" = "SchoolAddress"))

all_school_data_df <- public_schools_directory_df %>%
  stringdist_inner_join(free_reduced_lunch_df, by = "fuzzyjoin_school") %>%
  mutate(fuzzyjoin_test_results = paste(Street, City, "CA", Zip, sep = " ")) %>%
  stringdist_inner_join(test_results_df, by = c("fuzzyjoin_test_results", "SchoolAddress", max_dist = 3))
