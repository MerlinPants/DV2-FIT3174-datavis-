library(readr)
library(tidyverse)
library(readxl)

## Age sex

age_male <- read_excel("age_sex_state_01-25.xlsx", 
                                  sheet = "Table 1", skip = 5)

age_female <- read_excel("age_sex_state_01-25.xlsx", 
                       sheet = "Table 2", skip = 5)

age_male <- age_male %>% select(-c('S/T code', `LGA name`))
colnames(age_male) <- c('Year', 'State', 'LGAcode', '0-4', '5-9', '10-14', '15-19', '20-24', '25-29', '30-34', '35-39', '40-44', '45-49', '50-54', '55-59', '60-64', '65-69', '70-74', '75-79','80-84','85+', 'Total')

age_female <- age_female %>% select(-c('S/T code', `LGA name`))
colnames(age_female) <- c('Year', 'State', 'LGAcode', '0-4', '5-9', '10-14', '15-19', '20-24', '25-29', '30-34', '35-39', '40-44', '45-49', '50-54', '55-59', '60-64', '65-69', '70-74', '75-79','80-84','85+', 'Total')

age_male <- age_male %>% group_by(Year, State) %>%
  summarise('0-4' = sum(`0-4`)/sum(Total),
            '5-9' = sum(`5-9`)/sum(Total),
            '10-14' = sum(`10-14`)/sum(Total),
            '15-19' = sum(`15-19`)/sum(Total),
            '20-24' = sum(`20-24`)/sum(Total),
            '25-29' = sum(`25-29`)/sum(Total),
            '30-34' = sum(`30-34`)/sum(Total),
            '35-39' = sum(`35-39`)/sum(Total),
            '40-44' = sum(`40-44`)/sum(Total),
            '45-49' = sum(`45-49`)/sum(Total),
            '50-54' = sum(`50-54`)/sum(Total),
            '55-59' = sum(`55-59`)/sum(Total),
            '60-64' = sum(`60-64`)/sum(Total),
            '65-69' = sum(`65-69`)/sum(Total),
            '70-74' = sum(`70-74`)/sum(Total),
            '75-79' = sum(`75-79`)/sum(Total),
            '80-84' = sum(`80-84`)/sum(Total),
            '85+' = sum(`85+`)/sum(Total))

age_female <- age_female %>% group_by(Year, State) %>%
  summarise('0-4' = sum(`0-4`)/sum(Total),
            '5-9' = sum(`5-9`)/sum(Total),
            '10-14' = sum(`10-14`)/sum(Total),
            '15-19' = sum(`15-19`)/sum(Total),
            '20-24' = sum(`20-24`)/sum(Total),
            '25-29' = sum(`25-29`)/sum(Total),
            '30-34' = sum(`30-34`)/sum(Total),
            '35-39' = sum(`35-39`)/sum(Total),
            '40-44' = sum(`40-44`)/sum(Total),
            '45-49' = sum(`45-49`)/sum(Total),
            '50-54' = sum(`50-54`)/sum(Total),
            '55-59' = sum(`55-59`)/sum(Total),
            '60-64' = sum(`60-64`)/sum(Total),
            '65-69' = sum(`65-69`)/sum(Total),
            '70-74' = sum(`70-74`)/sum(Total),
            '75-79' = sum(`75-79`)/sum(Total),
            '80-84' = sum(`80-84`)/sum(Total),
            '85+' = sum(`85+`)/sum(Total))

age_male <- age_male %>% mutate(gender = 'Male')
age_female <- age_female %>% mutate(gender = 'Female')

age_sex <- rbind(age_male, age_female)

age_sex <- age_sex %>% pivot_longer(cols = c('0-4', '5-9', '10-14', '15-19', '20-24', '25-29', '30-34', '35-39', '40-44', '45-49', '50-54', '55-59', '60-64', '65-69', '70-74', '75-79','80-84','85+'),
                                    names_to = 'Age_group',
                                    values_to = 'Count')

write_csv(age_sex, 'C:/Users/ME/Documents/UniStuff/FIT3174_DataVis/DV2/data/prop_age_sex_01-25.csv')



## Migrant Job

migrant_job_prob <- migrant_job_prob %>% mutate(migrant = if_else(
  `Major country group (subcontinent) of birth: SACC (2011)` == 'Australia (includes External Territories)',
  'Australian',
  'Migrant'))

migrant_job <- migrant_job_prob %>% group_by(Month, migrant) %>%
  summarise(fulltime = sum(`Employed full-time ('000)`),
            parttime = sum(`Employed part-time ('000)`))

migrant_job <- read_csv("C:/Users/ME/Documents/UniStuff/FIT3174_DataVis/DV2/data/migrant_job.csv")

migrant_job <- migrant_job %>%
  pivot_longer(
    cols = c(fulltime, parttime),
    names_to = "employment",
    values_to = "value"
  ) %>%
  mutate(
    class = paste(migrant, employment, sep = "_")
  )

write_csv(migrant_job, 'C:/Users/ME/Documents/UniStuff/FIT3174_DataVis/DV2/data/migrant_job.csv')
