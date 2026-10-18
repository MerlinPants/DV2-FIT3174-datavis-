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



## Migrant CoB

migrant_CoB_96_25 <- read_excel("xlsx/migrant_CoB_96-25.xlsx", 
                                sheet = "Table 1.2", skip = 14)

migrant_CoB_96_25 <- migrant_CoB_96_25[c(1:27,30),] %>% filter(`Country of birth by minor group(c)` != 'Antarctica') %>%
  select(-`SACC code(c)`)

migrant_CoB_96_25 <- migrant_CoB_96_25 %>%
  mutate(region = case_when(
    `Country of birth by minor group(c)` %in% c('Melanesia', 'Micronesia', 'Polynesia (excludes Hawaii)', 'New Zealand') ~ 'New Zealand and Pacific Islands',
    `Country of birth by minor group(c)` %in% c('United Kingdom, Channel Is and Isle of Man(e)', 'Ireland') ~ 'British and Irish Isle',
    str_detect(`Country of birth by minor group(c)`, 'Europe') ~ 'Europe',
    str_detect(`Country of birth by minor group(c)`, 'South-East Asia') ~ 'Rest of Asia',
    `Country of birth by minor group(c)` %in% c('Southern Asia', 'Central Asia') ~ 'Rest of Asia',
    str_detect(`Country of birth by minor group(c)`, 'America') ~ 'America',
    `Country of birth by minor group(c)` %in% c('Caribbean') ~ 'America',
    str_detect(`Country of birth by minor group(c)`, 'Africa') ~ 'Africa',
    `Country of birth by minor group(c)` %in% c('Chinese Asia (includes Mongolia)', 'Japan and the Koreas') ~ 'Rest of Asia',
    TRUE ~ `Country of birth by minor group(c)`
  ))

migrant_CoB_96_25 <- migrant_CoB_96_25 %>% pivot_longer(
  cols = colnames(migrant_CoB_96_25[,2:31]),
  names_to = 'Year',
  values_to = 'Count'
)

migrant_CoB_96_25 <- migrant_CoB_96_25 %>%
  group_by(Year, region) %>%
  summarise(
    Count = sum(Count, na.rm = TRUE),
    .groups = "drop"
  )

migrant_CoB_96_25 <- migrant_CoB_96_25 %>%
  mutate(lon = case_when(
    region == 'British and Irish Isle' ~ -4,
    region == 'Europe' ~ 8.9959024,
    region == 'America' ~ -80.375805,
    region == 'Africa' ~ 17.7041361,
    region == 'Rest of Asia' ~ 104.1936215,
    region == 'Middle East' ~ 45.0019909,
    region == 'New Zealand and Pacific Islands' ~ 175.4157576,
    region == 'Total overseas-born' ~ 132.1334181
  ))

migrant_CoB_96_25 <- migrant_CoB_96_25 %>%
  mutate(lat = case_when(
    region == 'British and Irish Isle' ~ 54,
    region == 'Europe' ~ 53.0028051,
    region == 'America' ~ 15.18061165,
    region == 'Africa' ~ 7.1314008,
    region == 'Rest of Asia' ~ 35.8622589,
    region == 'Middle East' ~ 35.2623564,
    region == 'New Zealand and Pacific Islands' ~ -41.2235474,
    region == 'Total overseas-born' ~ -23.1168263
  ))

migrant_CoB_96_25 <- migrant_CoB_96_25 %>% mutate(dest_lon = 132.1334181, dest_lat = -23.1168263)

migrant_CoB_96_25 <- migrant_CoB_96_25 %>% filter(region != 'Australia (includes External Territories)')

write_csv(migrant_CoB_96_25, 'C:/Users/ME/Documents/UniStuff/FIT3174_DataVis/DV2/data/migrant_CoB_96_25.csv')


mines <- read_csv("mines.csv")

mines <- mines %>% mutate(Status = case_when(
  Status == 'Developing Mine' ~ 'Developing mine',
  TRUE ~ Status
))

write_csv(mines, "mines.csv")


merchandise_exports <- read_excel("xlsx/merchandise_exports.xlsx")

merchandise_exports <- merchandise_exports %>% filter(`2025` > 0)

merchandise_exports <- merchandise_exports %>% rename('Exports(millions)' = `2025`)

merchandise_exports <- merchandise_exports %>% mutate(Group = case_when(
  `Exports(millions)` < 20000 ~ 'Others',
  TRUE ~ Country
))

merchandise_exports <- merchandise_exports %>% group_by(Group) %>%
  summarise('Exports' = sum(`Exports(millions)`))

write_csv(merchandise_exports, 'merchandise_exports.csv')
