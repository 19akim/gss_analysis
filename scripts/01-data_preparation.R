#### Preamble ####
# Purpose: Prepare the 2021 GSS data
# Author: Ayoon Kim
# Data: 20 March 2022
# Contact: ayoon.kim@mail.utoronto.ca
# License: MIT
# Pre-requisites: 
# - Need to have downloaded the GSS data and saved it to inputs/data


#### Workspace setup ####
library(haven)
library(tidyverse)
# Read in the raw data. 
raw_data <- haven::read_dta("inputs/data/2021_stata/gss2021.dta")

names(raw_data)
 
reduced_data <- 
  raw_data %>% 
  select(age,
         sexnow1,
         educ,
         nateduc,
         degree,
         incom16,
         income
         ) %>%
  rename(gender = sexnow1)

rm(raw_data)
     

#### Recode gender ####
# The question for SEXMOW1 is:
# "Do you describe yourself as male, female, or transgender?"
# Options are from the Codebook 'GSS 2021 Codebook R1b.pdf'
reduced_data <- 
  reduced_data %>%
  mutate(age = case_when(
    age >= 10 & age <= 19 ~ "10-19 Years old",
    age >= 20 & age <= 29 ~ "20-29 Years old",
    age >= 30 & age <= 39 ~ "30-39 Years old",
    age >= 40 & age <= 49 ~ "40-49 Years old",
    age >= 50 & age <= 59 ~ "50-59 Years old",
    age >= 60 & age <= 69 ~ "60-69 Years old",
    age >= 70 & age <= 79 ~ "70-79 Years old",
    age >= 80 ~ "80 Years old or over"
  ))
  

#### Recode gender ####
# The question for SEXMOW1 is:
# "Do you describe yourself as male, female, or transgender?"
# Options are from the Codebook 'GSS 2021 Codebook R1b.pdf'
reduced_data <- 
  reduced_data %>%
  mutate(gender = case_when(
  gender == 1 ~ "Male",
  gender == 2 ~ "Female",
  gender == 3 ~ "Transgender",
  gender == 4 ~ "None of these"
  ))

#### Recode educ ####
# The label of EDUC is:
# Respondent's Education
# Options are from the Codebook 'GSS 2021 Codebook R1b.pdf'
reduced_data <-
  reduced_data %>%
  mutate(educ = case_when(
    educ == 0 ~ "No formal schooling",
    educ == 1 ~ "1st Grade",
    educ == 2 ~ "2nd Grade",
    educ == 3 ~ "3rd Grade",
    educ == 4 ~ "4th Grade",
    educ == 5 ~ "5th Grade",
    educ == 6 ~ "6th Grade",
    educ == 7 ~ "7th Grade",
    educ == 8 ~ "8th Grade",
    educ == 9 ~ "9th Grade",
    educ == 10 ~ "10th Grade",
    educ == 11 ~ "11th Grade",
    educ == 12 ~ "12th Grade",
    educ == 13 ~ "1 Year of College",
    educ == 14 ~ "2 Years of College",
    educ == 15 ~ "3 Years of College",
    educ == 16 ~ "4 Years of College",
    educ == 17 ~ "5 Years of College",
    educ == 18 ~ "6 Years of College",
    educ == 19 ~ "7 Years of College",
    educ == 20 ~ "8 Years of College"
  ))

#### Recode nateduc ####
# The question for NATEDUC is:
# "Are we spending too much, too little, or about the right amount on improving the nation's education system?"
# Options are from the Codebook 'GSS 2021 Codebook R1b.pdf'
reduced_data <- 
  reduced_data %>%
  mutate(nateduc = case_when(
    nateduc == 1 ~ "Too little",
    nateduc == 2 ~ "About right",
    nateduc == 3 ~ "Too much"
  ))

#### Recode degree ####
# The label of DEGREE is:
# Respondent's Degree
# Options are from the Codebook 'GSS 2021 Codebook R1b.pdf'
reduced_data <- 
  reduced_data %>%
  mutate(degree = case_when(
    degree == 0 ~ "Less than highschool",
    degree == 1 ~ "Highschool",
    degree == 2 ~ "Associate/Junior College",
    degree == 3 ~ "Bachelors",
    degree == 4 ~ "Graduate"
  ))

#### Recode incom16 ####
# The question for INCOM16 is:
# "Thinking about the time when you were 16 years old, compared with American families in general then,
# would you say your family income was: far below average, below average, average, above average, or far
# above average?"
# Options are from the Codebook 'GSS 2021 Codebook R1b.pdf'
reduced_data <- 
  reduced_data %>%
  mutate(incom16 = case_when(
    incom16 == 1 ~ "Far below average",
    incom16 == 2 ~ "Below average",
    incom16 == 3 ~ "Average",
    incom16 == 4 ~ "Above average",
    incom16 == 5 ~ "Far above average"
  ))

#### Recode income ####
# The question for INCOME is:
# "In which of these groups did your total family income, from all sources, fall last year before taxes, that is?"
# Options are from the Codebook 'GSS 2021 Codebook R1b.pdf'
reduced_data <-
  reduced_data %>%
  mutate(income = case_when(
    income == 1 ~ "Under $1,000",
    income == 2 ~ "$1,000 to $2,999",
    income == 3 ~ "$3,000 to $3,999",
    income == 4 ~ "$4,000 to $4,999",
    income == 5 ~ "$5,000 to $5,999",
    income == 6 ~ "$6,000 to $6,999",
    income == 7 ~ "$7,000 to $7,999",
    income == 8 ~ "$8,000 to $9,999",
    income == 9 ~ "$10,000 to $14,999",
    income == 10 ~ "$15,000 to $19,999",
    income == 11 ~ "$20,000 to $24,999",
    income == 12 ~ "$25,000 or more",
    income == 13 ~ "Refused"
    ))


#### Save ####
write_csv(reduced_data, "outputs/data/prepared_gss.csv")



         