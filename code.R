library(dplyr)
library(readr)
library(ggplot2)
library(readxl)

# # Read in congo data
# CSV:
#   Rows = one mother-child pair
# Columns = variables related to delivery, antenatal care, baby
congo <- read_csv('data/congo/20230807 database Monkole.csv', local = locale(encoding = "latin1"))
# remove na rows
congo <- congo[1:319,]
# congox <- readxl::read_excel('data/congo/20230807 database Monkole EXCEL.xlsx')

# Replace non-reports with NA
for(j in 1:ncol(congo)){
  vals <- unlist(congo[,j])
  is_character <- class(vals) == 'character'
  if(is_character){
    vals[grepl('non report', vals)] <- NA
    vals[grepl('non raport', vals)] <- NA
    vals[grepl('non rapport', vals)] <- NA
    vals[vals == '*'] <- NA
    
    congo[,j] <- vals
  }
}

# Identify date variables
var_names <- names(congo)
date_vars <- c()
for(j in 1:ncol(congo)){
  message(j)
  vals <- unlist(congo[,j])
  is_character <- class(vals) == 'character'
  if(is_character){
    try({
      is_date <- any(nchar(vals) == 10 & grepl('/', vals, fixed = TRUE))
      if(is_date){
        date_vars <- c(date_vars, var_names[j])
      }
    })
  }
}
# Add to the date vars
date_vars <- c(date_vars, 'last_menstrual_period', names(congo)[grepl('date', tolower(names(congo)))])
date_vars <- sort(unique(date_vars))
# Change date variables to date format
for(j in 1:length(date_vars)){
  message(j)
  this_var <- date_vars[j]
  vals <- unlist(congo[,this_var])
  vals <- as.character(vals)
  date_vals <- as.Date(vals, '%d/%m/%Y')
  congo[,this_var] <- date_vals
}

# Completeness
completeness <- congo %>% 
  summarise_all(function(x){length(which(is.na(x)))}) %>%
  tidyr::gather(key, value, mother_chart_number:Baby_2_outcome) %>%
  mutate(denom = nrow(congo)) %>%
  mutate(percent_missing = value / denom * 100) %>%
  filter(percent_missing > 0) %>%
  arrange(percent_missing) %>%
  mutate(key = factor(key, levels = key))
ggplot(data = completeness,
       aes(x = key,
           y = percent_missing)) +
  geom_point() +
  coord_flip() +
  theme(axis.text.x = element_text(size = 1))

# Number of women with ANC data
pd <- congo %>%
  mutate(ANC = toupper(ANC)) %>%
  group_by(ANC) %>%
  tally
ggplot(data = pd,
       aes(x = ANC,
           y = n)) +
  geom_bar(stat = 'identity')

# ### Number of women with malaria reported in ANC (regardless of type)
pd <- congo %>%
  mutate(ANC = toupper(ANC)) %>%
  filter(ANC == 'OUI') %>%
  group_by(malaria_diagnosis) %>%
  tally

ggplot(data = pd,
       aes(x = malaria_diagnosis,
           y = n)) +
  geom_bar(stat = 'identity')

## Delivery hour and apgar results
# doing just baby 1 for simplicity / completeness
library(chron)

pd <- congo %>%
  filter(nchar(time_delivery_baby_1) == 5) %>%
  filter(!is.na(time_delivery_baby_1)) %>%
  mutate(gsub(".", ":", time_delivery_baby_1, fixed = TRUE)) %>%
  mutate(time_delivery_baby_1 = ifelse(time_delivery_baby_1 == '11.00', '11:00', time_delivery_baby_1)) %>%
  mutate(delivery_hour = times(paste0(time_delivery_baby_1, ':00'))) %>%
  mutate(apgar1 = Apgar_1_baby_1 ) %>%
  mutate(apgar5 = Apgar_5_baby_1 ) %>%
  mutate(apgar10 = Apgar_10_baby_1 ) %>%
  filter(apgar1 <= 10)

ggplot(data = pd,
       aes(x = delivery_hour * 24,
           y = apgar1)) +
  geom_point()
fit <- lm(apgar1 ~ delivery_hour, data = pd)
coefs <- coef(fit)
summary(fit)
# p = 0.9, not significant

ggplot(data = pd,
       aes(x = delivery_hour * 24,
           y = apgar5)) +
  geom_point()
fit <- lm(apgar5 ~ delivery_hour, data = pd)
coefs <- coef(fit)
summary(fit)
# p = 0.84, not significant

ggplot(data = pd,
       aes(x = delivery_hour * 24,
           y = apgar10)) +
  geom_point()
fit <- lm(apgar10 ~ delivery_hour, data = pd)
coefs <- coef(fit)
summary(fit)
# p = 0.43, not significant

# Now onto cote divoire analysis
# # Read in cote d'ivoire data
# one CSV for rains
cote_rain <- read_csv('data/cotedivoire/20230808 Rain data tamboukro 2013-22.csv')
# Other csv for medical data
cote <- read_csv('data/cotedivoire/20230808 Tomboukro database.csv')
# Rows = years
# Columns = months
# Rain in mm
# one CSV for malaria
# Rows = one malaria test
# Columns = date, results (1 = pos, 0= neg), sex, age in years
cote <- cote %>%
  mutate(date = as.Date(Test_date, format = '%d/%m/%Y'))


# Questions/graphs:
#   Tests done per month 2013-2023
pd <- cote %>%
  group_by(month = lubridate::floor_date(date, 'month')) %>%
  tally
ggplot(data = pd,
       aes(x = month,
           y = n)) +
  geom_point() +
  geom_line() +
  labs(title = 'Tests per month')

# Mean and range per month (mean of daily tests)
pd <- cote %>%
  group_by(month = lubridate::floor_date(date, 'month')) %>%
  summarise(avg )

# Type of test (GE = gout espese, TDR = RDT) per month
# Test positivity rate per month
# Simple
# Mean and range
# positivity rate by sex (monthly)
# posititivty rate by age tranch (<5, 5-15, >15) (monthly)
# Mean age of those with positive tests
# Rains per month
# Simple
# Mean and range
# Correlation rain-test positivity rate (monthly)
# Correlation rain-number of malaria cases (monthly)
