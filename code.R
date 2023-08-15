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
  group_by(date) %>%
  summarise(n_tests = n()) %>%
  ungroup %>%
  group_by(month = lubridate::floor_date(date, 'month')) %>%
  summarise(avg_daily_tests = mean(n_tests),
            min_daily_tests = min(n_tests),
            max_daily_tests = max(n_tests),
            p25_daily_tests = quantile(n_tests, 0.25),
            p75_daily_tests = quantile(n_tests, 0.75))
ggplot(data = pd) +
  geom_ribbon(aes(x = month,
                  ymin = p25_daily_tests,
                  ymax = p75_daily_tests),
              fill = 'orange') +
  geom_line(aes(x = month,
           y = avg_daily_tests))

# Type of test (GE = gout espese, TDR = RDT) per month
pd <- cote %>%
  group_by(month = lubridate::floor_date(date, 'month'),
           test) %>%
  tally
ggplot(data = pd,
       aes(x = month,
           y = n,
           fill = test,
           group = test)) +
  geom_bar(stat = 'identity',
           position = position_stack())
  


# Test positivity rate per month
# Simple
pd <- cote %>%
  group_by(month = lubridate::floor_date(date, 'month')) %>%
  summarise(n_tests = n(),
            n_positve = sum(result)) %>%
  ungroup %>%
  mutate(p_positive = n_positve / n_tests * 100)
ggplot(data = pd,
       aes(x = month,
           y = p_positive)) +
  geom_line()
  
# Mean and range
mean(pd$p_positive, na.rm = TRUE)
range(pd$p_positive, na.rm = TRUE)

# positivity rate by sex (monthly)
pd <- cote %>%
  mutate(sex = toupper(sex)) %>%
  filter(!is.na(sex)) %>%
  group_by(month = lubridate::floor_date(date, 'month'),
           sex) %>%
  summarise(n_tests = n(),
            n_positve = sum(result)) %>%
  ungroup %>%
  mutate(p_positive = n_positve / n_tests * 100)
ggplot(data = pd,
       aes(x = month,
           y = p_positive,
           color = sex)) +
  geom_line() +
  facet_wrap(~sex) +
  theme(legend.position = 'none')
# posititivty rate by age tranch (<5, 5-15, >15) (monthly)
pd <- cote %>%
  mutate(age = `age (years)`) %>%
  filter(!is.na(age)) %>%
  mutate(age = cut(age, c(0, 6, seq(6.01, 66, 12), 100))) %>%
  filter(!is.na(age)) %>%
  group_by(month = lubridate::floor_date(date, 'month'),
           age) %>%
  summarise(n_tests = n(),
            n_positve = sum(result)) %>%
  ungroup %>%
  mutate(p_positive = n_positve / n_tests * 100)
ggplot(data = pd,
       aes(x = month,
           y = p_positive,
           color = age)) +
  geom_point(aes(size = n_tests)) +
  geom_line() +
  facet_wrap(~age) +
  theme(legend.position = 'none')
# Mean age of those with positive tests
mean(cote$`age (years)`[cote$result == 1], na.rm = TRUE)

# (got to here)
# Rains per month

# Simple
# Mean and range
# Correlation rain-test positivity rate (monthly)
# Correlation rain-number of malaria cases (monthly)
