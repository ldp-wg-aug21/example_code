## where am I working?
getwd()
# here::here()

## load required packages
library(tidyverse)
library(GGally)
library(data.table)
library(stringi)
library(rlpi)
library(mgcv)
library(ggthemes)
library(patchwork)

## set a plotting theme
theme_set(theme_few())


## load data
cLPI <- read_csv("data/CIEE_LPI_dataset.csv")

## lpi_pop_data (lambdas as calculated in test_script_canada.Rmd)
lpi_pop_data = fread("canadian_poplevel/all_canada_pops_poplevel.csv_pops_lambda.csv", na.strings = "NA")
split_index = stri_locate_last(lpi_pop_data$V1, fixed = "_")[, 1] + 1
lpi_pop_data$ID = as.numeric(stri_sub(lpi_pop_data$V1, split_index, nchar(lpi_pop_data$V1)))
# Calculate average and total change in annual columns
lpi_pop_data$sumlambda = rowSums(dplyr:::select(lpi_pop_data, starts_with(c("19", "20"))), na.rm = T)
lpi_pop_data$avlambda = rowMeans(dplyr:::select(lpi_pop_data, starts_with(c("19", "20"))), na.rm = T)


## select the most complete time series to test some ideas
range(lpi_pop_data$Freq)
lpi_pop_data$ID[lpi_pop_data$Freq == 65]

## lambdas
l_4368 <- as_tibble(lpi_pop_data) %>% 
  filter(ID == 4368) %>% 
  select(`1950`:`2019`) %>% 
  pivot_longer(., cols = `1950`:`2019`, 
               names_to = "year", values_to = "lambda") %>% 
  mutate(year = as.numeric(year))

## plot lambdas for selected time series
# plot(l_4368)
# plot(l_4368[l_4368$year != 1950, ])

ggplot(data = filter(l_4368, year != 1950) %>% drop_na(), 
       mapping = aes(x = year, y = lambda)) + 
  geom_point() + 
  geom_line() + 
  labs(title = "ts 4368: lambdas calculated from test_script_canada")


## pull raw data for the same time series
ts_4368 <- fread("data/CIEE_LPI_dataset.csv", na.strings = "NULL") %>% 
  filter(ID == 4368) %>% 
  mutate(across(.cols = `1950`:`2019`, ~ (as.numeric(.)))) %>% 
  select(`1950`:`2019`) %>% 
  pivot_longer(., cols = `1950`:`2019`, 
               names_to = "year", values_to = "value") %>% 
  mutate(year = as.numeric(year)) %>% 
  drop_na() ## drop the 5 empty years at the end 

## plot raw data
# plot(ts_4368)

ggplot(data = ts_4368, 
       mapping = aes(x = year, y = value)) + 
  geom_point() + 
  geom_line() + 
  labs(title = "ts 4368: raw data from CIEE_LPI_dataset")


## calculate log10 of the raw value, and join the raw and lambda data
ts_4368 <- ts_4368 %>% 
  mutate(log10_val = log10(value)) %>%
  left_join(l_4368, by = "year") %>%
  arrange(year)


## apply the GAM to the logged raw data
gam_4368 <- mgcv::gam(log10_val ~ s(year, k = nrow(ts_4368) / 2), 
                      data = ts_4368)
## NOTE: default in LPI is for number of kernels 
## k = 1/2 the number of observations in the time series

## extract the predicted values from the GAM
ts_4368 <- ts_4368 %>% 
  mutate(log10_pred = predict(gam_4368))

## plot raw and predicted log10 values
ggplot(data = ts_4368, 
       mapping = aes(x = year)) + 
  geom_point(mapping = aes(y = log10_val), colour = "red") + 
  geom_line(mapping = aes(y = log10_val), colour = "red") + 
  geom_point(mapping = aes(y = log10_pred), colour = "blue") + 
  geom_line(mapping = aes(y = log10_pred), colour = "blue") + 
  labs(y = "log10(value)", 
       title = "ts 4368: raw (red) and predicted (blue) log10 values")


## try re-calculating lambdas
ts_4368 <- ts_4368 %>% 
  mutate(lambda_val = log10_val - lag(log10_val), 
         lambda_pred = log10_pred - lag(log10_pred))

## Note: lambda is calculated as Nt+1 / N(t), which is 
## equivalent to log(Nt+1) - log(Nt)
## The quotient of raw values is the same as the difference of the 
## logged values

## plot the original, raw, and predicted lambdas
ggplot(data = ts_4368 %>% filter(year > 1950), 
       mapping = aes(x = year)) + 
  geom_point(mapping = aes(y = lambda), colour = "black") + 
  geom_line(mapping = aes(y = lambda), colour = "black") + 
  geom_point(mapping = aes(y = lambda_val), colour = "red") + 
  geom_line(mapping = aes(y = lambda_val), colour = "red") + 
  geom_point(mapping = aes(y = lambda_pred), colour = "blue") +
  geom_line(mapping = aes(y = lambda_pred), colour = "blue") +
  labs(title = "ts 4368: original (black), raw (red) and predicted (blue) lambdas")

## compare previously-calculated lambdas and new derived lambdas
ggplot(data = filter(ts_4368, year > 1950), 
       mapping = aes(x = lambda, y = lambda_pred)) + 
  geom_abline(slope = 1, intercept = 0, colour = "grey50", lty = 2) + 
  geom_point() + 
  labs(x = "original lambda", y = "predicted lambda")



# data exploration --------------------------------------------------------

names(cLPI)
skimr::skim(cLPI)

## how many years are sampled in each time series?
# cLPI <- cLPI %>% 
#   # rowwise() %>% 
#   # mutate(Freq = sum(is.numeric()))
#   mutate(Freq = rowSums(is.numeric(.)))
# 
# names(lpi_pop_data)
# 
# lpi_pop_data %>% 
#   rowwise() %>% 
#   mutate(first_year )

## pivot to long format (so much easier to work with!!)
cLPI_l <- cLPI %>% 
  select(ID, `1950`:`2020`) %>% 
  pivot_longer(cols = `1950`:`2020`, 
               names_to = "year", 
               values_to = "value") %>% 
  ## drop the unsampled years
  mutate(year = as.numeric(year)) %>% 
  filter(value != "NULL")

## find the total number observations for each time series
n_samples <- cLPI_l %>% 
  group_by(ID) %>% 
  tally()

## first sampling year per time series?
first_y <- cLPI_l %>% 
  group_by(ID) %>% 
  slice_head() %>% 
  select(-value)

## last sampling year per time series?
last_y <- cLPI_l %>% 
  group_by(ID) %>% 
  slice_tail() %>% 
  select(-value)

## add this summary info as columns in cLPI
cLPI <- cLPI %>% 
  mutate(first_year = first_y$year[first_y$ID == .$ID], 
         last_year = last_y$year[last_y$ID == .$ID], 
         timespan = last_year - first_year + 1, 
         n_years = n_samples$n[n_samples$ID == .$ID], 
         coverage = n_years / timespan)

## clean up
rm(first_y, last_y, n_samples, n_samples)


## histograms of summary information
## first year of sampling
(p1 <- ggplot(data = cLPI, 
       mapping = aes(x = first_year)) + 
  geom_histogram(colour = "black", fill = "grey50") + 
    geom_vline(xintercept = mean(cLPI$first_year), colour = "red") + 
    geom_vline(xintercept = median(cLPI$first_year), colour = "blue") + 
    labs(title = "First year of sampling", 
         x = NULL))

## last year of sampling
(p2 <- ggplot(data = cLPI, 
       mapping = aes(x = last_year)) + 
    geom_histogram(colour = "black", fill = "grey50") + 
    geom_vline(xintercept = mean(cLPI$last_year), colour = "red") + 
    geom_vline(xintercept = median(cLPI$last_year), colour = "blue") + 
    labs(title = "Last year of sampling", 
         x = NULL))

## timespan
(p3 <- ggplot(data = cLPI, 
       mapping = aes(x = timespan)) + 
    geom_histogram(colour = "black", fill = "grey50") + 
    geom_vline(xintercept = mean(cLPI$timespan), colour = "red") + 
    geom_vline(xintercept = median(cLPI$timespan), colour = "blue") + 
    labs(title = "Timespan (last - first + 1)", 
         x = NULL))

## coverage
(p4 <- ggplot(data = cLPI, 
       mapping = aes(x = coverage)) + 
    geom_histogram(colour = "black", fill = "grey50") + 
    geom_vline(xintercept = mean(cLPI$coverage), colour = "red") + 
    geom_vline(xintercept = median(cLPI$coverage), colour = "blue") + 
    labs(title = "Coverage (sampled years / timespan)", 
         x = NULL))

(p1 + p2) / (p3 + p4)
rm(p1, p2, p3, p4)


## create similar histograms, but facetted by 
## taxonomic Class and System (terrestrial, marine, FW)

## first year of sampling
(p1.2 <- ggplot(data = cLPI, 
                mapping = aes(x = first_year)) + 
    geom_histogram() + 
    geom_vline(data = cLPI %>% 
                 group_by(Class, System) %>% 
                 summarise(mean = mean(first_year)), 
               mapping = aes(xintercept = mean), 
               colour = "red") + 
    geom_vline(data = cLPI %>% 
                 group_by(Class, System) %>% 
                 summarise(median = median(first_year)), 
               mapping = aes(xintercept = median), 
               colour = "blue") + 
    facet_grid(System ~ Class, scales = "free_y") + 
    labs(title = "First year of sampling", x = NULL) + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1)))

## last year of sampling
(p2.2 <- ggplot(data = cLPI, 
                mapping = aes(x = last_year)) + 
    geom_histogram() + 
    geom_vline(data = cLPI %>% 
                 group_by(Class, System) %>% 
                 summarise(mean = mean(last_year)), 
               mapping = aes(xintercept = mean), 
               colour = "red") + 
    geom_vline(data = cLPI %>% 
                 group_by(Class, System) %>% 
                 summarise(median = median(last_year)), 
               mapping = aes(xintercept = median), 
               colour = "blue") + 
    facet_grid(System ~ Class, scales = "free_y") + 
    labs(title = "Last year of sampling", x = NULL) + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1)))

## timespan
(p3.2 <- ggplot(data = cLPI, 
                mapping = aes(x = timespan)) + 
    geom_histogram() + 
    geom_vline(data = cLPI %>% 
                 group_by(Class, System) %>% 
                 summarise(mean = mean(timespan)), 
               mapping = aes(xintercept = mean), 
               colour = "red") + 
    geom_vline(data = cLPI %>% 
                 group_by(Class, System) %>% 
                 summarise(median = median(timespan)), 
               mapping = aes(xintercept = median), 
               colour = "blue") + 
    facet_grid(System ~ Class, scales = "free_y") + 
    labs(title = "Timespan (last - first + 1)", x = NULL))

## coverage
(p4.2 <- ggplot(data = cLPI, 
                mapping = aes(x = coverage)) + 
    geom_histogram() + 
    geom_vline(data = cLPI %>% 
                 group_by(Class, System) %>% 
                 summarise(mean = mean(coverage)), 
               mapping = aes(xintercept = mean), 
               colour = "red") + 
    geom_vline(data = cLPI %>% 
                 group_by(Class, System) %>% 
                 summarise(median = median(coverage)), 
               mapping = aes(xintercept = median), 
               colour = "blue") + 
    facet_grid(System ~ Class, scales = "free_y") + 
    labs(title = "Coverage (sampled years / timespan)", x = NULL) + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1)))

(p1.2 + p2.2) / (p3.2 + p4.2)
rm(p1.2, p2.2, p3.2, p4.2)


# sub-sampling ------------------------------------------------------------

## How many time series have complete coverage for at least 5 years?
cLPI %>% 
  filter(coverage == 1, timespan >= 5) %>% 
  n_distinct("ID") %>% ## [1] 1421
  ggplot(data = ., 
         mapping = aes(x = timespan)) + 
  geom_histogram()

## what is the composition of that subset?
sub_sum <- cLPI %>% 
  filter(coverage == 1, timespan >= 5) %>% 
  group_by(Class, System) %>% 
  summarise(n_sub = n_distinct(ID), 
            avg_span_sub = mean(timespan)) %>% 
  mutate(prop_samp = n_sub / 1421)

## how does this composition compare to that of the whole dataset?
cLPI_sum <- cLPI %>% 
  group_by(Class, System) %>% 
  summarise(n_all = n_distinct(ID), 
            avg_span_all = mean(timespan)) %>% 
  mutate(prop_all = n_all / n_distinct(cLPI$ID))

## combine subset and overall summary into one table
summ <- left_join(sub_sum, cLPI_sum, 
                  by = c("Class", "System")) %>% 
  mutate(across(.cols = n_sub:prop_all, ~ round(., digits = 2)))
knitr::kable(summ)


