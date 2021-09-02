# initial set-up ----------------------------------------------------------

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


# data import -------------------------------------------------------------

## Canadian LPI dataset
cLPI <- read_csv("data/CIEE_LPI_dataset.csv")
names(cLPI)

## list of Canadian species in the Wild Species Report
wild_sp <- read_csv("data/WildSpecies2015Data.csv", 
                    na = "NA", 
                    col_types = cols(.default = col_character()))
names(wild_sp)

## Canadian LPI lambdas (as calculated in test_script_canada.Rmd)
lpi_pop_data = fread("canadian_poplevel/all_canada_pops_poplevel.csv_pops_lambda.csv", 
                     na.strings = "NA")
split_index = stri_locate_last(lpi_pop_data$V1, 
                               fixed = "_")[, 1] + 1
lpi_pop_data$ID = as.numeric(stri_sub(lpi_pop_data$V1, 
                                      split_index, 
                                      nchar(lpi_pop_data$V1)))
# Calculate average and total change in annual columns
lpi_pop_data$sumlambda = rowSums(dplyr:::select(lpi_pop_data, 
                                                starts_with(c("19", "20"))), 
                                 na.rm = TRUE)
lpi_pop_data$avlambda = rowMeans(dplyr:::select(lpi_pop_data, 
                                                starts_with(c("19", "20"))), 
                                 na.rm = TRUE)
## COMMENT: 
## will need the lambdas later for single time series exploration


# Wild Species list -------------------------------------------------------

## we need to resolve the taxonomic discrepancies between
## the global Living Planet Index and the Canadian species list
## from the Wild Species Report

## how many unique species are in the WSR?
n_distinct(wild_sp$Binomial) ## [1] 1779

## how many unique species are in the cLPI?
n_distinct(cLPI$Binomial) ## [1] 907

## create a list of the species that intersect between the two
intersect(cLPI$Binomial, wild_sp$Binomial)

## which species names occur in cLPI but not in wild_sp?
lpi_wsr_diff <- setdiff(cLPI$Binomial, wild_sp$Binomial) %>% 
  sort() %>% 
  tibble()

n_distinct(lpi_wsr_diff) ## [1] 58
## COMMENT: 58 species exists in cLPI that aren't in wild_sp
## (Note: 2 of these will be excluded as they do not regularly 
## occur in Canada -- see below)

## how many time series does that represent?
filter(cLPI, Binomial %in% lpi_wsr_diff$.) %>% 
  n_distinct() ## [1] 142 time series

## write the un-matched list to CSV 
## (will find the correct matches manually)
write_csv(lpi_wsr_diff, "data/cLPI_WSR_setdiff.csv")      

## import the revised matches
lpi_wsr_syn <- read_csv("data/cLPI_WSR_synonyms.csv")
lpi_wsr_syn

## add correctly matched species to the synomyms list
lpi_wsr_int <- tibble(Binomial_LPI = intersect(cLPI$Binomial, wild_sp$Binomial), 
                      Binomial_WSR = intersect(cLPI$Binomial, wild_sp$Binomial), 
                      Note = as.character(NA))
lpi_wsr_syn <- bind_rows(lpi_wsr_syn, lpi_wsr_int)

## add a new column to cLPI with the resolved binomials
cLPI <- cLPI %>% 
  left_join(., lpi_wsr_syn[1:2], by = c("Binomial" = "Binomial_LPI")) %>%
  rename(Binomial_resolved = Binomial_WSR) %>%
  relocate(Binomial_resolved, .after = Binomial)

## check merge
which(is.na(cLPI$Binomial_resolved))

table((cLPI$Binomial == cLPI$Binomial_resolved))
intersect(cLPI$Binomial, cLPI$Binomial_resolved)

## COMMENT:
## seems to have done what I wanted, but it's hard to check because 
## a couple species that are grouped in cLPI are split by WSR


## some time series belong to species which do not regularly
## occur in Canada, according to WSR and should not be included
## when calculating the Canadian LPI. Add a column to indicate
## which species should be excluded (== 1)
cLPI <- cLPI %>% 
  mutate(Exclude = if_else(Binomial_resolved %in% 
                             c("Bathyraja_aleutica", 
                               "Bathyraja_minispinosa"), 1, 0))

table(cLPI$Exclude)
## COMMENT: 6 time series will be excluded


## write the updated cLPI data to CSV
write_csv(cLPI, "cLPI_data_resolved_species.csv")


# compare cLPI and WSR ----------------------------------------------------

## what is the taxonomic breakdown for the WSR species list?
wild_sp %>% 
  group_by(TAXONOMIC_GROUP) %>% 
  summarise(n_species_WSR = n_distinct(Binomial), 
            prop_WSR = round(n_species_WSR / 1779, 2)) %>% 
  knitr::kable()

## how does that compare to the same for the cLPI dataset?
cLPI %>% 
  group_by(Class) %>% 
  summarise(n_ID_cLPI = n_distinct(ID), 
            n_species_cLPI = n_distinct(Binomial_resolved)) %>% 
  mutate(prop_ID_cLPI = round(n_ID_cLPI / n_distinct(cLPI$ID), 2),
         prop_species_cLPI =  round(
           n_species_cLPI / n_distinct(cLPI$Binomial_resolved), 2)) %>% 
  knitr::kable()


# calculate time series summary info --------------------------------------

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
rm(first_y, last_y, n_samples)


# plot time series summary info -------------------------------------------

### histograms of summary information ###
## first year of sampling
p1 <- ggplot(data = cLPI, 
             mapping = aes(x = first_year)) + 
  geom_histogram(colour = "black", fill = "grey50") + 
  geom_vline(xintercept = mean(cLPI$first_year), colour = "red") + 
  geom_vline(xintercept = median(cLPI$first_year), colour = "blue") + 
  labs(title = "First year of sampling", x = NULL)

## last year of sampling
p2 <- ggplot(data = cLPI, 
             mapping = aes(x = last_year)) + 
  geom_histogram(colour = "black", fill = "grey50") + 
  geom_vline(xintercept = mean(cLPI$last_year), colour = "red") + 
  geom_vline(xintercept = median(cLPI$last_year), colour = "blue") + 
  labs(title = "Last year of sampling", x = NULL)

## timespan
p3 <- ggplot(data = cLPI, 
             mapping = aes(x = timespan)) + 
  geom_histogram(colour = "black", fill = "grey50") + 
  geom_vline(xintercept = mean(cLPI$timespan), colour = "red") + 
  geom_vline(xintercept = median(cLPI$timespan), colour = "blue") + 
  labs(title = "Timespan (last - first + 1)", x = NULL)

## coverage
p4 <- ggplot(data = cLPI, 
             mapping = aes(x = coverage)) + 
  geom_histogram(colour = "black", fill = "grey50") + 
  geom_vline(xintercept = mean(cLPI$coverage), colour = "red") + 
  geom_vline(xintercept = median(cLPI$coverage), colour = "blue") + 
  labs(title = "Coverage (sampled years / timespan)", x = NULL)

(p1 + p2) / (p3 + p4)
rm(p1, p2, p3, p4)


### histograms facetted by Class and System ###
## first year of sampling
p1.2 <- ggplot(data = cLPI, 
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
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

## last year of sampling
p2.2 <- ggplot(data = cLPI, 
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
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

## timespan
p3.2 <- ggplot(data = cLPI, 
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
  labs(title = "Timespan (last - first + 1)", x = NULL)

## coverage
p4.2 <- ggplot(data = cLPI, 
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
  labs(title = "Coverage (sampled years / timespan)", 
       x = NULL) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

(p1.2 + p2.2) / (p3.2 + p4.2)
rm(p1.2, p2.2, p3.2, p4.2)


# single time series exploration ------------------------------------------

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

## NOTE: lambda is calculated as Nt+1 / N(t), which is 
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


# find the 'ideal sub-sample' ---------------------------------------------

## How many time series have complete coverage for at least 5 years?
cLPI %>% 
  filter(coverage == 1, timespan >= 5) %>% 
  # n_distinct("ID") %>% ## [1] 1421
  ggplot(data = ., 
         mapping = aes(x = timespan)) + 
  geom_histogram()

## what is the composition of that subset?
(sub_sum <- cLPI %>% 
  filter(coverage == 1, timespan >= 5) %>% 
  group_by(Class, System) %>% 
  summarise(n_sub = n_distinct(ID), 
            avg_span_sub = mean(timespan)) %>% 
  mutate(prop_samp = n_sub / 1421))

## how does this composition compare to that of the whole dataset?
(cLPI_sum <- cLPI %>% 
  group_by(Class, System) %>% 
  summarise(n_all = n_distinct(ID), 
            avg_span_all = mean(timespan)) %>% 
  mutate(prop_all = n_all / n_distinct(cLPI$ID)))

## combine subset and overall summary into one table
left_join(sub_sum, cLPI_sum, 
          by = c("Class", "System")) %>% 
  mutate(across(.cols = n_sub:prop_all, ~ round(., digits = 2))) %>% 
  knitr::kable()

## COMMENT:
## based on a comparison of the proportions of time series for each
## System * Class, it seems like, relative to the cLPI dataset, the 
## sub-sampled data (100% and 5+ years):
## 1. over-represents TERRESTRIAL BIRDS (19% of sub vs. 7% of full)
## 2. under-represents MARINE FISH (49% of sub vs. 64% of full)

## let's try a slightly less restrictive filter...

## if the time series is more than 10 years, relax the coverage 
## requirement to 80%, and 75% for those of 20 or more years
cLPI %>% 
  filter(timespan >= 5) %>% 
  filter(coverage >= 
           case_when(timespan < 10 ~ 1, 
                     timespan >= 10 & timespan < 20 ~ 0.8, 
                     timespan >= 20 ~ 0.75)) %>% 
  # n_distinct("ID") %>% ## [1] 1954
  ggplot(data = ., 
         mapping = aes(x = timespan)) + 
  geom_histogram()

## what is the composition of that subset?
sub2_sum <- cLPI %>% 
  filter(timespan >= 5) %>% 
  filter(coverage >= 
           case_when(timespan < 10 ~ 1, 
                     timespan >= 10 & timespan < 20 ~ 0.8, 
                     timespan >= 20 ~ 0.75)) %>% 
  group_by(Class, System) %>% 
  summarise(n_sub = n_distinct(ID), 
            avg_span_sub = mean(timespan)) %>% 
  mutate(prop_samp = n_sub / 1984)

## combine subset and overall summary into one table
left_join(sub2_sum, cLPI_sum, 
          by = c("Class", "System")) %>% 
  mutate(across(.cols = n_sub:prop_all, ~ round(., digits = 2))) %>% 
  knitr::kable()


