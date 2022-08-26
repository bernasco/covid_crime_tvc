# covid_crime_tvc.R
# Wim Bernasco
# August 18, 2022

#
#  1. Load packages ----------------------------------------------------------------
library(tidyverse)   # data manipulation
library(cbsodataR)   # access CBS data (cbs_get_data() function)
library(here)        # file/folder localization
library(lubridate)   # dates and times
#library(cowplot)     # arrange plots
library(tsibble)     # tidy time series
library(fable)       # forecasting for tidy time series
library(forecast)    # time series forecasting
library(feasts)      # fable extension
library(forcats)     # factor utilities (not a 'forecast' typo)!

#  2. Set up folder structure ------------------------------------------------------
if (dir.exists(here::here("data")) == FALSE) {
  dir.create(here::here("data"))
}
if (dir.exists(here::here("output")) == FALSE) {
  dir.create(here::here("output"))
}
if (dir.exists(here::here("objects")) == FALSE) {
  dir.create(here::here("objects"))
}

#  3. Define functions and constants -----------------------------------------------

# Filenames
nl_crime_filename      <- here("data", "NL_Crime.csv")
nl_population_filename <- here("data", "NL_Population.csv")

# Colors
color_burglary <- "blue"
color_crime    <- "grey"

# store ggplot figure as PNG file
ggsave_png <- function(ggp, output, ...) {
  ggsave(filename = paste(substitute(ggp),".png", sep=""), 
         device = "png", plot = ggp, path = output, 
         limitsize = TRUE, ...)
}

#  4. Read crime and population data -------------------------------------

# Download or read NL-level crime frequencies of all years 
if (file.exists(nl_crime_filename) == FALSE) {
  nl_allcrime_allyears <-
    cbs_get_data(
      # this can be found with cbs_get_datasets(catalog="Politie")
      id = "47022NED", catalog = "Politie",
      base_url = "https://dataderden.cbs.nl",
      dir = here("data"),
      # only national level frequencies are needed
      WijkenEnBuurten = has_substring("NL")
    ) |>
    # create a date-class for time
    cbs_add_date_column() |>
    select(crime_type_code  = SoortMisdrijf,
           date             = Perioden_Date,
           frequency        = GeregistreerdeMisdrijven_1) |>
    # replace NA with 0
    mutate(frequency = replace_na(frequency, 0),
           # remove trailing whitespace
           crime_type_code  = trimws(crime_type_code)) |>
    group_by(crime_type_code) |>
    mutate(total_crime_count = sum(frequency)) |>
    ungroup()
  write_csv(nl_allcrime_allyears, nl_crime_filename)
  # Read local copy if it exists
} else {
  nl_allcrime_allyears <- read_csv(nl_crime_filename,
                                   show_col_types = FALSE)
}


# Read crime type codes and their labels 
crime_type_cat <- read_csv(here("data", "SoortMisdrijf.csv"),
                           show_col_types=FALSE) |>
  select(crime_type_code = Key,
         crime_type_label = Title,
         crime_type_description = Description)



# Download or read NL-level population frequencies of all years 
if (file.exists(nl_population_filename) == FALSE) {
  nl_population_allyears <-
    cbs_get_data(
      # this can be found with cbs_get_datasets(catalog="CBS")
      id = "83474NED", catalog = "CBS",
      dir = here("data")
    ) |>
    # create a date-class for time
    cbs_add_date_column() |>
    # select only monthly (not yearly) records
    filter(Perioden_freq == "M") |>
    select(date             = Perioden_Date,
           population       = BevolkingAanHetBeginVanDePeriode_1) 
  write_csv(nl_population_allyears, nl_population_filename)
  # Read local copy if it exists
} else {
  nl_population_allyears <- read_csv(nl_population_filename,
                                   show_col_types = FALSE)
}


# merge crime, crime categories and population data together
nl_allcrime_allyears_clean <- 
  # merge with crime type category labels
  left_join(nl_allcrime_allyears, 
            crime_type_cat, 
            by = "crime_type_code") |>
  # merge with population data
  left_join(nl_population_allyears, by = "date")

#  5. Define time series and COVID period -------------------------------------

# most recent date in data
last_date <-
  yearmonth(nl_allcrime_allyears_clean 
          |> summarize(max = max(date)) 
          |> pull())
first_date <-
  yearmonth(nl_allcrime_allyears_clean 
            |> summarize(min = min(date)) 
            |> pull())

covid_start_date <-
  yearmonth(as_date("2020-02-01"))

covid_end_date <-
  yearmonth(as_date("2022-05-01"))

# COVID period
covid_period <- seq(covid_start_date, covid_end_date, 1)

# pre-COVID period
pre_covid_period <- seq(first_date, covid_start_date - 1,1)

# 2012 - 2017
period_2012_2017 <- seq(yearmonth(as_date("2012-01-01")),
                        yearmonth(as_date("2017-12-31")),
                        1)
# 2018-2019
period_2018_2019 <- seq(yearmonth(as_date("2018-01-01")),
                        yearmonth(as_date("2019-12-31")),
                        1)

# measures
intel_lockdown_first_date <- 
  yearmonth(as_date("2020-04-01"))
intel_lockdown_last_date  <- 
  yearmonth(as_date("2020-05-01"))

flexibility_first_date <-
  yearmonth(as_date("2020-06-01"))
flexibility_last_date  <-
  yearmonth(as_date("2020-11-01"))

partial_lockdown_first_date <-
  yearmonth(as_date("2020-12-01"))
partial_lockdown_last_date <-
  yearmonth(as_date("2021-01-01"))

full_lockdown_first_date <-
  yearmonth(as_date("2021-02-01"))
full_lockdown_last_date <- 
  yearmonth(as_date("2020-12-01"))



# Create time series crime

# We will select the top 16 most common crimes 2012-2022
#   excluding "verkeer (weg)"
observed_crime_series <-
  nl_allcrime_allyears_clean |> 
  # exclude totals
  filter(crime_type_label != "Totaal misdrijven") |>
  # exclude traffic accidents
  filter(crime_type_label != "1.3.1 Ongevallen (weg)") |> 
  # corrected for different month lengths
  mutate(rel_frequency = frequency / days_in_month(date) * 
           (365.25/12) / (population / 100000)) |> 
  group_by(crime_type_code) |>
  summarize(frequency = mean(frequency, na.rm=TRUE),
            rel_frequency = mean(rel_frequency, na.rm=TRUE)) |>
  arrange(-rel_frequency) |>
  filter(row_number() < 17) |>
  select(crime_type_code) |>
  left_join(nl_allcrime_allyears_clean, by = "crime_type_code") |>
  mutate(simple_crime_label =
           case_when(crime_type_code == "1.1.1" ~ "Woninginbraak",
                     crime_type_code == "1.1.2" ~ "Inbraak schuur",
                     crime_type_code == "1.2.1" ~ "Diefstal uit motorvoertuig",
                     crime_type_code == "1.2.2" ~ "Diefstal motorvoertuig",
                     crime_type_code == "1.2.3" ~ "Fietsdiefstal",
                     crime_type_code == "1.2.4" ~ "Zakkenrollen",
                     crime_type_code == "1.2.5" ~ "Diefstal uit overig voertuig",
                     crime_type_code == "1.4.4" ~ "Bedreiging",
                     crime_type_code == "1.4.5" ~ "Mishandeling",
                     crime_type_code == "1.4.6" ~ "Straatroof",
                     crime_type_code == "1.6.2" ~ "Overige diefstal",
                     crime_type_code == "2.2.1" ~ "Vernieling",
                     crime_type_code == "2.5.1" ~ "Bedrijfsinbraak",
                     crime_type_code == "2.5.2" ~ "Winkeldiefstal",
                     crime_type_code == "3.5.2" ~ "Rijden onder invloed",
                     crime_type_code == "3.9.1" ~ "Horizontale fraude",
                     crime_type_code == "3.1.1" ~ "Drugshandel"
           )
  ) |>
  mutate(year_month = yearmonth(date, format="%m%Y")) |>
  mutate(
    month = factor(month(date)),
    covid = date %in% covid_period
  ) |>
  # corrected for different month lengths
  mutate(rel_frequency = frequency / days_in_month(date) * 
           (365.25/12) / (population / 100000)) |> 
  # According to the data provider and as suggested by the data
  #  'Horizontale fraude' was not registered before 2016
  filter(!(year_month %in% seq(yearmonth(as_date("2012-01-01")),
                               yearmonth(as_date("2015-12-31")), 1) &
             simple_crime_label == "Horizontale fraude")) |>
  select(-total_crime_count, -population)

observed_crime_series_ts <-
  observed_crime_series |>
  as_tsibble(index = year_month, key=simple_crime_label) |>
  fill_gaps(count = 0)
#  6. Visualize size of crime category frequencies -------------------------------------------------

crime_categories_bar_ggp <-
  observed_crime_series |>
  group_by(simple_crime_label) |>
  summarize(aantal = round(mean(frequency)),0) |>
  mutate(simple_crime_label = 
           fct_reorder(simple_crime_label, aantal)) |>
  ggplot() +
  geom_col(aes(x=aantal, y=simple_crime_label),
           color="black",
           fill = "lightgrey") +
  geom_text(aes(x=aantal, y=simple_crime_label, label=aantal), 
            nudge_x = 300, size=3) +
  xlab("Gemiddeld aantal misdrijven per maand") +
  ylab("Misdaadcategorie")
crime_categories_bar_ggp

ggsave_png(ggp=crime_categories_bar_ggp,
           output = here("output"),
           units = "mm",
           width = 120,
           height = 60,
           scale=2
)

#  7. Visualize population growth ---------------------------------------------
population_growth_ggp <-
  nl_population_allyears |>
  mutate(year_month = yearmonth(date, format="%m%Y")) |>
  filter(date >= as.Date("2012-01-01")) |>
  ggplot() +
  geom_line(aes(x=date, y=population/1000000)) +
  scale_x_yearmonth(
    # name = "Period",
    date_breaks = "1 year",
    date_minor_breaks = "1 month",
    date_labels = "%Y") +
  geom_vline(
    xintercept =
      as.numeric(seq(
        from = as.Date("2012-01-01"),  
        to = as.Date("2022-01-01"), by = "year"
      )),
    linetype = 1,
    color = "lightgrey",
    size = .1,
  ) + 
  theme_minimal() +
  theme(legend.position = "none",
        axis.text.x = element_text(size=6, hjust=0),
        axis.text.y = element_text(size=6, hjust=0)) +
  xlab("Jaar en maand") +
  ylab("Omvang bevolking in miljoen")
population_growth_ggp

ggsave_png(ggp=population_growth_ggp,
           output = here("output"),
           units = "mm",
           width = 120, scale=2)


#  8. Model crime rate before pandemic  ----------------------------------------
model_crime_precovid <- 
  observed_crime_series_ts |>
  # The model is estimated on the period 1.1.2012 - 1.1.2020
  filter(year_month %in% pre_covid_period) |>
  # This procedure selects best fitting model based on AIC, as- 
  #   described by Hyndman and Khandakar (2008)
  model(arima = ARIMA(rel_frequency, ic="aic"))

#  9. Forecast crime during pandemic ------------------------------------------
forecast_crime_covid <-
  model_crime_precovid  |> 
  forecast(h = length(covid_period), bootstrap = TRUE, robust = TRUE)

# 10. Visualize forecasts and observed crime, full period --------------------------

crime_model_longterm_ggp <-
  forecast_crime_covid |>
  hilo(level = c(95,99)) |>
  unpack_hilo(cols= c("95%", "99%")) |>
  right_join(observed_crime_series_ts |> rename(rate=rel_frequency), 
             by = c("simple_crime_label", "year_month")) |> 
  ggplot() +
  facet_wrap(facets = "simple_crime_label", scales = "free_y", ncol=4) +
  geom_ribbon(aes(x=year_month, ymin = `99%_lower`, ymax = `99%_upper`),
              alpha = .1, color="grey") +
  geom_line(aes(x = year_month, y = rate)) +
  geom_line(aes(x=year_month, y = .mean), color = "darkgrey") +
  scale_x_yearmonth(
    # name = "Period",
    date_breaks = "1 year",
#    date_minor_breaks = "1 month",
    date_labels = "%Y") +
  geom_vline(
    xintercept =
      as.numeric(seq(
        from = as.Date("2012-01-01"),
        to = as.Date("2022-01-01"), by = "year"
      )),
    linetype = "dashed",
    color = "grey",
    size = .1,
  ) +
  theme_minimal() +
  theme(legend.position = "bottom",
        axis.text.x = element_text(size=4, hjust=0),
        axis.text.y = element_text(size=6, hjust=0)) +
  xlab("Jaar en maand") +
  ylab("Misdrijven / maand / 100000")# +

crime_model_longterm_ggp

ggsave_png(ggp=crime_model_longterm_ggp, output = here("output"),
           units = "mm", width = 120, height = 60,
           scale=2)

# 11. Visualize forecasts and observed crime during pandemic -------------------------
crime_model_shortterm_ggp <-
  forecast_crime_covid |>
  hilo(level = c(95,99)) |>
  unpack_hilo(cols= c("95%", "99%")) |>
  inner_join(observed_crime_series_ts |> rename(rate=rel_frequency), 
             by = c("simple_crime_label", "year_month")) |> 
  mutate(outside_ci = factor(rate < `99%_lower` | rate > `99%_upper`)) |>  
  ggplot() +
  facet_wrap(facets = "simple_crime_label", scales = "free_y", ncol=4) +
  geom_line(aes(x=year_month, y = .mean), color = "darkgrey") +
  geom_point(aes(x=year_month, y = rate, color = outside_ci)) +
  #   scale_fill_manual(values = c("black", "white")) +
  scale_color_manual(name="",  values =c("grey", "black", "red")) +
  geom_ribbon(aes(x=year_month, ymin = `95%_lower`, ymax = `95%_upper`),
              alpha = .4, color="grey") +
  # geom_ribbon(aes(x=year_month, ymin = `99%_lower`, ymax = `99%_upper`),
  #             alpha = .1, color="grey") +
  geom_line(aes(x = year_month, y = rate)) +
  scale_x_yearmonth(
    # name = "Period",
    date_breaks = "1 year",
    date_minor_breaks = "1 month",
    date_labels = "%Y") +
  geom_vline(
    xintercept =
      as.numeric(seq(
        from = as.Date("2012-01-01"),  
        to = as.Date("2022-01-01"), by = "year"
      )),
    linetype = 1,
    color = "lightgrey",
    size = .1,
  ) + 
  theme_minimal() +
  theme(legend.position = "none",
        axis.text.x = element_text(size=4, hjust=0),
        axis.text.y = element_text(size=6, hjust=0)) +
  xlab("Jaar en maand") +
  ylab("Misdrijven / maand / 100000")

crime_model_shortterm_ggp

ggsave_png(ggp=crime_model_shortterm_ggp,
           output = here("output"),
           units = "mm",
           width = 120,
           height = 60,
           scale=2)


# 12. Visualize pre-covid seasonality pattern in crime ------------------------

# Dutch language abbreviated month labels
month_abb <- c("J", "F", "M", "A", "M", "J",
               "J", "A", "S", "O", "N", "D")
# This plots crime rates per month on a plain coordinate system 
seasonality_linear_crime_precovid_ggp <-
  observed_crime_series_ts |>
  as_tibble() |>
  filter(year_month %in% pre_covid_period) |>  
  group_by(simple_crime_label, month) |> 
  summarize(rate = mean(frequency, na.rm = TRUE), .groups = "drop") |>
  ggplot() +
  facet_wrap(facets = "simple_crime_label", scales = "free_y") +
  geom_point(aes(x=month, y=rate)) +
  geom_line(aes(x=month, y=rate, group=simple_crime_label)) +
  scale_x_discrete(labels = month_abb)+
  xlab("Maand") +
  ylab("Misdrijven / maand / 100000") 
seasonality_linear_crime_precovid_ggp 
ggsave_png(ggp=seasonality_linear_crime_precovid_ggp,
           output = here("output"),
           units = "mm",
           width = 120,
           height = 60,
           scale=2)

# This plots crime rates per month on a polar coordinate system
#   (to make this work, we must standardize by crime type )
seasonality_polar_crime_precovid_ggp <- 
  observed_crime_series_ts |>
  as_tibble() |>
  filter(year_month %in% pre_covid_period) |>  
  group_by(simple_crime_label, month) |> 
  summarize(rate = mean(frequency, na.rm = TRUE), .groups = "drop_last") |>
  mutate(rate = (rate - mean(rate)) / sd(rate)) |>
  ggplot() +
  facet_wrap(facets = "simple_crime_label") +
  geom_point(aes(x=month, y=rate)) +
  geom_line(aes(x=month, y=rate, group=simple_crime_label)) +
  coord_polar() +
  scale_x_discrete(labels = month_abb) +
  xlab("Maand") +
  ylab("Misdrijven / maand / 100000 (gestandaardiseerd)") 
seasonality_polar_crime_precovid_ggp
ggsave_png(ggp=seasonality_polar_crime_precovid_ggp,
           output = here("output"),
           units = "mm",
           width = 120,
           height = 60,
           scale=2)




# Not reported in paper:
# 13. Model crime frequencies 2012-2017 ----------------------------------------
crime_model_2012_2017 <- 
  observed_crime_series_ts |> 
  filter(year_month %in% period_2012_2017) |>
  model(arima = ARIMA(rel_frequency, ic="aic"))
# 14. Forecast crime rates 2018-2019 -------------------------------------------
forecast_crime_2018_2019 <-
  crime_model_2012_2017  |> 
  forecast(h = "24 months", bootstrap=T, robust=T) 

# 15. Visualize forecasts and observed crime, 2012-2019 ------------------------

crime_2012_2019_longterm_ggp <-
  forecast_crime_2018_2019 |> 
  hilo(level = c(95,99)) |>
  unpack_hilo(cols= c("95%", "99%")) |>
  right_join(observed_crime_series_ts |> rename(rate=rel_frequency), 
             by = c("simple_crime_label", "year_month")) |> 
  filter(year_month %in% period_2018_2019) |>
  ggplot() +
  facet_wrap(facets = "simple_crime_label", scales = "free_y", ncol=4) +
  geom_ribbon(aes(x=year_month, ymin = `99%_lower`, ymax = `99%_upper`),
              alpha = .1, color="grey") +
  geom_line(aes(x = year_month, y = rate)) +
  geom_line(aes(x=year_month, y = .mean), color = "darkgrey") +
  scale_x_yearmonth(
    # name = "Period",
    date_breaks = "1 year",
    #    date_minor_breaks = "1 month",
    date_labels = "%Y") +
  geom_vline(
    xintercept =
      as.numeric(seq(
        from = as.Date("2012-01-01"),
        to = as.Date("2022-01-01"), by = "year"
      )),
    linetype = "dashed",
    color = "grey",
    size = .1,
  ) +
  theme_minimal() +
  theme(legend.position = "bottom",
        axis.text.x = element_text(size=4, hjust=0),
        axis.text.y = element_text(size=6, hjust=0)) +
  xlab("Jaar en maand") +
  ylab("Misdrijven / maand / 100000")# +

crime_2012_2019_longterm_ggp

ggsave_png(ggp=crime_2012_2019_longterm_ggp,
           output = here("output"),
           units = "mm",
           width = 120,
           height = 60,
           scale=2)

# 16. Visualize forecasts and observed crime 2018-2019 --------------------
crime_2012_2019_shortterm_ggp <-
  forecast_crime_2018_2019 |> 
  hilo(level = c(95,99)) |>
  unpack_hilo(cols= c("95%", "99%")) |>
  inner_join(observed_crime_series_ts |> rename(rate=rel_frequency), 
             by = c("simple_crime_label", "year_month")) |> 
  mutate(outside_ci = factor(rate < `99%_lower` | rate > `99%_upper`)) |>  
  filter(year_month %in% period_2018_2019) |>
  ggplot() +
  facet_wrap(facets = "simple_crime_label", scales = "free_y", ncol=4) +
  geom_line(aes(x=year_month, y = .mean), color = "darkgrey") +
  geom_point(aes(x=year_month, y = rate, color = outside_ci)) +
  #   scale_fill_manual(values = c("black", "white")) +
  scale_color_manual(name="",  values =c("grey", "black", "red")) +
  #geom_ribbon(aes(x=year_month, ymin = `95%_lower`, ymax = `95%_upper`),
  #            alpha = .4, color="grey") +
  geom_ribbon(aes(x=year_month, ymin = `99%_lower`, ymax = `99%_upper`),
              alpha = .1, color="grey") +
  geom_line(aes(x = year_month, y = rate)) +
  scale_x_yearmonth(
    # name = "Period",
    date_breaks = "1 year",
    date_minor_breaks = "1 month",
    date_labels = "%Y") +
  geom_vline(
    xintercept =
      as.numeric(seq(
        from = as.Date("2012-01-01"),  
        to = as.Date("2022-01-01"), by = "year"
      )),
    linetype = 1,
    color = "lightgrey",
    size = .1,
  ) + 
  theme_minimal() +
  theme(legend.position = "none",
        axis.text.x = element_text(size=4, hjust=0),
        axis.text.y = element_text(size=6, hjust=0)) +
  xlab("Jaar en maand") +
  ylab("Misdrijven / maand / 100000")

crime_2012_2019_shortterm_ggp 

ggsave_png(ggp=crime_2012_2019_shortterm_ggp,
           output = here("output"),
           units = "mm",
           width = 120,
           height = 60,
           scale=2)





# sessioninfo()

# R version 4.2.0 (2022-04-22 ucrt)
# Platform: x86_64-w64-mingw32/x64 (64-bit)
# Running under: Windows 10 x64 (build 19044)
# 
# Matrix products: default
# 
# locale:
# [1] LC_COLLATE=English_United States.utf8  LC_CTYPE=English_United States.utf8   
# [3] LC_MONETARY=English_United States.utf8 LC_NUMERIC=C                          
# [5] LC_TIME=English_United States.utf8    
# 
# attached base packages:
# [1] stats     graphics  grDevices utils     datasets  methods   base     
# 
# other attached packages:
# [1] feasts_0.2.2     forecast_8.16    fable_0.3.1      fabletools_0.3.2 tsibble_1.1.1   
# [6] lubridate_1.8.0  here_1.0.1       cbsodataR_0.5.1  forcats_0.5.1    stringr_1.4.0   
# [11] dplyr_1.0.9      purrr_0.3.4      readr_2.1.2      tidyr_1.2.0      tibble_3.1.7    
# [16] ggplot2_3.3.6    tidyverse_1.3.1 
# 
# loaded via a namespace (and not attached):
# [1] tseries_0.10-51      httr_1.4.3           bit64_4.0.5          vroom_1.5.7         
# [5] jsonlite_1.8.0       modelr_0.1.8         assertthat_0.2.1     distributional_0.3.0
# [9] TTR_0.24.3           cellranger_1.1.0     pillar_1.7.0         backports_1.4.1     
# [13] lattice_0.20-45      glue_1.6.2           quadprog_1.5-8       digest_0.6.29       
# [17] rvest_1.0.2          colorspace_2.0-3     timeDate_3043.102    pkgconfig_2.0.3     
# [21] broom_0.8.0          haven_2.5.0          scales_1.2.0         whisker_0.4         
# [25] tzdb_0.3.0           generics_0.1.2       farver_2.1.0         ellipsis_0.3.2      
# [29] withr_2.5.0          urca_1.3-0           nnet_7.3-17          cli_3.3.0           
# [33] quantmod_0.4.20      magrittr_2.0.3       crayon_1.5.1         readxl_1.4.0        
# [37] fs_1.5.2             fansi_1.0.3          nlme_3.1-157         anytime_0.3.9       
# [41] xts_0.12.1           xml2_1.3.3           progressr_0.10.1     tools_4.2.0         
# [45] hms_1.1.1            lifecycle_1.0.1      munsell_0.5.0        reprex_2.0.1        
# [49] compiler_4.2.0       rlang_1.0.2          grid_4.2.0           rstudioapi_0.13     
# [53] labeling_0.4.2       gtable_0.3.0         fracdiff_1.5-1       DBI_1.1.2           
# [57] curl_4.3.2           R6_2.5.1             zoo_1.8-10           bit_4.0.4           
# [61] utf8_1.2.2           rprojroot_2.0.3      stringi_1.7.6        parallel_4.2.0      
# [65] Rcpp_1.0.8.3         vctrs_0.4.1          dbplyr_2.2.0         tidyselect_1.1.2    
# [69] lmtest_0.9-40  