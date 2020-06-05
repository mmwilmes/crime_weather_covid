## ----install packages, eval=FALSE, message=FALSE, warning=FALSE---------------------------------------------------
## # set eval=TRUE if running script for the first time
## if(!require("ggseas")) install.packages("ggseas")
## if(!require("forecast")) install.packages("forecast")
## if(!require("data.table")) install.packages("data.table")
## if(!require("knitr")) install.packages("knitr")
## if(!require("devtools")) install.packages("devtools")
## if(!require("bigrquery")) install.packages("bigrquery")
## if(!require("tsibble")) install.packages("tsibble")
## if(!require("fable")) install.packages("fable")  # forecasting package of the tidyverse family


## ----eval=TRUE, message=FALSE, warning=FALSE----------------------------------------------------------------------
library(bigrquery)
library(ggplot2)
library(tidyr)
library(dplyr)
library(lubridate) # handle date information
library(readr) # read txt file with credentials
library(fable)
library(tsibble)
library(feasts)
library(ggpmisc) # plot R^2 in scatter plot


## ----set up credentials, eval = TRUE------------------------------------------------------------------------------
# Set Google Cloud project ID here
read_creds <- function(){
    project_name <- read_file("./GC_credentials.txt")
    return(project_name)
}


## ----setup crime data request, eval = TRUE, echo = FALSE, message=FALSE, warning=FALSE----------------------------
build_crime_SQL_query <- function(){
  sql <- "SELECT iucr, primary_type, description, date FROM `bigquery-public-data.chicago_crime.crime`"
  return(sql)
}


## ----execute SQL call for crime data------------------------------------------------------------------------------
get_crime_data <- function(){
  billing <- read_creds()
  sql <- build_crime_SQL_query()
  tb <- bq_project_query(billing, sql)
  counts_day <- bq_table_download(tb, max_results = Inf)
  return(counts_day)
}


## ----add colum of ones --> one row is one offense count-----------------------------------------------------------
crime_data_processing <- function(counts_day){
  counts_day$count <- rep(1, nrow(counts_day))
  # format as date (and drop the time part)
  counts_day$date <- as.Date(ymd_hms(counts_day$date))
  # sort by ascending date
  counts_day <- counts_day %>% arrange(date)
  return(counts_day)
}


## ----aggregate counts per day-------------------------------------------------------------------------------------
aggregate_crime_counts_per_day <- function(counts_day){
  # group by day (across offense types)
  counts_day <- tibble(counts_day)

  daily_counts <- counts_day %>%
  group_by(date) %>%
  summarize(counts = sum(count))
  return(daily_counts)
}


## ----aggregate counts per offense and day-------------------------------------------------------------------------
aggregate_crime_counts_per_offence_and_day <- function(counts_day){
  # group by day (across offense types)
  counts_day <- tibble(counts_day)

  daily_counts <- counts_day %>%
  group_by(date, iucr) %>%
  summarize(counts = sum(count))
  return(daily_counts)
}






## ----main function to get total crime data per day----------------------------------------------------------------
get_crime_per_day <- function(){
  counts_day <- get_crime_data()
  counts_day <- crime_data_processing(counts_day)
  daily_counts_per_day <- aggregate_crime_counts_per_day(counts_day)
  daily_counts_counts_per_day <- as_tsibble(daily_counts_per_day, index=date) %>% arrange(date)
  return(daily_counts_per_day)
}


## ----main function to get crime data per offense type and day-----------------------------------------------------
get_crime_counts_per_type_and_day <- function(){
  counts_day <- get_crime_data()
  counts_day <- crime_data_processing(counts_day)
  daily_counts_per_type_day <- aggregate_crime_counts_per_offence_and_day(counts_day)
  daily_counts_per_type_day  <- as_tsibble(daily_counts_per_type_day , index=date, key=iucr) %>% arrange(date)
  return(daily_counts_per_type_day)
}


## ----setup weather request----------------------------------------------------------------------------------------
build_weather_sql <- function(){
sql <- "
#standardsql
SELECT
  -- Create a timestamp from the date components.
  timestamp(concat(year,'-',mo,'-',da)) as date,
  -- Replace numerical null values with actual nulls
  ROUND(AVG(IF (temp=9999.9, null, temp)),2) AS temperature,
  ROUND(AVG(IF (visib=999.9, null, visib)),2) AS visibility,
  ROUND(AVG(IF (wdsp='999.9', null, CAST(wdsp AS Float64))),2) AS wind_speed,
  ROUND(AVG(IF (gust=999.9, null, gust)),2) AS wind_gust,
  ROUND(AVG(IF (prcp=99.99, null, prcp)),3) AS precipitation,
  AVG(IF (sndp=999.9, null, sndp)) AS snow_depth
FROM
  `bigquery-public-data.noaa_gsod.gsod*`
WHERE
   `stn` IN 
  (SELECT `usaf`
    FROM bigquery-public-data.noaa_gsod.stations
    WHERE state='IL' AND (`lat` BETWEEN 41.6 AND 42) AND (`lon` BETWEEN -88 AND -87)
      AND `end` > '20010000'
    ORDER BY `end` ASC) AND CAST(YEAR AS INT64) > 2000
 GROUP BY date;
"
}


## ----execute weather SQL call-------------------------------------------------------------------------------------
# Execute the query and store the result
fetch_weather_data <- function(){
  sql <- build_weather_sql()
  billing <- read_creds()
  tb <- bq_project_query(billing, sql)
  weather <- bq_table_download(tb, max_results = Inf)
  weather <- arrange(weather, weather$date)
  return(weather)
}




## ----transform into tsibble---------------------------------------------------------------------------------------
weather_processing <- function(weather){
  weather$date <- as.Date(weather$date)
  daily_weather <- as_tsibble(weather, index = date, regular = TRUE, interval = day)
  return(daily_weather)
}


## ----main function to get weather data----------------------------------------------------------------------------
get_weather_data <- function(){
  weather <- fetch_weather_data()
  daily_weather <- weather_processing(weather)
  return(daily_weather)
}

