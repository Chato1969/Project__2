## Objective 3 - Zooming into our State
# After reading the top tables, you are stunned! The US overtakes every other
# country in terms of COVID-19 confirmations. As such, you are concerned about
# the state you live in and would like to understand how COVID-19 events have
# shaped the trajectory of the disease. Create two scatter plots to gain a
# better understanding. The first scatter plot should be California’s trajectory
# for confirmations. The second scatter plot should show California’s top three
# city trajectories for confirmations. You are interested in studying how the
# vaccine affected the number of confirmations. The Moderna vaccine was first
# available as an emergency use authorized (EUA) vaccine and required two shots
# spaced six weeks apart. Indicate on the plots the day the second dosage was
# given to those that received the first dosage the day Moderna was EUA 
# (January 29th, 2021). As a diligent scientist that knows that new COVID
# variants have mutations in the spike protein (the region that the vaccine was
# developed for), you also want to study how confirmation rates change as new 
# variants become the dominant infectious strain. Indicate on the plots when 
# the delta and omicron variants became the dominant strain in California
# (May 11th, 2021 and November 26th, 2021 respectively). In the example below,
# the function plot_grid from the R package cowplot was to organize the graphs 
# into a grid to more easily compare statewide vs top city plots.

library(knitr)
library(magrittr)
library(RCurl)
library(dplyr)
library(kableExtra)
library(ggplot2)
library(tidyr)
library(stringr)


confirmed_US_download <- getURL("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv")
confirmed_US <- read.csv(text=USconfirmed_download, stringsAsFactors = FALSE)

deaths_US_download <- getURL("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_US.csv")
deaths_US <- read.csv(text=USdeath_download, stringsAsFactors = FALSE)


# creates new data frame for only CA confirmed cases
dfConfirmedCAChartData <- confirmed_US %>%
  
  # removes all columns with numeric values that aren't case counts
  select(-(`UID`)) %>%
  select(-(`code3`)) %>%
  select(-(`FIPS`)) %>%
  select(-(`Lat`)) %>%
  select(-(`Long_`)) %>%
  
  # retains only the California row
  filter(`Province_State` == "California") %>%
  
  # groups by State
  group_by(`Province_State`) %>%
  
  # totals all columns that are numeric
  summarise_if(is.numeric, sum) %>%
  
  # gathers data into long form
  gather(key=Date, value=`Confirmed Cases`, -`Province_State`) %>%
  
  # converts the date column into real dates for the chart
  mutate(`Date`=as.Date(`Date`, format = "%m/%d/%y"))


# creates new data frame for only CA deaths
dfDeathsCAChartData <- deaths_US %>%
  
  # removes all columns with numeric values that aren't case counts
  select(-(`UID`)) %>%
  select(-(`code3`)) %>%
  select(-(`FIPS`)) %>%
  select(-(`Lat`)) %>%
  select(-(`Long_`)) %>%
  select(-(`Population`)) %>%
  
  # retains only the California row
  filter(`Province_State` == "California") %>%
  
  # groups by State
  group_by(`Province_State`) %>%
  
  # totals all columns that are numeric
  summarise_if(is.numeric, sum) %>%
  
  # gathers data into long form
  gather(key=Date, value=`Deaths`, -`Province_State`) %>%
  
  # converts the date column into real dates for the chart
  mutate(`Date`=as.Date(`Date`, format = "%m/%d/%y"))


#Below is a chart showing the deaths and confirmed cases for the state of California over time. The black vertical line provides a visual representation of when the first shutdowns occurred on March 19, 2020.


obj4chart1colors <- c("Confirmed Cases" = "blue", "Deaths" = "red")

# these are two disparate time series glued together
ggplot() +
  geom_line(data = dfConfirmedCAChartData,
            aes(x = `Date`, y = `Confirmed Cases`,
                color = "Confirmed Cases")) +
  geom_line(data = dfDeathsCAChartData,
            aes(x = `Date`, y = `Deaths`,
                color = "Deaths")) +
  scale_y_continuous(labels = scales::comma) +
  labs(x = "Date", y = "Cases", color = "California Total") +
  ggtitle("California's Trajectory for COVID-19") +
  scale_color_manual(values = obj4chart1colors) +
  geom_vline(xintercept = as.numeric(as.Date("2020-03-19")),
             color = "black")



# reuses earlier data frame for only CA confirmed cases by county
dfConfirmedCAChartData <- confirmed_US %>%
  
  # removes all columns with numeric values that aren't case counts
  select(-(`UID`)) %>%
  select(-(`code3`)) %>%
  select(-(`FIPS`)) %>%
  select(-(`Lat`)) %>%
  select(-(`Long_`)) %>%
  
  # retains only the California rows
  filter(`Province_State` == "California") %>%
  
  # retains only the rows for San Diego, Riverside, LA
  filter(`Admin2` %in%
           c("San Diego", "Riverside", "Los Angeles")) %>%
  
  # groups by County (aka Admin2)
  group_by(`Admin2`) %>%
  
  # totals all columns that are numeric
  summarise_if(is.numeric, sum) %>%
  
  # gathers data into long form
  gather(key=Date, value=`Confirmed Cases`, -`Admin2`) %>%
  
  # converts the date column into real dates for the chart
  mutate(`Date`=as.Date(`Date`, format = "%m/%d/%y"))


#Below is a chart showing the confirmed cases over time, only for the counties of Los Angeles, Riverside, and San Diego. The black vertical line provides a visual representation of when the first shutdowns occurred on March 19, 2020.


ggplot(dfConfirmedCAChartData,
       aes(x = `Date`, y = `Confirmed Cases`,
           group = `Admin2`)) +
  geom_line(aes(color=`Admin2`)) +
  ggtitle("Southern California Confirmed Cases of COVID-19") +
  scale_y_continuous(labels = scales::comma) +
  labs(x = "Date", y = "Cases", color = "Counties") +
  geom_vline(xintercept = as.numeric(as.Date("2020-03-19")),
             color = "black")

