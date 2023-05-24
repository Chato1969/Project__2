### Objective 2 - Narrowing Down Hot Spots

#Seeing the global map of COVID-19 cases results in the stark realization that some countries are more affected than others. In order to narrow down your studies, create a table using kable from knitr listing the top countries as far as confirmations and deaths (sum values for provinces of the same country into one value and show the country only). 


#extract data of global confirmed COVID cases
confirmed <- 
  read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv", stringsAsFactors=FALSE)

#extract data of global COVID deaths
deaths <- 
  read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv", stringsAsFactors=FALSE)

#variable holding dataframe with necessary columns for confirmed
lastday_confirmed <- 
  select(confirmed, Province.State, Country.Region, 
         "Latest_Day"=tail(names(confirmed),1))

lastday_confirmed <- lastday_confirmed[-c(107 ,245 ,286),]

#variable holding dataframe with necessary columns for deaths
lastday_deaths <-
  select(deaths, Province.State, Country.Region, 
         "Latest_Day"=tail(names(deaths),1))

#remove unnecesary data points like Olympics and cruise ship.
lastday_deaths <- lastday_deaths[-c(107 ,245 ,286),]

#Make dataframe for confirmations data. Group by country, sum, add commas to numbers.
COVID_confirmed <- lastday_confirmed %>%
  group_by(Country.Region) %>%
  summarize(sum(Latest_Day)) %>%
  rename("Counts"="sum(Latest_Day)", "Country"="Country.Region") %>%
  arrange(desc(Counts))

COVID_confirmed$Counts <- prettyNum(COVID_confirmed$Counts, big.mark = ',', scientific=FALSE)

#Make dataframe for deaths data. Group by country, sum, arrange in desc order, add commas to numbers.
COVID_deaths <- lastday_deaths %>%
  group_by(Country.Region) %>%
  summarize(sum(Latest_Day)) %>%
  rename("Counts"="sum(Latest_Day)", "Country"="Country.Region") %>%
  arrange(desc(Counts))

COVID_deaths$Counts <- prettyNum(COVID_deaths$Counts, big.mark = ',', scientific=FALSE)

#Add rank numbers to table, and order columns.
COVID_confirmed <- COVID_confirmed %>% mutate(Rank = row_number()) 

col_order <- c(3,1,2)
COVID_confirmed <- COVID_confirmed[, col_order]

#combine confirmations and deaths tables.
COVID_Table <- cbind(COVID_confirmed, COVID_deaths)

#Format table
COVID_Table %>%
  kbl(align=c(rep("r",1), rep("l",1), rep("r",1), rep("l",1), rep("r",1)), 
      caption = "Table of Top Countries") %>%
  kable_styling(bootstrap_options = "striped", "responsive",
                full_width = FALSE) %>%
  add_header_above(c(" " = 1, "Confirmations" = 2, "Deaths" = 2), font_size="large") %>%
  scroll_box(width = "800px", height = "550px")
