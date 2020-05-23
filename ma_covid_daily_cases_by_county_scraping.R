##### OVERVIEW #####
#THIS ANALYSIS SCRAPES DATA FROM MASSACHUSETTS DEPARTMENT OF PUBLIC HEALTH DAILY COVID-19 REPORTS
#REPORTS ARE AVAILABLE HERE: https://www.mass.gov/info-details/archive-of-covid-19-cases-in-massachusetts
#THIS ANALYSIS SCRAPES TESTING, HOSPITALIZATION AND FATALITY DATA AND EXPORTS IT TO A CSV TO BE USED IN FURTHER APPLICATIONS
#BELOW IS THE RAW R SCRIPT. FOR THE RMARKDOWN FILE, PLEASE SEE XXXXXXXXX

#Step 1: clear workspace, set options, load packages
rm(list=ls())
options(stringsAsFactors = FALSE)
library(dplyr);library(stringr);library(lubridate);library(ggplot2);library(ggthemes);library(pdftools);library(zoo); library(forcats)

#Step 2: prepare the scraping loop by providing all relevant inputs

#provide the root url for MA DPH COVID PDFs
root<- "https://www.mass.gov/doc/covid-19-cases-in-massachusetts-as-of-"

#provide a dates data frame for iteration & daily tracking
dates<-data.frame(month = c(rep("March",16),rep("April",19)),
                  mm = c(rep(3,16),rep(4,19)),
                  day = c(seq(16,31),seq(1,19)))

#provide a blank df to begin binding rows within the for loop
county_all_dates<-data.frame(county = as.character(), 
                             total_cases = as.integer(),
                             date = mdy(),
                             new_cases = as.integer())

#provide a df with blank values for each county to start calculating daily case deltas in the loop
yesterdays_cases<-data.frame(county = c("Barnstable", "Berkshire", "Bristol", "Dukes", "Essex", "Franklin",
                                        "Hampden", "Hampshire", "Middlesex", "Nantucket", "Norfolk", 
                                        "Plymouth", "Suffolk", "Worcester", "Unknown"),
                             cases = c(rep(0,15)))

#for loop iterates through each row in the dates data frame - one iteration for each date & each new pdf
for(row in 1:nrow(dates)){
  #each url is a combination of a root, and different dates. the 3/19 url is the only unconventional one, hence the "if" clause
  url<-paste(root,dates[row,1],"-",dates[row,3],"-2020", ifelse(dates[row,3] == 19 & dates[row,2] == 3,"-x-updated4pm/download", "/download"), sep ="")
  download.file(url, "test", mode = "wb")
  #creates a two-element character vector, with each vector consisting of all text on each page of the pdf
  txt<-pdf_text("test")
  #removes just the text on the first page of the pdf
  result <- pdf_text("test")[1] %>%
    # splits string by newline character
    str_split("\n") %>%
    #makes each newline a row in a one-column data frame
    as.data.frame()  %>%
    #trims each row in data frame
    apply(1, str_trim)
  #now we need to subset our data by just the rows we're going to analyze
  start<-grep("Barnstable", result) #finds index that matches Barnstable
  end<-grep("Unknown", result)[1] #finds first index that matches "Unknown"
  county_raw<-data.frame(x = result[start:end]) #subsets original data frame by county rows
  #uses regular expressions to extract the names of the counties and the total tests
  county<-
    county_raw%>%
    mutate(
      county = as.character(sapply(county_raw, str_extract, "^[A-Z]{1}[\\S]*")),
      total_cases = as.integer(sapply(county_raw, str_extract, "[0-9]+")),
      date = mdy(paste0(dates[row,2], "/", dates[row,3], "/", "2020"))
    ) %>%
    #filters out some of the erroneous rows picked up due to the PDF's formatting
    filter(!county %in% c("Please", NA, "<NA>")) %>%
    select(-x)
  #adds yesterday's case numbers, subtracts from today's to get new cases, updates yesterday's case number and cleans
  county<-left_join(county, yesterdays_cases, by = "county")
  county$new_cases<- county$total_cases - county$cases
  yesterdays_cases<-county[,c("county", "total_cases")]
  colnames(yesterdays_cases)<-c("county", "cases")
  county<-select(county, -cases)
  county_all_dates[is.na(county_all_dates)] <-0
  #adds iterable date's data to larger set
  county_all_dates<-rbind(county_all_dates, county)
}

#on April 11th, the State converted its report format to a longer "dashboard" with a landscape
#formatting and more data visualizations. On May 18th the State added a cover page to the dashboard
#so now we need to create a second loop for all of the new dashboard-formatted reports
#and we need to have a conditional statement that pulls out the right vector element depending
#on if the pdf is from before or after May 18th

db_root<-"https://www.mass.gov/doc/covid-19-dashboard-"
db_dates<-data.frame(month = c(rep("april",11), rep("may", 22)),
                     mm = c(rep(4,11), rep(5, 22)),
                     day = c(seq(20,30), seq(1,22)))

db_county_all_dates<-data.frame(county = as.character(), 
                                total_cases = as.integer(),
                                date = mdy(),
                                new_cases = as.integer())
for(row in 1:nrow(db_dates)){
  db_url<-paste(db_root,db_dates[row,1],"-",db_dates[row,3],"-2020/download", sep ="")
  
  download.file(db_url, "test", mode = "wb")
  
  #identify the relevant page based on v2 or v3 of the pdf dashboards
  db_result <- ifelse(row<29,
                      pdf_text("test")[5],
                      pdf_text("test")[6])%>%
    # split first page of pdf by newline character
    str_split("\n") %>%
    as.data.frame()  %>%
    #makes each newline a row in a one-column data frame
    apply(1, str_trim)
  
  db_start<-grep("Middlesex", db_result)[1] #finds index that matches Barnstable
  db_end<-grep("Nantucket", db_result)[1] #finds first index that matches "Unknown"
  db_county_raw<-data.frame(x = db_result[db_start:db_end])
  
  db_county<-
    db_county_raw%>%
    mutate(
      county = as.character(sapply(db_county_raw, str_extract, "^[A-Z]{1}[\\S]*")),
      total_cases = sapply(db_county_raw, str_extract, "[\\d]?[\\d]?[,]?[\\d]+"),
      date = mdy(paste0(db_dates[row,2], "/", db_dates[row,3], "/", "2020"))) %>%
    filter(!county %in% c(NA, "Rate", "M", "Date")) %>%
    select(-x)
  
  db_county$total_cases<-as.integer(str_remove(db_county$total_cases, fixed(",")))
  
  db_county<-left_join(db_county, yesterdays_cases, by = "county")
  db_county$new_cases<- db_county$total_cases - db_county$cases
  yesterdays_cases<-db_county[,c("county", "total_cases")]
  colnames(yesterdays_cases)<-c("county", "cases")
  db_county<-select(db_county, -cases)
  
  #adds iterable dates data to larger set
  county_all_dates<-rbind(county_all_dates, db_county)
}

#replaces all of the NAs in the last column, representing new daily tests
county_all_dates[is.na(county_all_dates)] <-0
county_all_dates <- filter(county_all_dates,county!="A")

write.csv(county_all_dates, "daily_ma_covid_cases_by_county.csv", row.names = F)
