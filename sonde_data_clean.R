#### Consolidating sonde data

library(tidyverse)
library(readxl)
library(lubridate)


#### recursive search for .csv under top level directory

# top level directory: 
top_dir<-"C:/Users/emorrison/Dropbox (UFL)/Tampa/PineyPoint/PineyPoint_RAPID_shared/Data/Sonde data/April2021_to_May2022"

file_list<-list.files(path=top_dir, recursive = TRUE, pattern=("*.csv"))

#### check file name for consistent site name
#### 2021-04-16_2021-04-29_PineyPoint_sonde1_StJosephSound_YSI4a_20A102230_UF_Norby

#loop over list of file names

firstfile<-TRUE

for (filenamedir in file_list){
  print("----------------------------------------------")
  print(paste0("    ", filenamedir))
  
  #extract the file name for each .csv
  file_name<-str_split(string = filenamedir, pattern="/")[[1]][2]
  ##take first element of list [[1]], second string [2]
  
  #extract the site name from file name
  # site name is 5th block with _ delimiter
  site_name<-str_split(string=file_name, pattern="_")[[1]][5]
  
  print(paste0("               ", site_name))
  
  filedata<-tibble(read_csv(paste0(top_dir, "/", filenamedir), skip=8, local = locale(encoding = "latin1"), show_col_types = FALSE)) %>% #remove top 8 rows
    rename("Project" = "Site Name") %>%  #Project ID included as site name in original file
    select(-'Time (Fract. Sec)') %>% 
    mutate("Site_ID" = site_name,
           "Date_ymd" = as_date(mdy(`Date (MM/DD/YYYY)`)), 
           "Time_hms" = hms(`Time (HH:mm:ss)`))
  
  if(firstfile==TRUE){
    agglomerateddata <- filedata
    firstfile <- FALSE
  }
  else{
    agglomerateddata<-bind_rows(agglomerateddata, filedata)
  }
  
  print(nrow(agglomerateddata))
  
  print(paste0(" ======================== ", site_name, "     done"))
  
}

## file 2022-02-17_2022-03-10_PineyPoint_sonde4_JoeBay_YSI3b_20D103218_UF_Norby
## had issues with extra headers throughout the file with no explanation
## data looked ok at a quick glance

#### issue with 2022-04-05_2022-04-21_PineyPoint_sonde2_CockroachBay_YSI1b_19F000406_UF_Norby
#### there appears to be two sets of data from 


NA_agglomerated<-filter(agglomerateddata, is.na(Date_ymd))
