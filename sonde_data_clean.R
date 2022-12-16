#### Consolidating sonde data

library(tidyverse)
library(readxl)
library(lubridate)
library(cowplot)
library(ggtext)

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


## If there are additional headers, there will be an error
## use this to check what Date_ymd were NA values to troubleshoot

NA_agglomerated<-filter(agglomerateddata, is.na(Date_ymd))


## file 2022-02-17_2022-03-10_PineyPoint_sonde4_JoeBay_YSI3b_20D103218_UF_Norby
## had issues with extra headers throughout the file with no explanation
## data looked ok at a quick glance

#### issue with 2022-04-05_2022-04-21_PineyPoint_sonde2_CockroachBay_YSI1b_19F000406_UF_Norby
#### there appears to be two sets of data from
#### Patrick Norby confirmed on 2/11: there was data from two sets of sondes, he cleaned the file and it was saved into this directory
#### replaced incorrect files with the file he cleaned
#### 12/12: issue has been resolved

### Write csv with agglomerated data
write.csv(agglomerateddata, "SondeOutput/2022_12_12_sonde_data_agglomerated.csv")
### TODO: either need to have a better way to export, or import, right now csv import puts . in filenames

#### after running above code once, import csv
agglomerateddata<-read.csv("SondeOutput/2022_12_12_sonde_data_agglomerated.csv")

#date does not import correctly
agglomerateddata$Date_ymd<-as_date(agglomerateddata$Date_ymd)


#BH has anomalous spike in Oct 2021
#JB very low in March 2022
#SSJS anomalous values in Aug 2022

#filter each variable to the range of the sensor, reported in Exo2 manual

trimmed<-agglomerateddata %>% 
  mutate(`Chlorophyll RFU` = replace(`Chlorophyll RFU`, `Chlorophyll RFU`>100 | `Chlorophyll RFU`<0, NA), 
         `Chlorophyll ug/L` = replace(`Chlorophyll ug/L`, `Chlorophyll ug/L`>400 | `Chlorophyll ug/L`<0, NA),
         `fDOM RFU` = replace(`fDOM RFU`, `fDOM RFU`<0 | `fDOM RFU` >100, NA), 
         `ODO % sat` = replace(`ODO % sat`, `ODO % sat`<0  | `ODO % sat`>500, NA),
         `ODO mg/L` = replace(`ODO mg/L`, `ODO mg/L`<0  | `ODO mg/L`>50, NA),
         `SpCond µS/cm` = replace(`SpCond µS/cm`, `SpCond µS/cm`<0  | `SpCond µS/cm`>200, NA),                      
         `BGA PE RFU` = replace(`BGA PE RFU`, `BGA PE RFU`<0  | `BGA PE RFU`>100, NA),
         `BGA PE ug/L` = replace(`BGA PE ug/L`, `BGA PE ug/L`<0  | `BGA PE ug/L`>280, NA),                   
         `Turbidity FNU` = replace(`Turbidity FNU`, `Turbidity FNU`<0  | `Turbidity FNU`>4000, NA), 
         `Temp °C` = replace(`Temp °C`, `Temp °C`<5  | `Temp °C`>50, NA),
         `pH` = replace(`pH`, `pH`<0  | `pH`>14, NA),
         `ORP mV` = replace(`ORP mV`, `ORP mV`<-999  | `ORP mV`>999, NA))


########### TO DO: repeat this approach with all sensors

fDOM<-ggplot(agglomerateddata, aes(`Date_ymd`, `fDOM RFU`))+
  geom_point()+
  geom_smooth(se=FALSE)+
  facet_wrap(.~Site_ID, scales="free_y")+
  theme_cowplot(font_size = 8)+
  panel_border(color="black")+
  geom_vline(xintercept = as.Date("2021-07-05"), linetype=4, color="black")+
  geom_vline(xintercept = as.Date("2021-04-07"), linetype=4, color="red")+
  theme(text = element_text(size = 14), 
        axis.text = element_text(size = 9), 
        axis.text.x = element_text(angle = -45, hjust = 0.1, vjust = 0.1)
  ) 


#### generate summary statistics for all columns
summary_all_data<-trimmed %>% 
  group_by(Site_ID, Date_ymd) %>% 
  summarise_all(list(min=min, max=max, mean=mean, sd=sd), na.rm=TRUE)

write.csv(summary_all_data, "SondeOutput/2022_12_14_sonde_data_agglomerated_summarized.csv")

#generate test plots
fDOM<-ggplot(summary_all_data, aes(`Date_ymd`, `fDOM RFU_mean`))+
  geom_point()+
  geom_smooth(se=FALSE)+
  facet_wrap(.~Site_ID, scales="free_y")+
  theme_cowplot(font_size = 8)+
  panel_border(color="black")+
  geom_vline(xintercept = as.Date("2021-07-05"), linetype=4, color="black")+
  geom_vline(xintercept = as.Date("2021-04-07"), linetype=4, color="red")+
  theme(text = element_text(size = 14), 
        axis.text = element_text(size = 9), 
        axis.text.x = element_text(angle = -45, hjust = 0.1, vjust = 0.1)
  ) 



ODO_mg_L<-ggplot(summary_all_data, aes(`Date_ymd`, `ODO mg/L_mean`))+
  geom_point()+
  geom_smooth(se=FALSE)+
  facet_wrap(.~Site_ID, scales="free_y")+
  theme_cowplot(font_size = 8)+
  panel_border(color="black")+
  geom_vline(xintercept = as.Date("2021-07-05"), linetype=4, color="black")+
  geom_vline(xintercept = as.Date("2021-04-07"), linetype=4, color="red")+
  theme(text = element_text(size = 14), 
        axis.text = element_text(size = 9), 
        axis.text.x = element_text(angle = -45, hjust = 0.1, vjust = 0.1)
  ) 

chl_RFU<-ggplot(summary_all_data, aes(`Date_ymd`, `Chlorophyll RFU_mean`))+
  geom_point()+
  geom_smooth(se=FALSE)+
  facet_wrap(.~Site_ID, scales="free_y")+
  theme_cowplot(font_size = 8)+
  geom_vline(xintercept = as.Date("2021-07-05"), linetype=4, color="black")+
  geom_vline(xintercept = as.Date("2021-04-07"), linetype=4, color="red")+
  panel_border(color="black")

PE_RFU<-ggplot(summary_all_data, aes(`Date_ymd`, `BGA PE RFU_mean`))+
  geom_point()+
  geom_smooth(se=FALSE)+
  facet_wrap(.~Site_ID, scales="free_y")+
  theme_cowplot(font_size = 8)+
  panel_border(color="black")+
  geom_vline(xintercept = as.Date("2021-07-05"), linetype=4, color="black")+
  geom_vline(xintercept = as.Date("2021-04-07"), linetype=4, color="red")+
  theme(text = element_text(size = 14), 
        axis.text = element_text(size = 9), 
        axis.text.x = element_text(angle = -45, hjust = 0.1, vjust = 0.1)
  ) 
