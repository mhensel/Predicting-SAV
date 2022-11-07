#Projected Climate Change Data from CBP Modelling Group, turning it into the Multiverse
library(tidyverse); library(vroom); library(lubridate); library(naniar)

#This is where we build the datasets that go into the projection for loop. After detrending and smoothing, the output of CC.wl_AllFut is in the MultiversalFutures folder as CC.wlAllFutsoodetre

#Can skip down to line 251, where the daily matchups are, bc the _2021_2060 data wont change and is in the Rdrive

#####Daves read in code####
wif.df <- data.table()
file_list <- list.files(path="/Volumes/savshare2/Current Projects/Predicting-SAV/data/climate modelling data/WIP25_WithLandUseChange_2021_2030", pattern="*csv", full.names=TRUE, recursive=FALSE)

for (i in 1:length(file_list)){
  file <- file_list[i]
  station <- substr(file,148,nchar(file)-4)
  temp_data <- read.table(file, header=TRUE) # load file
  temp_data$Station = station
  wif.df <- rbindlist(list(wif.df, temp_data),use.names = TRUE) }
  

#Codes for this shit: WIP25_LUC_startyear_endyear

wif.df <- data.table()

file_list <- list.files(path="/Volumes/savshare2/Current Projects/Predicting-SAV/data/climate modelling data/WIP25_WithLandUseChange_2021_2030", pattern="*csv", full.names=TRUE, recursive=FALSE)

for (i in 1:length(file_list)){
  file <- file_list[i]
  station <- substr(file,148,nchar(file)-4)
  temp_data <- read.table(file, header=TRUE) # load file
  temp_data$Station = station
  wif.df <- rbindlist(list(wif.df, temp_data),use.names = TRUE) }

library(data.table)



wif.df <- data.table()

file_list <- list.files(path="R:/Current Projects/Predicting-SAV/data/climate modelling data/WIP25_WithLandUseChange_2021_2030", pattern="*csv", full.names=TRUE, recursive=FALSE)

for (i in 1:length(file_list)){
  file <- file_list[i]
  filebase = basename(file)
  station <- substr(filebase,35,nchar(file)-4)
  temp_data <- read.table(file, header=TRUE) # load file
  temp_data$Station = station
  wif.df <- rbindlist(list(wif.df, temp_data),use.names = TRUE)
  
}

######
  
#Tidy projected dataframes, Baseline, CC.wl, WIP WIP#####

#Baseline 1991-2000 dataframe####
baseline_all = read.csv("/Volumes/savshare2/Current Projects/Predicting-SAV/data/climate modelling data/Base_Chris.csv")

baseline = baseline_all %>% 
  rename(Year = yyyy, Month = mm, Day = dd, Depth = dpt.m., Temp = Tmp.oC., 
         Sal = Sal_ppt, Secc = Ke.1.m., Chla = Chl_.ug.l., TN = TN___.mg.l., 
         TP = TP___.mg.l.) %>% #rename cols, note that K isnt really Secchi
  select(Year, Month, Station, everything()) %>%
  group_by(Year, Month, Day, Station) %>%
  summarize(across(Depth:TP, mean)) %>% ungroup() #get daily means
  
bascb1.1TN = baseline %>% filter(Station == "CB1.1") #%>% select(Day, Month, Year, TN)

#load in each of the scenarios by the 9 year chunks

#CC With land use change dataframe####
#CC25 = with land use change, 2021-2030
CC25.wland_all = read.csv("/Volumes/savshare2/Current Projects/Predicting-SAV/data/climate modelling data/CC25_WithLandUseChange_2021_2030.csv")

CC25.wland = CC25.wland_all %>% 
  rename(Year = yyyy, Month = mm, Day = dd, Depth = dpt.m., Temp = Tmp.oC., 
         Sal = Sal_ppt, Secc = Ke.1.m., Chla = Chl_.ug.l., TN = TN___.mg.l., TP = TP___.mg.l.) %>%
  select(Year, Month, Station, everything()) %>%
  group_by(Year, Month, Day, Station) %>%
  summarize(across(Depth:TP, mean)) %>%
  mutate(Year = Year + 30) %>% ungroup() 

#CC35 = with land use change, 2031-2040
CC35.wland_all = read.csv("/Volumes/savshare2/Current Projects/Predicting-SAV/data/climate modelling data/CC35_WithLandUseChange_2031_2040.csv")

CC35.wland = CC35.wland_all %>% 
  rename(Year = yyyy, Month = mm, Day = dd, Depth = dpt.m., Temp = Tmp.oC., 
         Sal = Sal_ppt, Secc = Ke.1.m., Chla = Chl_.ug.l., TN = TN___.mg.l., TP = TP___.mg.l.) %>%
  select(Year, Month, Station, everything()) %>%
  group_by(Year, Month, Day, Station) %>%
  summarize(across(Depth:TP, mean)) %>%
  mutate(Year = Year + 40) %>% ungroup() 

#CC45 = with land use change, 2041-2050
CC45.wland_all = read.csv("/Volumes/savshare2/Current Projects/Predicting-SAV/data/climate modelling data/CC45_WithLandUseChange_2041_2050.csv")

CC45.wland = CC45.wland_all %>% 
  rename(Year = yyyy, Month = mm, Day = dd, Depth = dpt.m., Temp = Tmp.oC., 
         Sal = Sal_ppt, Secc = Ke.1.m., Chla = Chl_.ug.l., TN = TN___.mg.l., TP = TP___.mg.l.) %>%
  select(Year, Month, Station, everything()) %>%
  group_by(Year, Month, Day, Station) %>%
  summarize(across(Depth:TP, mean)) %>%
  mutate(Year = Year + 50) %>% ungroup() 

#CC55 = with land use change, 2051-2060
CC55.wland_all = read.csv("/Volumes/savshare2/Current Projects/Predicting-SAV/data/climate modelling data/CC55_WithLandUseChange_2051_2060.csv")

CC55.wland = CC55.wland_all %>% 
  rename(Year = yyyy, Month = mm, Day = dd, Depth = dpt.m., Temp = Tmp.oC., 
         Sal = Sal_ppt, Secc = Ke.1.m., Chla = Chl_.ug.l., TN = TN___.mg.l., TP = TP___.mg.l.) %>%
  select(Year, Month, Station, everything()) %>%
  group_by(Year, Month, Day, Station) %>%
  summarize(across(Depth:TP, mean)) %>%
  mutate(Year = Year + 60) %>% ungroup() 

#CC.wland_2021_2060####
#bind them all toghether
CC.wland_2021_2060 = bind_rows(CC25.wland, CC35.wland) %>%
  bind_rows(CC45.wland) %>% bind_rows(CC55.wland) %>%
  mutate(Date = make_date(year = Year, month = Month, day = Day))

ggplot(data = CC.wland_2021_2060) +
  stat_summary(aes(x = Year, y = Temp), geom = "smooth", fun = max)

CCcb1.1TN = CC.wland_2021_2060 %>% filter(Station == "CB1.1") #%>% select(Day, Month, Year, TN)

#WIP With land use change dataframe####
#WIP25 = with land use change, 2021-2030
WIP25.wland_all = read.csv("/Volumes/savshare2/Current Projects/Predicting-SAV/data/climate modelling data/WIP25_WithLandUseChange_2021_2030.csv")

WIP25.wland = WIP25.wland_all %>% 
  rename(Year = yyyy, Month = mm, Day = dd, Depth = dpt.m., Temp = Tmp.oC., 
         Sal = Sal_ppt, Secc = Ke.1.m., Chla = Chl_.ug.l., TN = TN___.mg.l., TP = TP___.mg.l.) %>%
  select(Year, Month, Station, everything()) %>%
  group_by(Year, Month, Day, Station) %>%
  summarize(across(Depth:TP, mean)) %>%
  mutate(Year = Year + 30) %>% ungroup() 

#WIP35 = with land use change, 2031-2040
WIP35.wland_all = read.csv("/Volumes/savshare2/Current Projects/Predicting-SAV/data/climate modelling data/WIP35_WithLandUseChange_2031_2040.csv")

WIP35.wland = WIP35.wland_all %>% 
  rename(Year = yyyy, Month = mm, Day = dd, Depth = dpt.m., Temp = Tmp.oC., 
         Sal = Sal_ppt, Secc = Ke.1.m., Chla = Chl_.ug.l., TN = TN___.mg.l., TP = TP___.mg.l.) %>%
  select(Year, Month, Station, everything()) %>%
  group_by(Year, Month, Day, Station) %>%
  summarize(across(Depth:TP, mean)) %>%
  mutate(Year = Year + 40) %>% ungroup() 

#WIP45 = with land use change, 2041-2050
WIP45.wland_all = read.csv("/Volumes/savshare2/Current Projects/Predicting-SAV/data/climate modelling data/WIP45_WithLandUseChange_2041_2050.csv")

WIP45.wland = WIP45.wland_all %>% 
  rename(Year = yyyy, Month = mm, Day = dd, Depth = dpt.m., Temp = Tmp.oC., 
         Sal = Sal_ppt, Secc = Ke.1.m., Chla = Chl_.ug.l., TN = TN___.mg.l., TP = TP___.mg.l.) %>%
  select(Year, Month, Station, everything()) %>%
  group_by(Year, Month, Day, Station) %>%
  summarize(across(Depth:TP, mean)) %>%
  mutate(Year = Year + 50) %>% ungroup() 

#WIP55 = with land use change, 2051-2060
WIP55.wland_all = read.csv("/Volumes/savshare2/Current Projects/Predicting-SAV/data/climate modelling data/WIP55_WithLandUseChange_2051_2060.csv")

WIP55.wland = WIP55.wland_all %>% 
  rename(Year = yyyy, Month = mm, Day = dd, Depth = dpt.m., Temp = Tmp.oC., 
         Sal = Sal_ppt, Secc = Ke.1.m., Chla = Chl_.ug.l., TN = TN___.mg.l., TP = TP___.mg.l.) %>%
  select(Year, Month, Station, everything()) %>%
  group_by(Year, Month, Day, Station) %>%
  summarize(across(Depth:TP, mean)) %>%
  mutate(Year = Year + 60) %>% ungroup() 


#bind them all toghether
WIP.wland_2021_2060 = bind_rows(WIP25.wland, WIP35.wland) %>%
  bind_rows(WIP45.wland) %>% bind_rows(WIP55.wland) %>%
  mutate(Date = make_date(year = Year, month = Month, day = Day))

WIPwcb1.1TN = WIP.wland_2021_2060 %>% filter(Station == "CB1.1") #%>% select(Day, Month, Year, TN)

#WIP Without land use change dataframe####
#WIP25 = without land use change, 2021-2030 THIS DOES NOT EXIST!!!!
#WIP25.woland_all = read.csv("/Volumes/savshare2/Current Projects/Predicting-SAV/data/climate modelling data/WIP25_WithoutLandUseChange_2021_2030.csv")

##WIP25.woland = WIP25.woland_all %>% 
#  rename(Year = yyyy, Month = mm, Day = dd, Depth = dpt.m., Temp = Tmp.oC., 
#         Sal = Sal_ppt, Secc = Ke.1.m., Chla = Chl_.ug.l., TN = TN___.mg.l., TP = TP___.mg.l.) %>%
#  select(Year, Month, Station, everything()) %>%
#  group_by(Year, Month, Day, Station) %>%
#  summarize(across(Depth:TP, mean)) %>%
#  mutate(Year = Year + 30) %>% ungroup() 

#WIP35 = with land use change, 2031-2040
WIP35.woland_all = read.csv("/Volumes/savshare2/Current Projects/Predicting-SAV/data/climate modelling data/WIP35_WithoutLandUseChange_2031-2040.csv")

WIP35.woland = WIP35.woland_all %>% 
  rename(Year = yyyy, Month = mm, Day = dd, Depth = dpt.m., Temp = Tmp.oC., 
         Sal = Sal_ppt, Secc = Ke.1.m., Chla = Chl_.ug.l., TN = TN___.mg.l., TP = TP___.mg.l.) %>%
  select(Year, Month, Station, everything()) %>%
  group_by(Year, Month, Day, Station) %>%
  summarize(across(Depth:TP, mean)) %>%
  mutate(Year = Year + 40) %>% ungroup() 

#WIP45 = with land use change, 2041-2050
WIP45.woland_all = read.csv("/Volumes/savshare2/Current Projects/Predicting-SAV/data/climate modelling data/WIP45_WithoutLandUseChange_2041-2050.csv")

WIP45.woland = WIP45.woland_all %>% 
  rename(Year = yyyy, Month = mm, Day = dd, Depth = dpt.m., Temp = Tmp.oC., 
         Sal = Sal_ppt, Secc = Ke.1.m., Chla = Chl_.ug.l., TN = TN___.mg.l., TP = TP___.mg.l.) %>%
  select(Year, Month, Station, everything()) %>%
  group_by(Year, Month, Day, Station) %>%
  summarize(across(Depth:TP, mean)) %>%
  mutate(Year = Year + 50) %>% ungroup() 

#WIP55 = with land use change, 2051-2060
WIP55.woland_all = read.csv("/Volumes/savshare2/Current Projects/Predicting-SAV/data/climate modelling data/WIP55_WithoutLandUseChange_2051-2060.csv")

WIP55.woland = WIP55.woland_all %>% 
  rename(Year = yyyy, Month = mm, Day = dd, Depth = dpt.m., Temp = Tmp.oC., 
         Sal = Sal_ppt, Secc = Ke.1.m., Chla = Chl_.ug.l., TN = TN___.mg.l., TP = TP___.mg.l.) %>%
  select(Year, Month, Station, everything()) %>%
  group_by(Year, Month, Day, Station) %>%
  summarize(across(Depth:TP, mean)) %>%
  mutate(Year = Year + 60) %>% ungroup() 

#bind them all toghether
WIP.woland_2031_2060 = bind_rows(WIP35.woland, WIP45.woland) %>%
  bind_rows(WIP55.woland) %>%
  mutate(Date = make_date(year = Year, month = Month, day = Day))

WIPwocb1.1TN = WIP.woland_2031_2060 %>% filter(Station == "CB1.1") #%>% select(Day, Month, Year, TN)



###DO DAILY MATCHUPS!!!: 1/24/2021####
#Shouldnt need to re-run above. some read.csvs below can load it in

#write_csv(CC.wland_2021_2060, "/Volumes/savshare2/Current Projects/Predicting-SAV/data/climate modelling data/CC.wland_2021_2060.csv")
#write_csv(WIP.wland_2021_2060, "/Volumes/savshare2/Current Projects/Predicting-SAV/data/climate modelling data/WIP.wland_2021_2060.csv")
#write_csv(WIP.woland_2031_2060, "/Volumes/savshare2/Current Projects/Predicting-SAV/data/climate modelling data/WIP.woland_2031_2060.csv")
#write_csv(baseline, "/Volumes/savshare2/Current Projects/Predicting-SAV/data/climate modelling data/baseline.csv")

#START HERE AND READ THIS, about workflow change####
#Major change in the workflow where we dont do the summarizing right away. Follow this workflow now:
#CC.wland_2021_2060 (created already, above) and baseline_Dprep (created below) get daily matched up to create CC.wland_D, which are daily differences in the baseline and the future (i.e., June 11 2028 - June 11 1990 = one possible future scalar. June 11 2040 - June 11 1993 is also a possible scalar). [NOTE: 4/20/22 I cant remember exactly why we do this. Why not just match by month?] This creates CC.wland_ProjPrep which then is CC.wland_summerPP and springPP. Still PP (ProjPrep) because these are still DAILY. Very Important change!
#April 2022 update: Detrending past data and smoothing future data occurs too.
#4/20/22 update: We may need to ditch the DM matchup because we have a new problem where neighboring stations are having data selected from different years. this creates a big averaging problem doesnt it??
#5/4/22 Update: Needed to fix all of the NAs in the CBPall dataset. We also change to pull past data only from 2000-2020. And there is a new nest_by() in the climate projections code so we make sure that neighboring stations arent pulling from different past years! 
#6/30/22 Update: New issues with the output not being variable enough, plus bad predictions. May need to start from scratch.... 
#8/2/22 Update: Moving to using DETREND84, instead of DETREND. checking everything through. Might revert to not using DETREND. SMOO needs to be used though. 

#SAVCommDensWQ_ForPred####
SAVCommDensWQ_ForPred =vroom("/Volumes/savshare2/Current Projects/Predicting-SAV/data/communityDFs/SAVCommDensWQ_ForPredictions.csv")
SAVCommDensWQ_ForPred.with0s = vroom("/Volumes/savshare2/Current Projects/Predicting-SAV/data/communityDFs/SAVCommDensWQ_ForPredictions.with0s.csv")

#CBP.WQ_forPredictions = vroom("/Volumes/savshare2/Current Projects/Predicting-SAV/data/Water Quality/CBP.WQ_forPredictions.csv")

#need a DF that has "y1" data for the start year of 2020. So we are making up a dataset for 2021.
twentyone = SAVCommDensWQ_ForPred %>%  #twentyone should work for all Communities
  filter(year %in% c("2020", "2019", "2018", "2017", "2007")) %>% #2007 bc some of the NAs are bc no data for a while
  select(STATION, year, Temp.summed, Temp.summe, Sal.summax) %>% #im just selecting the ones we need
  group_by(STATION) %>%
  summarize(across(Temp.summed:Sal.summax, ~mean(., na.rm = T))) %>%
  mutate(year = 2021) %>% #change year, bc y being changed to y1
  rename(Temp.sumy1med = Temp.summed, Temp.sumy1me = Temp.summe, Sal.sumy1max = Sal.summax) %>%
  rename(Station = STATION, Year = year) %>%
  select(Station, Year, everything()) %>% 
  #replace_na(list(Temp.sumy1me = 27.04, Temp.sumy1med = 26.34, Sal.sumy1max = 7.940)) %>% #Xhh is messed up
  ungroup() 
#vroom_write(twentyone, "/Volumes/savshare2/Current Projects/Predicting-SAV/data/Water Quality/twentyone.csv")

#Create DETREND Dataset here, ####
#This is assuming that 2021-2060 will just basically be randomly drawn from the past data that we have from CBP 1984-2020. future projections from real CBP Data, randomly drawn as NO CC#
#NOTE: 1984-1987 have NAs that could cause problems. Also, spring 2020 data is incomplete because of covid
#start w raw CBPall (is this risky????)
CBPall = vroom("/Volumes/savshare2/Current Projects/Predicting-SAV/data/Water Quality/CBPall_2020.csv") #%>% 
  #mutate(STATION = recode(STATION, "LE5.5" = "LE5.5-W")) #LE5.5 ceased to exist, and no SAV

FutureYears = as_tibble(seq(from = 2021, to = 2060, by = 1)) %>% rename("Year" = "value")
CBPstations = unique(CBPall$STATION) #we use this now, although we should probably make sure that this is only from the stations that actually have SAV

#Edited workflow to fix incomplete years by station issue####
#NOTE: this is a different median replace method than we did for SAVCommDensWQ_ForPred. Why? I dont really know but I guess it's because the CBPall data matches up with the CC.wl data better that way
#build this DF of empty rows but its the "complete" set of years and months
CBPYears = as_tibble(seq(from = 2000, to = 2020, by = 1)) %>% #Note, not 2019 but 2020
  rename("year" = "value") %>% 
  group_by(year) %>%
  summarise(month = seq(from = 1, to = 12, by = 1)) %>%
  group_by(year, month) %>%
 # summarise(day = seq(from = 1, to = 31, by = 1)) %>%
 # group_by(year, month, day) %>%
  summarize(Station = CBPstations, Temp = NA, Sal = NA, Secc = NA, Chla = NA, TN = NA, TP = NA)

#CBPall_Future created. Years 2000-2020#### 
CBPall_Future = CBPall  %>%
  filter(year > 1999) %>% #Filter out years before 2000. 
  rename("Station" = "STATION") %>% #rename to match w projction data
  select(Station:Chla) %>% #we dont need the .D variables
 # drop_na() %>% #drops 20,000 points #new median method 5/3/22... maybe dont do this? 
  group_by(Station, year) %>% #
  mutate(day = day(date)) %>%
  mutate(DM = format(as.Date(date), "%m-%d")) %>%
  mutate(date = as.Date(date)) %>%
  select(Station, date, DM, year, month, day, TN:Chla) %>%
  select(-TSS)

#DF of every missing month of data for every station. We will replace these with medians
CBPall_missingmo = anti_join(CBPYears, CBPall_Future, by = c("year", "month", "Station"))

CBPall_allmo = full_join(CBPall_Future, CBPall_missingmo) 



#monthly medians for all stations
CBPall_MEDIANS = CBPall_Future %>% 
 # group_by(Station, month) %>% 
 # mutate(date_med = median(as.Date(date))) %>% #janky creation of fake date for DM matchup later
 # mutate(DM_med = format(as.Date(date_med), "%m-%d")) %>%
  group_by(Station, month) %>%
  summarize(across(TN:Chla, ~median(., na.rm = T), .names = "{.col}_med")) %>%
  select(Station, month, TN_med:Chla_med) %>% ungroup() %>% #dude...just stop here??
  full_join(CBPYears) %>% #and now fill in missing months
  group_by(Station) %>% 
  mutate(across(TN_med:Chla_med, ~median(., na.rm = T), .names = "{.col}_Y")) %>% 
  ungroup() %>%
  group_by(Station, month) %>%
  mutate(Temp_med = coalesce(Temp_med, Temp_med_Y), Sal_med = coalesce(Sal_med, Sal_med_Y), Secc_med = coalesce(Secc_med, Secc_med_Y), 
         Chla_med = coalesce(Chla_med, Chla_med_Y), TN_med = coalesce(TN_med, TN_med_Y), TP_med = coalesce(TP_med, TP_med_Y)) %>% #i cant remember why we need this one too? 
  group_by(Station, month) %>%
  mutate(year = seq(from = 2000, to = 2020, by = 1)) %>%
  mutate(day = 20) %>% 
  mutate(date = make_date(year, month, day)) %>%
  mutate(DM = format(as.Date(date), "%m-%d")) %>% 
  group_by(Station, month, DM, year) %>% 
  summarize(across(TN_med:Chla_med, ~mean(., na.rm = T)) ) #not sure why we need this last line either but im moving on

#View(CBPall_MEDIANS %>% group_by(Station) %>% summarize(length(month)))

#View(CBPall_0020 %>% group_by(Station, month) %>% 
#       summarise(across(everything(), ~ sum(is.na(.)))))
#complete CBPall from 2000-2020, monthly median replaced #####

CBPall_0020 = full_join(CBPall_allmo, 
                        CBPall_MEDIANS, by = c("Station", "month", "year")) %>%
#  group_by(Station, month) %>%
  mutate(DM = coalesce(DM.x, DM.y), Temp = coalesce(Temp, Temp_med), Sal = coalesce(Sal, Sal_med), Secc = coalesce(Secc, Secc_med), 
         Chla = coalesce(Chla, Chla_med), TN = coalesce(TN, TN_med), TP = coalesce(TP, TP_med)) %>%
 # ungroup() %>% 
  select(Station, DM, year, month, TN:Chla) %>%
  arrange(Station, year, month, DM)


#vroom_write(CBPall_0020, "/Volumes/savshare2/Current Projects/Predicting-SAV/data/Water Quality/CBPall_0020.csv")

ggplot(data = CBPall_0020 %>% filter(str_detect(Station, "^TF"))) + 
  stat_smooth(aes(x = year, y = TN, group = Station, color = Station), method = "lm") +
  facet_wrap(~Station, scales = "free_y")

#CBPall_F is the yearly mean so that we can get the trend in the means... 

#STOP!!!!!! 
#Do we really need to detrend the data from just the last 20 years? I dont think so! 

CBPall_F = CBPall_0020 %>% #these are yearly means
 # rename("Station" = "STATION") %>% #rename to match w projction data
 # select(Station:Chla) %>% #we dont need the .D variables
  #filter(Station %in% c("EE3.1")) %>% #check a few stations if you want
 # drop_na() %>% #drops 20,000 points
 # group_by(Station, year) %>% #
 # mutate(day = day(date)) %>%
#  mutate(DM = format(as.Date(date), "%m-%d")) %>%
 # select(Station, date, DM, year, month, day, TN:Chla) %>% 
  group_by(Station, year) %>%
  summarize(across(TN:Chla, ~mean(.x, na.rm = T))) %>% 
  drop_na()

#Create CBPall_DETREND####
CBPall_DETREND = CBPall_F %>% 
  filter(between(year,2010,2020)) %>% 
  group_by(Station) %>%
  summarize(across(TN:Chla, ~((mean(.))), na.rm = T, .names = "{.col}end")) %>% #get mean of last decade, call that variable Tempend
  full_join(CBPall_F) %>% #bring that summarized 2010-2020 data back into the DF for nesting
  ungroup() %>%
  nest_by(Station) %>%
  mutate(Tempmod = list(lm(Temp ~ year, data = data)), #a model of Temp over time by year
         Salmod = list(lm(Sal ~ year, data = data)), 
         Seccmod = list(lm(Secc ~ year, data = data)), 
         Chlamod = list(lm(Chla ~ year, data = data)), 
         TNmod = list(lm(TN ~ year, data = data)), 
         TPmod = list(lm(TP ~ year, data = data))) %>% 
  mutate(predTemp = list(predict(Tempmod, data)), #predicted Temp over time by year
         predSal = list(predict(Salmod, data)), 
         predSecc = list(predict(Seccmod, data)), 
         predChla = list(predict(Chlamod, data)), 
         predTN = list(predict(TNmod, data)), 
         predTP = list(predict(TPmod, data))) %>% 
  unnest(cols = c(data, predTemp, predSal, predSecc, predChla, predTN, predTP)) %>% 
  select(-c(Temp, Sal, Secc, Chla, TN, TP, Tempmod, Salmod, Seccmod, Chlamod, TNmod, TPmod)) %>%
  full_join(CBPall_0020, by = c("Station", "year")) %>% #join these predicteds into the full CBPall. This used to be CBPall_Future but inserting the compelte data set now 
  #  filter(str_detect(Station, "^CB4")) %>% 
  mutate(detreTemp = (Temp - predTemp) + Tempend, #Voila! detrended Temp = (Actual Observed Temp - Predicted temp) + Last decades mean Temp
         detreSal = (Sal - predSal) + Salend, 
         detreSecc = (Secc - predSecc) + Seccend, 
         detreChla = (Chla - predChla) + Chlaend, 
         detreTN = (TN - predTN) + TNend, 
         detreTP = (TP - predTP) + TPend) %>% 
  mutate(across(detreSal:detreTP, ~case_when(.x < 0 ~ 0.01, TRUE ~ .x))) %>%  #get rid of neg val detrends
  select(Station, year, month, DM, detreTemp:detreTP)


#Detrend written here####
#the detrend data comes up a bit so lets just write it
#vroom_write(CBPall_DETREND, "/Volumes/savshare2/Current Projects/Predicting-SAV/data/Water Quality/CBPall_DETREND.csv")
#CBPall_DETREND = vroom("/Volumes/savshare2/Current Projects/Predicting-SAV/data/Water Quality/CBPall_DETREND.csv")


##DETREND full dataset 1984-2020####
CBPYears84 = as_tibble(seq(from = 1984, to = 2020, by = 1)) %>% #Note, not 2019 but 2020
  rename("year" = "value") %>% 
  group_by(year) %>%
  summarise(month = seq(from = 1, to = 12, by = 1)) %>%
  group_by(year, month) %>%
  # summarise(day = seq(from = 1, to = 31, by = 1)) %>%
  # group_by(year, month, day) %>%
  summarize(Station = CBPstations, Temp = NA, Sal = NA, Secc = NA, Chla = NA, TN = NA, TP = NA)

CBPall_Future84 = CBPall  %>%
 # filter(year > 1999) %>% Only difference from all_Future
  rename("Station" = "STATION") %>% #rename to match w projction data
  select(Station:Chla) %>% #we dont need the .D variables
  # drop_na() %>% #drops 20,000 points #new median method 5/3/22... maybe dont do this? 
  group_by(Station, year) %>% #
  mutate(day = day(date)) %>%
  mutate(DM = format(as.Date(date), "%m-%d")) %>%
  mutate(date = as.Date(date)) %>%
  select(Station, date, DM, year, month, day, TN:Chla) %>%
  select(-TSS)

#DF of every missing month of data for every station. We will replace these with medians
CBPall_missingmo84 = anti_join(CBPYears84, CBPall_Future84, by = c("year", "month", "Station"))

CBPall_allmo84 = full_join(CBPall_Future84, CBPall_missingmo84) 

#monthly medians for all stations
CBPall_MEDIANS84 = CBPall_Future84 %>% 
  # group_by(Station, month) %>% 
  # mutate(date_med = median(as.Date(date))) %>% #janky creation of fake date for DM matchup later
  # mutate(DM_med = format(as.Date(date_med), "%m-%d")) %>%
  group_by(Station, month) %>%
  summarize(across(TN:Chla, ~median(., na.rm = T), .names = "{.col}_med")) %>%
  select(Station, month, TN_med:Chla_med) %>% ungroup() %>%
  full_join(CBPYears84) %>% #and now fill in missing months
  group_by(Station) %>% 
  mutate(across(TN_med:Chla_med, ~median(., na.rm = T), .names = "{.col}_Y")) %>% 
  ungroup() %>%
  group_by(Station, month) %>%
  mutate(Temp_med = coalesce(Temp_med, Temp_med_Y), Sal_med = coalesce(Sal_med, Sal_med_Y), Secc_med = coalesce(Secc_med, Secc_med_Y), 
         Chla_med = coalesce(Chla_med, Chla_med_Y), TN_med = coalesce(TN_med, TN_med_Y), TP_med = coalesce(TP_med, TP_med_Y)) %>%
  group_by(Station, month) %>%
  mutate(year = seq(from = 1984, to = 2020, by = 1)) %>%
  mutate(day = 20) %>% 
  mutate(date = make_date(year, month, day)) %>%
  mutate(DM = format(as.Date(date), "%m-%d")) %>% 
  group_by(Station, month, DM, year) %>% 
  summarize(across(TN_med:Chla_med, ~mean(., na.rm = T)) )

#View(CBPall_MEDIANS %>% group_by(Station) %>% summarize(length(month)))

#CBPall_8420 median replaced NAs####
#should write this, bc its actually complete for the first time ever
CBPall_8420 = full_join(CBPall_allmo84, 
                        CBPall_MEDIANS84, by = c("Station", "month", "year")) %>%
  #  group_by(Station, month) %>%
  mutate(DM = coalesce(DM.x, DM.y), Temp = coalesce(Temp, Temp_med), Sal = coalesce(Sal, Sal_med), Secc = coalesce(Secc, Secc_med), 
         Chla = coalesce(Chla, Chla_med), TN = coalesce(TN, TN_med), TP = coalesce(TP, TP_med)) %>%
  # ungroup() %>% 
  select(Station, DM, year, month, TN:Chla) %>%
  arrange(Station, year, month, DM)

#vroom_write(CBPall_8420, "/Volumes/savshare2/Current Projects/Predicting-SAV/data/Water Quality/CBPall_8420.csv")

#CBPall_8420 = vroom("/Volumes/savshare2/Current Projects/Predicting-SAV/data/Water Quality/CBPall_8420.csv")

CBPall_F84 = CBPall_8420 %>% #these are yearly means
  group_by(Station, year) %>%
  summarize(across(TN:Chla, ~mean(.x, na.rm = T))) %>% 
  drop_na()

#Create CBPall_DETREND84###
CBPall_DETREND84 = CBPall_F84 %>% 
  filter(between(year,2010,2020)) %>% 
  group_by(Station) %>%
  summarize(across(TN:Chla, ~((mean(.))), na.rm = T, .names = "{.col}end")) %>% #get mean of last decade, call that variable Tempend
  full_join(CBPall_F84) %>% #bring that summarized 2010-2020 data back into the DF for nesting
  ungroup() %>%
  nest_by(Station) %>%
  mutate(Tempmod = list(lm(Temp ~ year, data = data)), #a model of Temp over time by year
         Salmod = list(lm(Sal ~ year, data = data)), 
         Seccmod = list(lm(Secc ~ year, data = data)), 
         Chlamod = list(lm(Chla ~ year, data = data)), 
         TNmod = list(lm(TN ~ year, data = data)), 
         TPmod = list(lm(TP ~ year, data = data))) %>% 
  mutate(predTemp = list(predict(Tempmod, data)), #predicted Temp over time by year
         predSal = list(predict(Salmod, data)), 
         predSecc = list(predict(Seccmod, data)), 
         predChla = list(predict(Chlamod, data)), 
         predTN = list(predict(TNmod, data)), 
         predTP = list(predict(TPmod, data))) %>% 
  unnest(cols = c(data, predTemp, predSal, predSecc, predChla, predTN, predTP)) %>% 
  select(-c(Temp, Sal, Secc, Chla, TN, TP, Tempmod, Salmod, Seccmod, Chlamod, TNmod, TPmod)) %>%
  full_join(CBPall_8420, by = c("Station", "year")) %>% #join these predicteds into the full CBPall. This used to be CBPall_Future but inserting the compelte data set now 
  #  filter(str_detect(Station, "^CB4")) %>% 
  mutate(detreTemp = (Temp - predTemp) + Tempend, #Voila! detrended Temp = (Actual Observed Temp - Predicted temp) + Last decades mean Temp
         detreSal = (Sal - predSal) + Salend, 
         detreSecc = (Secc - predSecc) + Seccend, 
         detreChla = (Chla - predChla) + Chlaend, 
         detreTN = (TN - predTN) + TNend, 
         detreTP = (TP - predTP) + TPend) %>% 
  mutate(across(detreSal:detreTP, ~case_when(.x < 0 ~ 0.01, TRUE ~ .x))) %>%  #get rid of neg val detrends
  select(Station, year, month, DM, detreTemp:detreTP, TN:Chla) #just added these last 2 bc i wanted to see

#Detrend84 written here####
#vroom_write(CBPall_DETREND84, "/Volumes/savshare2/Current Projects/Predicting-SAV/data/Water Quality/CBPall_DETREND84.csv")
#8/2/22 = Using DETREND84 now and seeing how it works
CBPall_DETREND84 = vroom("/Volumes/savshare2/Current Projects/Predicting-SAV/data/Water Quality/CBPall_DETREND84.csv")

#If we usin the 84 data, detrend might be good. but using the 0020 data we realy dont need to do it I dont think. 
#detrevsorig = ####
#Graph to look at each of the Detrends. I dont think we really need to do it!
ggplot(data = CBPall_DETREND84 %>% filter(str_detect(Station, "^TF"))) +
  stat_summary(aes(x = year, y = detreTN), geom = "point", color = "black", fun.data = "mean_se", size = 1) +
  stat_summary(aes(x = year, y = TN), data = CBPall_0020 %>% filter(str_detect(Station, "^TF")), color = "blue", geom = "point", fun.data = "mean_se", size = 1.2) + #compare to all 
  stat_summary(aes(x = year, y = TN), data = CBPall_8420 %>% filter(str_detect(Station, "^TF")), color = "green", geom = "point", fun.data = "mean_se", size = .7) + 
  stat_summary(aes(x = year, y = detreTN), geom = "line", color = "black", fun.data = "mean_se", size = 1) +
  stat_summary(aes(x = year, y = TN), data = CBPall_0020 %>% filter(str_detect(Station, "^TF")), color = "blue", geom = "line", fun.data = "mean_se", size = 1.2) + #compare to all 
  stat_summary(aes(x = year, y = TN), data = CBPall_8420 %>% filter(str_detect(Station, "^TF")), color = "green", geom = "line", fun.data = "mean_se", size = .7) + 
  stat_summary(aes(x = year, y = TN), data = CBPall %>% filter(str_detect(STATION, "^TF")) %>% rename(Station = STATION), shape = 11,color = "darkslategrey", geom = "point", fun.data = "mean_se", size = 1.4) + 
  stat_summary(aes(x = year, y = TN), data = CBPall %>% filter(str_detect(STATION, "^TF")) %>% rename(Station = STATION), color = "darkslategrey", geom = "line", fun.data = "mean_se", size = 1.2) +
  facet_wrap(~Station, scales = "free_y")



ggplot(data = CBPall_DETREND84 %>% filter(str_detect(Station, "^CB"))) +
  stat_summary(aes(x = year, y = detreTemp), geom = "point", color = "green", fun.data = "mean_se") +
  stat_summary(aes(x = year, y = Temp, group = Station), data = CBPall %>% filter(str_detect(STATION, "^CB")) %>% rename(Station = STATION) %>% filter(!year == "1984"), fun.data = "mean_se", color = "black", size = 1.2, geom = "line") +
  stat_summary(aes(x = year, y = Temp), data = CBPall_8420 %>% filter(str_detect(Station, "^CB")), fun.data = "mean_se", color = "blue", geom = "point") + #compare to all 
#stat_summary(data = CBPall_DETREND %>% filter(str_detect(Station, "^CB")), fun.data = "mean_se", aes(x = year, y = detreTemp), geom = "point", color = "orange") +
#  stat_summary(aes(x = year, y = Temp), data = CBPall_0020 %>% filter(str_detect(Station, "^CB")), fun.data = "mean_se", color = "pink", geom = "point") + 
  stat_summary(aes(x = year, y = detreTemp), geom = "line", color = "green", fun.data = "mean_se") +
  stat_summary(aes(x = year, y = Temp), data = CBPall_8420 %>% filter(str_detect(Station, "^CB")), fun.data = "mean_se", color = "blue", geom = "line") + #compare to all 
 # stat_summary(data = CBPall_DETREND %>% filter(str_detect(Station, "^CB")), fun.data = "mean_se", aes(x = year, y = detreTemp), geom = "line", color = "orange") +
 # stat_summary(aes(x = year, y = Temp), data = CBPall_0020 %>% filter(str_detect(Station, "^CB")), fun.data = "mean_se", color = "pink", geom = "line") + 
  facet_wrap(~Station, scales = "free_y")



#the combo of data gaps and drop NAs and random sampling means that we have not the full years of data for the future. (I fixed this eventually)

#View(CBPall_Future %>% group_by(Station) %>% summarize(unique(year)))
#CBPall %>% filter(STATION == "EE3.1" & year == 1997) %>% select(month, Temp)

#Bring in Baseline, CC.wland, WIP.wland####

CC.wland_2021_2060 = vroom("/Volumes/savshare2/Current Projects/Predicting-SAV/data/climate modelling data/CC.wland_2021_2060.csv") %>%
  mutate(Date = make_date(year = Year, month = Month, day = Day))
WIP.wland_2021_2060 = vroom("/Volumes/savshare2/Current Projects/Predicting-SAV/data/climate modelling data/WIP.wland_2021_2060.csv") %>%
  mutate(Date = make_date(year = Year, month = Month, day = Day))
#WIP.woland_2031_2060 = vroom("/Volumes/savshare2/Current Projects/Predicting-SAV/data/climate modelling data/WIP.woland_2031_2060.csv") %>%
#  mutate(Date = make_date(year = Year, month = Month, day = Day))
baseline = vroom("/Volumes/savshare2/Current Projects/Predicting-SAV/data/climate modelling data/baseline.csv") %>%
  mutate(Date = make_date(year = Year, month = Month, day = Day))

#baseline
#CC.wland_2021_2060
#WIP.wland_2021_2060
#WIP.woland_2031_2060

#CC.wland_2021_2060 %>% filter(Station == "LE5.4") %>% filter(Year == 2048) %>% filter(Month == 12) %>% filter(Day == 15) 

#Next, create the Delta dataset for each scenario, Predictions - Baseline####
#Should i do a NoCC too?

#baseline_Dprep####
#Baseline df just repeated 4 total times: 
#should baseline be randomized within decade? i say yes..........
baseline25 = baseline %>% 
  mutate(Year = Year + 30)
baseline35 = baseline %>%
  mutate(Year = Year + 40)
baseline45 = baseline %>%
  mutate(Year = Year + 50)
baseline55 = baseline %>%
  mutate(Year = Year + 60)

baseline_Dprep = bind_rows(baseline25, baseline35, baseline45, baseline55) %>%
  mutate(Date = make_date(year = Year, month = Month, day = Day)) 

#CC.wland_D####
#Calculates the D
#This takes like 15 mins to run, 30 sec for a station
CC.wland_D = CC.wland_2021_2060 %>% #filter(Station %in% c("CB1.1", "LE5.4")) %>% #check a station
  bind_rows(baseline_Dprep %>% add_column(b = "b")) %>% #filter(Station %in% c("CB1.1", "LE5.4")) ) %>%  #b col for baseline check
  select(Date, Station, everything(), -Day, -Month, -Year, -Depth) %>% 
  arrange(Station, Date) %>% #idk if needed
 #group_by(Station, Date) %>% #dont need this, adds literally 15 minutes to this just to output a col of NAs
  mutate(across(Temp:TP, ~.x - lead(.x, order_by = Date), na.rm = F)) %>% #future - baseline = delta from baseline (32 degrees - 28 degrees = -4 delta)
  filter(is.na(b)) %>% #filter out the "b"s, they subtract wrong things
  select(-b) 

vroom_write(CC.wland_D, "/Volumes/savshare2/Current Projects/Predicting-SAV/data/climate modelling data/CC.wland_D.csv")

CC.wland_D = vroom("/Volumes/savshare2/Current Projects/Predicting-SAV/data/climate modelling data/CC.wland_D.csv")

#CC.wl_AllFut SMOOTHED AND DETRENDED####
#create annual means, to help find the annual trends
CC_D.yrme = CC.wland_D %>%
  mutate(day = day(Date), month = month(Date), year = year(Date)) %>%
#  mutate(DM = format(as.Date(Date), "%m-%d")) %>%
  group_by(Station, year) %>%
  summarize(across(Temp:TP, ~mean(.x, na.rm = T))) #mean delta per year per station here

#Smooth: take the final decade and move the trend so it goes from start (2021) to end (2060)
CC.meanyearSMOO = CC_D.yrme %>% 
  filter(between(year,2051,2060)) %>% #final decade only
  mutate(across(Temp:TP, ~((mean(.)/40)), na.rm = T, .names = "{.col}m")) %>% #30 years not 40, calc mean of the final decade
  summarize(across(Tempm:TPm, ~ .*(seq(2021,2060)-2020))) %>% #builds slope of each final decade
  mutate(year = seq(2021,2060)) %>%
  full_join(CC_D.yrme) %>% 
  ungroup() %>%
  nest_by(Station) %>%
  mutate(Tempmod = list(lm(Temp ~ year, data = data)), #model for ENV and time, this will be what we smooth to
         Salmod = list(lm(Sal ~ year, data = data)), 
         Seccmod = list(lm(Secc ~ year, data = data)), 
         Chlamod = list(lm(Chla ~ year, data = data)), 
         TNmod = list(lm(TN ~ year, data = data)), 
         TPmod = list(lm(TP ~ year, data = data))) %>% 
  mutate(predTemp = list(predict(Tempmod, data)), 
         predSal = list(predict(Salmod, data)), 
         predSecc = list(predict(Seccmod, data)), 
         predChla = list(predict(Chlamod, data)), 
         predTN = list(predict(TNmod, data)), 
         predTP = list(predict(TPmod, data))) %>% 
  unnest(cols = c(data, predTemp, predSal, predSecc, predChla, predTN, predTP)) %>% 
  select(-c(Temp, Sal, Secc, Chla, TN, TP, Tempmod, Salmod, Seccmod, Chlamod, TNmod, TPmod)) %>%
  full_join(CC.wland_D %>% 
              mutate(day = day(Date), month = month(Date), year = year(Date)) %>%
              mutate(DM = format(as.Date(Date), "%m-%d")), by = c("Station", "year")) %>% 
  mutate(smoothTemp = Temp - (predTemp - Tempm),  #smoothTemp = the scalar smoothed out! 
         smoothSal = Sal - (predSal - Salm), 
         smoothSecc = Secc - (predSecc - Seccm), 
         smoothChla = Chla - (predChla - Chlam), 
         smoothTN = TN - (predTN - TNm), 
         smoothTP = TP - (predTP - TPm)) %>% 
  select(Station, year, month, smoothTemp:smoothTP) %>% 
  group_by(Station, year, month) %>%
  summarise(across(smoothTemp:smoothTP, ~mean(.)))



#combine Future and Past to get Deltas! SMOO and DETREND84 (formerly _ProjPrep)####
#For
CC.wl_PP = CC.meanyearSMOO %>% #filter(Station == "CB1.1") %>% 
  full_join(CBPall_DETREND84 %>% filter(between(year,1990,2020)), #%>% filter(Station == "CB1.1"),
                                                by = c("Station", "month")) %>% #detre = NoCC data, smooth = delta. So should be .y + .x 
  #CHECK THE RANDOM SELECTION HERE!!!!! dont be confused that this has Temp:TP those are the non detrended! 
  drop_na() %>% #like 5K points per station over time from unmatched dates
  group_by(Station, month) %>%
  mutate(Temp = detreTemp + smoothTemp, #CREATE FUTURE ENV FOR SIMULATIONS HERE!!####
         Sal = detreSal + smoothSal,     #detre(ie detrended past data, e.g. ) + smooth(ie smoothed future scalar) = 
         Secc = detreSecc + smoothSecc,  #dont
         Chla = detreChla + smoothChla, 
         TN = detreTN + smoothTN, 
         TP = detreTP + smoothTP) %>%
  select(Station, month, year.x, year.y, Temp:TP) %>% 
  mutate(across(Temp:TP, ~case_when(.x < 0 ~ 0.01, #get rid of negative values that came about from the delta math
                                    TRUE ~ .x))) %>% 
  ungroup()  #this is just all of the possible things to pull 40 years worth of data from. 

CC.wl_sumPP = CC.wl_PP %>% 
  filter(dplyr::between(month, 5, 8)) %>%
  group_by(Station, year.x, year.y) %>%  #do i need to rowwise instead. no...
  summarize(Chla.summe = mean(Chla, na.rm = T), 
            Chla.summax = max(Chla, na.rm = T),
            Secc.summe = mean(Secc, na.rm = T), 
            Secc.summed = median(Secc, na.rm = T), 
            Sal.summed = median(Sal, na.rm = T), 
            Sal.summax = max(Sal, na.rm = T),
            Sal.summe = mean(Sal, na.rm = T), 
            Temp.summin = min(Temp, na.rm = T), 
            Temp.summax = max(Temp, na.rm = T),
            Temp.summe = mean(Temp, na.rm = T), 
            Temp.summed = median(Temp, na.rm = T),
            TP.summe = mean(TP, na.rm = T), 
            TN.summe = mean(TN, na.rm = T)) %>% 
  ungroup() 
####Summarize Spring 
#Temp.spmed, Temp.spme,Chla.spme, Sal.spme, Secc.spme, TP.spme, TP.spmed, TN.spme
CC.wl_spPP = CC.wl_PP %>% 
  filter(dplyr::between(month, 3, 5)) %>% #late june isnt spring.... 69 vars one has 6 as this month FYI
  group_by(Station, year.x, year.y) %>% 
  summarise(Chla.spme = mean(Chla, na.rm = T), 
            Secc.spme = mean(Secc, na.rm = T), 
            Sal.spme = mean(Sal, na.rm = T), 
            Temp.spme = mean(Temp, na.rm = T), 
            Temp.spmed = median(Temp, na.rm = T),
            TP.spme = mean(TP, na.rm = T), 
            TP.spmed = median(TP, na.rm = T), 
            TN.spme = mean(TN, na.rm = T)) %>% 
  ungroup()


####Create CC.wl_AllFut####
#change this back from ONEBAY if it works.
CC.wlAllFut_ONEBAY = left_join(CC.wl_sumPP, CC.wl_spPP) %>% 
  rename("Year" = "year.x", "Year.ref" = "year.y") #%>% #keeping the year.y because we need the reference years to be the same between stations for each simulation.
  #arrange(Year.ref, Year, Station)

vroom_write(CC.wlAllFut_ONEBAY, "/Volumes/savshare2/Current Projects/Predicting-SAV/data/Multiversal Futures/CC.wlAllFut_ONEBAY.csv")

CC.wland_OneFuture.no2021 = CC.wlAllFut_ONEBAY %>% 
  group_by(Station) %>%
  mutate(Sal.sumy1max = lag(Sal.summax, n = 1, order_by = Year), 
         Temp.sumy1me = lag(Temp.summe, n = 1, order_by = Year), 
         Temp.sumy1med = lag(Temp.summed, n = 1, order_by = Year))
#Filter out 
CC.wland_One2021 = CC.wland_OneFuture.no2021 %>% filter(Year == "2021") %>% 
  select(-Sal.sumy1max, -Temp.sumy1me, -Temp.sumy1med) %>%
  full_join(twentyone) 

#CC.wland_OneFuture, actually all futures ;)####
CC.wland_OneFuture = CC.wland_OneFuture.no2021 %>% #OneFuture is a nisnomer... this is all futures
  filter(!Year == "2021") %>%
  bind_rows(CC.wland_One2021) %>% 
  replace_na(list(Temp.sumy1me = 25.13100, Temp.sumy1med = 25.13100, Sal.sumy1max = 0.1)) %>% #this is only for the XHH station..
  ungroup() %>%
  arrange(Station, Year)

vroom_write(CC.wland_OneFuture, "/Volumes/savshare2/Current Projects/Predicting-SAV/data/Multiversal Futures ONEBAY/CC.wland_OneFuture.csv")


#View(CC.wlAllFut_ONEBAY %>% group_by(Station) %>% 
#       summarise(across(everything(), ~ sum(is.na(.)))))

#CC.wl_PP.NoDetre####
CC.wl_PP.nodetre = CC.meanyearSMOO %>% #filter(Station == "CB1.1") %>% 
  full_join(CBPall_8420 %>% filter(between(year,1990,2020)), #%>% filter(Station == "CB1.1"),
            by = c("Station", "month")) %>% #detre = NoCC data, smooth = delta. So should be .y + .x 
  #CHECK THE RANDOM SELECTION HERE!!!!! dont be confused that this has Temp:TP those are the non detrended! 
  drop_na() %>% #like 5K points per station over time from unmatched dates
  group_by(Station, month) %>%
  mutate(Temp = Temp + smoothTemp, 
         Sal = Sal + smoothSal,     #detre(ie detrended past data, e.g. ) + smooth(ie smoothed future scalar) = 
         Secc = Secc + smoothSecc,  #dont
         Chla = Chla + smoothChla, 
         TN = TN + smoothTN, 
         TP = TP + smoothTP) %>%
  select(Station, month, year.x, year.y, TN:Chla) %>% 
  mutate(across(TN:Chla, ~case_when(.x < 0 ~ 0.01, #get rid of negative values that came about from the delta math
                                    TRUE ~ .x))) %>% 
  ungroup()  #

CC.wl_sumPP.nodetre = CC.wl_PP.nodetre %>% 
  filter(dplyr::between(month, 5, 8)) %>%
  group_by(Station, year.x, year.y) %>%  #do i need to rowwise instead. no...
  summarize(Chla.summe = mean(Chla, na.rm = T), 
            Chla.summax = max(Chla, na.rm = T),
            Secc.summe = mean(Secc, na.rm = T), 
            Secc.summed = median(Secc, na.rm = T), 
            Sal.summed = median(Sal, na.rm = T), 
            Sal.summax = max(Sal, na.rm = T),
            Sal.summe = mean(Sal, na.rm = T), 
            Temp.summin = min(Temp, na.rm = T), 
            Temp.summax = max(Temp, na.rm = T),
            Temp.summe = mean(Temp, na.rm = T), 
            Temp.summed = median(Temp, na.rm = T),
            TP.summe = mean(TP, na.rm = T), 
            TN.summe = mean(TN, na.rm = T)) %>% 
  ungroup() 
####Summarize Spring 
#Temp.spmed, Temp.spme,Chla.spme, Sal.spme, Secc.spme, TP.spme, TP.spmed, TN.spme
CC.wl_spPP.nodetre = CC.wl_PP.nodetre %>% 
  filter(dplyr::between(month, 3, 5)) %>% #late june isnt spring.... 69 vars one has 6 as this month FYI
  group_by(Station, year.x, year.y) %>% 
  summarise(Chla.spme = mean(Chla, na.rm = T), 
            Secc.spme = mean(Secc, na.rm = T), 
            Sal.spme = mean(Sal, na.rm = T), 
            Temp.spme = mean(Temp, na.rm = T), 
            Temp.spmed = median(Temp, na.rm = T),
            TP.spme = mean(TP, na.rm = T), 
            TP.spmed = median(TP, na.rm = T), 
            TN.spme = mean(TN, na.rm = T)) %>% 
  ungroup()


####Create CC.wl_AllFut####
#change this back from ONEBAY if it works.
CC.wlAllFut_ONEBAY.nodetre = left_join(CC.wl_sumPP.nodetre, CC.wl_spPP.nodetre) %>% 
  rename("Year" = "year.x", "Year.ref" = "year.y") #%>% #keeping the year.y because we need the reference years to be the same between stations for each simulation.
#arrange(Year.ref, Year, Station)

vroom_write(CC.wlAllFut_ONEBAY.nodetre, "/Volumes/savshare2/Current Projects/Predicting-SAV/data/Multiversal Futures/CC.wlAllFut_ONEBAY.nodetre.csv")


CC.wland_OneFuture.no2021.nodetre = CC.wlAllFut_ONEBAY.nodetre %>% 
  group_by(Station) %>%
  mutate(Sal.sumy1max = lag(Sal.summax, n = 1, order_by = Year), 
         Temp.sumy1me = lag(Temp.summe, n = 1, order_by = Year), 
         Temp.sumy1med = lag(Temp.summed, n = 1, order_by = Year))
#Filter out 
CC.wland_One2021.nodetre = CC.wland_OneFuture.no2021.nodetre %>% filter(Year == "2021") %>% 
  select(-Sal.sumy1max, -Temp.sumy1me, -Temp.sumy1med) %>%
  full_join(twentyone) 

#CC.wland_OneFuture, actually all futures ;)####
CC.wland_OneFuture.nodetre = CC.wland_OneFuture.no2021.nodetre %>% #OneFuture is a nisnomer... this is all futures
  filter(!Year == "2021") %>%
  bind_rows(CC.wland_One2021.nodetre) %>% 
  replace_na(list(Temp.sumy1me = 25.13100, Temp.sumy1med = 25.13100, Sal.sumy1max = 0.1)) %>% #this is only for the XHH station..
  ungroup() %>%
  arrange(Station, Year)

vroom_write(CC.wland_OneFuture.nodetre, "/Volumes/savshare2/Current Projects/Predicting-SAV/data/Multiversal Futures ONEBAY/CC.wland_OneFuture.nodetre.csv")
CC.wland_OneFuture.nodetre = vroom("/Volumes/savshare2/Current Projects/Predicting-SAV/data/Multiversal Futures ONEBAY/CC.wland_OneFuture.nodetre.csv")

ggplot(data = CC.wland_OneFuture %>% filter(str_detect(Station, "^TF"))) +
  geom_point(aes(x = Year, y = TN.spme, group = Station, color = Station), size = .9) +
  stat_smooth(aes(x = Year, y = TN.spme, group = Station, color = Station), method = "gam") +
  facet_wrap(~Station, scales = "free_y") 

ggplot(data = CC.wlAllFut_ONEBAY %>% filter(str_detect(Station, "^CB"))) + 
  stat_summary(aes(x = Year, y = TN.spme, group = Station), size = .9) +
  geom_line(aes(x = Year, y = TN.spme, group = Station), size = .9) +
  stat_smooth(aes(x = Year, y = TN.spme, group = Station), method = "lm") +
  facet_wrap(~Station)


#WIP.wland_D####
#Calculates the D
#This takes like 15 mins to run, 30 sec for a station
WIP.wland_D = WIP.wland_2021_2060 %>% #filter(Station %in% c("CB1.1", "LE5.4")) %>% #check a station
  bind_rows(baseline_Dprep %>% add_column(b = "b")) %>% #filter(Station %in% c("CB1.1", "LE5.4")) ) %>%  #b col for baseline check
  select(Date, Station, everything(), -Day, -Month, -Year, -Depth) %>% 
  arrange(Station, Date) %>% #idk if needed
  #group_by(Station, Date) %>% #dont need this, adds literally 15 minutes to this just to output a col of NAs
  mutate(across(Temp:TP, ~.x - lead(.x, order_by = Date), na.rm = F)) %>% #future - baseline = delta from baseline (32 degrees - 28 degrees = -4 delta)
  filter(is.na(b)) %>% #filter out the "b"s, they subtract wrong things
  select(-b) 

vroom_write(WIP.wland_D, "/Volumes/savshare2/Current Projects/Predicting-SAV/data/climate modelling data/WIP.wland_D.csv")

WIP.wland_D = vroom("/Volumes/savshare2/Current Projects/Predicting-SAV/data/climate modelling data/WIP.wland_D.csv")

#WIP.wl_AllFut SMOOTHED AND DETRENDED####
#create annual means, to help find the annual trends
WIP.wl_D.yrme = WIP.wland_D %>%
  mutate(day = day(Date), month = month(Date), year = year(Date)) %>%
  #  mutate(DM = format(as.Date(Date), "%m-%d")) %>%
  group_by(Station, year) %>%
  summarize(across(Temp:TP, ~mean(.x, na.rm = T))) #mean delta per year per station here

WIP.wl.meanyearSMOO = WIP.wl_D.yrme %>% 
  filter(between(year,2051,2060)) %>% #final decade only
  mutate(across(Temp:TP, ~((mean(.)/40)), na.rm = T, .names = "{.col}m")) %>% #30 years not 40, calc mean of the final decade
  summarize(across(Tempm:TPm, ~ .*(seq(2021,2060)-2020))) %>% #builds slope of each final decade
  mutate(year = seq(2021,2060)) %>%
  full_join(WIP.wl_D.yrme) %>% 
  ungroup() %>%
  nest_by(Station) %>%
  mutate(Tempmod = list(lm(Temp ~ year, data = data)), #model for ENV and time, this will be what we smooth to
         Salmod = list(lm(Sal ~ year, data = data)), 
         Seccmod = list(lm(Secc ~ year, data = data)), 
         Chlamod = list(lm(Chla ~ year, data = data)), 
         TNmod = list(lm(TN ~ year, data = data)), 
         TPmod = list(lm(TP ~ year, data = data))) %>% 
  mutate(predTemp = list(predict(Tempmod, data)), 
         predSal = list(predict(Salmod, data)), 
         predSecc = list(predict(Seccmod, data)), 
         predChla = list(predict(Chlamod, data)), 
         predTN = list(predict(TNmod, data)), 
         predTP = list(predict(TPmod, data))) %>% 
  unnest(cols = c(data, predTemp, predSal, predSecc, predChla, predTN, predTP)) %>% 
  select(-c(Temp, Sal, Secc, Chla, TN, TP, Tempmod, Salmod, Seccmod, Chlamod, TNmod, TPmod)) %>%
  full_join(WIP.wland_D %>% 
              mutate(day = day(Date), month = month(Date), year = year(Date)) %>%
              mutate(DM = format(as.Date(Date), "%m-%d")), by = c("Station", "year")) %>% 
  mutate(smoothTemp = Temp - (predTemp - Tempm), 
         smoothSal = Sal - (predSal - Salm), 
         smoothSecc = Secc - (predSecc - Seccm), 
         smoothChla = Chla - (predChla - Chlam), 
         smoothTN = TN - (predTN - TNm), 
         smoothTP = TP - (predTP - TPm)) %>% 
  select(Station, year, month, smoothTemp:smoothTP) %>% 
  group_by(Station, year, month) %>%
  summarise(across(smoothTemp:smoothTP, ~mean(.)))

#combine Future and Past to get Deltas! SMOO and DETREND84 (formerly _ProjPrep)####

WIP.wl_PP = WIP.wl.meanyearSMOO %>% #filter(Station == "CB1.1") %>% 
  full_join(CBPall_DETREND84 %>% filter(between(year,1990,2020)), #%>% filter(Station == "CB1.1"),
            by = c("Station", "month")) %>% #detre = NoCC data, smooth = delta. So should be .y + .x 
  #CHECK THE RANDOM SELECTION HERE!!!!! dont be confused that this has Temp:TP those are the non detrended! 
  drop_na() %>% #like 5K points per station over time from unmatched dates
  group_by(Station, month) %>%
  mutate(Temp = detreTemp + smoothTemp, #CREATE FUTURE ENV FOR SIMULATIONS HERE!!####
         Sal = detreSal + smoothSal,     #detre(ie detrended past data, e.g. ) + smooth(ie smoothed future scalar) = 
         Secc = detreSecc + smoothSecc,  #dont
         Chla = detreChla + smoothChla, 
         TN = detreTN + smoothTN, 
         TP = detreTP + smoothTP) %>%
  select(Station, month, year.x, year.y, Temp:TP) %>% 
  mutate(across(Temp:TP, ~case_when(.x < 0 ~ 0.01, #get rid of negative values that came about from the delta math
                                    TRUE ~ .x))) %>% 
  ungroup()  #this is just all of the possible things to pull 40 years worth of data from. 

WIP.wl_sumPP = WIP.wl_PP %>% 
  filter(dplyr::between(month, 5, 8)) %>%
  group_by(Station, year.x, year.y) %>%  #do i need to rowwise instead. no...
  summarize(Chla.summe = mean(Chla, na.rm = T), 
            Chla.summax = max(Chla, na.rm = T),
            Secc.summe = mean(Secc, na.rm = T), 
            Secc.summed = median(Secc, na.rm = T), 
            Sal.summed = median(Sal, na.rm = T), 
            Sal.summax = max(Sal, na.rm = T),
            Sal.summe = mean(Sal, na.rm = T), 
            Temp.summin = min(Temp, na.rm = T), 
            Temp.summax = max(Temp, na.rm = T),
            Temp.summe = mean(Temp, na.rm = T), 
            Temp.summed = median(Temp, na.rm = T),
            TP.summe = mean(TP, na.rm = T), 
            TN.summe = mean(TN, na.rm = T)) %>% 
  ungroup() 
####Summarize Spring 
#Temp.spmed, Temp.spme,Chla.spme, Sal.spme, Secc.spme, TP.spme, TP.spmed, TN.spme
WIP.wl_spPP = WIP.wl_PP %>% 
  filter(dplyr::between(month, 3, 5)) %>% #late june isnt spring.... 69 vars one has 6 as this month FYI
  group_by(Station, year.x, year.y) %>% 
  summarise(Chla.spme = mean(Chla, na.rm = T), 
            Secc.spme = mean(Secc, na.rm = T), 
            Sal.spme = mean(Sal, na.rm = T), 
            Temp.spme = mean(Temp, na.rm = T), 
            Temp.spmed = median(Temp, na.rm = T),
            TP.spme = mean(TP, na.rm = T), 
            TP.spmed = median(TP, na.rm = T), 
            TN.spme = mean(TN, na.rm = T)) %>% 
  ungroup()


####Create WIP.wl_AllFut####
#change this back from ONEBAY if it works.
WIP.wlAllFut_ONEBAY = left_join(WIP.wl_sumPP, WIP.wl_spPP) %>% 
  rename("Year" = "year.x", "Year.ref" = "year.y") #%>% #keeping the year.y because we need the reference years to be the same between stations for each simulation.
#arrange(Year.ref, Year, Station)

write_csv(WIP.wlAllFut_ONEBAY, "/Volumes/savshare2/Current Projects/Predicting-SAV/data/Multiversal Futures/WIP.wlAllFut_ONEBAY.csv")
WIP.wlAllFut_ONEBAY = vroom("/Volumes/savshare2/Current Projects/Predicting-SAV/data/Multiversal Futures/WIP.wlAllFut_ONEBAY.csv")

WIP.wland_OneFuture.no2021 = WIP.wlAllFut_ONEBAY %>% 
  group_by(Station) %>%
  mutate(Sal.sumy1max = lag(Sal.summax, n = 1, order_by = Year), 
         Temp.sumy1me = lag(Temp.summe, n = 1, order_by = Year), 
         Temp.sumy1med = lag(Temp.summed, n = 1, order_by = Year))
#Filter out 
WIP.wland_One2021 = WIP.wland_OneFuture.no2021 %>% filter(Year == "2021") %>% 
  select(-Sal.sumy1max, -Temp.sumy1me, -Temp.sumy1med) %>%
  full_join(twentyone) 

#WIP.wland_OneFuture, actually all futures ;)####
WIP.wland_OneFuture = WIP.wland_OneFuture.no2021 %>% #OneFuture is a nisnomer... this is all futures
  filter(!Year == "2021") %>%
  bind_rows(WIP.wland_One2021) %>% 
  replace_na(list(Temp.sumy1me = 25.13100, Temp.sumy1med = 25.13100, Sal.sumy1max = 0.1)) %>% #this is only for the XHH station..
  ungroup() %>%
  arrange(Station, Year)

vroom_write(WIP.wland_OneFuture,"/Volumes/savshare2/Current Projects/Predicting-SAV/data/Multiversal Futures ONEBAY/WIP.wland_OneFuture.csv")


#View(WIP.wlAllFut_ONEBAY %>% group_by(Station) %>% 
#       summarise(across(everything(), ~ sum(is.na(.)))))

ggplot(data = WIP.wlAllFut_ONEBAY %>% filter(Station %in% c("WT4.1", "CB1.1", "EE3.1", "LE5.5-W"))) +
  geom_point(aes(x = year, y = dens.weight.mean, group = Station, color = Station), size = .9) +
  stat_smooth(aes(x = year, y = dens.weight.mean, group = Station, color = Station), method = "lm")

ggplot(data = WIP.wlAllFut_ONEBAY %>% filter(str_detect(Station, "^TF"))) + 
  stat_smooth(aes(x = Year, y = TP.spme, group = Station, color = Station), method = "lm")

#WIP.wl_PP.NoDetre####
WIP.wl_PP.nodetre = WIP.wl.meanyearSMOO %>% #filter(Station == "CB1.1") %>% 
  full_join(CBPall_8420 %>% filter(between(year,1990,2020)), #%>% filter(Station == "CB1.1"),
            by = c("Station", "month")) %>% #detre = NoWIP data, smooth = delta. So should be .y + .x 
  #CHECK THE RANDOM SELECTION HERE!!!!! dont be confused that this has Temp:TP those are the non detrended! 
  drop_na() %>% #like 5K points per station over time from unmatched dates
  group_by(Station, month) %>%
  mutate(Temp = Temp + smoothTemp, 
         Sal = Sal + smoothSal,     #detre(ie detrended past data, e.g. ) + smooth(ie smoothed future scalar) = 
         Secc = Secc + smoothSecc,  #dont
         Chla = Chla + smoothChla, 
         TN = TN + smoothTN, 
         TP = TP + smoothTP) %>%
  select(Station, month, year.x, year.y, TN:Chla) %>% 
  mutate(across(TN:Chla, ~case_when(.x < 0 ~ 0.01, #get rid of negative values that came about from the delta math
                                    TRUE ~ .x))) %>% 
  ungroup()  #

WIP.wl_sumPP.nodetre = WIP.wl_PP.nodetre %>% 
  filter(dplyr::between(month, 5, 8)) %>%
  group_by(Station, year.x, year.y) %>%  #do i need to rowwise instead. no...
  summarize(Chla.summe = mean(Chla, na.rm = T), 
            Chla.summax = max(Chla, na.rm = T),
            Secc.summe = mean(Secc, na.rm = T), 
            Secc.summed = median(Secc, na.rm = T), 
            Sal.summed = median(Sal, na.rm = T), 
            Sal.summax = max(Sal, na.rm = T),
            Sal.summe = mean(Sal, na.rm = T), 
            Temp.summin = min(Temp, na.rm = T), 
            Temp.summax = max(Temp, na.rm = T),
            Temp.summe = mean(Temp, na.rm = T), 
            Temp.summed = median(Temp, na.rm = T),
            TP.summe = mean(TP, na.rm = T), 
            TN.summe = mean(TN, na.rm = T)) %>% 
  ungroup() 
####Summarize Spring 
#Temp.spmed, Temp.spme,Chla.spme, Sal.spme, Secc.spme, TP.spme, TP.spmed, TN.spme
WIP.wl_spPP.nodetre = WIP.wl_PP.nodetre %>% 
  filter(dplyr::between(month, 3, 5)) %>% #late june isnt spring.... 69 vars one has 6 as this month FYI
  group_by(Station, year.x, year.y) %>% 
  summarise(Chla.spme = mean(Chla, na.rm = T), 
            Secc.spme = mean(Secc, na.rm = T), 
            Sal.spme = mean(Sal, na.rm = T), 
            Temp.spme = mean(Temp, na.rm = T), 
            Temp.spmed = median(Temp, na.rm = T),
            TP.spme = mean(TP, na.rm = T), 
            TP.spmed = median(TP, na.rm = T), 
            TN.spme = mean(TN, na.rm = T)) %>% 
  ungroup()


####Create WIP.wl_AllFut####
#change this back from ONEBAY if it works.
WIP.wlAllFut_ONEBAY.nodetre = left_join(WIP.wl_sumPP.nodetre, WIP.wl_spPP.nodetre) %>% 
  rename("Year" = "year.x", "Year.ref" = "year.y") #%>% #keeping the year.y because we need the reference years to be the same between stations for each simulation.
#arrange(Year.ref, Year, Station)

vroom_write(WIP.wlAllFut_ONEBAY.nodetre, "/Volumes/savshare2/Current Projects/Predicting-SAV/data/Multiversal Futures/WIP.wlAllFut_ONEBAY.nodetre.csv")
WIP.wlAllFut_ONEBAY.nodetre = 
  vroom("/Volumes/savshare2/Current Projects/Predicting-SAV/data/Multiversal Futures/WIP.wlAllFut_ONEBAY.nodetre.csv")

WIP.wland_OneFuture.no2021.nodetre = WIP.wlAllFut_ONEBAY.nodetre %>% 
  group_by(Station) %>%
  mutate(Sal.sumy1max = lag(Sal.summax, n = 1, order_by = Year), 
         Temp.sumy1me = lag(Temp.summe, n = 1, order_by = Year), 
         Temp.sumy1med = lag(Temp.summed, n = 1, order_by = Year))
#Filter out 
WIP.wland_One2021.nodetre = WIP.wland_OneFuture.no2021.nodetre %>% filter(Year == "2021") %>% 
  select(-Sal.sumy1max, -Temp.sumy1me, -Temp.sumy1med) %>%
  full_join(twentyone) 

#WIP.wland_OneFuture.nodetre, actually all futures ;)####
WIP.wland_OneFuture.nodetre = WIP.wland_OneFuture.no2021.nodetre %>% #OneFuture is a nisnomer... this is all futures
  filter(!Year == "2021") %>%
  bind_rows(WIP.wland_One2021.nodetre) %>% 
  replace_na(list(Temp.sumy1me = 25.13100, Temp.sumy1med = 25.13100, Sal.sumy1max = 0.1)) %>% #this is only for the XHH station..
  ungroup() %>%
  arrange(Station, Year)

vroom_write(WIP.wland_OneFuture.nodetre,"/Volumes/savshare2/Current Projects/Predicting-SAV/data/Multiversal Futures ONEBAY/WIP.wland_OneFuture.nodetre.csv")





#OLD ONEBAY CODE can delete maybe. 8/4/22 ####
#combine SMOO and DETREND (formerly _ProjPrep)####
WIP.wl_PP = WIP.wl.meanyearSMOO %>% 
  left_join(CBPall_DETREND84, #%>% filter(!year < 2000), #%>% #NEW! DETREND is Filtered only year 2000 data and beyond. Too many missing values in the past and also why compare 2040 to 1988?? #NEW AGAIN!!!! Went back to 84 becauyse its detrended
            #  filter(! year %in% c(2020)),  #2020 has incomplete data #but i might fill it in with medians.......
            by = c("Station", "DM")) %>% #detre = NoWIP.wl data, smooth = delta. So should be .y + .x 
  drop_na() %>% #like 5K points per station over time from unmatched dates
  group_by(Station, DM) %>%
  mutate(Temp = detreTemp + smoothTemp, #CREATE FUTURE ENV FOR SIMULATIONS HERE!!####
         Sal = detreSal + smoothSal,     #detre(ie detrended past data, e.g. ) + smooth(ie smoothed future scalar) = 
         Secc = detreSecc + smoothSecc, 
         Chla = detreChla + smoothChla, 
         TN = detreTN + smoothTN, 
         TP = detreTP + smoothTP) %>%
  select(Station, DM, year.x, year.y, month.x, Temp:TP) %>% 
  mutate(across(Temp:TP, ~case_when(.x < 0 ~ 0.01, #get rid of negative values that came about from the delta math
                                    TRUE ~ .x))) %>% 
  ungroup()  #this is just all of the possible things to pull 40 years worth of data from. 

WIP.wl_sumPP = WIP.wl_PP %>% 
  filter(dplyr::between(month.x, 5, 8)) %>%
  group_by(Station, year.x, year.y) %>%  #do i need to rowwise instead. no...
  summarize(Chla.summe = mean(Chla, na.rm = T), 
            Chla.summax = max(Chla, na.rm = T),
            Secc.summe = mean(Secc, na.rm = T), 
            Secc.summed = median(Secc, na.rm = T), 
            Sal.summed = median(Sal, na.rm = T), 
            Sal.summax = max(Sal, na.rm = T),
            Sal.summe = mean(Sal, na.rm = T), 
            Temp.summin = min(Temp, na.rm = T), 
            Temp.summax = max(Temp, na.rm = T),
            Temp.summe = mean(Temp, na.rm = T), 
            Temp.summed = median(Temp, na.rm = T),
            TP.summe = mean(TP, na.rm = T), 
            TN.summe = mean(TN, na.rm = T)) %>% 
  ungroup() 
####Summarize Spring 
#Temp.spmed, Temp.spme,Chla.spme, Sal.spme, Secc.spme, TP.spme, TP.spmed, TN.spme
WIP.wl_spPP = WIP.wl_PP %>% 
  filter(dplyr::between(month.x, 3, 5)) %>% #late june isnt spring.... 
  group_by(Station, year.x, year.y) %>% 
  summarise(Chla.spme = mean(Chla, na.rm = T), 
            Secc.spme = mean(Secc, na.rm = T), 
            Sal.spme = mean(Sal, na.rm = T), 
            Temp.spme = mean(Temp, na.rm = T), 
            Temp.spmed = median(Temp, na.rm = T),
            TP.spme = mean(TP, na.rm = T), 
            TP.spmed = median(TP, na.rm = T), 
            TN.spme = mean(TN, na.rm = T)) %>% 
  ungroup()


####Create WIP.wl.wl_AllFut####
#change this back from ONEBAY if it works.
WIP.wlAllFut_ONEBAY = left_join(WIP.wl_sumPP, WIP.wl_spPP) %>% 
  # select(-year.y) %>% 
  rename("Year" = "year.x", "Year.ref" = "year.y") #%>% #keeping the year.y because we need the reference years to be the same between stations for each simulation.
#arrange(Year.ref, Year, Station)

vroom_write(WIP.wlAllFut_ONEBAY, "/Volumes/savshare2/Current Projects/Predicting-SAV/data/Multiversal Futures/WIP.wl.AllFutsmoodetre_ONEBAY.csv")

#View(WIP.wlAllFut_ONEBAY %>% group_by(Station) %>% 
#       summarise(across(everything(), ~ sum(is.na(.)))))

ggplot(data = WIP.wlAllFut_ONEBAY %>% filter(Station %in% c("WT4.1", "CB1.1", "EE3.1", "LE5.5-W"))) +
  geom_point(aes(x = Year, y = Temp.summe, group = Station, color = Station), size = .9) +
  stat_smooth(aes(x = Year, y = Temp.summe, group = Station, color = Station), method = "lm")

ggplot(data = WIP.wlAllFut_ONEBAY %>% filter(str_detect(Station, "^TF"))) + 
  stat_smooth(aes(x = Year, y = TP.spme, group = Station), color = "green", method = "lm") +
  stat_smooth(data = CC.wlAllFut_ONEBAY %>% filter(str_detect(Station, "^TF")),
              aes(x = Year, y = TP.spme, group = Station), color = "brown", method = "lm")


###END OLD ONEBAY CODE #


#Build a NoChange Scenario and also just make a clean, no NA, CBP_all ####
CBPall_0020 = vroom("/Volumes/savshare2/Current Projects/Predicting-SAV/data/Water Quality/CBPall_0020.csv")

CBPallclean_sum = CBPall_0020 %>% 
  filter(dplyr::between(month, 5, 8)) %>%
  group_by(Station, year) %>%  #do i need to rowwise instead. no...
  summarize(Chla.summe = mean(Chla, na.rm = T), 
            Chla.summax = max(Chla, na.rm = T),
            Secc.summe = mean(Secc, na.rm = T), 
            Secc.summed = median(Secc, na.rm = T), 
            Sal.summed = median(Sal, na.rm = T), 
            Sal.summax = max(Sal, na.rm = T),
            Sal.summe = mean(Sal, na.rm = T), 
            Temp.summin = min(Temp, na.rm = T), 
            Temp.summax = max(Temp, na.rm = T),
            Temp.summe = mean(Temp, na.rm = T), 
            Temp.summed = median(Temp, na.rm = T),
            TP.summe = mean(TP, na.rm = T), 
            TN.summe = mean(TN, na.rm = T)) %>% 
  group_by(Station) %>%
  mutate(Sal.sumy1max = lag(Sal.summax, order_by = year), 
         Temp.sumy1med = lag(Temp.summed, order_by = year), 
         Temp.sumy1me = lag(Temp.summe, order_by = year)) %>%
  mutate(Sal.sumy1max = coalesce(Sal.sumy1max, Sal.summax), 
         Temp.sumy1med = coalesce(Temp.sumy1med, Temp.summed), 
         Temp.sumy1me = coalesce(Temp.sumy1me, Temp.summe)) %>%
  ungroup() 

CBPallclean_grow = CBPall_0020 %>% 
  filter(dplyr::between(month, 3, 7)) %>%
  group_by(Station, year) %>%  #do i need to rowwise instead. no...
  summarize(Chla.growme = mean(Chla, na.rm = T), 
            Chla.growmax = max(Chla, na.rm = T),
            Secc.growme = mean(Secc, na.rm = T), 
            Secc.growmed = median(Secc, na.rm = T), 
            Sal.growmed = median(Sal, na.rm = T), 
            Sal.growmax = max(Sal, na.rm = T),
            Sal.growme = mean(Sal, na.rm = T), 
            Temp.growmin = min(Temp, na.rm = T), 
            Temp.growmax = max(Temp, na.rm = T),
            Temp.growme = mean(Temp, na.rm = T), 
            Temp.growmed = median(Temp, na.rm = T),
            TP.growme = mean(TP, na.rm = T), 
            TN.growme = mean(TN, na.rm = T)) %>% 
  group_by(Station) %>%
  mutate(Sal.growy1max = lag(Sal.growmax, order_by = year), 
         Temp.growy1med = lag(Temp.growmed, order_by = year), 
         Temp.growy1me = lag(Temp.growme, order_by = year)) %>%
  mutate(Sal.growy1max = coalesce(Sal.growy1max, Sal.growmax), 
         Temp.growy1med = coalesce(Temp.growy1med, Temp.growmed), 
         Temp.growy1me = coalesce(Temp.growy1me, Temp.growme)) %>%
  ungroup() 

####Summarize Spring 
CBPallclean_sp = CBPall_0020 %>% 
  filter(dplyr::between(month, 3, 5)) %>% #late june isnt spring.... #FYI i found this to be 3, 9 for the model data
  group_by(Station, year) %>% 
  summarise(Chla.spme = mean(Chla, na.rm = T), 
            Secc.spme = mean(Secc, na.rm = T), 
            Sal.spme = mean(Sal, na.rm = T), 
            Temp.spme = mean(Temp, na.rm = T), 
            Temp.spmed = median(Temp, na.rm = T),
            TP.spme = mean(TP, na.rm = T), 
            TP.spmed = median(TP, na.rm = T), 
            TN.spme = mean(TN, na.rm = T)) %>% 
  ungroup()



CBPallClean = left_join(CBPallclean_sp, CBPallclean_sum) %>% 
  left_join(CBPallclean_grow) %>%
  drop_na() #NAs/INFs are from the problem stations. Can prob drop

SAVCommDensWQ_ForPred = vroom("/Volumes/savshare2/Current Projects/Predicting-SAV/data/communityDFs/SAVCommDensWQ_ForPredictions.csv") 

#SAV WQ ALL CLEAN 00-2020####
#I think this is a great and beautiful dataset of the past 20 years 2000-2020
SAVWQallClean = CBPallClean %>% rename(STATION = Station) %>%
  right_join(SAVCommDensWQ_ForPred %>% select(STATION:SAVArea) %>% filter(year > 1999))

vroom_write(SAVWQallClean, "/Volumes/savshare2/Current Projects/Predicting-SAV/data/communityDFs/SAVWQallClean.csv") 

SAVWQallClean = vroom("/Volumes/savshare2/Current Projects/Predicting-SAV/data/communityDFs/SAVWQallClean.csv")

#Build complete AllClean from 1985 onward####
CBPall_8420 = vroom("/Volumes/savshare2/Current Projects/Predicting-SAV/data/Water Quality/CBPall_8420.csv")

CBPallclean_sum84 = CBPall_8420 %>% 
  filter(dplyr::between(month, 5, 8)) %>%
  group_by(Station, year) %>%  #do i need to rowwise instead. no...
  summarize(Chla.summe = mean(Chla, na.rm = T), 
            Chla.summax = max(Chla, na.rm = T),
            Secc.summe = mean(Secc, na.rm = T), 
            Secc.summed = median(Secc, na.rm = T), 
            Sal.summed = median(Sal, na.rm = T), 
            Sal.summax = max(Sal, na.rm = T),
            Sal.summe = mean(Sal, na.rm = T), 
            Temp.summin = min(Temp, na.rm = T), 
            Temp.summax = max(Temp, na.rm = T),
            Temp.summe = mean(Temp, na.rm = T), 
            Temp.summed = median(Temp, na.rm = T),
            TP.summe = mean(TP, na.rm = T), 
            TN.summe = mean(TN, na.rm = T)) %>% 
  group_by(Station) %>%
  mutate(Sal.sumy1max = lag(Sal.summax, order_by = year), 
         Temp.sumy1med = lag(Temp.summed, order_by = year), 
         Temp.sumy1me = lag(Temp.summe, order_by = year)) %>%
  mutate(Sal.sumy1max = coalesce(Sal.sumy1max, Sal.summax), 
         Temp.sumy1med = coalesce(Temp.sumy1med, Temp.summed), 
         Temp.sumy1me = coalesce(Temp.sumy1me, Temp.summe)) %>%
  ungroup() 

CBPallclean_grow84 = CBPall_8420 %>% 
  filter(dplyr::between(month, 3, 7)) %>%
  group_by(Station, year) %>%  #do i need to rowwise instead. no...
  summarize(Chla.growme = mean(Chla, na.rm = T), 
            Chla.growmax = max(Chla, na.rm = T),
            Secc.growme = mean(Secc, na.rm = T), 
            Secc.growmed = median(Secc, na.rm = T), 
            Sal.growmed = median(Sal, na.rm = T), 
            Sal.growmax = max(Sal, na.rm = T),
            Sal.growme = mean(Sal, na.rm = T), 
            Temp.growmin = min(Temp, na.rm = T), 
            Temp.growmax = max(Temp, na.rm = T),
            Temp.growme = mean(Temp, na.rm = T), 
            Temp.growmed = median(Temp, na.rm = T),
            TP.growme = mean(TP, na.rm = T), 
            TN.growme = mean(TN, na.rm = T)) %>% 
  group_by(Station) %>%
  mutate(Sal.growy1max = lag(Sal.growmax, order_by = year), 
         Temp.growy1med = lag(Temp.growmed, order_by = year), 
         Temp.growy1me = lag(Temp.growme, order_by = year)) %>%
  mutate(Sal.growy1max = coalesce(Sal.growy1max, Sal.growmax), 
         Temp.growy1med = coalesce(Temp.growy1med, Temp.growmed), 
         Temp.growy1me = coalesce(Temp.growy1me, Temp.growme)) %>%
  ungroup() 

####Summarize Spring 
CBPallclean_sp84 = CBPall_8420 %>% 
  filter(dplyr::between(month, 3, 5)) %>% #late june isnt spring.... #FYI i found this to be 3, 9 for the model data
  group_by(Station, year) %>% 
  summarise(Chla.spme = mean(Chla, na.rm = T), 
            Secc.spme = mean(Secc, na.rm = T), 
            Sal.spme = mean(Sal, na.rm = T), 
            Temp.spme = mean(Temp, na.rm = T), 
            Temp.spmed = median(Temp, na.rm = T),
            TP.spme = mean(TP, na.rm = T), 
            TP.spmed = median(TP, na.rm = T), 
            TN.spme = mean(TN, na.rm = T)) %>% 
  ungroup()



CBPallClean84 = left_join(CBPallclean_sp84, CBPallclean_sum84) %>% 
  left_join(CBPallclean_grow84) %>%
  drop_na() #NAs/INFs are from the problem stations. Can prob drop

SAVCommDensWQ_ForPred = vroom("/Volumes/savshare2/Current Projects/Predicting-SAV/data/communityDFs/SAVCommDensWQ_ForPredictions.csv") 

#SAV WQ ALL CLEAN 84-2020####
#I think this is a great and beautiful dataset of the past 20 years 2000-2020
SAVWQallClean84 = CBPallClean84 %>% rename(STATION = Station) %>%
  right_join(SAVCommDensWQ_ForPred %>% select(STATION:SAVArea))

vroom_write(SAVWQallClean84, "/Volumes/savshare2/Current Projects/Predicting-SAV/data/communityDFs/SAVWQallClean84.csv")




#
##
###
###
#Below code is obsolete after 5/4/22 changes. keeping is in case we need to reference but ONEBAY is the way to go now####
###
###
###
#NOTE::: THIS NEED TO CHANGE IF I DID THE ABOVE CORRECTLY###### 4/20
WIP.wland_D = WIP.wland_2021_2060 %>%
  bind_rows(baseline_Dprep) %>% #group_by(Station) %>%
  select(Date, Year, Month, Day, Station, everything()) %>%
  arrange(Station, Date) %>% #idk if needed
  group_by(Station, Date) %>%
  summarise(across(Depth:TP, ~.x - lead(.x, order_by = Date))) %>%
  drop_na()

vroom_write(WIP.wland_D, "/Volumes/savshare2/Current Projects/Predicting-SAV/data/climate modelling data/WIP.wland_D.csv")

##newpasted workflow##
#first, get the mean delta values per year
WWL_D.yrme = WIP.wland_D %>% 
  mutate(day = day(Date), month = month(Date), year = year(Date)) %>%
  mutate(DM = format(as.Date(Date), "%m-%d")) %>%
  group_by(Station, year) %>%
  summarize(across(Temp:TP, ~mean(.x, na.rm = T))) #mean delta per year per station here

WW.meanyearSMOO = WWL_D.yrme %>% 
  filter(between(year,2051,2060)) %>% 
  mutate(across(Temp:TP, ~((mean(.)/40)), na.rm = T, .names = "{.col}m")) %>% #30 years not 40, calc mean of the final decade
  summarize(across(Tempm:TPm, ~ .*(seq(2021,2060)-2020))) %>%
  mutate(year = seq(2021,2060)) %>%
  full_join(WWL_D.yrme) %>%
  ungroup() %>%
  nest_by(Station) %>%
  mutate(Tempmod = list(lm(Temp ~ year, data = data)), 
         Salmod = list(lm(Sal ~ year, data = data)), 
         Seccmod = list(lm(Secc ~ year, data = data)), 
         Chlamod = list(lm(Chla ~ year, data = data)), 
         TNmod = list(lm(TN ~ year, data = data)), 
         TPmod = list(lm(TP ~ year, data = data))) %>% 
  mutate(predTemp = list(predict(Tempmod, data)), 
         predSal = list(predict(Salmod, data)), 
         predSecc = list(predict(Seccmod, data)), 
         predChla = list(predict(Chlamod, data)), 
         predTN = list(predict(TNmod, data)), 
         predTP = list(predict(TPmod, data))) %>% 
  unnest(cols = c(data, predTemp, predSal, predSecc, predChla, predTN, predTP)) %>% 
  select(-c(Temp, Sal, Secc, Chla, TN, TP, Tempmod, Salmod, Seccmod, Chlamod, TNmod, TPmod)) %>%
  full_join(WIP.wland_D %>% 
              mutate(day = day(Date), month = month(Date), year = year(Date)) %>%
              mutate(DM = format(as.Date(Date), "%m-%d")), by = c("Station", "year")) %>%
  mutate(smoothTemp = Temp - (predTemp - Tempm), 
         smoothSal = Sal - (predSal - Salm), 
         smoothSecc = Secc - (predSecc - Seccm), 
         smoothChla = Chla - (predChla - Chlam), 
         smoothTN = TN - (predTN - TNm), 
         smoothTP = TP - (predTP - TPm))

WIP.wl_PP = WW.meanyearSMOO %>% 
  left_join(CBPall_DETREND %>% 
              filter(! year %in% c(2020)),  #2020 has incomplete data 
            by = c("Station", "DM")) %>% #detre = NoCC data, smooth = delta. So should be .y + .x 
  drop_na() %>% #like 50K points per station over time from unmatched dates
  group_by(Station, DM) %>%
  mutate(Temp = detreTemp + smoothTemp, 
         Sal = detreSal + smoothSal, 
         Secc = detreSecc + smoothSecc, 
         Chla = detreChla + smoothChla, 
         TN = detreTN + smoothTN, 
         TP = detreTP + smoothTP) %>%
  select(Station, DM, year.x, year.y, month.x, Temp:TP) %>% 
  mutate(across(Temp:TP, ~case_when(.x < 0 ~ 0.01, #get rid of negative values that came about from the delta math
                                    TRUE ~ .x))) %>% 
  ungroup() #this is just all of the possible things to pull 40 years worth of data from. 


WIP.wl_sumPP = WIP.wl_PP %>% 
  filter(dplyr::between(month.x, 5, 8)) %>%
  group_by(Station, year.x, year.y) %>%  #do i need to rowwise instead. no...
  summarize(Chla.summe = mean(Chla, na.rm = T), 
            Chla.summax = max(Chla, na.rm = T),
            Secc.summe = mean(Secc, na.rm = T), 
            Secc.summed = median(Secc, na.rm = T), 
            Sal.summed = median(Sal, na.rm = T), 
            Sal.summax = max(Sal, na.rm = T),
            Sal.summe = mean(Sal, na.rm = T), 
            Temp.summin = min(Temp, na.rm = T), 
            Temp.summax = max(Temp, na.rm = T),
            Temp.summe = mean(Temp, na.rm = T), 
            Temp.summed = median(Temp, na.rm = T),
            TP.summe = mean(TP, na.rm = T), 
            TN.summe = mean(TN, na.rm = T)) %>% 
  ungroup() 
####Summarize Spring 
#Temp.spmed, Temp.spme,Chla.spme, Sal.spme, Secc.spme, TP.spme, TP.spmed, TN.spme
WIP.wl_spPP = WIP.wl_PP %>% 
  filter(dplyr::between(month.x, 3, 6)) %>% #late june isnt spring.... 
  group_by(Station, year.x, year.y) %>% 
  summarise(Chla.spme = mean(Chla, na.rm = T), 
            Secc.spme = mean(Secc, na.rm = T), 
            Sal.spme = mean(Sal, na.rm = T), 
            Temp.spme = mean(Temp, na.rm = T), 
            Temp.spmed = median(Temp, na.rm = T),
            TP.spme = mean(TP, na.rm = T), 
            TP.spmed = median(TP, na.rm = T), 
            TN.spme = mean(TN, na.rm = T)) %>% 
  ungroup()


####Create WIP.wl_AllFut####
WIP.wlAllFut = left_join(WIP.wl_sumPP, WIP.wl_spPP) %>% 
  select(-year.y) %>% 
  rename("Year" = "year.x")

vroom_write(WIP.wlAllFut, "/Volumes/savshare2/Current Projects/Predicting-SAV/data/Multiversal Futures/WIP.wlAllFutsmoodetre.csv")

##newpasted workflow##

#WIP.woland_D, do we need this anymore?####
#HEY!!! STOP!!!! You should add 2021-2030 in now. DO IT####3
WIP.woland_D = WIP.woland_2031_2060 %>% 
  bind_rows(baseline_Dprep) %>% 
 # filter(Date %in% CBPall_Future$Date) %>% 
  select(Date, Year, Month, Day, Station, everything()) %>%
  arrange(Station, Date) %>% #idk if needed
  group_by(Station, Date) %>%
  summarise(across(Depth:TP, ~.x - lead(.x, order_by = Date))) %>%
  drop_na()

vroom_write(WIP.woland_D, "/Volumes/savshare2/Current Projects/Predicting-SAV/data/climate modelling data/WIP.woland_D.csv")

#Then, create Projection Prepared dataframes####
#NOTE: 3/11/22 workflow changed again for smoothing and detrending, just do the above rows####
#create a DF of all the possible daily matchup predictions, where CC.wland is the climate projection DELTA, and CBP all future is the No Climate Change data. 
CC.wland_ProjPrep = CC.wland_D %>% 
  mutate(day = day(Date), month = month(Date), year = year(Date)) %>%
  mutate(DM = format(as.Date(Date), "%m-%d")) %>%
  left_join(CBPall_Future %>% 
            filter(! year %in% c(2020)) ,  #2020 has incomplete data so remove for now
            by = c("Station", "DM")) %>% #.y = NoCC data, .x = delta. So should be .y + .x 
  drop_na() %>% #like 50K points per station over time.
  group_by(Station, DM) %>%
  mutate(Temp = Temp.y + Temp.x, 
         Sal = Sal.y + Sal.x, 
         Secc = Secc.y + Secc.x, 
         Chla = Chla.y + Chla.x, 
         TN = TN.y + TN.x, 
         TP = TP.y + TP.x) %>%
  select(Station, DM, year.x, year.y, month.x, Temp:TP) %>% 
  mutate(across(Temp:TP, ~case_when(.x < 0 ~ 0.01, #get rid of negative values that came about from the delta math
                                    TRUE ~ .x))) %>% 
  ungroup() #this is just all of the possible things to pull 40 years worth of data from. 

CC.wland_summerPP = CC.wland_ProjPrep %>% 
  filter(dplyr::between(month.x, 5, 8)) %>%
  group_by(Station, year.x, year.y) %>%  #do i need to rowwise instead. no...
  summarize(Chla.summe = mean(Chla, na.rm = T), 
            Chla.summax = max(Chla, na.rm = T),
            Secc.summe = mean(Secc, na.rm = T), 
            Secc.summed = median(Secc, na.rm = T), 
            Sal.summed = median(Sal, na.rm = T), 
            Sal.summax = max(Sal, na.rm = T),
            Sal.summe = mean(Sal, na.rm = T), 
            Temp.summin = min(Temp, na.rm = T), 
            Temp.summax = max(Temp, na.rm = T),
            Temp.summe = mean(Temp, na.rm = T), 
            Temp.summed = median(Temp, na.rm = T),
            TP.summe = mean(TP, na.rm = T), 
            TN.summe = mean(TN, na.rm = T)) %>% 
  ungroup() 
####Summarize Spring 
#Temp.spmed, Temp.spme,Chla.spme, Sal.spme, Secc.spme, TP.spme, TP.spmed, TN.spme
CC.wland_springPP = CC.wland_ProjPrep %>%  
  filter(dplyr::between(month.x, 3, 6)) %>% #late june isnt spring.... 
  group_by(Station, year.x, year.y) %>% 
  summarise(Chla.spme = mean(Chla, na.rm = T), 
            Secc.spme = mean(Secc, na.rm = T), 
            Sal.spme = mean(Sal, na.rm = T), 
            Temp.spme = mean(Temp, na.rm = T), 
            Temp.spmed = median(Temp, na.rm = T),
            TP.spme = mean(TP, na.rm = T), 
            TP.spmed = median(TP, na.rm = T), 
            TN.spme = mean(TN, na.rm = T)) %>% 
  ungroup()

#run these next 4 DFs to get the CC.wland_OneFuture
####twentyone code####
twentyone = CBP.WQ_69vars %>% filter(year == "2020") %>%
  select(STATION, year, Temp.summed, Temp.summe, Sal.summax) %>% #im just selecting the ones we need
  group_by(STATION) %>%
  #slice_sample(n = 1 , replace = T) %>% #pick a random year
  mutate(Year = case_when(year == 2020 ~ 2021)) %>% #change year, bc y being changed to y1
  rename(Temp.sumy1med = Temp.summed, Temp.sumy1me = Temp.summe, Sal.sumy1max = Sal.summax) %>%
  rename(Station = STATION) %>%
  select(Station, Year, everything()) %>% select(-year) %>% 
  replace_na(list(Temp.sumy1me = 31.2550, Temp.sumy1med = 31.2550, Sal.sumy1max = 0.001)) %>% 
  ungroup() 

####Create CC.wland_AllFutures####
CC.wland_AllFutures = left_join(CC.wland_summerPP, CC.wland_springPP) %>% 
  select(-year.y) %>% 
  rename("Year" = "year.x")

write.csv(CC.wland_AllFutures, "/Volumes/savshare2/Current Projects/Predicting-SAV/data/Multiversal Futures/CC.wland_AllFutures.csv")
CC.wland_AllFutures = read.csv("/Volumes/savshare2/Current Projects/Predicting-SAV/data/Multiversal Futures/CC.wland_AllFutures.csv")

####Create CC.wland_OneFuture####
CC.wland_OneFuture.no2021 = CC.wland_AllFutures %>% 
  group_by(Station, Year) %>% 
  slice_sample(., n = 1, replace = T) %>% #randomly select a year, will give a timeline of 40 years bc grouped by Year
  group_by(Station) %>%
  mutate(Sal.sumy1max = lag(Sal.summax, n = 1, order_by = Year), 
         Temp.sumy1me = lag(Temp.summe, n = 1, order_by = Year), 
         Temp.sumy1med = lag(Temp.summed, n = 1, order_by = Year))

CC.wland_One2021 = CC.wland_OneFuture.no2021 %>% filter(Year == "2021") %>% 
  select(-Sal.sumy1max, -Temp.sumy1me, -Temp.sumy1med) %>%
  full_join(twentyone) 

CC.wland_OneFuture = CC.wland_OneFuture.no2021 %>% 
  filter(!Year == "2021") %>%
  bind_rows(CC.wland_One2021) %>% 
  ungroup() %>%
  arrange(Station, Year)


###WIP wland####
WIP.wland_ProjPrep = WIP.wland_D %>% #this takes a few mins to run
  mutate(day = day(Date), month = month(Date), year = year(Date)) %>%
  mutate(DM = format(as.Date(Date), "%m-%d")) %>%
  left_join(CBPall_Future %>% #should this be a semi join? i think yes.........
              filter(! year %in% c(2020)) ,  #2020 has incomplete data so remove for now
            by = c("Station", "DM")) %>% #.y = NoCC data, .x = delta. So should be .y + .x 
  drop_na() %>% #like 50K points per station over time.
  group_by(Station, DM) %>%
  mutate(Temp = Temp.y + Temp.x, 
         Sal = Sal.y + Sal.x, 
         Secc = Secc.y + Secc.x, 
         Chla = Chla.y + Chla.x, 
         TN = TN.y + TN.x, 
         TP = TP.y + TP.x) %>%
  select(Station, DM, year.x, year.y, month.x, Temp:TP) %>% 
  mutate(across(Temp:TP, ~case_when(.x < 0 ~ 0.01, #get rid of negative values that came about from the delta math
                                    TRUE ~ .x))) %>% 
  ungroup() #this is just all of the possible things to pull 40 years worth of data from. 


WIP.wland_summerPP = WIP.wland_ProjPrep %>% 
  filter(dplyr::between(month.x, 5, 8)) %>%
  group_by(Station, year.x, year.y) %>%  #do i need to rowwise instead. no...
  summarize(Chla.summe = mean(Chla, na.rm = T), 
            Chla.summax = max(Chla, na.rm = T),
            Secc.summe = mean(Secc, na.rm = T), 
            Secc.summed = median(Secc, na.rm = T), 
            Sal.summed = median(Sal, na.rm = T), 
            Sal.summax = max(Sal, na.rm = T),
            Sal.summe = mean(Sal, na.rm = T), 
            Temp.summin = min(Temp, na.rm = T), 
            Temp.summax = max(Temp, na.rm = T),
            Temp.summe = mean(Temp, na.rm = T), 
            Temp.summed = median(Temp, na.rm = T),
            TP.summe = mean(TP, na.rm = T), 
            TN.summe = mean(TN, na.rm = T)) %>% 
  ungroup() 
####Summarize Spring 
#Temp.spmed, Temp.spme,Chla.spme, Sal.spme, Secc.spme, TP.spme, TP.spmed, TN.spme
WIP.wland_springPP = WIP.wland_ProjPrep %>%  
  filter(dplyr::between(month.x, 3, 6)) %>% #late june isnt spring.... 
  group_by(Station, year.x, year.y) %>% 
  summarise(Chla.spme = mean(Chla, na.rm = T), 
            Secc.spme = mean(Secc, na.rm = T), 
            Sal.spme = mean(Sal, na.rm = T), 
            Temp.spme = mean(Temp, na.rm = T), 
            Temp.spmed = median(Temp, na.rm = T),
            TP.spme = mean(TP, na.rm = T), 
            TP.spmed = median(TP, na.rm = T), 
            TN.spme = mean(TN, na.rm = T)) %>% 
  ungroup()

#run these next 4 DFs to get the WIP.wland_OneFuture
####twentyone code####
twentyone = CBP.WQ_69vars %>% filter(year == "2020") %>%
  select(STATION, year, Temp.summed, Temp.summe, Sal.summax) %>% #im just selecting the ones we need
  group_by(STATION) %>%
  #slice_sample(n = 1 , replace = T) %>% #pick a random year
  mutate(Year = case_when(year == 2020 ~ 2021)) %>% #change year, bc y being changed to y1
  rename(Temp.sumy1med = Temp.summed, Temp.sumy1me = Temp.summe, Sal.sumy1max = Sal.summax) %>%
  rename(Station = STATION) %>%
  select(Station, Year, everything()) %>% select(-year) %>% 
  replace_na(list(Temp.sumy1me = 31.2550, Temp.sumy1med = 31.2550, Sal.sumy1max = 0.001)) %>% 
  ungroup() 

####Create WIP.wland_AllFuture####
WIP.wland_AllFutures = left_join(WIP.wland_summerPP, WIP.wland_springPP) %>% 
  select(-year.y) %>% 
  rename("Year" = "year.x")

write.csv(WIP.wland_AllFutures, "/Volumes/savshare2/Current Projects/Predicting-SAV/data/Multiversal Futures/WIP.wland_AllFutures.csv")
WIP.wland_AllFutures = read.csv("/Volumes/savshare2/Current Projects/Predicting-SAV/data/Multiversal Futures/WIP.wland_AllFutures.csv")

####Create WIP.wland_OneFuture####
WIP.wland_OneFuture.no2021 = WIP.wland_AllFutures %>% 
  group_by(Station, Year) %>% 
  slice_sample(., n = 1, replace = T) %>% #randomly select a year, will give a timeline of 40 years bc grouped by Year
  group_by(Station) %>%
  mutate(Sal.sumy1max = lag(Sal.summax, n = 1, order_by = Year), 
         Temp.sumy1me = lag(Temp.summe, n = 1, order_by = Year), 
         Temp.sumy1med = lag(Temp.summed, n = 1, order_by = Year))

WIP.wland_One2021 = WIP.wland_OneFuture.no2021 %>% filter(Year == "2021") %>% 
  select(-Sal.sumy1max, -Temp.sumy1me, -Temp.sumy1med) %>%
  full_join(twentyone) 

WIP.wland_OneFuture = WIP.wland_OneFuture.no2021 %>% 
  filter(!Year == "2021") %>%
  bind_rows(WIP.wland_One2021) %>% 
  ungroup() %>%
  arrange(Station, Year)

###WIPwoland####
#be careful here bc it dont start till 2030
WIP.woland_ProjPrep = WIP.woland_D %>% 
  mutate(day = day(Date), month = month(Date), year = year(Date)) %>%
  mutate(DM = format(as.Date(Date), "%m-%d")) %>%
  left_join(CBPall_Future %>% 
              filter(! year %in% c(2020)) ,  #2020 has incomplete data so remove for now
            by = c("Station", "DM")) %>% #.y = NoCC data, .x = delta. So should be .y + .x 
  drop_na() %>% #like 50K points per station over time.
  group_by(Station, DM) %>%
  mutate(Temp = Temp.y + Temp.x, 
         Sal = Sal.y + Sal.x, 
         Secc = Secc.y + Secc.x, 
         Chla = Chla.y + Chla.x, 
         TN = TN.y + TN.x, 
         TP = TP.y + TP.x) %>%
  select(Station, DM, year.x, year.y, month.x, Temp:TP) %>% 
  mutate(across(Temp:TP, ~case_when(.x < 0 ~ 0.01, #get rid of negative values that came about from the delta math
                                    TRUE ~ .x))) %>% 
  ungroup() #this is just all of the possible things to pull 40 years worth of data from. 


WIP.woland_summerPP = WIP.woland_ProjPrep %>% 
  filter(dplyr::between(month.x, 5, 8)) %>%
  group_by(Station, year.x, year.y) %>%  #do i need to rowwise instead. no...
  summarize(Chla.summe = mean(Chla, na.rm = T), 
            Chla.summax = max(Chla, na.rm = T),
            Secc.summe = mean(Secc, na.rm = T), 
            Secc.summed = median(Secc, na.rm = T), 
            Sal.summed = median(Sal, na.rm = T), 
            Sal.summax = max(Sal, na.rm = T),
            Sal.summe = mean(Sal, na.rm = T), 
            Temp.summin = min(Temp, na.rm = T), 
            Temp.summax = max(Temp, na.rm = T),
            Temp.summe = mean(Temp, na.rm = T), 
            Temp.summed = median(Temp, na.rm = T),
            TP.summe = mean(TP, na.rm = T), 
            TN.summe = mean(TN, na.rm = T)) %>% 
  ungroup() 
####Summarize Spring 
#Temp.spmed, Temp.spme,Chla.spme, Sal.spme, Secc.spme, TP.spme, TP.spmed, TN.spme
WIP.woland_springPP = WIP.woland_ProjPrep %>%  
  filter(dplyr::between(month.x, 3, 6)) %>% #late june isnt spring.... 
  group_by(Station, year.x, year.y) %>% 
  summarise(Chla.spme = mean(Chla, na.rm = T), 
            Secc.spme = mean(Secc, na.rm = T), 
            Sal.spme = mean(Sal, na.rm = T), 
            Temp.spme = mean(Temp, na.rm = T), 
            Temp.spmed = median(Temp, na.rm = T),
            TP.spme = mean(TP, na.rm = T), 
            TP.spmed = median(TP, na.rm = T), 
            TN.spme = mean(TN, na.rm = T)) %>% 
  ungroup()

#run these next 4 DFs to get the CC.wland_OneFuture Iteration happens here
####twentyone code####
twentyone = CBP.WQ_69vars %>% filter(year == "2020") %>%
  select(STATION, year, Temp.summed, Temp.summe, Sal.summax) %>% #im just selecting the ones we need
  group_by(STATION) %>%
  #slice_sample(n = 1 , replace = T) %>% #pick a random year
  mutate(Year = case_when(year == 2020 ~ 2031)) %>% #change year, bc y being changed to y1
  rename(Temp.sumy1med = Temp.summed, Temp.sumy1me = Temp.summe, Sal.sumy1max = Sal.summax) %>%
  rename(Station = STATION) %>%
  select(Station, Year, everything()) %>% select(-year) %>% 
  replace_na(list(Temp.sumy1me = 31.2550, Temp.sumy1med = 31.2550, Sal.sumy1max = 0.001)) %>% 
  ungroup() 

####Create WIP.woland_AllFuture####
WIP.woland_AllFutures = left_join(WIP.woland_summerPP, WIP.woland_springPP) %>% 
  select(-year.y) %>% 
  rename("Year" = "year.x")

write.csv(WIP.woland_AllFutures, "/Volumes/savshare2/Current Projects/Predicting-SAV/data/Multiversal Futures/WIP.woland_AllFutures.csv")
WIP.woland_AllFutures = vroom("/Volumes/savshare2/Current Projects/Predicting-SAV/data/Multiversal Futures/WIP.woland_AllFutures.csv")

####Create WIP.woland_OneFuture####
WIP.woland_OneFuture.no2031 = WIP.woland_AllFutures %>% 
  group_by(Station, Year) %>% 
  slice_sample(., n = 1, replace = T) %>% #randomly select a year, will give a timeline of 40 years bc grouped by Year
  group_by(Station) %>%
  mutate(Sal.sumy1max = lag(Sal.summax, n = 1, order_by = Year), 
         Temp.sumy1me = lag(Temp.summe, n = 1, order_by = Year), 
         Temp.sumy1med = lag(Temp.summed, n = 1, order_by = Year))

WIP.woland_One2031 = WIP.woland_OneFuture.no2021 %>% filter(Year == "2031") %>% 
  select(-Sal.sumy1max, -Temp.sumy1me, -Temp.sumy1med) %>%
  full_join(twentyone) 

WIP.woland_OneFuture = WIP.woland_OneFuture.no2031 %>% 
  filter(!Year == "2031") %>%
  bind_rows(WIP.woland_One2031) %>% 
  ungroup() %>%
  arrange(Station, Year)



###OBSOLETE CODE after 1/26######

#Build Temporal variable DFs, reminder that 69 Vars has longer grow and spring####
#baseline_1990-2000####
baselinesummer = baseline %>% 
  filter(dplyr::between(Month, 5, 8)) %>%
  group_by(Year, Station) %>% 
  summarise(Chla.summax = max(Chla, na.rm = T), Chla.summin = min(Chla, na.rm = T), 
            Chla.summe = mean(Chla, na.rm = T), 
            Secc.summax = max(Secc, na.rm = T), Secc.summin = min(Secc, na.rm = T), 
            Secc.summe = mean(Secc, na.rm = T), 
            Sal.summax = max(Sal, na.rm = T), Sal.summin = min(Sal, na.rm = T), 
            Sal.summe = mean(Sal, na.rm = T), 
            Temp.summax = max(Temp, na.rm = T), Temp.summin = min(Temp, na.rm = T), 
            Temp.summe = mean(Temp, na.rm = T), 
            TP.summax = max(TP, na.rm = T), TP.summin = min(TP, na.rm = T), 
            TP.summe = mean(TP, na.rm = T), 
            TN.summax = max(TN, na.rm = T), TN.summin = min(TN, na.rm = T), 
            TN.summe = mean(TN, na.rm = T), 
            Depth.summax = max(Depth, na.rm = T), Depth.summin = min(Depth, na.rm = T), 
            Depth.summe = mean(Depth, na.rm = T), 
            Depth.summed = median(Depth, na.rm = T),TN.summed = median(TN, na.rm = T),
            TP.summed = median(TP, na.rm = T),Temp.summed = median(Temp, na.rm = T),
            Sal.summed = median(Sal, na.rm = T),Chla.summed = median(Chla, na.rm = T), 
            Secc.summed = median(Secc, na.rm = T)) %>% 
  ungroup() %>%
  group_by(Station) %>%
  mutate(Chla.sumy1max = lag(Chla.summax), Chla.sumy1min = lag(Chla.summin), 
         Chla.sumy1me = lag(Chla.summe), 
         Secc.sumy1max = lag(Secc.summax), Secc.sumy1min = lag(Secc.summin), 
         Secc.sumy1me = lag(Secc.summe), 
         Sal.sumy1max = lag(Sal.summax), Sal.sumy1min = lag(Sal.summin), 
         Sal.sumy1me = lag(Sal.summe), 
         Temp.sumy1max = lag(Temp.summax), Temp.sumy1min = lag(Temp.summin), 
         Temp.sumy1me = lag(Temp.summe), 
         TN.sumy1max = lag(TN.summax), TN.sumy1min = lag(TN.summin), 
         TN.sumy1me = lag(TN.summe), 
         TP.sumy1max = lag(TP.summax), TP.sumy1min = lag(TP.summin), 
         TP.sumy1me = lag(TP.summe), 
         Depth.sumy1max = lag(Depth.summax), Depth.sumy1min = lag(Depth.summin), 
         Depth.sumy1me = lag(Depth.summe), 
         Depth.sumy1med = lag(Depth.summed),TN.sumy1med = lag(TN.summed),
         TP.sumy1med= lag(TP.summed), Temp.sumy1med = lag(Temp.summed),
         Sal.sumy1med = lag(Sal.summed), Chla.sumy1med = lag(Sal.summed), 
         Secc.sumy1med = lag(Secc.summed))

###Summarize growing season
baselinegrow = baseline %>% 
  filter(between(Month, 3, 8)) %>%
  group_by(Year, Station) %>% 
  summarise(Chla.growmax = max(Chla, na.rm = T), Chla.growmin = min(Chla, na.rm = T), 
            Chla.growme = mean(Chla, na.rm = T), 
            Secc.growmax = max(Secc, na.rm = T), Secc.growmin = min(Secc, na.rm = T), 
            Secc.growme = mean(Secc, na.rm = T), 
            Sal.growmax = max(Sal, na.rm = T), Sal.growmin = min(Sal, na.rm = T), 
            Sal.growme = mean(Sal, na.rm = T), 
            Temp.growmax = max(Temp, na.rm = T), Temp.growmin = min(Temp, na.rm = T), 
            Temp.growme = mean(Temp, na.rm = T), 
            TP.growmax = max(TP, na.rm = T), TP.growmin = min(TP, na.rm = T), 
            TP.growme = mean(TP, na.rm = T), 
            TN.growmax = max(TN, na.rm = T), TN.growmin = min(TN, na.rm = T), 
            TN.growme = mean(TN, na.rm = T),  TN.growmed = median(TN, na.rm = T),
            TP.growmed = median(TP, na.rm = T),Temp.growmed = median(Temp, na.rm = T),
            Sal.growmed = median(Sal, na.rm = T),Chla.growmed = median(Chla, na.rm = T), 
            Secc.growmed = median(Secc, na.rm = T), 
            Depth.growmax = max(Depth, na.rm = T), Depth.growmin = min(Depth, na.rm = T), 
            Depth.growme= mean(Depth, na.rm = T), Depth.growmed = median(Depth, na.rm = T)) %>% 
  ungroup() %>% 
  group_by(Station) %>%
  mutate(Chla.growy1max = lag(Chla.growmax), Chla.growy1min = lag(Chla.growmin), 
         Chla.growy1me = lag(Chla.growme), 
         Secc.growy1max = lag(Secc.growmax), Secc.growy1min = lag(Secc.growmin), 
         Secc.growy1me = lag(Secc.growme), 
         Sal.growy1max = lag(Sal.growmax), Sal.growy1min = lag(Sal.growmin), 
         Sal.growy1me = lag(Sal.growme), 
         Temp.growy1max = lag(Temp.growmax), Temp.growy1min = lag(Temp.growmin), 
         Temp.growy1me = lag(Temp.growme), 
         TN.growy1max = lag(TN.growmax), TN.growy1min = lag(TN.growmin), 
         TN.growy1me = lag(TN.growme), 
         TP.growy1max = lag(TP.growmax), TP.growy1min = lag(TP.growmin), 
         TP.growy1me = lag(TP.growme), TN.growy1med = lag(TN.growmed),
         TP.growy1med= lag(TP.growmed), Temp.growy1med = lag(Temp.growmed),
         Sal.growy1med = lag(Sal.growmed), Chla.growy1med = lag(Sal.growmed), 
         Secc.growy1med = lag(Secc.growmed), 
         Depth.growy1max = lag(Depth.growmax), Depth.growy1min = lag(Depth.growmin), 
         Depth.growy1me = lag(Depth.growme), Depth.growy1med = lag(Depth.growmed)) %>% ungroup() %>%
  select(Year, Station, Chla.growy1max:Depth.growy1med) #we dont use the grow data. its too soon

####Summarize Spring
baselinesp = baseline %>% 
  filter(between(Month, 3, 6)) %>%
  group_by(Year, Station) %>% 
  summarise(Chla.spmax = max(Chla, na.rm = T), Chla.spmin = min(Chla, na.rm = T), 
            Chla.spme = mean(Chla, na.rm = T), 
            Secc.spmax = max(Secc, na.rm = T), Secc.spmin = min(Secc, na.rm = T), 
            Secc.spme = mean(Secc, na.rm = T), 
            Sal.spmax = max(Sal, na.rm = T), Sal.spmin = min(Sal, na.rm = T), 
            Sal.spme = mean(Sal, na.rm = T), 
            Temp.spmax = max(Temp, na.rm = T), Temp.spmin = min(Temp, na.rm = T), 
            Temp.spme = mean(Temp, na.rm = T), 
            TP.spmax = max(TP, na.rm = T), TP.spmin = min(TP, na.rm = T), 
            TP.spme = mean(TP, na.rm = T), 
            TN.spmax = max(TN, na.rm = T), TN.spmin = min(TN, na.rm = T), 
            TN.spme = mean(TN, na.rm = T), TN.spmed = median(TN, na.rm = T),
            TP.spmed = median(TP, na.rm = T),Temp.spmed = median(Temp, na.rm = T),
            Sal.spmed = median(Sal, na.rm = T),Chla.spmed = median(Chla, na.rm = T), 
            Secc.spmed = median(Secc, na.rm = T), 
            Depth.spmax = max(Depth, na.rm = T), Depth.spmin = min(Depth, na.rm = T), 
            Depth.spme = mean(Depth, na.rm = T), Depth.spmed = median(Depth, na.rm = T)) %>% 
  ungroup()

##Join 69 vars together##

baseline_69vars = full_join(baselinegrow, baselinesummer) %>% full_join(baselinesp) #%>%
#select(-Chla.y1me, -TN.y1med, -TN.spmed, -TN.sumy1me, -TN.sumy1med, -TN.growmed, -TN.growy1med, -TN.growy1min, -TN.growy1max, -TN.growy1me, -TP.growy1med, -Secc.growy1med, -Chla.growy1med, -TN.summed, -TN.summax, -TN.summin) #removing some cols w over 400 NAs
#mutate(STATION = replace(STATION, STATION == "LE5.5", "LE5.5-W"))

#I recommend using this code to standardize NAs
is.na(baseline_69vars) <- baseline_69vars == "NaN"
is.na(baseline_69vars) <- baseline_69vars == "Inf"
is.na(baseline_69vars) <- baseline_69vars == "-Inf"

colSums(is.na(baseline_69vars))

write_csv(baseline_69vars, "/Volumes/savshare2/Current Projects/Predicting-SAV/data/baseline_69vars.csv")


#CC.wland_2021_2060####
#summarize Summer
CC.wland_2021_2060summer = CC.wland_2021_2060 %>% 
  filter(dplyr::between(Month, 5, 8)) %>%
  group_by(Year, Station) %>% 
  summarise(Chla.summax = max(Chla, na.rm = T), Chla.summin = min(Chla, na.rm = T), 
            Chla.summe = mean(Chla, na.rm = T), 
            Secc.summax = max(Secc, na.rm = T), Secc.summin = min(Secc, na.rm = T), 
            Secc.summe = mean(Secc, na.rm = T), 
            Sal.summax = max(Sal, na.rm = T), Sal.summin = min(Sal, na.rm = T), 
            Sal.summe = mean(Sal, na.rm = T), 
            Temp.summax = max(Temp, na.rm = T), Temp.summin = min(Temp, na.rm = T), 
            Temp.summe = mean(Temp, na.rm = T), 
            TP.summax = max(TP, na.rm = T), TP.summin = min(TP, na.rm = T), 
            TP.summe = mean(TP, na.rm = T), 
            TN.summax = max(TN, na.rm = T), TN.summin = min(TN, na.rm = T), 
            TN.summe = mean(TN, na.rm = T), 
            Depth.summax = max(Depth, na.rm = T), Depth.summin = min(Depth, na.rm = T), 
            Depth.summe = mean(Depth, na.rm = T), 
            Depth.summed = median(Depth, na.rm = T),TN.summed = median(TN, na.rm = T),
            TP.summed = median(TP, na.rm = T),Temp.summed = median(Temp, na.rm = T),
            Sal.summed = median(Sal, na.rm = T),Chla.summed = median(Chla, na.rm = T), 
            Secc.summed = median(Secc, na.rm = T)) %>% 
  ungroup() %>%
  group_by(Station) %>%
  mutate(Chla.sumy1max = lag(Chla.summax), Chla.sumy1min = lag(Chla.summin), 
         Chla.sumy1me = lag(Chla.summe), 
         Secc.sumy1max = lag(Secc.summax), Secc.sumy1min = lag(Secc.summin), 
         Secc.sumy1me = lag(Secc.summe), 
         Sal.sumy1max = lag(Sal.summax), Sal.sumy1min = lag(Sal.summin), 
         Sal.sumy1me = lag(Sal.summe), 
         Temp.sumy1max = lag(Temp.summax), Temp.sumy1min = lag(Temp.summin), 
         Temp.sumy1me = lag(Temp.summe), 
         TN.sumy1max = lag(TN.summax), TN.sumy1min = lag(TN.summin), 
         TN.sumy1me = lag(TN.summe), 
         TP.sumy1max = lag(TP.summax), TP.sumy1min = lag(TP.summin), 
         TP.sumy1me = lag(TP.summe), 
         Depth.sumy1max = lag(Depth.summax), Depth.sumy1min = lag(Depth.summin), 
         Depth.sumy1me = lag(Depth.summe), 
         Depth.sumy1med = lag(Depth.summed),TN.sumy1med = lag(TN.summed),
         TP.sumy1med= lag(TP.summed), Temp.sumy1med = lag(Temp.summed),
         Sal.sumy1med = lag(Sal.summed), Chla.sumy1med = lag(Sal.summed), 
         Secc.sumy1med = lag(Secc.summed))

###Summarize growing season
CC.wland_2021_2060grow = CC.wland_2021_2060 %>% 
  filter(between(Month, 3, 8)) %>%
  group_by(Year, Station) %>% 
  summarise(Chla.growmax = max(Chla, na.rm = T), Chla.growmin = min(Chla, na.rm = T), 
            Chla.growme = mean(Chla, na.rm = T), 
            Secc.growmax = max(Secc, na.rm = T), Secc.growmin = min(Secc, na.rm = T), 
            Secc.growme = mean(Secc, na.rm = T), 
            Sal.growmax = max(Sal, na.rm = T), Sal.growmin = min(Sal, na.rm = T), 
            Sal.growme = mean(Sal, na.rm = T), 
            Temp.growmax = max(Temp, na.rm = T), Temp.growmin = min(Temp, na.rm = T), 
            Temp.growme = mean(Temp, na.rm = T), 
            TP.growmax = max(TP, na.rm = T), TP.growmin = min(TP, na.rm = T), 
            TP.growme = mean(TP, na.rm = T), 
            TN.growmax = max(TN, na.rm = T), TN.growmin = min(TN, na.rm = T), 
            TN.growme = mean(TN, na.rm = T),  TN.growmed = median(TN, na.rm = T),
            TP.growmed = median(TP, na.rm = T),Temp.growmed = median(Temp, na.rm = T),
            Sal.growmed = median(Sal, na.rm = T),Chla.growmed = median(Chla, na.rm = T), 
            Secc.growmed = median(Secc, na.rm = T), 
            Depth.growmax = max(Depth, na.rm = T), Depth.growmin = min(Depth, na.rm = T), 
            Depth.growme= mean(Depth, na.rm = T), Depth.growmed = median(Depth, na.rm = T)) %>% 
  ungroup() %>% 
  group_by(Station) %>%
  mutate(Chla.growy1max = lag(Chla.growmax), Chla.growy1min = lag(Chla.growmin), 
         Chla.growy1me = lag(Chla.growme), 
         Secc.growy1max = lag(Secc.growmax), Secc.growy1min = lag(Secc.growmin), 
         Secc.growy1me = lag(Secc.growme), 
         Sal.growy1max = lag(Sal.growmax), Sal.growy1min = lag(Sal.growmin), 
         Sal.growy1me = lag(Sal.growme), 
         Temp.growy1max = lag(Temp.growmax), Temp.growy1min = lag(Temp.growmin), 
         Temp.growy1me = lag(Temp.growme), 
         TN.growy1max = lag(TN.growmax), TN.growy1min = lag(TN.growmin), 
         TN.growy1me = lag(TN.growme), 
         TP.growy1max = lag(TP.growmax), TP.growy1min = lag(TP.growmin), 
         TP.growy1me = lag(TP.growme), TN.growy1med = lag(TN.growmed),
         TP.growy1med= lag(TP.growmed), Temp.growy1med = lag(Temp.growmed),
         Sal.growy1med = lag(Sal.growmed), Chla.growy1med = lag(Sal.growmed), 
         Secc.growy1med = lag(Secc.growmed), 
         Depth.growy1max = lag(Depth.growmax), Depth.growy1min = lag(Depth.growmin), 
         Depth.growy1me = lag(Depth.growme), Depth.growy1med = lag(Depth.growmed)) %>% ungroup() %>%
  select(Year, Station, everything())


####Summarize Spring
CC.wland_2021_2060sp = CC.wland_2021_2060 %>% 
  filter(between(Month, 3, 6)) %>%
  group_by(Year, Station) %>% 
  summarise(Chla.spmax = max(Chla, na.rm = T), Chla.spmin = min(Chla, na.rm = T), 
            Chla.spme = mean(Chla, na.rm = T), 
            Secc.spmax = max(Secc, na.rm = T), Secc.spmin = min(Secc, na.rm = T), 
            Secc.spme = mean(Secc, na.rm = T), 
            Sal.spmax = max(Sal, na.rm = T), Sal.spmin = min(Sal, na.rm = T), 
            Sal.spme = mean(Sal, na.rm = T), 
            Temp.spmax = max(Temp, na.rm = T), Temp.spmin = min(Temp, na.rm = T), 
            Temp.spme = mean(Temp, na.rm = T), 
            TP.spmax = max(TP, na.rm = T), TP.spmin = min(TP, na.rm = T), 
            TP.spme = mean(TP, na.rm = T), 
            TN.spmax = max(TN, na.rm = T), TN.spmin = min(TN, na.rm = T), 
            TN.spme = mean(TN, na.rm = T), TN.spmed = median(TN, na.rm = T),
            TP.spmed = median(TP, na.rm = T),Temp.spmed = median(Temp, na.rm = T),
            Sal.spmed = median(Sal, na.rm = T),Chla.spmed = median(Chla, na.rm = T), 
            Secc.spmed = median(Secc, na.rm = T), 
            Depth.spmax = max(Depth, na.rm = T), Depth.spmin = min(Depth, na.rm = T), 
            Depth.spme = mean(Depth, na.rm = T), Depth.spmed = median(Depth, na.rm = T)) %>% 
  ungroup()

##Join 69 vars together##

CC.wland_2021_2060_69vars = full_join(CC.wland_2021_2060grow, CC.wland_2021_2060summer) %>% full_join(CC.wland_2021_2060sp) #%>%
#select(-Chla.y1me, -TN.y1med, -TN.spmed, -TN.sumy1me, -TN.sumy1med, -TN.growmed, -TN.growy1med, -TN.growy1min, -TN.growy1max, -TN.growy1me, -TP.growy1med, -Secc.growy1med, -Chla.growy1med, -TN.summed, -TN.summax, -TN.summin) #removing some cols w over 400 NAs
#mutate(STATION = replace(STATION, STATION == "LE5.5", "LE5.5-W"))

#I recommend using this code to standardize NAs
is.na(CC.wland_2021_2060_69vars) <- CC.wland_2021_2060_69vars == "NaN"
is.na(CC.wland_2021_2060_69vars) <- CC.wland_2021_2060_69vars == "Inf"
is.na(CC.wland_2021_2060_69vars) <- CC.wland_2021_2060_69vars == "-Inf"

colSums(is.na(CC.wland_2021_2060_69vars))

write_csv(CC.wland_2021_2060_69vars, "/Volumes/savshare2/Current Projects/Predicting-SAV/data/CC.wland_2021_2060_69vars.csv")


#WIP.wland_2021_2060####
#summarize Summer
WIP.wland_2021_2060summer = WIP.wland_2021_2060 %>% 
  filter(dplyr::between(Month, 5, 8)) %>%
  group_by(Year, Station) %>% 
  summarise(Chla.summax = max(Chla, na.rm = T), Chla.summin = min(Chla, na.rm = T), 
            Chla.summe = mean(Chla, na.rm = T), 
            Secc.summax = max(Secc, na.rm = T), Secc.summin = min(Secc, na.rm = T), 
            Secc.summe = mean(Secc, na.rm = T), 
            Sal.summax = max(Sal, na.rm = T), Sal.summin = min(Sal, na.rm = T), 
            Sal.summe = mean(Sal, na.rm = T), 
            Temp.summax = max(Temp, na.rm = T), Temp.summin = min(Temp, na.rm = T), 
            Temp.summe = mean(Temp, na.rm = T), 
            TP.summax = max(TP, na.rm = T), TP.summin = min(TP, na.rm = T), 
            TP.summe = mean(TP, na.rm = T), 
            TN.summax = max(TN, na.rm = T), TN.summin = min(TN, na.rm = T), 
            TN.summe = mean(TN, na.rm = T), 
            Depth.summax = max(Depth, na.rm = T), Depth.summin = min(Depth, na.rm = T), 
            Depth.summe = mean(Depth, na.rm = T), 
            Depth.summed = median(Depth, na.rm = T),TN.summed = median(TN, na.rm = T),
            TP.summed = median(TP, na.rm = T),Temp.summed = median(Temp, na.rm = T),
            Sal.summed = median(Sal, na.rm = T),Chla.summed = median(Chla, na.rm = T), 
            Secc.summed = median(Secc, na.rm = T)) %>% 
  ungroup() %>%
  group_by(Station) %>%
  mutate(Chla.sumy1max = lag(Chla.summax), Chla.sumy1min = lag(Chla.summin), 
         Chla.sumy1me = lag(Chla.summe), 
         Secc.sumy1max = lag(Secc.summax), Secc.sumy1min = lag(Secc.summin), 
         Secc.sumy1me = lag(Secc.summe), 
         Sal.sumy1max = lag(Sal.summax), Sal.sumy1min = lag(Sal.summin), 
         Sal.sumy1me = lag(Sal.summe), 
         Temp.sumy1max = lag(Temp.summax), Temp.sumy1min = lag(Temp.summin), 
         Temp.sumy1me = lag(Temp.summe), 
         TN.sumy1max = lag(TN.summax), TN.sumy1min = lag(TN.summin), 
         TN.sumy1me = lag(TN.summe), 
         TP.sumy1max = lag(TP.summax), TP.sumy1min = lag(TP.summin), 
         TP.sumy1me = lag(TP.summe), 
         Depth.sumy1max = lag(Depth.summax), Depth.sumy1min = lag(Depth.summin), 
         Depth.sumy1me = lag(Depth.summe), 
         Depth.sumy1med = lag(Depth.summed),TN.sumy1med = lag(TN.summed),
         TP.sumy1med= lag(TP.summed), Temp.sumy1med = lag(Temp.summed),
         Sal.sumy1med = lag(Sal.summed), Chla.sumy1med = lag(Sal.summed), 
         Secc.sumy1med = lag(Secc.summed))

###Summarize growing season
WIP.wland_2021_2060grow = WIP.wland_2021_2060 %>% 
  filter(between(Month, 3, 8)) %>%
  group_by(Year, Station) %>% 
  summarise(Chla.growmax = max(Chla, na.rm = T), Chla.growmin = min(Chla, na.rm = T), 
            Chla.growme = mean(Chla, na.rm = T), 
            Secc.growmax = max(Secc, na.rm = T), Secc.growmin = min(Secc, na.rm = T), 
            Secc.growme = mean(Secc, na.rm = T), 
            Sal.growmax = max(Sal, na.rm = T), Sal.growmin = min(Sal, na.rm = T), 
            Sal.growme = mean(Sal, na.rm = T), 
            Temp.growmax = max(Temp, na.rm = T), Temp.growmin = min(Temp, na.rm = T), 
            Temp.growme = mean(Temp, na.rm = T), 
            TP.growmax = max(TP, na.rm = T), TP.growmin = min(TP, na.rm = T), 
            TP.growme = mean(TP, na.rm = T), 
            TN.growmax = max(TN, na.rm = T), TN.growmin = min(TN, na.rm = T), 
            TN.growme = mean(TN, na.rm = T),  TN.growmed = median(TN, na.rm = T),
            TP.growmed = median(TP, na.rm = T),Temp.growmed = median(Temp, na.rm = T),
            Sal.growmed = median(Sal, na.rm = T),Chla.growmed = median(Chla, na.rm = T), 
            Secc.growmed = median(Secc, na.rm = T), 
            Depth.growmax = max(Depth, na.rm = T), Depth.growmin = min(Depth, na.rm = T), 
            Depth.growme= mean(Depth, na.rm = T), Depth.growmed = median(Depth, na.rm = T)) %>% 
  ungroup() %>% 
  group_by(Station) %>%
  mutate(Chla.growy1max = lag(Chla.growmax), Chla.growy1min = lag(Chla.growmin), 
         Chla.growy1me = lag(Chla.growme), 
         Secc.growy1max = lag(Secc.growmax), Secc.growy1min = lag(Secc.growmin), 
         Secc.growy1me = lag(Secc.growme), 
         Sal.growy1max = lag(Sal.growmax), Sal.growy1min = lag(Sal.growmin), 
         Sal.growy1me = lag(Sal.growme), 
         Temp.growy1max = lag(Temp.growmax), Temp.growy1min = lag(Temp.growmin), 
         Temp.growy1me = lag(Temp.growme), 
         TN.growy1max = lag(TN.growmax), TN.growy1min = lag(TN.growmin), 
         TN.growy1me = lag(TN.growme), 
         TP.growy1max = lag(TP.growmax), TP.growy1min = lag(TP.growmin), 
         TP.growy1me = lag(TP.growme), TN.growy1med = lag(TN.growmed),
         TP.growy1med= lag(TP.growmed), Temp.growy1med = lag(Temp.growmed),
         Sal.growy1med = lag(Sal.growmed), Chla.growy1med = lag(Sal.growmed), 
         Secc.growy1med = lag(Secc.growmed), 
         Depth.growy1max = lag(Depth.growmax), Depth.growy1min = lag(Depth.growmin), 
         Depth.growy1me = lag(Depth.growme), Depth.growy1med = lag(Depth.growmed)) %>% ungroup() %>%
  select(Year, Station, everything())

####Summarize Spring
WIP.wland_2021_2060sp = WIP.wland_2021_2060 %>% 
  filter(between(Month, 3, 6)) %>%
  group_by(Year, Station) %>% 
  summarise(Chla.spmax = max(Chla, na.rm = T), Chla.spmin = min(Chla, na.rm = T), 
            Chla.spme = mean(Chla, na.rm = T), 
            Secc.spmax = max(Secc, na.rm = T), Secc.spmin = min(Secc, na.rm = T), 
            Secc.spme = mean(Secc, na.rm = T), 
            Sal.spmax = max(Sal, na.rm = T), Sal.spmin = min(Sal, na.rm = T), 
            Sal.spme = mean(Sal, na.rm = T), 
            Temp.spmax = max(Temp, na.rm = T), Temp.spmin = min(Temp, na.rm = T), 
            Temp.spme = mean(Temp, na.rm = T), 
            TP.spmax = max(TP, na.rm = T), TP.spmin = min(TP, na.rm = T), 
            TP.spme = mean(TP, na.rm = T), 
            TN.spmax = max(TN, na.rm = T), TN.spmin = min(TN, na.rm = T), 
            TN.spme = mean(TN, na.rm = T), TN.spmed = median(TN, na.rm = T),
            TP.spmed = median(TP, na.rm = T),Temp.spmed = median(Temp, na.rm = T),
            Sal.spmed = median(Sal, na.rm = T),Chla.spmed = median(Chla, na.rm = T), 
            Secc.spmed = median(Secc, na.rm = T), 
            Depth.spmax = max(Depth, na.rm = T), Depth.spmin = min(Depth, na.rm = T), 
            Depth.spme = mean(Depth, na.rm = T), Depth.spmed = median(Depth, na.rm = T)) %>% 
  ungroup()

##Join 69 vars together#

WIP.wland_2021_2060_69vars = full_join(WIP.wland_2021_2060grow, WIP.wland_2021_2060summer) %>% full_join(WIP.wland_2021_2060sp) #%>%
#select(-Chla.y1me, -TN.y1med, -TN.spmed, -TN.sumy1me, -TN.sumy1med, -TN.growmed, -TN.growy1med, -TN.growy1min, -TN.growy1max, -TN.growy1me, -TP.growy1med, -Secc.growy1med, -Chla.growy1med, -TN.summed, -TN.summax, -TN.summin) #removing some cols w over 400 NAs
#mutate(STATION = replace(STATION, STATION == "LE5.5", "LE5.5-W"))

#I recommend using this code to standardize NAs
is.na(WIP.wland_2021_2060_69vars) <- WIP.wland_2021_2060_69vars == "NaN"
is.na(WIP.wland_2021_2060_69vars) <- WIP.wland_2021_2060_69vars == "Inf"
is.na(WIP.wland_2021_2060_69vars) <- WIP.wland_2021_2060_69vars == "-Inf"

colSums(is.na(WIP.wland_2021_2060_69vars))

write_csv(WIP.wland_2021_2060_69vars, "/Volumes/savshare2/Current Projects/Predicting-SAV/data/WIP.wland_2021_2060_69vars.csv")


#WIP.woland_2031_2060####
#summarize Summer
WIP.woland_2031_2060summer = WIP.woland_2031_2060 %>% 
  filter(dplyr::between(Month, 5, 8)) %>%
  group_by(Year, Station) %>% 
  summarise(Chla.summax = max(Chla, na.rm = T), Chla.summin = min(Chla, na.rm = T), 
            Chla.summe = mean(Chla, na.rm = T), 
            Secc.summax = max(Secc, na.rm = T), Secc.summin = min(Secc, na.rm = T), 
            Secc.summe = mean(Secc, na.rm = T), 
            Sal.summax = max(Sal, na.rm = T), Sal.summin = min(Sal, na.rm = T), 
            Sal.summe = mean(Sal, na.rm = T), 
            Temp.summax = max(Temp, na.rm = T), Temp.summin = min(Temp, na.rm = T), 
            Temp.summe = mean(Temp, na.rm = T), 
            TP.summax = max(TP, na.rm = T), TP.summin = min(TP, na.rm = T), 
            TP.summe = mean(TP, na.rm = T), 
            TN.summax = max(TN, na.rm = T), TN.summin = min(TN, na.rm = T), 
            TN.summe = mean(TN, na.rm = T), 
            Depth.summax = max(Depth, na.rm = T), Depth.summin = min(Depth, na.rm = T), 
            Depth.summe = mean(Depth, na.rm = T), 
            Depth.summed = median(Depth, na.rm = T),TN.summed = median(TN, na.rm = T),
            TP.summed = median(TP, na.rm = T),Temp.summed = median(Temp, na.rm = T),
            Sal.summed = median(Sal, na.rm = T),Chla.summed = median(Chla, na.rm = T), 
            Secc.summed = median(Secc, na.rm = T)) %>% 
  ungroup() %>%
  group_by(Station) %>%
  mutate(Chla.sumy1max = lag(Chla.summax), Chla.sumy1min = lag(Chla.summin), 
         Chla.sumy1me = lag(Chla.summe), 
         Secc.sumy1max = lag(Secc.summax), Secc.sumy1min = lag(Secc.summin), 
         Secc.sumy1me = lag(Secc.summe), 
         Sal.sumy1max = lag(Sal.summax), Sal.sumy1min = lag(Sal.summin), 
         Sal.sumy1me = lag(Sal.summe), 
         Temp.sumy1max = lag(Temp.summax), Temp.sumy1min = lag(Temp.summin), 
         Temp.sumy1me = lag(Temp.summe), 
         TN.sumy1max = lag(TN.summax), TN.sumy1min = lag(TN.summin), 
         TN.sumy1me = lag(TN.summe), 
         TP.sumy1max = lag(TP.summax), TP.sumy1min = lag(TP.summin), 
         TP.sumy1me = lag(TP.summe), 
         Depth.sumy1max = lag(Depth.summax), Depth.sumy1min = lag(Depth.summin), 
         Depth.sumy1me = lag(Depth.summe), 
         Depth.sumy1med = lag(Depth.summed),TN.sumy1med = lag(TN.summed),
         TP.sumy1med= lag(TP.summed), Temp.sumy1med = lag(Temp.summed),
         Sal.sumy1med = lag(Sal.summed), Chla.sumy1med = lag(Sal.summed), 
         Secc.sumy1med = lag(Secc.summed))

###Summarize growing season
WIP.woland_2031_2060grow = WIP.woland_2031_2060 %>% 
  filter(between(Month, 3, 8)) %>%
  group_by(Year, Station) %>% 
  summarise(Chla.growmax = max(Chla, na.rm = T), Chla.growmin = min(Chla, na.rm = T), 
            Chla.growme = mean(Chla, na.rm = T), 
            Secc.growmax = max(Secc, na.rm = T), Secc.growmin = min(Secc, na.rm = T), 
            Secc.growme = mean(Secc, na.rm = T), 
            Sal.growmax = max(Sal, na.rm = T), Sal.growmin = min(Sal, na.rm = T), 
            Sal.growme = mean(Sal, na.rm = T), 
            Temp.growmax = max(Temp, na.rm = T), Temp.growmin = min(Temp, na.rm = T), 
            Temp.growme = mean(Temp, na.rm = T), 
            TP.growmax = max(TP, na.rm = T), TP.growmin = min(TP, na.rm = T), 
            TP.growme = mean(TP, na.rm = T), 
            TN.growmax = max(TN, na.rm = T), TN.growmin = min(TN, na.rm = T), 
            TN.growme = mean(TN, na.rm = T),  TN.growmed = median(TN, na.rm = T),
            TP.growmed = median(TP, na.rm = T),Temp.growmed = median(Temp, na.rm = T),
            Sal.growmed = median(Sal, na.rm = T),Chla.growmed = median(Chla, na.rm = T), 
            Secc.growmed = median(Secc, na.rm = T), 
            Depth.growmax = max(Depth, na.rm = T), Depth.growmin = min(Depth, na.rm = T), 
            Depth.growme= mean(Depth, na.rm = T), Depth.growmed = median(Depth, na.rm = T)) %>% 
  ungroup() %>% 
  group_by(Station) %>%
  mutate(Chla.growy1max = lag(Chla.growmax), Chla.growy1min = lag(Chla.growmin), 
         Chla.growy1me = lag(Chla.growme), 
         Secc.growy1max = lag(Secc.growmax), Secc.growy1min = lag(Secc.growmin), 
         Secc.growy1me = lag(Secc.growme), 
         Sal.growy1max = lag(Sal.growmax), Sal.growy1min = lag(Sal.growmin), 
         Sal.growy1me = lag(Sal.growme), 
         Temp.growy1max = lag(Temp.growmax), Temp.growy1min = lag(Temp.growmin), 
         Temp.growy1me = lag(Temp.growme), 
         TN.growy1max = lag(TN.growmax), TN.growy1min = lag(TN.growmin), 
         TN.growy1me = lag(TN.growme), 
         TP.growy1max = lag(TP.growmax), TP.growy1min = lag(TP.growmin), 
         TP.growy1me = lag(TP.growme), TN.growy1med = lag(TN.growmed),
         TP.growy1med= lag(TP.growmed), Temp.growy1med = lag(Temp.growmed),
         Sal.growy1med = lag(Sal.growmed), Chla.growy1med = lag(Sal.growmed), 
         Secc.growy1med = lag(Secc.growmed), 
         Depth.growy1max = lag(Depth.growmax), Depth.growy1min = lag(Depth.growmin), 
         Depth.growy1me = lag(Depth.growme), Depth.growy1med = lag(Depth.growmed)) %>% ungroup() %>%
  select(Year, Station, everything())

####Summarize Spring
WIP.woland_2031_2060sp = WIP.woland_2031_2060 %>% 
  filter(between(Month, 3, 6)) %>%
  group_by(Year, Station) %>% 
  summarise(Chla.spmax = max(Chla, na.rm = T), Chla.spmin = min(Chla, na.rm = T), 
            Chla.spme = mean(Chla, na.rm = T), 
            Secc.spmax = max(Secc, na.rm = T), Secc.spmin = min(Secc, na.rm = T), 
            Secc.spme = mean(Secc, na.rm = T), 
            Sal.spmax = max(Sal, na.rm = T), Sal.spmin = min(Sal, na.rm = T), 
            Sal.spme = mean(Sal, na.rm = T), 
            Temp.spmax = max(Temp, na.rm = T), Temp.spmin = min(Temp, na.rm = T), 
            Temp.spme = mean(Temp, na.rm = T), 
            TP.spmax = max(TP, na.rm = T), TP.spmin = min(TP, na.rm = T), 
            TP.spme = mean(TP, na.rm = T), 
            TN.spmax = max(TN, na.rm = T), TN.spmin = min(TN, na.rm = T), 
            TN.spme = mean(TN, na.rm = T), TN.spmed = median(TN, na.rm = T),
            TP.spmed = median(TP, na.rm = T),Temp.spmed = median(Temp, na.rm = T),
            Sal.spmed = median(Sal, na.rm = T),Chla.spmed = median(Chla, na.rm = T), 
            Secc.spmed = median(Secc, na.rm = T), 
            Depth.spmax = max(Depth, na.rm = T), Depth.spmin = min(Depth, na.rm = T), 
            Depth.spme = mean(Depth, na.rm = T), Depth.spmed = median(Depth, na.rm = T)) %>% 
  ungroup()

##Join 69 vars together#

WIP.woland_2031_2060_69vars = full_join(WIP.woland_2031_2060grow, WIP.woland_2031_2060summer) %>% full_join(WIP.woland_2031_2060sp) #%>%
#select(-Chla.y1me, -TN.y1med, -TN.spmed, -TN.sumy1me, -TN.sumy1med, -TN.growmed, -TN.growy1med, -TN.growy1min, -TN.growy1max, -TN.growy1me, -TP.growy1med, -Secc.growy1med, -Chla.growy1med, -TN.summed, -TN.summax, -TN.summin) #removing some cols w over 400 NAs
#mutate(STATION = replace(STATION, STATION == "LE5.5", "LE5.5-W"))

#I recommend using this code to standardize NAs
is.na(WIP.woland_2031_2060_69vars) <- WIP.woland_2031_2060_69vars == "NaN"
is.na(WIP.woland_2031_2060_69vars) <- WIP.woland_2031_2060_69vars == "Inf"
is.na(WIP.woland_2031_2060_69vars) <- WIP.woland_2031_2060_69vars == "-Inf"

colSums(is.na(WIP.woland_2031_2060_69vars))

write_csv(WIP.woland_2031_2060_69vars, "/Volumes/savshare2/Current Projects/Predicting-SAV/data/WIP.woland_2031_2060_69vars.csv")

#Vizualize this shit####

CC.wland_2021_2060_69vars = read.csv("/Volumes/savshare2/Current Projects/Predicting-SAV/data/CC.wland_2021_2060_69vars.csv")

WIP.wland_2021_2060_69vars = read.csv("/Volumes/savshare2/Current Projects/Predicting-SAV/data/WIP.wland_2021_2060_69vars.csv")

WIP.woland_2031_2060_69vars = read.csv("/Volumes/savshare2/Current Projects/Predicting-SAV/data/WIP.woland_2031_2060_69vars.csv")

CC.wland_2021_2060_69vars
WIP.wland_2021_2060_69vars
WIP.woland_2031_2060_69vars

temptime.cc = lmer(Temp.summax ~ Year + (1|Station), data = CC.wland_2021_2060_69vars)
temptime.wipw = lm(Temp.summax ~ Year, data = WIP.wland_2021_2060_69vars)
temptime.wipwo = lm(Temp.summax ~ Year, data = WIP.woland_2031_2060_69vars)

summary(temptime.cc)
car::Anova(temptime.cc)
car::Anova(temptime.wipw)
car::Anova(temptime.wipwo)

check_wip =
WIP.woland_2031_2060_69vars %>%
  select(Year, Station, Temp.summed, TN.spme, Chla.spme, Sal.growy1med, TP.spmax, Secc.growmax) %>%
  filter(Station %in% c("EE3.1", "LE5.4", "CB7.1")) %>%
  filter(Year %in% c(2025, 2030, 2035, 2040, 2045, 2050, 2055, 2060)) %>%
  group_by(Station) %>%
  summarise(across(Temp.summed:Secc.growmax, ~max(.x)-min(.x)))
          


ggplot() +
  stat_summary(data = CC.wland_2021_2060_69vars %>% filter(Station == "CB7.1"), aes(x = Year, y = TN.summe),
               geom = "smooth", fun.data = mean_se, color = "blue") +
  stat_summary(data = WIP.wland_2021_2060_69vars %>% filter(Station == "CB7.1"), aes(x = Year, y = TN.summe),
             geom = "smooth", fun.data = mean_se, color = "black") +
  stat_summary(data = WIP.woland_2031_2060_69vars %>% filter(Station == "CB7.1"), aes(x = Year, y = TN.summe),
               geom = "smooth", fun.data = mean_se, color = "red") +
  stat_summary(data = baseline_69vars %>% filter(Station == "CB7.1"), aes(x = Year, y = TN.summe),
               geom = "smooth", fun.data = mean_se, color = "purple") +
  stat_summary(data = CBP.WQ_69vars %>% filter(STATION == "CB7.1"), aes(x = year, y = TN.summe),
               geom = "smooth", fun.data = mean_se, color = "pink") +
  labs(x = "", y = expression("TN, summer mean, station CB7.1")) +
  theme(axis.line = element_line(colour = "black"), text = element_text(size=13),
        axis.text.x = element_text(angle=0, vjust=, hjust =.5, color = "black"),
        legend.position = "right", panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), panel.border = element_blank(),
        panel.background = element_blank())

ggplot() +
  geom_point(data = CC.wland_2021_2060_69vars %>% filter(Station == "CB7.1"), aes(x = Year, y = TN.summe), size = 1, color = "blue") +
  geom_point(data = WIP.wland_2021_2060_69vars %>% filter(Station == "CB7.1"), aes(x = Year, y = TN.summe), size = 1, color = "black") +
  geom_point(data = WIP.woland_2031_2060_69vars %>% filter(Station == "CB7.1"), aes(x = Year, y = TN.summe),size = 1, color = "red") +
  geom_point(data = baseline_69vars %>% filter(Station == "CB7.1"), aes(x = Year, y = TN.summe), size = 1, color = "purple") +
  geom_point(data = CBP.WQ_69vars %>% filter(STATION == "CB7.1"), aes(x = year, y = TN.summe),size = 1, color = "pink") +
  labs(x = "", y = expression("TN, summer mean, station CB7.1")) +
  theme(axis.line = element_line(colour = "black"), text = element_text(size=13),
        axis.text.x = element_text(angle=0, vjust=, hjust =.5, color = "black"),
        legend.position = "right", panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), panel.border = element_blank(),
        panel.background = element_blank())

ggplot() +
  stat_summary(data = CC.wland_2021_2060_69vars %>% filter(Station == "CB6.4"), aes(x = Year, y = Temp.summax),
               geom = "smooth", fun.data = mean_se, color = "blue") +
  stat_summary(data = WIP.wland_2021_2060_69vars %>% filter(Station == "CB6.4"), aes(x = Year, y = Temp.summax),
               geom = "smooth", fun.data = mean_se, color = "black") +
  stat_summary(data = WIP.woland_2031_2060_69vars %>% filter(Station == "CB6.4"), aes(x = Year, y = Temp.summax),
               geom = "smooth", fun.data = mean_se, color = "red") +
  stat_summary(data = baseline_69vars %>% filter(Station == "CB6.4"), aes(x = Year, y = Temp.summax),
               geom = "smooth", fun.data = mean_se, color = "purple") +
  stat_summary(data = CBP.WQ_69vars %>% filter(STATION == "CB6.4"), aes(x = year, y = Temp.summax),
               geom = "smooth", fun.data = mean_se, color = "pink") +
  ylim(20, 35) +
  labs(x = "", y = expression("Temp, summer max at CB6.4")) +
  theme(axis.line = element_line(colour = "black"),
        text = element_text(size=13),
        axis.text.x = element_text(angle=0, vjust=, hjust =.5, color = "black"),
        legend.position = "right",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())

ggplot() +
  stat_summary(data = CC.wland_2021_2060_69vars, aes(x = Year, y = Temp.spmed),
               geom = "smooth", fun.data = mean_se, color = "blue") +
  stat_summary(data = WIP.wland_2021_2060_69vars, aes(x = Year, y = Temp.spmed),
               geom = "smooth", fun.data = mean_se, color = "black") +
  stat_summary(data = WIP.woland_2031_2060_69vars, aes(x = Year, y = Temp.spmed),
               geom = "smooth", fun.data = mean_se, color = "red") +
  stat_summary(data = baseline_69vars, aes(x = Year, y = Temp.spmed),
               geom = "smooth", fun.data = mean_se, color = "purple") +
  stat_summary(data = CBP.WQ_69vars, aes(x = year, y = Temp.spmed),
               geom = "smooth", fun.data = mean_se, color = "pink") +
  labs(x = "", y = expression("Temp baywide, spring mean")) +
  theme(axis.line = element_line(colour = "black"),
        text = element_text(size=13),
        axis.text.x = element_text(angle=0, vjust=, hjust =.5, color = "black"),
        legend.position = "right",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())

#why do mean and max look different
ggplot() +
  stat_summary(data = CC.wland_2021_2060_69vars %>% filter(Station == "EE3.1"), aes(x = Year, y = Chla.spmax),
               geom = "smooth", fun.data = mean_se, color = "blue") +
  stat_summary(data = WIP.wland_2021_2060_69vars %>% filter(Station == "EE3.1"), aes(x = Year, y = Chla.spmax),
               geom = "smooth", fun.data = mean_se, color = "black") +
  stat_summary(data = WIP.woland_2031_2060_69vars %>% filter(Station == "EE3.1"), aes(x = Year, y = Chla.spmax),
               geom = "smooth", fun.data = mean_se, color = "red") +
  stat_summary(data = baseline_69vars %>% filter(Station == "EE3.1"), aes(x = Year, y = Chla.spmax),
               geom = "smooth", fun.data = mean_se, color = "purple") +
  stat_summary(data = CBP.WQ_69vars %>% filter(STATION == "EE3.1"), aes(x = year, y = Chla.spmax),
               geom = "smooth", fun.data = mean_se, color = "pink") +
  ylim(0, 100) +
  labs(x = "", y = expression("Chl-a, spring max, Station EE3.1")) +
  theme(axis.line = element_line(colour = "black"),
        text = element_text(size=13),
        axis.text.x = element_text(angle=0, vjust=, hjust =.5, color = "black"),
        legend.position = "right",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())


ggplot() +
  stat_summary(data = CC.wland_2021_2060_69vars %>% filter(Station == "EE3.1"), aes(x = Year, y = Chla.spme),
               geom = "smooth", fun.data = mean_se, color = "blue") +
  stat_summary(data = WIP.wland_2021_2060_69vars %>% filter(Station == "EE3.1"), aes(x = Year, y = Chla.spme),
               geom = "smooth", fun.data = mean_se, color = "black") +
  stat_summary(data = WIP.woland_2031_2060_69vars %>% filter(Station == "EE3.1"), aes(x = Year, y = Chla.spme),
               geom = "smooth", fun.data = mean_se, color = "red") +
  stat_summary(data = baseline_69vars %>% filter(Station == "EE3.1"), aes(x = Year, y = Chla.spme),
               geom = "smooth", fun.data = mean_se, color = "purple") +
  stat_summary(data = CBP.WQ_69vars %>% filter(STATION == "EE3.1"), aes(x = year, y = Chla.spme),
               geom = "smooth", fun.data = mean_se, color = "pink") +
 # ylim(0, 100) +
  labs(x = "", y = expression("Chl-a, spring mean, Station EE3.1")) +
  theme(axis.line = element_line(colour = "black"),
        text = element_text(size=13),
        axis.text.x = element_text(angle=0, vjust=, hjust =.5, color = "black"),
        legend.position = "right",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())

