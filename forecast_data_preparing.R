#Packages
required_packages <- 
  c("triptych", "ggplot2", "data.table",
    "scoringutils", "scoringRules", "statcomp",
    "pdc", "entropy", "runner", "dplyr", "tidyverse",
    "maps", "isotone", "ggrepel")


required_packages <- 
  c( "ggplot2", "data.table",
    "scoringRules", "statcomp",
    "pdc", "entropy", "runner", "dplyr", "tidyverse",
    "isotone", "ggrepel", "sf", "rnaturalearth",
    "rnaturalearthdata", "colorspace", "tidycensus",
    "slider", "ecp")
#Not using tidycensus currently
options(repos = c(CRAN = "http://cran.us.r-project.org"))
# Function to check and install packages
suppressMessages(
  for (package in required_packages) {
    if (!require(package, character.only = TRUE)) {
      install.packages(package, repos = "http://cran.us.r-project.org")
    }
    library(package, character.only = TRUE)
  }
)

#Directories
getwd()
forecasting.base.dir <- "/home/zoo/lina4258/forecasting"
forecasting.covid_forecasts.data.dir <- 
  "/home/zoo/lina4258/forecasting/data-processed"
forecasting.covid_forecasts.data.rt.dir <- 
  "/home/zoo/lina4258/forecasting/data-processed/rt_estimates"
forecasting.covid_forecasts.local.dir <- 
  "/home/zoo/lina4258/forecasting/from_local" #triptych wouldn't install on server
forecasting.covid_forecasts.out.dir <- 
  "/home/zoo/lina4258/forecasting/output"

#Themes
theme_custom <- function() {
  theme(text = element_text(size = 24),
        axis.text.x = element_text(size=24),
        axis.text.y = element_text(size=24),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_blank(),
        plot.title = element_blank(),
        legend.key.size = unit(2.5, "cm"),
        plot.subtitle = element_blank(),
        axis.title=element_text(size=24),
        legend.text=element_text(size=24)+geom_text(size = 24),
        legend.position = "bottom")
}


#Data Processing Start ----
inclusion_dates <-seq(as.Date("2020-07-27"), as.Date("2021-12-20"), by="week") #Dates should be the Monday of the week since that's when forecast were due
sec_models  <- c("CU-high", "CU-low","CU-mid","CU-nochange", "CU-scenario_high", "CU-scenario_low", "CU-scenario_mid",
                 "COVIDhub-ensemble", "COVIDhub_CDC-ensemble") #Note ensemble replaced with 4_week ensemble
selected_target <-paste0(seq(1:4), " wk ahead inc case") ## Selected targets
all_dat <-list()
filenames <-list()
model_names <-list()
dat_list <-list()
head(list.files(forecasting.base.dir,
           recursive = TRUE))
datapath <- forecasting.base.dir #Hubpath is a cloned repository of the US COVID-19 Forecast Hub Repo
# tmp <- c(list.files(path=datapath, pattern=as.character(date_to_pull[1]), 
#            full.names = TRUE, recursive = TRUE),
# list.files(path=datapath, pattern=as.character(date_to_pull[2]), 
#            full.names = TRUE, recursive = TRUE),
# list.files(path=datapath, pattern=as.character(date_to_pull[3]), 
#            full.names = TRUE, recursive = TRUE),
# list.files(path=datapath, pattern=as.character(date_to_pull[4]), 
#            full.names = TRUE, recursive = TRUE),
# list.files(path=datapath, pattern=as.character(date_to_pull[5]), 
#            full.names = TRUE, recursive = TRUE),
# list.files(path=datapath, pattern=as.character(date_to_pull[6]), 
#            full.names = TRUE, recursive = TRUE))
# exclude <-grep(paste(sec_models, collapse = "|"), tmp)
# tmp <-tmp[-c(exclude)] # Remove secondary models 
# tmp_model_names <-str_split(tmp, "/", simplify = TRUE)[ , 8]
# tmp_model_names <- str_remove(tmp_model_names, "^\\d{4}-\\d{2}-\\d{2}-")
# tmp_model_names <-  str_remove(tmp_model_names, "\\.csv$")
# tmp_model_names
# list.files(forecasting.base.dir, recursive = TRUE)
print(length(inclusion_dates))
all_dat <- list()
for(i in 1:length(inclusion_dates)){
  print(paste0("Date ", i))
  date_to_pull <-inclusion_dates[i]-0:6
  
  datapath <- forecasting.covid_forecasts.data.dir #Hubpath is a cloned repository of the US COVID-19 Forecast Hub Repo
  
  # CSV files to pull based on name and date range
  filenames[[i]] <- c(list.files(path=datapath, pattern=as.character(date_to_pull[1]), 
                                 full.names = TRUE, recursive = TRUE),
                      list.files(path=datapath, pattern=as.character(date_to_pull[2]), 
                                 full.names = TRUE, recursive = TRUE),
                      list.files(path=datapath, pattern=as.character(date_to_pull[3]), 
                                 full.names = TRUE, recursive = TRUE),
                      list.files(path=datapath, pattern=as.character(date_to_pull[4]), 
                                 full.names = TRUE, recursive = TRUE),
                      list.files(path=datapath, pattern=as.character(date_to_pull[5]), 
                                 full.names = TRUE, recursive = TRUE),
                      list.files(path=datapath, pattern=as.character(date_to_pull[6]), 
                                 full.names = TRUE, recursive = TRUE)
  )
  
  exclude <-grep(paste(sec_models, collapse = "|"), filenames[[i]])
  filenames[[i]] <-filenames[[i]][-c(exclude)] # Remove secondary models 
  model_names[[i]] <-str_split(filenames[[i]], "/", simplify = TRUE)[ , 8]
  model_names[[i]] <- str_remove(model_names[[i]], "^\\d{4}-\\d{2}-\\d{2}-")
  model_names[[i]] <-  str_remove(model_names[[i]], "\\.csv$")
  
  # Read in all submitted forecasts for week prior to date
  dat_list[[i]] <-lapply(filenames[[i]],
                         FUN = function(x) read_csv(x, col_types = cols(.default = "c")))
  for (j in c(1:length(model_names[[i]]))) {
    dat_list[[i]][[j]]$model <-model_names[[i]][j] #Add model names
    all_dat[[i]] <-bind_rows(dat_list[[i]], dat_list[[i]][[j]]) #Merge files from same data submission
  } 
  num_models <- length(unique(all_dat[[i]]$model))
  print(paste0("Checkpoint 1: ",num_models, " models"))
  all_dat[[i]] <-all_dat[[i]] %>%
    mutate(location = ifelse(location %in% c("1","2","3","4","5","6","7","8","9"),
                             paste0("0",location),location)) %>%
    filter(target %in% selected_target, ## Only read in cases
           nchar(location) < 3) %>% ## Only national and state forecasts
           # nchar(location) > 3) %>% ## Only county forecasts - note this file is ~20 GB of data
    arrange(desc(forecast_date)) %>%
    group_by(model, target, location, type, quantile) %>%
    slice(1) %>% ## Selects the most recent submission within the week
    ungroup() %>%
    mutate(type = ifelse(type=="Point", "point", type),
           type = ifelse(type=="Quantile", "quantile", type))  %>%
    filter(!is.na(type)) %>%
    mutate(value = as.numeric(value),
           quantile = as.numeric(quantile),
           quantile=ifelse(quantile=="NaN", NA, quantile),
           value = case_when(quantile==0.5 ~ round(value),
                             quantile<0.5 ~ floor(value),
                             quantile>0.5 ~ ceiling(value),
                             type=='point' ~ round(value)),
           target_end_date = as.Date(target_end_date),
           sub_date = as.Date(inclusion_dates[i] +1)) %>% #Add the due date for forecasts into the data frame
      dplyr::select(where(~any(!is.na(.))))
  num_models <- length(unique(all_dat[[i]]$model))
  print(paste0(num_models, " models"))
}

# unique(dat_list[[74]][[10]]$location)
# all_dat_df <-do.call(rbind.data.frame, all_dat)
# print(model_names[[74]])
# all_dat_df <-all_dat_df %>%
#   filter(location!="11001", #DC as an county
#          location!="02063", #AK county
#          location!="02066") #AK county
# print((unique(all_dat$model)))
# print(length(unique(tmp$model)))
# tmp <-lapply(filenames[[i]],
#                        FUN = function(x) read_csv(x, col_types = cols(.default = "c")))
# tmp2 <- NULL
# for (j in c(1:length(model_names[[74]]))) {
#   tmp[[j]]$model <-model_names[[74]][j] #Add model names
#   tmp2 <-bind_rows(tmp, tmp[[j]]) #Merge files from same data submission
# } 
# unique(tmp2$model)
# saveRDS(all_dat, "all_dat.RDS")
# all_dat <- readRDS("all_dat.RDS")
# 
# head(all_dat)
# 
# 
# filenames[[1]]
# 
# all_dat2 <- all_dat %>%
#   mutate(model = str_remove(model, "^\\d{4}-\\d{2}-\\d{2}-") %>% 
#            str_remove("\\.csv$"))
# length(unique(all_dat$model))
# 
# 
# 
# 
# 
# 
# 
state_and_national_covid_forecasts <- data.table(do.call(rbind.data.frame, all_dat))
length(unique(state_and_national_covid_forecasts$model)) #22 models
print(unique(state_and_national_covid_forecasts$target)) #1-4 weeks
state_and_national_covid_forecasts <- data.table(state_and_national_covid_forecasts)
state_and_national_covid_forecasts <- state_and_national_covid_forecasts[which(type == "quantile")]
state_and_national_covid_forecasts
#Function to pull observed case data from COVID-19 Forecast Hub
rd_obs <-function(){
  
  # Pulls population information from CSSE
  pop.state <-read.csv('https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_US.csv') %>%
    rename(location_name = "Province_State",
           population = "Population") %>%
    filter(location_name!="Diamond Princess", location_name!="Grand Princess") %>%
    mutate(id = ifelse(nchar(FIPS)==4, paste("0", FIPS, sep = ""), #To adjust for location_names with fips codes < 10
                       ifelse(nchar(FIPS)==2, paste(FIPS, "000", sep = ""), FIPS)), #to adjust for territories
           location = substr(id, 1, 2)) %>%
    dplyr::select(location_name, population, location) %>%
    filter(population > 0) %>% #oddly KA City,MO is not given a fips code?
    mutate(location = ifelse(location_name=="Missouri" & is.na(location), '29', location)) %>%
    group_by(location_name, location) %>%
    summarise(population = sum(population)) %>% 
    ungroup() %>% 
    add_row(location="US",
            location_name="National",
            population=0) %>%
    mutate(population = ifelse(location=='US', sum(population), population)) 
  # print(pop.state)
  start_date <-as.Date("2020-07-28")
  end_date <-as.Date("2022-03-19")
  
  # Reported cases counts from Forecast hub as of April 2, 2022
  obs_data <- read_csv(file = file.path(forecasting.covid_forecasts.data.dir, "truth-Incident Cases.csv")) %>%
    mutate(wk_end_date = as.Date(date, "%m/%d/%y"),
           wk_end_date=as.Date(cut(wk_end_date,"week", start.on.monday = FALSE)) + 6) %>% #,
    #value=ifelse(value < 0, 0, value)) %>%
    group_by(location_name, location, wk_end_date) %>%
    summarize(true_value=sum(value)) %>%
    rename(date=wk_end_date) %>%
    arrange(location_name, date) %>%
    mutate(location_name=ifelse(location_name=="United States", "National", location_name),
           grouping=ifelse(location_name=="National", "National", "State/Territory/DC")) %>%
    filter(date > start_date) %>%
    filter(date <= end_date) %>%
    left_join(., pop.state, by=c("location_name", "location")) %>%
    mutate(true_value.pop=(true_value*1E5)/population) %>%
    filter(location!=11001, #DC
           location!=02063, #AK county that was split
           location!=02066, #AK county that was split
           location!=66,#"Guam"
           location!=69,#"Northern Mariana Islands" 
           location!=60) #"American Samoa"
}
# Cleaning for 0 and negative counts
neg_or_zero <-function() {
  counties <-rd_obs() %>%
    filter(nchar(location) > 2) %>%
    filter(true_value<0) %>%
    mutate(id=paste0(location, "_", date))
  
  states <-rd_obs() %>%
    filter(nchar(location) < 3) %>%
    filter(true_value<=0) %>%
    mutate(id=paste0(location, "_", date))
  
  neg_or_zero <-counties %>%
    bind_rows(states)
}
neg_or_zero <-neg_or_zero()
neg_or_zero


obs_data <-rd_obs()
obs_data <- obs_data %>%
  filter(nchar(location) < 3) %>% 
  dplyr::select(location_name, location, date, true_value)
setnames(obs_data, "date", "target_end_date")


state_and_national_covid_forecasts <- state_and_national_covid_forecasts %>%
  # rename(date=target_end_date,
  #        prediction=value) %>%
  left_join(., obs_data, by=c("target_end_date", "location")) %>%
  mutate(id=paste0(location, "_", target_end_date)) %>%
  filter(!id %in% neg_or_zero$id)
state_and_national_covid_forecasts <- state_and_national_covid_forecasts[which(!is.na(true_value))] #Only NA location names
state_and_national_covid_forecasts
colnames(validated)
setnames(state_and_national_covid_forecasts,
         c("target", "value", "true_value", "quantile"),
         c("target_type", "predicted", "observed", "quantile_level"))

state_and_national_covid_forecasts[, horizon:= as.numeric(substr(target_type, 1, 1))]
state_and_national_covid_forecasts
unique(subset(state_and_national_covid_forecasts,
              select = c("target_end_date", "target_type", "quantile_level", "location", "model")))

#Clean:
tmp <- unique(state_and_national_covid_forecasts[, length(quantile_level), by = c("model", "target_type", "target_end_date",
                                                                                  "location")])
#Omit if don't report 7 quantile levels
tmp <- tmp[which(V1 == 5)]

cleaned_state_and_national_covid_forecasts <- 
  merge(state_and_national_covid_forecasts, tmp, by = c("model", "target_type", "target_end_date",
                                                        "location"),
        all.x  = TRUE)
cleaned_state_and_national_covid_forecasts <- cleaned_state_and_national_covid_forecasts[which(is.na(V1))]
cleaned_state_and_national_covid_forecasts[, V1:= NULL]

# saveRDS(cleaned_state_and_national_covid_forecasts, "cleaned_state_and_national_covid_forecasts.RDS")
# saveRDS(state_and_national_covid_forecasts, "state_and_national_covid_forecasts.RDS")


#Applying inclusion criteria of Lopez et al (2024)

## Must have 4 weeks of forecasts for all locations
include_wks <-function(x, df){
  
  df <-df %>%
    group_by(model, sub_date, location) %>%
    mutate(no_horizons=length(unique(target_type))) %>% 
    ungroup() %>%
    filter(no_horizons>=4) #There should be at least 4 horizons
}

## Must have quantities for all forecasts
include_quant <-function(x, df){
  
  df <-df %>%
    group_by(model, location, forecast_date, target_type) %>%
    mutate(quants=length(quantile_level[!is.na(quantile_level)])) %>%
    ungroup() %>%
    filter(quants==7) #There should be 7 quantiles 
}

## Must have forecasts for at least 50 locations for states/territories/district and national
include_locs <-function(x, df){
  
  df <-df %>%
    mutate(location=as.factor(location)) %>%
    group_by(model, forecast_date) %>%
    mutate(locs=length(unique(location))) %>% 
    ungroup() %>%
    filter(locs>=50) 
  
}
start_date <-as.Date('2020-07-28')
stop_date <-as.Date('2021-12-21')
## Must have submitted at least 50% of weeks
include_subs <-function(x, df){
  
  df <-df %>%
    filter(sub_date >= start_date) %>%
    mutate(total=length(unique(sub_date))) %>% 
    group_by(model) %>%
    mutate(subs=length(unique(sub_date))) %>%
    ungroup() %>%
    mutate(percent_sub=subs/total) %>%
    filter(percent_sub>=0.50)
  
}
cleaned_state_and_national_covid_forecasts
location_sub <-c("60", "66", "69")

dat_US_state <- cleaned_state_and_national_covid_forecasts %>%
  group_by(model, sub_date, target_type, location) %>%
  filter(any(!is.na(predicted))) %>%
  ungroup() %>%
  # inclusion criteria
  include_quant(df=.) %>%
  include_wks(df=.) %>%
  include_locs(df=.) %>%
  include_subs(df=.) %>%
  dplyr::select(-quants, -no_horizons, -locs, -total, -subs, -percent_sub) %>%
  filter(!(location %in% location_sub)) #remove territories that few teams forecasted


dat_US_state <- data.table(dat_US_state)
state_covid_forecasts2[which(!(state_covid_forecasts2 == state_covid_forecasts))]
state_covid_forecasts2 <- dat_US_state[which(location != "National")]
setkeyv(state_covid_forecasts2, c("target_end_date", "target_type", "model", "quantile_level"))
saveRDS(state_covid_forecasts2,
        "state_covid_forecasts_MAR_2025.RDS")
state_covid_forecasts <- readRDS("state_covid_forecasts_MAR_2025.RDS")
length(unique(state_covid_forecasts$location_name))
#53: 50 states, DC, Puerto Rico, Virgin Islands 




#Read back in from local processing
state_level_covid_forecasts_dt <- readRDS(file.path(forecasting.covid_forecasts.data.dir,
                                                        "state_level_covid_forecasts_dt.RDS"))
state_level_covid_forecasts_dt <- subset(state_level_covid_forecasts_dt,
                                         location_name != "National")
state_level_covid_forecasts_dt <- subset(state_level_covid_forecasts_dt,
                                         location != "US")
state_level_covid_forecasts_dt

#Merge in Rt estimates

datapath <- paste0(forecasting.covid_forecasts.data.rt.dir, "/")
fileList <-dir(path = datapath, pattern = '*.csv')  # list of file names, not including their paths

rt <-list()
for (i in 1:length(fileList)) {
  rt[[i]] <-read_csv(paste0(datapath, fileList[i])) %>%
    mutate(file_date = gsub(".csv", "", fileList[i])) %>%
    filter(type=="estimate") 
}

rt <-do.call(bind_rows, rt)
head(rt)
####--- Pull midweek estimates and calculate means per date ---####

## Pick midweek estimates for all submissions
rt <-rt %>%
  mutate(median=as.numeric(median),
         mean=as.numeric(mean),
         sd=as.numeric(sd),
         upper_90=as.numeric(upper_90),
         lower_90=as.numeric(lower_90),
         date=as.Date(date, "%m/%d/%Y"),
         file_date=gsub(".csv","", file_date),
         file_date=gsub("./","", file_date), 
         state=ifelse(is.na(state), region, state),
         wk_day=weekdays(as.Date(date))) %>%
  filter(wk_day=="Wednesday",
         state!="Northern Mariana Islands",
         state!="Guam",
         state!="American Samoa") %>%
  dplyr::select(file_date, state, date, mean, median, lower_90, upper_90)


## Median values per date
rt_med <-rt %>%
  group_by(state, date) %>%
  summarize(median_med=median(median, na.rm=TRUE),
            lower_90_med=median(lower_90, na.rm=TRUE),
            upper_90_med=median(upper_90, na.rm=TRUE))

####--- Create phases---####

rt_phases <-function() {
  
  rt_phases <-rt_med %>%
    ungroup() %>%
    rename(location_name=state) %>%
    group_by(location_name) %>%
    
    #Set up initial classifications
    mutate(phase=ifelse(lower_90_med > 1.0, "increasing",
                        ifelse(upper_90_med < 1.0, "decreasing", "unclassified")),
           phase=ifelse(lower_90_med == 1.0 & upper_90_med > 1, "increasing",
                        ifelse(upper_90_med == 1.0 & lower_90_med < 1.0, "decreasing", phase)),
           
           #Create groups of each initial phase
           groups_phase=rleid(phase),
           
           #Note first and last observation in each group
           order_groups=ifelse(groups_phase==last(groups_phase), "last", 
                               ifelse(groups_phase==first(groups_phase), "first", "other"))) %>%
    group_by(location_name, groups_phase) %>%
    #Count observations within each group
    mutate(seq=1:n()) %>%
    ungroup() %>%
    group_by(location_name) %>%
    arrange(seq, groups_phase)
  
  # Pull first classifications from each group of phases & and create windows of every three groups
  states <-c(unique(rt_phases$location_name))
  df_phases <-list()
  
  for (i in 1:length(states)){
    
    df_states <-rt_phases %>%
      filter(seq==1) %>% 
      dplyr::select( -seq) %>%
      filter(location_name==states[i])
    
    df_phases[[i]] <-df_states %>%
      slide(., ~.x, .after = 2) 
    
    df_phases[[i]] <-df_phases[[i]][sapply(df_phases[[i]], nrow)==3]
    
    names(df_phases[[i]]) <- paste("df_", seq_along(df_phases[[i]]), sep = "")
    df_phases[[i]] <- map_df(df_phases[[i]], ~as.data.frame(.x), .id="id")
    
  }
  
  df_phases <-do.call(rbind.data.frame, df_phases) 
  btw_phase <-df_phases %>%
    
    #Fills in "unclassified" when in b/w increasing/decreasing phases
    group_by(location_name, id) %>% 
    mutate(count=seq(1:n()),
           phase_reclass=ifelse(first(phase)=="increasing" & last(phase)=="increasing", "increasing",
                                ifelse(first(phase)=="decreasing" & last(phase)=="decreasing", "decreasing", NA)),
           phase_reclass=ifelse(first(phase)=="decreasing" & last(phase)=="increasing" & phase=="unclassified", "nadir", 
                                ifelse(first(phase)=="increasing" & last(phase)=="decreasing" & phase=="unclassified", "peak", phase_reclass))
    ) %>%
    ungroup() %>%
    
    #Fills in "unclassified" when first or last phase
    group_by(location_name) %>%
    mutate(phase_reclass=ifelse(order_groups=="first" & phase=="unclassified", lead(phase), phase_reclass),
           phase_reclass=ifelse(order_groups=="last" & phase=="unclassified", lag(phase), phase_reclass)
    ) %>%    
    drop_na() %>%
    dplyr::select(location_name, date, phase_reclass) 
  
  rt_phases <-rt_phases %>%
    left_join(., btw_phase, by=c("location_name", "date")) %>%
    distinct() %>%
    mutate(phase=ifelse(phase=="unclassified", phase_reclass, phase)) %>%
    group_by(location_name) %>%
    arrange(location_name, date) %>%
    fill(phase) %>%
    dplyr::select(-groups_phase, -order_groups, -seq, -phase_reclass) %>%
    ungroup()
  
  #Update weeks between increase/decrease and decrease/increase due to rapid changes
  rt_phases <-rt_phases %>%
    group_by(location_name) %>%
    mutate(phase2=ifelse(phase=="increasing" & lead(phase)=="decreasing", "peak", 
                         ifelse(phase=="decreasing" & lag(phase)=="increasing", "peak", phase)),
           phase2=ifelse(phase=="decreasing" & lead(phase)=="increasing", "nadir", 
                         ifelse(phase=="increasing" & lag(phase)=="decreasing", "nadir", phase2)),
           phase2=ifelse(is.na(phase2), phase, phase2)
    ) %>%
    dplyr::select(-phase) %>%
    rename(phase=phase2)
  
  # ## Truth data
  # obs_data <- read_csv(filetruth-Incident Cases.csv") %>%
  #   filter(nchar(location) < 3) %>%
  #   mutate(wk_end_date = as.Date(date, "%m/%d/%y"),
  #          wk_end_date=as.Date(cut(wk_end_date,"week", start.on.monday = FALSE)) + 3,
  #          value=ifelse(value < 0, 0, value)) %>%
  #   group_by(location_name, location, wk_end_date) %>%
  #   summarize(sum=sum(value)) %>%
  #   rename(date=wk_end_date) %>%
  #   arrange(location_name, date) 
  # 
  # rt_phases <-rt_phases %>%
  #   left_join(., obs_data, by=c("location_name", "date")) 
  
}
rt_phases <-rt_phases()
rt_phases
rt_phases <-rt_phases %>%
  filter(location_name!="Virgin Islands", 
         location_name!="Puerto Rico")
rt_phases         
tmp <- observed_state_level_data %>% mutate(wk_end_date = as.Date(target_end_date, "%m/%d/%y"),
              wk_end_date=as.Date(cut(wk_end_date,"week", start.on.monday = FALSE)) + 3)%>%
  group_by(location_name, location, wk_end_date, target_end_date) %>%
  summarize(sum=sum(observed)) %>%
  rename(date=wk_end_date) %>%
  arrange(location_name, date)
rt_phases
tmp
rt_phases <- merge(rt_phases, tmp, by = c("location_name", "date"))
colnames(rt_phases)
tmp_rt_phases <- data.table(copy(rt_phases))
tmp_rt_phases[, date:= NULL]
tmp_rt_phases[, sum:= NULL]
tmp_rt_phases
setnames(tmp_rt_phases, c("median_med",
                          "lower_90_med",
                          "upper_90_med",
                          "phase"),
         c("median_med_Rt",
           "lower_90_med_Rt",
           "upper_90_med_Rt",
           "phase_Rt"))
state_level_covid_forecasts_dt <- merge(state_level_covid_forecasts_dt,
      tmp_rt_phases,
      by = c("location_name", "target_end_date", "location"))
# location_name!="District of Columbia")
save(rt_phases, file="rt_phases.rdata") ### UPDATE THIS LINE FOR CORRECT FILE NAME!!

sort(unique(rt_phases$date))
rt_phases <-filter(rt_phases, date <= "2022-01-12")
rt_phases


saveRDS(state_level_covid_forecasts_dt,
        "state_level_covid_forecasts_dt.RDS")
state_level_covid_forecasts_dt[, length(unique(target_end_date)), by = c("quantile_level", "location", "horizon")]
state_level_covid_forecasts_dt
observed_state_level_data <- unique(subset(state_level_covid_forecasts_dt,
                                           select = c("target_end_date",
                                                      "observed",
                                                      "location",
                                                      "location_name")))

ggplot(observed_state_level_data) +
  geom_line(aes(x = target_end_date, y = observed))+
  facet_wrap(location_name ~., scales = "free_y")+
  theme_light()

#Model colours ----
num_covid_models <- length(unique(state_level_covid_forecasts_dt$model))
num_covid_models
covid_model_names <- unique(state_level_covid_forecasts_dt$model)
covid_model_names
corrected_covid_model_names <- c("Ensemble", "Baseline",
                                 "CU-Select", "CA-DELPHI", "Karlen-pypm", "RW-ESG")
corrected_covid_model_names
covid_model_colours <- c(
  "COVIDhub-4_week_ensemble" = "#4C78A8",  # Muted Blue
  "COVIDhub-baseline" = "darkgreen",  # Bright Orange
  "CU-select" = "#F58518",  # Bright Orange
  "CovidAnalytics-DELPHI" = "#7B3294",  
  "Karlen-pypm" = "#E45756",  
  "RobertWalraven-ESG" = "goldenrod2"  # Green
)
covid_model_levels <- names(covid_model_colours)


covid_model_labels <- 
  c("COVIDhub-4_week_ensemble" = "Ensemble", 
    "COVIDhub-baseline" ="Baseline",
    "CU-select" ="CU-Select", 
    "CovidAnalytics-DELPHI" ="CA-DELPHI", 
    "Karlen-pypm" = "Karlen-pypm", 
    "RobertWalraven-ESG" = "RW-ESG")

corrected_covid_model_names
covid_model_colours <- c(
  "COVIDhub-4_week_ensemble" = "#4C78A8",  # Muted Blue
  "COVIDhub-baseline" = "darkgreen",  # Bright Orange
  "CU-select" = "#F58518",  # Bright Orange
  "CovidAnalytics-DELPHI" = "#7B3294",  
  "Karlen-pypm" = "#E45756",  
  "RobertWalraven-ESG" = "goldenrod2"  # Green
)
covid_model_colours
corrected_covid_model_names

#location-location_names
location_location_names <- unique(subset(state_level_covid_forecasts_dt,
              select = c("location", "location_name")))
location_location_names
# Get US states map data
us_states <- ne_states(country = "United States of America", returnclass = "sf")
length(which(unique(us_states$name) %in% state_level_covid_forecasts_dt$location_name))
ggplot(us_states) +
  geom_sf(fill = "lightblue", color = "black") +
  theme_minimal() +
  labs(title = "Map of US States")




#US Population Data
