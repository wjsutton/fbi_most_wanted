# AIM: Order FBIs Most Wanted list to show Top 10 for the viz, i.e. the 
# members will fall into 10 swim lanes, where they shouldn't overlap 

library(lubridate)
library(dplyr)

# load data
fbi <- read.csv('output/fbi_most_wanted.csv',stringsAsFactors = F)
women <- c('Ruth Eisemann-Schier'
           ,'Marie Dean Arrington'
           ,'Angela Yvonne Davis'
           ,'Bernardine Rae Dohrn'
           ,'Katherine Ann Power'
           ,'Susan Edith Saxe'
           ,'Donna Jean Willmott'
           ,'Shauntay L. Henderson'
           ,'Brenda Delgado'
           ,'Shanika S. Minor')

famous_cases <- read.csv('data/famous_cases.csv',stringsAsFactors = F)
names(famous_cases)[1] <- 'number'

# split data, metadata and date ranges, join later on number
fbi_meta <- fbi[, c("number","name","description")]
fbi_df <- fbi[, c("number","date_placed_on_list","date_located")]

# Enriching metadata
fbi_meta$dismissed <- grepl('dismiss',tolower(fbi_meta$description))
fbi_meta$removed <- grepl('removed',tolower(fbi_meta$description))
fbi_meta$found_dead <- grepl('dead|remains were located|skeletal remains|self-inflicted shotgun wound',tolower(fbi_meta$description))
fbi_meta$women <- fbi_meta$name %in% women
fbi_meta$repeat_offenders <- fbi_meta$name %in% filter(fbi_meta %>% count(name),n>1)$name
fbi_meta$still_on_list <- fbi_meta$name %in% fbi[fbi$date_located == 'still on list',]$name

# convert dates from month/day/year to year-month-day
fbi_df$date_placed_on_list <- mdy(fbi_df$date_placed_on_list)
fbi_df$date_located <- mdy(fbi_df$date_located)

# replace NAs with today's date
fbi_df[is.na(fbi_df)] <- Sys.Date()

max_lanes <- 24

# initial swim lane lookup to put items in a lane so no date overlap
swim_lanes <- data.frame(col=1:max_lanes ,stringsAsFactors = F)
swim_lanes$located <- filter(fbi_df,number %in% 1:max_lanes )$date_located

lane_lookup_df <- data.frame(number=1:max_lanes ,col=1:max_lanes)

loop_start <- max_lanes +1

for(i in loop_start:max(fbi_df$number)){
  entry <- filter(fbi_df,number == i)
  on_list <- entry$date_placed_on_list
  off_list <- entry$date_located
  
  lanes_available <- filter(swim_lanes,located < on_list)
  if(nrow(lanes_available)==0){
    # if there is no lane available add a new lane
    swim_lanes <- rbind(swim_lanes,data.frame(col=max(swim_lanes$col)+1,located=on_list-1))
    lanes_available <- filter(swim_lanes,located < on_list)
  }
  earliest_lane <- min(lanes_available$located) 
  
  lanes_available <- filter(lanes_available,located == earliest_lane)
  first_earliest_lane <- min(lanes_available$col)
  
  chosen_lane <- filter(lanes_available,col == first_earliest_lane)$col
  lane_entry <- data.frame(col=chosen_lane, located=off_list)
  
  other_lanes <- filter(swim_lanes,col != chosen_lane)
  combined_lanes <- rbind(lane_entry,other_lanes)
  swim_lanes <- combined_lanes
  
  # add to lane lookup
  lane_lookup <- data.frame(number=i,col=chosen_lane)
  lane_lookup_df <- rbind(lane_lookup_df,lane_lookup)
  
}

fbi_df <- left_join(fbi_df,lane_lookup_df,by='number')
fbi_df <- inner_join(fbi_df,fbi_meta,by='number')
fbi_df <- left_join(fbi_df,famous_cases,by='number')

# Rejig numbers
fbi_df[fbi_df$number == 436,"col"] <- 15
fbi_df[fbi_df$number == 229,"col"] <- 17
fbi_df[fbi_df$number == 223,"col"] <- 22
fbi_df[fbi_df$number == 295,"col"] <- 12
fbi_df[fbi_df$number == 255,"col"] <- 22
fbi_df[fbi_df$number == 475,"col"] <- 9
fbi_df[fbi_df$number %in% c(481,496,511),"col"] <- 8

fbi_df[fbi_df$number == 123,"col"] <- 7
fbi_df[fbi_df$number == 113,"col"] <- 11
fbi_df[fbi_df$number == 51,"col"] <- 7
fbi_df[fbi_df$number == 59,"col"] <- 19
fbi_df[fbi_df$number == 28,"col"] <- 23
fbi_df[fbi_df$number == 23,"col"] <- 12
fbi_df[fbi_df$number == 188,"col"] <- 17
fbi_df[fbi_df$number == 105,"col"] <- 17
fbi_df[fbi_df$number == 104,"col"] <- 8
fbi_df[fbi_df$number == 464,"col"] <- 17
fbi_df[fbi_df$number == 459,"col"] <- 23

fbi_df$on_list_date <- fbi_df$date_placed_on_list
fbi_df$off_list_date <- fbi_df$date_located

fbi_df$date_located <- as.Date(ifelse(fbi_df$date_located<fbi_df$date_placed_on_list,fbi_df$date_placed_on_list,fbi_df$date_located), origin = "1970-01-01")
fbi_df$days_on_list <- difftime(fbi_df$off_list_date, fbi_df$on_list_date, units = "days") 

write.csv(fbi_df,'output/fbi_most_wanted_with_lanes.csv',row.names = F, fileEncoding="UTF-8")
