# AIM: Order FBIs Most Wanted list to show Top 10 for the viz, i.e. the 
# members will fall into 10 swim lanes, where they shouldn't overlap 

library(lubridate)
library(dplyr)

fbi <- read.csv('output/fbi_most_wanted.csv',stringsAsFactors = F)
fbi_meta <- fbi[, c("number","name","description")]
fbi_df <- fbi[, c("number","date_placed_on_list","date_located")]

# convert dates from month/day/year to year-month-day
fbi_df$date_placed_on_list <- mdy(fbi_df$date_placed_on_list)
fbi_df$date_located <- mdy(fbi_df$date_located)

# replace NAs with today's date
fbi_df[is.na(fbi_df)] <- Sys.Date()

max_lanes <- 30

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

write.csv(fbi_df,'output/fbi_most_wanted_with_lanes.csv',row.names = F)
