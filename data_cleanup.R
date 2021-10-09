library(readxl)
library(dplyr)
library(tidyr)

df <- read_excel('data/FBIs most wanted.xlsx', sheet = "1-525")

clean_df <- df %>% separate(data, c("number","name","date_placed_on_list","date_located"), sep = "(\\s\\s+)")

write.csv(clean_df,'output/fbi_most_wanted.csv',row.names = F)
