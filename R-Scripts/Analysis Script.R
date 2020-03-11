# Import Dependencies
library(jsonlite)
library(tibble)
library(ggplot2)
library(igraph)
library(tidyr)
library(rtweet)
library(tidyverse)
library(here)
library(tidygraph)
library(ggraph)
library(dplyr)
library("tm")
library("RColorBrewer")

# authenticate via access token - Twitter API
token <- create_token(
  app = "-----",
  consumer_key = "-----",
  consumer_secret = "-----",
  access_token = "-----",
  access_secret = "-----")

jsonfile <- readline(prompt="Enter file name: ")

# Read in JSON Files
Stream <- stream_in(file(jsonfile))
Stream_flat <- jsonlite::flatten(Stream) #Flatten the JSON Filw into a table
stream_tbl <- as_tibble(Stream_flat) #create a dataframe from the flattened JSON Table
stream_tbl = stream_tbl[!grepl("RT", stream_tbl$text),]  #Removes all retweets from the dataframe
stream_tbl = stream_tbl[!(!stream_tbl$lang=="en"),]  #remove all tweets that are not in english

# Remove unused tables
rm(Stream)
rm(Stream_flat)
rm(jsonfile)

within(stream_tbl, rm(x, y))

#Build Network Graph
edges <-stream_tbl$in_reply_to_status_id # Create edge list from Reply to id string
edges <-replace_na(edges, "NA") # Replace NA items with "NA" string
social_media_graph <- graph(edges, n=max(edges)+1, directed=FALSE) # Create the graph

# Calculate communities from the data
sm_community <- cluster_walktrap(social_media_graph)
SM_Mod <- modularity(sm_community)
SM_Mem <- membership(sm_community)
sm_centrality <- centr_degree(social_media_graph, mode = "all")

# Delete single node communities, NA Node, and recompute communities
sm_graph <- delete.vertices(social_media_graph, "NA")
sm_graph <- delete.vertices(simplify(sm_graph), degree(sm_graph)==1)
sm_graph <- delete.vertices(sm_graph, "NA")
sm_new_community <- cluster_walktrap(sm_graph)
sm_new_mod <- modularity(sm_new_community)
sm_new_mem <- membership(sm_new_community)
sm_new_centrality <- centr_degree(sm_graph, mode = "all")

# plot graph
plot(sm_community, social_media_graph, vertex.label=NA,vertex.size=0.5)
plot(sm_new_community, sm_graph, vertex.label=NA,vertex.size=0.5)

# Find followers of randomly selected users of data set Sample size of 5
#create function
follower_communities <- function(user, friends, i) {
  user_followers <- get_followers(user)
  ids <- sample.int(user_followers$user_id, 200, useHash = FALSE)
  user_friends <- list()
  Sys.sleep(15*60)
  for (a in 1:length(ids)){
    user_friends[[a]] <- get_friends(ids[a])
    
    # pause if divisible by 15
    if (a %% 15 == 0){
      Sys.sleep(15*60) # must enter time in seconds
    }
  }
  # Combine data tables in list
  friends[i] <- bind_rows(user_friends) %>% 
    rename(friend = user_id)
  rm(user,user_followers,user_friends)
}

usernames = stream_tbl$user.name
users_sample = sample(usernames, 40, replace = FALSE, prob = NULL)
user1 = users_sample[1]
user2 = users_sample[2]
user3 = users_sample[3]
user4 = users_sample[4]
user5 = users_sample[5]
user6 = users_sample[6]
user7 = users_sample[7]
user8 = users_sample[8]
user9 = users_sample[9]
user10 = users_sample[10]
user11 = users_sample[11]
user12 = users_sample[12]
user13 = users_sample[13]
user14 = users_sample[14]
user15 = users_sample[15]
user16 = users_sample[16]
user17 = users_sample[17]
user18 = users_sample[18]
user19 = users_sample[19]
user20 = users_sample[20]
user21 = users_sample[21]
user22 = users_sample[22]
user23 = users_sample[23]
user24 = users_sample[24]
user25 = users_sample[25]
user26 = users_sample[26]
user27 = users_sample[27]
user28 = users_sample[28]
user29 = users_sample[29]
user30 = users_sample[30]
user31 = users_sample[31]
user32 = users_sample[32]
user33 = users_sample[33]
user34 = users_sample[34]
user35 = users_sample[35]
user36 = users_sample[36]
user37 = users_sample[37]
user38 = users_sample[38]
user39 = users_sample[39]
user40 = users_sample[40]

# get followers from users above
user1_followers <- get_followers(user1)
user2_followers <- get_followers(user2)
user3_followers <- get_followers(user3)
user4_followers <- get_followers(user4)
user5_followers <- get_followers(user5)

# randomly select followers from the above lists Sample size = 50
ids_1 <- sample.int(user1_followers$user_id, 200, useHash = FALSE)
ids_2 <- sample.int(user2_followers$user_id, 200, useHash = FALSE)
ids_3 <- sample.int(user3_followers$user_id, 200, useHash = FALSE)
ids_4 <- sample.int(user4_followers$user_id, 200, useHash = FALSE)
ids_5 <- sample.int(user5_followers$user_id, 200, useHash = FALSE)

# Create empty list to store results
user1_friends <- list()
user2_friends <- list()
user3_friends <- list()
user4_friends <- list()
user5_friends <- list()


# To find the followers each user will take roughly 50 minutes due to twitter only 
# allowing 15 requests per 15 minutes. Sample size of 5 users pulling 50 followers.

Sys.sleep(15*60)

# User 1
for (a in 1:length(ids_1)){
  user1_friends[[a]] <- get_friends(ids_1[a])
  
  # pause if divisible by 15
  if (a %% 15 == 0){
    Sys.sleep(15*60) # must enter time in seconds
  }
}

# Combine data tables in list
friends1 <- bind_rows(user1_friends) %>% 
  rename(friend = user_id)

Sys.sleep(15*60)

# User 2
for (a in 1:length(ids_2)){
  user2_friends[[a]] <- get_friends(ids_2[a])
  
  # pause if divisible by 15
  if (a %% 15 == 0){
    Sys.sleep(15*60) # must enter time in seconds
  }
}

# Combine data tables in list
friends2 <- bind_rows(user2_friends) %>% 
  rename(friend = user_id)

Sys.sleep(15*60)

# User 3
for (a in 1:length(ids_3)){
  user3_friends[[a]] <- get_friends(ids_3[a])
  
  # pause if divisible by 15
  if (a %% 15 == 0){
    Sys.sleep(15*60) # must enter time in seconds
  }
}

# Combine data tables in list
friends3 <- bind_rows(user3_friends) %>% 
  rename(friend = user_id)

Sys.sleep(15*60)

# User 4
for (a in 1:length(ids_4)){
  user4_friends[[a]] <- get_friends(ids_4[a])
  
  # pause if divisible by 15
  if (a %% 15 == 0){
    Sys.sleep(15*60) # must enter time in seconds
  }
}

# Combine data tables in list
friends4 <- bind_rows(user4_friends) %>% 
  rename(friend = user_id)

Sys.sleep(15*60)

# User 5
for (a in 1:length(ids_5)){
  user5_friends[[a]] <- get_friends(ids_5[a])
  
  # pause if divisible by 15
  if (a %% 15 == 0){
    Sys.sleep(15*60) # must enter time in seconds
  }
}

# Combine data tables in list
friends5 <- bind_rows(user5_friends) %>% 
  rename(friend = user_id)

# Once files are written the values are no longer needed

rm(user1,user1_followers,user1_friends,user2,user2_followers,user2_friends,
   user3,user3_followers,user3_friends,user4,user4_followers,user4_friends,
   user5,user5_followers,user5_friends,ids_1,ids_2,ids_3,ids_4,ids_5)
