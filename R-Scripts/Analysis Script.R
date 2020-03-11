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

#Define Functions
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

# authenticate via access token - Twitter API
token <- create_token(
  app = "----",
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
#stream_tbl = stream_tbl[!(!stream_tbl$lang=="en"),]  #remove all tweets that are not in english

# Remove unused tables
rm(Stream)
rm(Stream_flat)
rm(jsonfile)

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
sm_new_community <- cluster_walktrap(sm_graph)
sm_new_mod <- modularity(sm_new_community)
sm_new_mem <- membership(sm_new_community)
sm_new_centrality <- centr_degree(sm_graph, mode = "all")

#Comparison statistics
noEdges_original <- gsize(social_media_graph)
degree_original <- mean(degree(social_media_graph))
noEdges_simplified <- gsize(sm_graph)
degree_simplified <- mean(degree(sm_graph))

# plot graph
plot(social_media_graph, vertex.label=NA,vertex.size=0.5)
plot(sm_new_community, sm_graph, vertex.label=NA,vertex.size=0.5)



# Find followers of randomly selected users of data set Sample size of 5
usernames = stream_tbl$user.name
users_sample = sample(usernames, 40, replace = FALSE, prob = NULL)

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
