# Import Dependencies
library(jsonlite)
library(tibble)
library(ggplot2)
library(igraph)
library(tidyr)
library(rtweet)
library(here)
library(ggraph)
library(dplyr)
library(CINNA)
library(igraph)
library("SnowballC")
library("wordcloud")
library("RColorBrewer")

setwd("~/Social Media Network Growth/Politics")

# authenticate via access token - Twitter API
token <- create_token(
  app = "Cody West",
  consumer_key = "uAMKZaMyqwAan03Cd5ZJpL56A",
  consumer_secret = "o9Q5OuYx6imBUcYvEIi8MVQ9WdSuCkCGIT9ieA0YNTJJZMBleL",
  access_token = "846302774-pPB0TfVkUWOO4aEr8hKNlKupkog1g1VFPnho1Td3",
  access_secret = "IUafuVm1YOk3S3hIflkUPxbI3Zb6pQubm2vseeBIRAx79")

jsonfile <- "poli_20200310.json"

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

#Build Network Graph
edges <-stream_tbl$in_reply_to_status_id # Create edge list from Reply to id string
edges <-replace_na(edges, "NA") # Replace NA items with "NA" string
social_media_graph <- graph(edges, n=max(edges)+1, directed=FALSE) # Create the graph

# Calculate communities from the data
sm_community <- cluster_walktrap(social_media_graph)
SM_Mod <- modularity(sm_community)
SM_Mem <- membership(sm_community)
sm_centrality <- centr_degree(social_media_graph, mode = "all")
sm_closeness <- closeness(social_media_graph, mode="all")
sm_centrality$res
sm_closeness
sm.max <- max(sm_centrality)
sm.min <- min(sm_centrality)
sm.range <- range(sm_centrality)
sm.min_index <- which.min(sm_centrality)
sm.max_index <- which.max(sm_centrality)




# Delete single node communities, NA Node, and recompute communities
sm_graph <- delete.vertices(social_media_graph, "NA")
sm_graph <- delete.vertices(simplify(sm_graph), degree(sm_graph)==1)
sm_graph <- delete.vertices(sm_graph, "NA")
sm_new_community <- cluster_walktrap(sm_graph)
sm_new_mod <- modularity(sm_new_community)
sm_new_mem <- membership(sm_new_community)
sm_new_centrality <- centr_degree(sm_graph, mode = "all")
sm_new_closeness <- closeness(sm_graph, mode="all")
sm_new_centrality$res
sm_new_closeness
sm_pr_cent <- proper_centralities(sm_graph)
sm_hub.score <- hub_score(sm_graph)
sm_authority.score <- authority_score(sm_graph)
sm_sub_cent <- subgraph_centrality(sm_graph)


