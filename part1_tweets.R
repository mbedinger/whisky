# -------------------------------------------------------------------------
# tweets.R
#
# Created by: Dr Melissa Bedinger, m.bedinger@hw.ac.uk
# Created: 2021-06-17
#
# Last revised by: Dr Melissa Bedinger
# Last revised: 2021-06-18
# -------------------------------------------------------------------------

# Prepare the environment
rm(list = ls()); cat("/014"); gc(verbose = TRUE)

# Set working directory to the script's folder
setwd(dirname(rstudioapi::getSourceEditorContext()$path)); getwd()

# Load the required packages
pacman::p_load(tidyverse, tidytext, rtweet, sf)


# -------------------------------------------------------------------------

# Retrieve tweets
# data <- 
#   search_tweets(
#   q = "whisky",
#   n = 2000, # return 2000 tweets
#   geocode = "56.8154,-4.1838,250mi", # central Scotland with 250 mile radius
#   include_rts = FALSE) # not including retweets


# Tidy by reordering columns for easier viewing & filling NAs
# data_ord <- 
#   data %>% 
#   relocate(
#     status_id, # unique id
#     location, place_name, place_full_name, place_type, country, country_code, # location descriptors
#     geo_coords, coords_coords, bbox_coords, # location coordinates
#     created_at, # date & time stamp
#     text) # tweet

# data_ord$location[data_ord$location==""] <- NA

# write.csv(data_ord, "data_ord.RDS")

# Read in data
data_ord <- readRDS("data_ord.RDS")

# Return only those in Scotland
# Need to exclude the following:

# 1. rows where location == "UK" AND all other location columns (place_name etc.) are empty
#    or rows where location == "United Kingdom" AND all other location columns (place_name etc.) are empty
#    or rows where location == "" AND all other location columns (place_name etc.) are empty
rm_generic <- 
  data_ord %>%
  filter(is.na(location) |
           location == "Britain" |
           location == "GB" |
           location == "Regno Unito" |
           location == "Reino Unido" |
           location == "United Kingdom" | 
           location == "UK" |
           location == "U\\.K\\." |
           location == "Uk" |
           location == "Uk " |
           location == "uk" |
           location == "Up and down" |
           location == "Bonnie Scotland" |
           location == "Bonny Scotland" |
           location == "Scotland" |
           location == "Scotland UK" |
           location == "Scotland, United Kingdom" |
           location == "Escocia, Reino Unido" |
           location == "Dubai, United Arab Emirates") %>%
  filter(is.na(place_name),
           is.na(place_full_name),
           is.na(place_type),
           is.na(country),
           is.na(country_code),
           geo_coords == "c(NA, NA)",
           coords_coords == "c(NA, NA)",
           bbox_coords == "c(NA, NA, NA, NA, NA, NA, NA, NA)") %>%
  pull(status_id)
  

# 2. rows which might include English locations
england <- 
  c("ngland", "lnwick", "ncaster", "Barnard Castle", "erwickshire", "lackburn", 
    "lackpool", "Cumbria", "Dalton-in-Furness", "urham", "Fylde", "ateshead", 
    "sle of Man", "Lake District", "ancashire", "iverpool", "IVERPOOL", "erseyside", 
    "onkseaton", "ewcastle", "reston", "emote", "South West UK", "St Annes", 
    "tockton", "hornaby", "Tyne and Wear", "Wigan", "Wirral", "Yarm")

rm_england <-
  data_ord %>%
  filter(str_detect(location, paste(england, collapse = "|")) |
           str_detect(place_name, paste(england, collapse = "|")) |
           str_detect(place_full_name, paste(england, collapse = "|")) |
           str_detect(country, paste(england, collapse = "|"))) %>%
  pull(status_id)


# 3. rows which might include Irish or Northern Irish locations 
# "elfast", "Co. Down", "Derry"
ireland <- c("reland", "elfast", "Co. Down", "Derry")

rm_ireland <-
  data_ord %>%
  filter(str_detect(location, paste(ireland, collapse = "|")) |
           str_detect(place_name, paste(ireland, collapse = "|")) |
           str_detect(place_full_name, paste(ireland, collapse = "|")) |
           str_detect(country, paste(ireland, collapse = "|")) |
           country_code == "IE") %>%
  pull(status_id)


# 4. rows which might include Welsh locations
wales <- c("Wales", "Llandudno")

rm_wales <-
  data_ord %>%
  filter(str_detect(location, paste(wales, collapse = "|")) |
           str_detect(place_name, paste(wales, collapse = "|")) |
           str_detect(place_full_name, paste(wales, collapse = "|")) |
           str_detect(country, paste(wales, collapse = "|"))) %>%
  pull(status_id)


# 5. check remaining rows for outliers
data_scotland <-
  data_ord %>%
  filter(!status_id %in% rm_generic,
         !status_id %in% rm_england,
         !status_id %in% rm_ireland,
         !status_id %in% rm_wales)


# -------------------------------------------------------------------------

# Read in data
shp <- 
  st_read("pub_las.shp")
# Inspect options for names of local authorities in the shapefile
shp$local_auth


# -------------------------------------------------------------------------

# Assign data_scotland to names compatible with shp$local_auth categories
# Did this the long way while working on something else, def could be more efficient!!
data_scotland_LA <-
  data_scotland %>%
  mutate(local_auth = case_when(
    location == "Aberdeen" | 
      place_name == "Aberdeen" | 
      place_full_name == "Aberdeen" ~ "Aberdeen City",
    str_detect(location, "berlour") | 
      str_detect(place_name, "berlour") | 
      str_detect(place_full_name, "berlour") ~ "Moray",
    str_detect(location, "Angus") | 
      str_detect(place_name, "Angus") | 
      str_detect(place_full_name, "Angus") ~ "Angus",
    str_detect(location, "Annan") | 
      str_detect(place_name, "Annan") | 
      str_detect(place_full_name, "Annan") ~ "Dumfries and Galloway",
    str_detect(location, "nstruther") | 
      str_detect(place_name, "nstruther") | 
      str_detect(place_full_name, "nstruther") ~ "Fife",
    str_detect(location, "rbroath") | 
      str_detect(place_name, "rbroath") | 
      str_detect(place_full_name, "rbroath") ~ "Angus",
    str_detect(location, "viemore") | 
      str_detect(place_name, "viemore") | 
      str_detect(place_full_name, "viemore") ~ "Highland",
    str_detect(location, "Banchory") | 
      str_detect(place_name, "Banchory") | 
      str_detect(place_full_name, "Banchory") ~ "Aberdeenshire",
    str_detect(location, "Barrhead") | 
      str_detect(place_name, "Barrhead") | 
      str_detect(place_full_name, "Barrhead") ~ "East Renfrewshire",
    str_detect(location, "ishopbriggs") | 
      str_detect(place_name, "ishopbriggs") | 
      str_detect(place_full_name, "ishopbriggs") ~ "East Dunbartonshire",
    str_detect(location, "Bo'ness") | 
      str_detect(place_name, "Bo'ness") | 
      str_detect(place_full_name, "Bo'ness") ~ "Falkirk",
    str_detect(location, "Bonnybridge") | 
      str_detect(place_name, "Bonnybridge") | 
      str_detect(place_full_name, "Bonnybridge") ~ "Falkirk",
    str_detect(location, "Brechin") | 
      str_detect(place_name, "Brechin") | 
      str_detect(place_full_name, "Brechin") ~ "Angus",
    str_detect(location, "Bridge of Allan") | 
      str_detect(place_name, "Bridge of Allan") | 
      str_detect(place_full_name, "Bridge of Allan") ~ "Stirling",
    str_detect(location, "Brora") | 
      str_detect(place_name, "Brora") | 
      str_detect(place_full_name, "Brora") ~ "Highland",
    str_detect(location, "Cairngorms") | 
      str_detect(place_name, "Cairngorms") | 
      str_detect(place_full_name, "Cairngorms") ~ "Highland",
    str_detect(location, "ambuslang") | 
      str_detect(place_name, "ambuslang") | 
      str_detect(place_full_name, "ambuslang") ~ "South Lanarkshire",
    str_detect(location, "ampbeltown") | 
      str_detect(place_name, "ampbeltown") | 
      str_detect(place_full_name, "ampbeltown") ~ "Argyll and Bute",
    str_detect(location, "umbernauld") | 
      str_detect(place_name, "umbernauld") | 
      str_detect(place_full_name, "umbernauld") ~ "North Lanarkshire",
    str_detect(location, "arnoustie") | 
      str_detect(place_name, "arnoustie") | 
      str_detect(place_full_name, "arnoustie") ~ "Angus",
    str_detect(location, "Cawdor") | 
      str_detect(place_name, "Cawdor") | 
      str_detect(place_full_name, "Cawdor") ~ "Highland",
    str_detect(location, "Celtic Park") | 
      str_detect(place_name, "Celtic Park") | 
      str_detect(place_full_name, "Celtic Park") ~ "Glasgow City",
    str_detect(location, "lydebank") | 
      str_detect(place_name, "lydebank") | 
      str_detect(place_full_name, "lydebank") ~ "West Dunbartonshire",
    str_detect(location, "oatbridge") | 
      str_detect(place_name, "oatbridge") | 
      str_detect(place_full_name, "oatbridge") ~ "North Lanarkshire",
    str_detect(location, "owdenbeath") | 
      str_detect(place_name, "owdenbeath") | 
      str_detect(place_full_name, "owdenbeath") ~ "Fife",
    str_detect(location, "raigellachie") | 
      str_detect(place_name, "raigellachie") | 
      str_detect(place_full_name, "raigellachie") ~ "Moray",
    str_detect(location, "rieff") | 
      str_detect(place_name, "rieff") | 
      str_detect(place_full_name, "rieff") ~ "Perth and Kinross",
    str_detect(location, "Denny") | 
      str_detect(place_name, "Denny") | 
      str_detect(place_full_name, "Denny") ~ "Falkirk",
    str_detect(location, "Dollar") | 
      str_detect(place_name, "Dollar") | 
      str_detect(place_full_name, "Dollar") ~ "Clackmannanshire",
    str_detect(location, "Dornoch") | 
      str_detect(place_name, "Dornoch") | 
      str_detect(place_full_name, "Dornoch") ~ "Highland",
    str_detect(location, "undee") | 
      str_detect(place_name, "undee") | 
      str_detect(place_full_name, "undee") ~ "Dundee City",
    str_detect(location, "ufftown") | 
      str_detect(place_name, "ufftown") | 
      str_detect(place_full_name, "ufftown") ~ "Moray",
    str_detect(location, "umbarton") | 
      str_detect(place_name, "umbarton") | 
      str_detect(place_full_name, "umbarton") ~ "West Dunbartonshire",
    str_detect(location, "umfries") | 
      str_detect(place_name, "umfries") | 
      str_detect(place_full_name, "umfries") ~ "Dumfries and Galloway",
    str_detect(location, "unblane") | 
      str_detect(place_name, "unblane") | 
      str_detect(place_full_name, "unblane") ~ "Stirling",
    str_detect(location, "unfermline") | 
      str_detect(place_name, "unfermline") | 
      str_detect(place_full_name, "unfermline") ~ "Fife",
    str_detect(location, "East Kilbride") | 
      str_detect(place_name, "East Kilbride") | 
      str_detect(place_full_name, "East Kilbride") ~ "South Lanarkshire",
    str_detect(location, "East Lothian") | 
      str_detect(place_name, "East Lothian") | 
      str_detect(place_full_name, "East Lothian") ~ "East Lothian",
    str_detect(location, "East Renfrewshire") | 
      str_detect(place_name, "East Renfrewshire") | 
      str_detect(place_full_name, "East Renfrewshire") ~ "East Renfrewshire",
    str_detect(location, "dinburgh") | 
      str_detect(place_name, "dinburgh") | 
      str_detect(place_full_name, "dinburgh") ~ "City of Edinburgh",
    str_detect(location, "dimbourg") | 
      str_detect(place_name, "dimbourg") | 
      str_detect(place_full_name, "dimbourg") ~ "City of Edinburgh",
    str_detect(location, "dimburgo") | 
      str_detect(place_name, "dimburgo") | 
      str_detect(place_full_name, "dimburgo") ~ "City of Edinburgh",
    str_detect(location, "Elgin") | 
      str_detect(place_name, "Elgin") | 
      str_detect(place_full_name, "Elgin") ~ "Moray",
    str_detect(location, "Ellon") | 
      str_detect(place_name, "Ellon") | 
      str_detect(place_full_name, "Ellon") ~ "Aberdeenshire",
    str_detect(location, "alkirk") | 
      str_detect(place_name, "alkirk") | 
      str_detect(place_full_name, "alkirk") ~ "Falkirk",
    str_detect(location, "Fife") | 
      str_detect(place_name, "Fife") | 
      str_detect(place_full_name, "Fife") ~ "Fife",
    str_detect(location, "Forres") | 
      str_detect(place_name, "Forres") | 
      str_detect(place_full_name, "Forres") ~ "Moray",
    str_detect(location, "Galston") | 
      str_detect(place_name, "Galston") | 
      str_detect(place_full_name, "Galston") ~ "East Ayrshire",
    str_detect(location, "lasgow") | 
      str_detect(place_name, "lasgow") |
      str_detect(place_full_name, "lasgow") ~ "Glasgow City",
    str_detect(location, "glesga") | 
      str_detect(place_name, "glesga") | 
      str_detect(place_full_name, "glesga") ~ "Glasgow City",
    str_detect(location, "lenfinnan") | 
      str_detect(place_name, "lenfinnan") | 
      str_detect(place_full_name, "lenfinnan") ~ "Highland",
    str_detect(location, "lenrothes") | 
      str_detect(place_name, "lenrothes") | 
      str_detect(place_full_name, "lenrothes") ~ "Fife",
    str_detect(location, "lenshee") | 
      str_detect(place_name, "lenshee") | 
      str_detect(place_full_name, "lenshee") ~ "Perth and Kinross",
    str_detect(location, "Gourock") | 
      str_detect(place_name, "Gourock") | 
      str_detect(place_full_name, "Gourock") ~ "Inverclyde",
    str_detect(location, "reenock") | 
      str_detect(place_name, "reenock") | 
      str_detect(place_full_name, "reenock") ~ "Inverclyde",
    str_detect(location, "ullane") | 
      str_detect(place_name, "ullane") | 
      str_detect(place_full_name, "ullane") ~ "East Lothian",
    str_detect(location, "addington") | 
      str_detect(place_name, "addington") | 
      str_detect(place_full_name, "addington") ~ "East Lothian",
    str_detect(location, "amilton") | 
      str_detect(place_name, "amilton") | 
      str_detect(place_full_name, "amilton") ~ "South Lanarkshire",
    str_detect(location, "Hawick") | 
      str_detect(place_name, "Hawick") | 
      str_detect(place_full_name, "Hawick") ~ "Scottish Borders",
    str_detect(location, "elensburgh") | 
      str_detect(place_name, "elensburgh") | 
      str_detect(place_full_name, "elensburgh") ~ "Argyll and Bute",
    str_detect(location, "ighlands") | 
      str_detect(place_name, "ighlands") | 
      str_detect(place_full_name, "ighlands") ~ "Highland",
    str_detect(location, "owwood") | 
      str_detect(place_name, "owwood") | 
      str_detect(place_full_name, "owwood") ~ "Renfrewshire",
    str_detect(location, "Huntly") | 
      str_detect(place_name, "Huntly") | 
      str_detect(place_full_name, "Huntly") ~ "Aberdeenshire",
    str_detect(location, "nverclyde") | 
      str_detect(place_name, "nverclyde") | 
      str_detect(place_full_name, "nverclyde") ~ "Inverclyde",
    str_detect(location, "nverness") | 
      str_detect(place_name, "nverness") | 
      str_detect(place_full_name, "nverness") ~ "Highland",
    str_detect(location, "nverurie") | 
      str_detect(place_name, "nverurie") | 
      str_detect(place_full_name, "nverurie") ~ "Aberdeenshire",
    str_detect(location, "rvine") | 
      str_detect(place_name, "rvine") | 
      str_detect(place_full_name, "rvine") ~ "North Ayrshire",
    str_detect(location, "Islay") | 
      str_detect(place_name, "Islay") | 
      str_detect(place_full_name, "Islay") ~ "Argyll and Bute",
    str_detect(location, "islay") | 
      str_detect(place_name, "islay") | 
      str_detect(place_full_name, "islay") ~ "Argyll and Bute",
    str_detect(location, "Arran") | 
      str_detect(place_name, "Arran") | 
      str_detect(place_full_name, "Arran") ~ "North Ayrshire",
    str_detect(location, "Barra") | 
      str_detect(place_name, "Barra") | 
      str_detect(place_full_name, "Barra") ~ "Eilean Siar",
    str_detect(location, "Harris") | 
      str_detect(place_name, "Harris") | 
      str_detect(place_full_name, "Harris") ~ "Eilean Siar",
    str_detect(location, "Lewis") | 
      str_detect(place_name, "Lewis") | 
      str_detect(place_full_name, "Lewis") ~ "Eilean Siar",
    str_detect(location, "Skye") | 
      str_detect(place_name, "Skye") | 
      str_detect(place_full_name, "Skye") ~ "Highland",
    str_detect(location, "skye") | 
      str_detect(place_name, "skye") | 
      str_detect(place_full_name, "skye") ~ "Highland",
    str_detect(location, "Jedburgh") | 
      str_detect(place_name, "Jedburgh") | 
      str_detect(place_full_name, "Jedburgh") ~ "Scottish Borders",
    str_detect(location, "ilmarnock") | 
      str_detect(place_name, "ilmarnock") | 
      str_detect(place_full_name, "ilmarnock") ~ "East Ayrshire",
    str_detect(location, "incardineshire") | 
      str_detect(place_name, "incardineshire") | 
      str_detect(place_full_name, "incardineshire") ~ "Aberdeenshire",
    str_detect(location, "ingussie") | 
      str_detect(place_name, "ingussie") | 
      str_detect(place_full_name, "ingussie") ~ "Highland",
    str_detect(location, "irkcaldy") | 
      str_detect(place_name, "irkcaldy") | 
      str_detect(place_full_name, "irkcaldy") ~ "Fife",
    str_detect(location, "irkintilloch") | 
      str_detect(place_name, "irkintilloch") | 
      str_detect(place_full_name, "irkintilloch") ~ "East Dunbartonshire",
    str_detect(location, "irknewton") | 
      str_detect(place_name, "irknewton") | 
      str_detect(place_full_name, "irknewton") ~ "West Lothian",
    str_detect(location, "Lanark") | 
      str_detect(place_name, "Lanark") | 
      str_detect(place_full_name, "Lanark") ~ "South Lanarkshire",
    str_detect(location, "angholm") | 
      str_detect(place_name, "angholm") | 
      str_detect(place_full_name, "angholm") ~ "Dumfries and Galloway",
    str_detect(location, "Leith") | 
      str_detect(place_name, "Leith") | 
      str_detect(place_full_name, "Leith") ~ "City of Edinburgh",
    str_detect(location, "Largs") | 
      str_detect(place_name, "Largs") | 
      str_detect(place_full_name, "Largs") ~ "North Ayrshire",
    str_detect(location, "ennoxtown") | 
      str_detect(place_name, "ennoxtown") | 
      str_detect(place_full_name, "ennoxtown") ~ "East Dunbartonshire",
    str_detect(location, "Leven") | 
      str_detect(place_name, "Leven") | 
      str_detect(place_full_name, "Leven") ~ "Fife",
    str_detect(location, "inlithgow") | 
      str_detect(place_name, "inlithgow") | 
      str_detect(place_full_name, "inlithgow") ~ "West Lothian",
    str_detect(location, "Linwood") | 
      str_detect(place_name, "Linwood") | 
      str_detect(place_full_name, "Linwood") ~ "Renfrewshire",
    str_detect(location, "ivingston") | 
      str_detect(place_name, "ivingston") | 
      str_detect(place_full_name, "ivingston") ~ "West Lothian",
    str_detect(location, "ilngavie") | 
      str_detect(place_name, "ilngavie") | 
      str_detect(place_full_name, "ilngavie") ~ "East Dunbartonshire",
    str_detect(location, "onikie") | 
      str_detect(place_name, "onikie") | 
      str_detect(place_full_name, "onikie") ~ "Angus",
    str_detect(location, "Moray") | 
      str_detect(place_name, "Moray") | 
      str_detect(place_full_name, "Moray") ~ "Moray",
    str_detect(location, "otherwell") | 
      str_detect(place_name, "otherwell") | 
      str_detect(place_full_name, "otherwell") ~ "North Lanarkshire",
    str_detect(location, "NEWBURGH") | 
      str_detect(place_name, "NEWBURGH") | 
      str_detect(place_full_name, "NEWBURGH") ~ "Aberdeenshire",
    str_detect(location, "ewburgh") | 
      str_detect(place_name, "ewburgh") | 
      str_detect(place_full_name, "ewburgh") ~ "Aberdeenshire",
    str_detect(location, "North Ayrshire") | 
      str_detect(place_name, "North Ayrshire") | 
      str_detect(place_full_name, "North Ayrshire") ~ "North Ayreshire",
    str_detect(location, "North Lanarkshire") | 
      str_detect(place_name, "North Lanarkshire") | 
      str_detect(place_full_name, "North Lanarkshire") ~ "North Lanarkshire",
    str_detect(location, "Oban") | 
      str_detect(place_name, "Oban") | 
      str_detect(place_full_name, "Oban") ~ "Argyll and Bute",
    str_detect(location, "Outer Hebrides") | 
      str_detect(place_name, "Outer Hebrides") | 
      str_detect(place_full_name, "Outer Hebrides") ~ "Eilean Siar",
    str_detect(location, "aisley") | 
      str_detect(place_name, "aisley") | 
      str_detect(place_full_name, "aisley") ~ "Renfrewshire",
    str_detect(location, "Perth") | 
      str_detect(place_name, "Perth") | 
      str_detect(place_full_name, "Perth") ~ "Perth and Kinross",
    str_detect(location, "itlochry") | 
      str_detect(place_name, "itlochry") | 
      str_detect(place_full_name, "itlochry") ~ "Perth and Kinross",
    str_detect(location, "ittenweem") | 
      str_detect(place_name, "ittenweem") | 
      str_detect(place_full_name, "ittenweem") ~ "Fife",
    str_detect(location, "ortree") | 
      str_detect(place_name, "ortree") | 
      str_detect(place_full_name, "ortree") ~ "Highland",
    str_detect(location, "Renfrew") | 
      str_detect(place_name, "Renfrew") | 
      str_detect(place_full_name, "Renfrew") ~ "Renfrewshire",
    str_detect(location, "Roslin") | 
      str_detect(place_name, "Roslin") | 
      str_detect(place_full_name, "Roslin") ~ "Midlothian",
    str_detect(location, "Fort William") | 
      str_detect(place_name, "Fort William") | 
      str_detect(place_full_name, "Fort William") ~ "Highland",
    str_detect(location, "Wemyss") | 
      str_detect(place_name, "Wemyss") | 
      str_detect(place_full_name, "Wemyss") ~ "Inverclyde",
    str_detect(location, "Govan") | 
      str_detect(place_name, "Govan") | 
      str_detect(place_full_name, "Govan") ~ "Glasgow City",
    str_detect(location, "ilmacolm") | 
      str_detect(place_name, "ilmacolm") | 
      str_detect(place_full_name, "ilmacolm") ~ "Inverclyde",
    str_detect(location, "Scottish Borders") | 
      str_detect(place_name, "Scottish Borders") | 
      str_detect(place_full_name, "Scottish Borders") ~ "Scottish Borders",
    str_detect(location, "hawlands") | 
      str_detect(place_name, "hawlands") | 
      str_detect(place_full_name, "hawlands") ~ "Glasgow City",
    str_detect(location, "South Ayrshire") | 
      str_detect(place_name, "South Ayrshire") | 
      str_detect(place_full_name, "South Ayrshire") ~ "South Ayrshire",
    str_detect(location, "peyside") | 
      str_detect(place_name, "peyside") | 
      str_detect(place_full_name, "peyside") ~ "Moray",
    str_detect(location, "ndrews") | 
      str_detect(place_name, "ndrews") | 
      str_detect(place_full_name, "ndrews") ~ "Fife",
    str_detect(location, "oswells") | 
      str_detect(place_name, "oswells") | 
      str_detect(place_full_name, "oswells") ~ "Scottish Borders",
    str_detect(location, "tirling") | 
      str_detect(place_name, "tirling") | 
      str_detect(place_full_name, "tirling") ~ "Stirling",
    str_detect(location, "tonehaven") | 
      str_detect(place_name, "tonehaven") | 
      str_detect(place_full_name, "tonehaven") ~ "Aberdeenshire",
    str_detect(location, "tornoway") | 
      str_detect(place_name, "tornoway") | 
      str_detect(place_full_name, "tornoway") ~ "Eilean Siar",
    str_detect(location, "Tain") | 
      str_detect(place_name, "Tain") | 
      str_detect(place_full_name, "Tain") ~ "Highland",
    str_detect(location, "hurso") | 
      str_detect(place_name, "hurso") | 
      str_detect(place_full_name, "hurso") ~ "Highland",
    str_detect(location, "Brodick") | 
      str_detect(place_name, "Brodick") | 
      str_detect(place_full_name, "Brodick") ~ "North Ayrshire",
    str_detect(location, "Tranent") | 
      str_detect(place_name, "Tranent") | 
      str_detect(place_full_name, "Tranent") ~ "East Lothian",
    str_detect(location, "Troon") | 
      str_detect(place_name, "Troon") | 
      str_detect(place_full_name, "Troon") ~ "South Ayrshire",
    str_detect(location, "urriff") | 
      str_detect(place_name, "urriff") | 
      str_detect(place_full_name, "urriff") ~ "Aberdeenshire",
    str_detect(location, "llapool") | 
      str_detect(place_name, "llapool") | 
      str_detect(place_full_name, "llapool") ~ "Highland",
    str_detect(location, "West Lothian") | 
      str_detect(place_name, "West Lothian") | 
      str_detect(place_full_name, "West Lothian") ~ "West Lothian",
    str_detect(location, "hitburn") | 
      str_detect(place_name, "hitburn") | 
      str_detect(place_full_name, "hitburn") ~ "West Lothian",
    str_detect(location, "hithorn") | 
      str_detect(place_name, "hithorn") | 
      str_detect(place_full_name, "hithorn") ~ "Dumfries and Galloway",
    str_detect(location, "ddingston") | 
      str_detect(place_name, "ddingston") | 
      str_detect(place_full_name, "ddingston") ~ "South Lanarkshire",
    str_detect(location, "owmore") | 
      str_detect(place_name, "owmore") | 
      str_detect(place_full_name, "owmore") ~ "Argyll and Bute",
    TRUE ~ "NA")
)
  

# Tidied
data_scotland_ord <- 
  data_scotland_LA %>%
  relocate(
    status_id, # unique id
    local_auth, # local authority as assigned
    location, place_name, place_full_name, place_type, country, country_code, # location descriptors
    geo_coords, coords_coords, bbox_coords, # location coordinates
    created_at, # date & time stamp
    text) # tweet

# For speed of exercise not trying to find location based solely on coordinates now
# Remove rows without descriptive names
data_scotland_final <- data_scotland_ord %>% filter(local_auth != "NA")

# Write out
# write_rds(data_scotland_final, "data/data_scotland_final.RDS")