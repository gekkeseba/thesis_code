install.packages("haven")
library(haven)
setwd('/Users/seba')
############################ Data loading and removing irrelevant columns#########
#Setting up the data and renaming the collumn login name to correspond with the other dataset's channelurl
ban_descriptives <- read_dta("ban_descriptives.dta")
twitch_streams <- read_dta("streams.dta")
names(ban_descriptives)[names(ban_descriptives) == "login_name"] <- "channelurl"
attach(twitch_streams)
#Keep only the relevant collumns which are used in the estimation.
twitch_streams <- subset(twitch_streams, select = -c(viewgain, numgames, followersperhour, played_star_game, played_topgame, new_game_played,
                                                     total_maxviewers, total_streamedminutes, total_viewminutes, mature, streamed_again_1day, streamed_again_1month, streamed_again_1week))

################ Loading required packages in order to transform into weekly data ###########################

#turn data in to weekly data
library(tidyquant)
library(dplyr)
library(plm)
library(tibbletime)
################### Unbalanced panel ####################################################
df <- subset(twitch_streams, twitch_streams$t <= '2020-08-01')

  #is balanced going well? Yes
library(dplyr)
#Gather the week numbers
WEEKSNUMBERS <- twitch_streams %>%
  tq_transmute(select     = avgviewers,
               mutate_fun = apply.weekly,
               FUN        = sum)

#subset to only have 1 year of data (52 weeks)

weekly_streams <- df %>%
  group_by(user, week = lubridate::week(t)) %>%
  summarise(avgviewers = mean(avgviewers), avgfollowers = mean(total_followers), meanstreams = mean(nstreams), total_avgviewers = mean(total_avgviewers))

is.pbalanced(weekly_streams)

weekly_streams$treatment_1 <- ifelse(weekly_streams$user == 89320, 1, 0)
weekly_streams$treatment_2 <- ifelse(weekly_streams$user == 9967283, 1, weekly_streams$treatment_1 )
weekly_streams$treatment_3 <- ifelse(weekly_streams$user == 4881212, 1, weekly_streams$treatment_2 )
weekly_streams$treatment_4 <- ifelse(weekly_streams$user == 5716, 1, weekly_streams$treatment_3 )

weekly_streams$treatment_5 <- ifelse(weekly_streams$user == 2398, 1, weekly_streams$treatment_4)
weekly_streams$treatment_6 <- ifelse(weekly_streams$user == 87374, 1, weekly_streams$treatment_5 )
weekly_streams$treatment_7 <- ifelse(weekly_streams$user == 16095569, 1, weekly_streams$treatment_6 )
weekly_streams$treatment_8 <- ifelse(weekly_streams$user == 201894, 1, weekly_streams$treatment_7 )

weekly_streams$treatment_9 <- ifelse(weekly_streams$user == 159814, 1, weekly_streams$treatment_8)
#weekly_streams$treatment_10 <- ifelse(weekly_streams$user == , 1, weekly_streams$treatment_9 )
weekly_streams$treatment_11 <- ifelse(weekly_streams$user == 11875, 1, weekly_streams$treatment_9 )
weekly_streams$treatment_12 <- ifelse(weekly_streams$user == 10751716, 1, weekly_streams$treatment_11 )

weekly_streams$treatment_13 <- ifelse(weekly_streams$user == 99163, 1, weekly_streams$treatment_12)
weekly_streams$treatment_14 <- ifelse(weekly_streams$user == 2946556, 1, weekly_streams$treatment_13 )
weekly_streams$treatment_15 <- ifelse(weekly_streams$user == 629695, 1, weekly_streams$treatment_14 )
weekly_streams$treatment_16 <- ifelse(weekly_streams$user == 432189, 1, weekly_streams$treatment_15 )

weekly_streams$treatment_17 <- ifelse(weekly_streams$user == 99657, 1, weekly_streams$treatment_16)
#weekly_streams$treatment_18 <- ifelse(weekly_streams$user == 'rocketbeanstv', 1, weekly_streams$treatment_17 )
weekly_streams$treatment_19 <- ifelse(weekly_streams$user == 11477953, 1, weekly_streams$treatment_17 )
weekly_streams$treatment_20 <- ifelse(weekly_streams$user == 5134475, 1, weekly_streams$treatment_19 )

#weekly_streams$treatment_21 <- ifelse(weekly_streams$user == 'power_bada', 1, weekly_streams$treatment_20)
weekly_streams$treatment_22 <- ifelse(weekly_streams$user == 9339152, 1, weekly_streams$treatment_20 )
weekly_streams$treatment_23 <- ifelse(weekly_streams$user == 20849534, 1, weekly_streams$treatment_22 )
weekly_streams$treatment_24 <- ifelse(weekly_streams$user == 76047, 1, weekly_streams$treatment_23 )

weekly_streams$treatment_25 <- ifelse(weekly_streams$user == 8536665, 1, weekly_streams$treatment_24)
weekly_streams$treatment_26 <- ifelse(weekly_streams$user == 11133898, 1, weekly_streams$treatment_25 )
weekly_streams$treatment_27 <- ifelse(weekly_streams$user == 720534, 1, weekly_streams$treatment_26 )
weekly_streams$treatment_28 <- ifelse(weekly_streams$user == 6779215, 1, weekly_streams$treatment_27 )
weekly_streams$treatment_29 <- ifelse(weekly_streams$user == 65077, 1, weekly_streams$treatment_28 )

weekly_streams$treatment <- ifelse(weekly_streams$user == 137446, 1, weekly_streams$treatment_29)

#Treatment variable created, now lets create a subset of only the relevant treatment variable
myvariables_1 <- c("user", "week", "avgviewers", "treatment", "avgfollowers", "meanstreams", "total_avgviewers", "meanstreams")
weekly_str <- weekly_streams[myvariables_1]
weekly_str$before_after_1 <- ifelse(weekly_str$user == 9967283, 2, 0)
weekly_str$before_after_2 <- ifelse(weekly_str$user == 89320 ,3, weekly_str$before_after_1)
weekly_str$before_after_3 <- ifelse(weekly_str$user == 4881212 ,4, weekly_str$before_after_2)
weekly_str$before_after_4 <- ifelse(weekly_str$user == 5716 , 7, weekly_str$before_after_3)

weekly_str$before_after_5 <- ifelse(weekly_str$user == 2398, 7, weekly_str$before_after_4)
weekly_str$before_after_6 <- ifelse(weekly_str$user == 87374, 7, weekly_str$before_after_5)
weekly_str$before_after_7 <- ifelse(weekly_str$user == 16095569, 8, weekly_str$before_after_6)
weekly_str$before_after_8 <- ifelse(weekly_str$user == 201894, 11, weekly_str$before_after_7)

weekly_str$before_after_9 <- ifelse(weekly_str$user == 159814, 12, weekly_str$before_after_8)
#weekly_str$before_after_10 <- ifelse(weekly_str$user == 'jessietv' & weekly_str$week >= 12 , 1, weekly_str$before_after_9)
weekly_str$before_after_11 <- ifelse(weekly_str$user == 11875 , 13, weekly_str$before_after_9)
weekly_str$before_after_12 <- ifelse(weekly_str$user == 10751716, 13, weekly_str$before_after_11)

weekly_str$before_after_13 <- ifelse(weekly_str$user == 99163, 14, weekly_str$before_after_12)
weekly_str$before_after_14 <- ifelse(weekly_str$user == 2946556 , 14, weekly_str$before_after_13)
weekly_str$before_after_15 <- ifelse(weekly_str$user == 629695 , 16, weekly_str$before_after_14)
weekly_str$before_after_16 <- ifelse(weekly_str$user == 432189 , 16, weekly_str$before_after_15)

weekly_str$before_after_17 <- ifelse(weekly_str$user == 99657, 24 , weekly_str$before_after_16)
#weekly_str$before_after_18 <- ifelse(weekly_str$user == 'rocketbeanstv' & weekly_str$week >= 29 , 1, weekly_str$before_after_17)
weekly_str$before_after_19 <- ifelse(weekly_str$user == 11477953 , 30, weekly_str$before_after_17)
weekly_str$before_after_20 <- ifelse(weekly_str$user == 65077 , 35, weekly_str$before_after_19)
weekly_str$before_after_21 <- ifelse(weekly_str$user == 5134475 , 36, weekly_str$before_after_20)

#weekly_str$before_after_22 <- ifelse(weekly_str$user == 'power_bada' & weekly_str$week >= 39 , 1, weekly_str$before_after_21)
weekly_str$before_after_23 <- ifelse(weekly_str$user == 9339152, 41, weekly_str$before_after_21)
weekly_str$before_after_24 <- ifelse(weekly_str$user == 76047 , 44, weekly_str$before_after_23)
weekly_str$before_after_25 <- ifelse(weekly_str$user == 20849534 , 42, weekly_str$before_after_24)

weekly_str$before_after_26 <- ifelse(weekly_str$user == 11133898 , 45, weekly_str$before_after_25)
weekly_str$before_after_27 <- ifelse(weekly_str$user == 8536665 ,44, weekly_str$before_after_26)
weekly_str$before_after_28 <- ifelse(weekly_str$user == 720534, 52, weekly_str$before_after_27)
weekly_str$before_after_29 <- ifelse(weekly_str$user == 6779215 , 52, weekly_str$before_after_28)
weekly_str$before_after <- ifelse(weekly_str$user == 137446, 52, weekly_str$before_after_29)

myvariables_2 <- c("user", "week", "avgviewers", "treatment", "before_after", "avgfollowers", "total_avgviewers", "meanstreams")

newdata_unbal <- weekly_str[myvariables_2]

#Resulting in a balanced panel dataset
is.pbalanced(newdata)

######################## Balanced panel ############################ ###################################
twitch_streams_bal <- subset(twitch_streams, twitch_streams$t <= '2020-08-01')


df_balanced <- make.pbalanced(twitch_streams_bal, balance.type = "fill")
df_balanced$t <- as.Date(df_balanced$t)

library(tidyr)
#fill with next values
df_balanced <- df_balanced %>% group_by(user) %>%fill(total_avgviewers, total_followers, .direction = 'down')

#only select the variables of interest else there will be issues arising due to charaqcter collumns
myvariables_0 <- c("user", "t", "avgviewers", "total_followers", "total_avgviewers")
df_balanced_1 <- df_balanced[myvariables_0]

df_balanced_1[is.na(df_balanced_1)] <- 0
is.pbalanced(df_balanced_1)

weekly_streams_balanced <- df_balanced_1 %>%
  group_by(user, week = lubridate::week(t)) %>%
  summarise(avgviewers = mean(avgviewers), total_avgfollowers = mean(total_followers), total_avgviewers = mean(total_avgviewers))
is.pbalanced(weekly_streams_balanced)

attach(weekly_streams_balanced)
weekly_streams_balanced$treatment_1 <- ifelse(weekly_streams_balanced$user == 89320, 1, 0)
weekly_streams_balanced$treatment_2 <- ifelse(weekly_streams_balanced$user == 9967283, 1, weekly_streams_balanced$treatment_1 )
weekly_streams_balanced$treatment_3 <- ifelse(weekly_streams_balanced$user == 4881212, 1, weekly_streams_balanced$treatment_2 )
weekly_streams_balanced$treatment_4 <- ifelse(weekly_streams_balanced$user == 5716, 1, weekly_streams_balanced$treatment_3 )

weekly_streams_balanced$treatment_5 <- ifelse(weekly_streams_balanced$user == 2398, 1, weekly_streams_balanced$treatment_4)
weekly_streams_balanced$treatment_6 <- ifelse(weekly_streams_balanced$user == 87374, 1, weekly_streams_balanced$treatment_5 )
weekly_streams_balanced$treatment_7 <- ifelse(weekly_streams_balanced$user == 16095569, 1, weekly_streams_balanced$treatment_6 )
weekly_streams_balanced$treatment_8 <- ifelse(weekly_streams_balanced$user == 201894, 1, weekly_streams_balanced$treatment_7 )

weekly_streams_balanced$treatment_9 <- ifelse(weekly_streams_balanced$user == 159814, 1, weekly_streams_balanced$treatment_8)
#weekly_streams_balanced$treatment_10 <- ifelse(weekly_streams_balanced$user == , 1, weekly_streams_balanced$treatment_9 )
weekly_streams_balanced$treatment_11 <- ifelse(weekly_streams_balanced$user == 11875, 1, weekly_streams_balanced$treatment_9 )
weekly_streams_balanced$treatment_12 <- ifelse(weekly_streams_balanced$user == 10751716, 1, weekly_streams_balanced$treatment_11 )

weekly_streams_balanced$treatment_13 <- ifelse(weekly_streams_balanced$user == 99163, 1, weekly_streams_balanced$treatment_12)
weekly_streams_balanced$treatment_14 <- ifelse(weekly_streams_balanced$user == 2946556, 1, weekly_streams_balanced$treatment_13 )
weekly_streams_balanced$treatment_15 <- ifelse(weekly_streams_balanced$user == 629695, 1, weekly_streams_balanced$treatment_14 )
weekly_streams_balanced$treatment_16 <- ifelse(weekly_streams_balanced$user == 432189, 1, weekly_streams_balanced$treatment_15 )

weekly_streams_balanced$treatment_17 <- ifelse(weekly_streams_balanced$user == 99657, 1, weekly_streams_balanced$treatment_16)
#weekly_streams_balanced$treatment_18 <- ifelse(weekly_streams_balanced$user == 'rocketbeanstv', 1, weekly_streams_balanced$treatment_17 )
weekly_streams_balanced$treatment_19 <- ifelse(weekly_streams_balanced$user == 11477953, 1, weekly_streams_balanced$treatment_17 )
weekly_streams_balanced$treatment_20 <- ifelse(weekly_streams_balanced$user == 5134475, 1, weekly_streams_balanced$treatment_19 )

#weekly_streams_balanced$treatment_21 <- ifelse(weekly_streams_balanced$user == 'power_bada', 1, weekly_streams_balanced$treatment_20)
weekly_streams_balanced$treatment_22 <- ifelse(weekly_streams_balanced$user == 9339152, 1, weekly_streams_balanced$treatment_20 )
weekly_streams_balanced$treatment_23 <- ifelse(weekly_streams_balanced$user == 20849534, 1, weekly_streams_balanced$treatment_22 )
weekly_streams_balanced$treatment_24 <- ifelse(weekly_streams_balanced$user == 76047, 1, weekly_streams_balanced$treatment_23 )

weekly_streams_balanced$treatment_25 <- ifelse(weekly_streams_balanced$user == 8536665, 1, weekly_streams_balanced$treatment_24)
weekly_streams_balanced$treatment_26 <- ifelse(weekly_streams_balanced$user == 11133898, 1, weekly_streams_balanced$treatment_25 )
weekly_streams_balanced$treatment_27 <- ifelse(weekly_streams_balanced$user == 720534, 1, weekly_streams_balanced$treatment_26 )
weekly_streams_balanced$treatment_28 <- ifelse(weekly_streams_balanced$user == 6779215, 1, weekly_streams_balanced$treatment_27 )
weekly_streams_balanced$treatment_29 <- ifelse(weekly_streams_balanced$user == 65077, 1, weekly_streams_balanced$treatment_28 )

weekly_streams_balanced$treatment <- ifelse(weekly_streams_balanced$user == 137446, 1, weekly_streams_balanced$treatment_29)

#Treatment variable created, now lets create a subset of only the relevant treatment variable
myvariables_1 <- c("user", "week", "avgviewers", "treatment", "total_avgfollowers", "total_avgviewers")
weekly_str_balanced <- weekly_streams_balanced[myvariables_1]

  #Check to see whether we now obtain a weekly balanced dataframe, we do!

weekly_str_balanced <- weekly_str_balanced %>% filter(week != '53')


weekly_str_balanced$before_after_1 <- ifelse(weekly_str_balanced$user == 9967283, 2, 0)
weekly_str_balanced$before_after_2 <- ifelse(weekly_str_balanced$user == 89320 ,3, weekly_str_balanced$before_after_1)
weekly_str_balanced$before_after_3 <- ifelse(weekly_str_balanced$user == 4881212 ,4, weekly_str_balanced$before_after_2)
weekly_str_balanced$before_after_4 <- ifelse(weekly_str_balanced$user == 5716 , 7, weekly_str_balanced$before_after_3)

weekly_str_balanced$before_after_5 <- ifelse(weekly_str_balanced$user == 2398, 7, weekly_str_balanced$before_after_4)
weekly_str_balanced$before_after_6 <- ifelse(weekly_str_balanced$user == 87374, 7, weekly_str_balanced$before_after_5)
weekly_str_balanced$before_after_7 <- ifelse(weekly_str_balanced$user == 16095569, 8, weekly_str_balanced$before_after_6)
weekly_str_balanced$before_after_8 <- ifelse(weekly_str_balanced$user == 201894, 11, weekly_str_balanced$before_after_7)

weekly_str_balanced$before_after_9 <- ifelse(weekly_str_balanced$user == 159814, 12, weekly_str_balanced$before_after_8)
#weekly_str_balanced$before_after_10 <- ifelse(weekly_str_balanced$user == 'jessietv' & weekly_str_balanced$week >= 12 , 1, weekly_str_balanced$before_after_9)
weekly_str_balanced$before_after_11 <- ifelse(weekly_str_balanced$user == 11875 , 13, weekly_str_balanced$before_after_9)
weekly_str_balanced$before_after_12 <- ifelse(weekly_str_balanced$user == 10751716, 13, weekly_str_balanced$before_after_11)

weekly_str_balanced$before_after_13 <- ifelse(weekly_str_balanced$user == 99163, 14, weekly_str_balanced$before_after_12)
weekly_str_balanced$before_after_14 <- ifelse(weekly_str_balanced$user == 2946556 , 14, weekly_str_balanced$before_after_13)
weekly_str_balanced$before_after_15 <- ifelse(weekly_str_balanced$user == 629695 , 16, weekly_str_balanced$before_after_14)
weekly_str_balanced$before_after_16 <- ifelse(weekly_str_balanced$user == 432189 , 16, weekly_str_balanced$before_after_15)

weekly_str_balanced$before_after_17 <- ifelse(weekly_str_balanced$user == 99657, 24 , weekly_str_balanced$before_after_16)
#weekly_str_balanced$before_after_18 <- ifelse(weekly_str_balanced$user == 'rocketbeanstv' & weekly_str_balanced$week >= 29 , 1, weekly_str_balanced$before_after_17)
weekly_str_balanced$before_after_19 <- ifelse(weekly_str_balanced$user == 11477953 , 30, weekly_str_balanced$before_after_17)
weekly_str_balanced$before_after_20 <- ifelse(weekly_str_balanced$user == 65077 , 35, weekly_str_balanced$before_after_19)
weekly_str_balanced$before_after_21 <- ifelse(weekly_str_balanced$user == 5134475 , 36, weekly_str_balanced$before_after_20)

#weekly_str_balanced$before_after_22 <- ifelse(weekly_str_balanced$user == 'power_bada' & weekly_str_balanced$week >= 39 , 1, weekly_str_balanced$before_after_21)
weekly_str_balanced$before_after_23 <- ifelse(weekly_str_balanced$user == 9339152, 41, weekly_str_balanced$before_after_21)
weekly_str_balanced$before_after_24 <- ifelse(weekly_str_balanced$user == 76047 , 44, weekly_str_balanced$before_after_23)
weekly_str_balanced$before_after_25 <- ifelse(weekly_str_balanced$user == 20849534 , 42, weekly_str_balanced$before_after_24)

weekly_str_balanced$before_after_26 <- ifelse(weekly_str_balanced$user == 11133898 , 45, weekly_str_balanced$before_after_25)
weekly_str_balanced$before_after_27 <- ifelse(weekly_str_balanced$user == 8536665 ,44, weekly_str_balanced$before_after_26)
weekly_str_balanced$before_after_28 <- ifelse(weekly_str_balanced$user == 720534, 52, weekly_str_balanced$before_after_27)
weekly_str_balanced$before_after_29 <- ifelse(weekly_str_balanced$user == 6779215 , 52, weekly_str_balanced$before_after_28)
weekly_str_balanced$before_after <- ifelse(weekly_str_balanced$user == 137446, 52, weekly_str_balanced$before_after_29)




myvariables_2 <- c("user", "week", "avgviewers", "treatment", "before_after","total_avgfollowers", "total_avgviewers" )

newdata_bal <- weekly_str_balanced[myvariables_2]

newdata_bal <- newdata_bal %>% filter(before_after != '52')
is.pbalanced(newdata_bal)
#########setup treatment dummy 1 / 0 for all banned streamers can b deleted ################
attach(weekly_streams)
weekly_streams$treatment_1 <- ifelse(weekly_streams$user == 89320, 1, 0)
weekly_streams$treatment_2 <- ifelse(weekly_streams$user == 9967283, 1, weekly_streams$treatment_1 )
weekly_streams$treatment_3 <- ifelse(weekly_streams$user == 4881212, 1, weekly_streams$treatment_2 )
weekly_streams$treatment_4 <- ifelse(weekly_streams$user == 5716, 1, weekly_streams$treatment_3 )

weekly_streams$treatment_5 <- ifelse(weekly_streams$user == 2398, 1, weekly_streams$treatment_4)
weekly_streams$treatment_6 <- ifelse(weekly_streams$user == 87374, 1, weekly_streams$treatment_5 )
weekly_streams$treatment_7 <- ifelse(weekly_streams$user == 16095569, 1, weekly_streams$treatment_6 )
weekly_streams$treatment_8 <- ifelse(weekly_streams$user == 201894, 1, weekly_streams$treatment_7 )

weekly_streams$treatment_9 <- ifelse(weekly_streams$user == 159814, 1, weekly_streams$treatment_8)
#weekly_streams$treatment_10 <- ifelse(weekly_streams$user == , 1, weekly_streams$treatment_9 )
weekly_streams$treatment_11 <- ifelse(weekly_streams$user == 11875, 1, weekly_streams$treatment_9 )
weekly_streams$treatment_12 <- ifelse(weekly_streams$user == 10751716, 1, weekly_streams$treatment_11 )

weekly_streams$treatment_13 <- ifelse(weekly_streams$user == 99163, 1, weekly_streams$treatment_12)
weekly_streams$treatment_14 <- ifelse(weekly_streams$user == 2946556, 1, weekly_streams$treatment_13 )
weekly_streams$treatment_15 <- ifelse(weekly_streams$user == 629695, 1, weekly_streams$treatment_14 )
weekly_streams$treatment_16 <- ifelse(weekly_streams$user == 432189, 1, weekly_streams$treatment_15 )

weekly_streams$treatment_17 <- ifelse(weekly_streams$user == 99657, 1, weekly_streams$treatment_16)
#weekly_streams$treatment_18 <- ifelse(weekly_streams$user == 'rocketbeanstv', 1, weekly_streams$treatment_17 )
weekly_streams$treatment_19 <- ifelse(weekly_streams$user == 11477953, 1, weekly_streams$treatment_17 )
weekly_streams$treatment_20 <- ifelse(weekly_streams$user == 5134475, 1, weekly_streams$treatment_19 )

#weekly_streams$treatment_21 <- ifelse(weekly_streams$user == 'power_bada', 1, weekly_streams$treatment_20)
weekly_streams$treatment_22 <- ifelse(weekly_streams$user == 9339152, 1, weekly_streams$treatment_20 )
weekly_streams$treatment_23 <- ifelse(weekly_streams$user == 20849534, 1, weekly_streams$treatment_22 )
weekly_streams$treatment_24 <- ifelse(weekly_streams$user == 76047, 1, weekly_streams$treatment_23 )

weekly_streams$treatment_25 <- ifelse(weekly_streams$user == 8536665, 1, weekly_streams$treatment_24)
weekly_streams$treatment_26 <- ifelse(weekly_streams$user == 11133898, 1, weekly_streams$treatment_25 )
weekly_streams$treatment_27 <- ifelse(weekly_streams$user == 720534, 1, weekly_streams$treatment_26 )
weekly_streams$treatment_28 <- ifelse(weekly_streams$user == 6779215, 1, weekly_streams$treatment_27 )
weekly_streams$treatment_29 <- ifelse(weekly_streams$user == 65077, 1, weekly_streams$treatment_28 )

weekly_streams$treatment <- ifelse(weekly_streams$user == 137446, 1, weekly_streams$treatment_29)

#Treatment variable created, now lets create a subset of only the relevant treatment variable
myvariables_1 <- c("user", "week", "avgviewers", "treatment", "avgfollowers", "meanstreams")
weekly_str <- weekly_streams[myvariables_1]


########################## attempts to use did package from s't anna ##################
weekly_str$before_after_1 <- ifelse(weekly_str$user == 9967283, 2, 0)
weekly_str$before_after_2 <- ifelse(weekly_str$user == 89320 ,3, weekly_str$before_after_1)
weekly_str$before_after_3 <- ifelse(weekly_str$user == 4881212 ,4, weekly_str$before_after_2)
weekly_str$before_after_4 <- ifelse(weekly_str$user == 5716 , 7, weekly_str$before_after_3)

weekly_str$before_after_5 <- ifelse(weekly_str$user == 2398, 7, weekly_str$before_after_4)
weekly_str$before_after_6 <- ifelse(weekly_str$user == 87374, 7, weekly_str$before_after_5)
weekly_str$before_after_7 <- ifelse(weekly_str$user == 16095569, 8, weekly_str$before_after_6)
weekly_str$before_after_8 <- ifelse(weekly_str$user == 201894, 11, weekly_str$before_after_7)

weekly_str$before_after_9 <- ifelse(weekly_str$user == 159814, 12, weekly_str$before_after_8)
#weekly_str$before_after_10 <- ifelse(weekly_str$user == 'jessietv' & weekly_str$week >= 12 , 1, weekly_str$before_after_9)
weekly_str$before_after_11 <- ifelse(weekly_str$user == 11875 , 13, weekly_str$before_after_9)
weekly_str$before_after_12 <- ifelse(weekly_str$user == 10751716, 13, weekly_str$before_after_11)

weekly_str$before_after_13 <- ifelse(weekly_str$user == 99163, 14, weekly_str$before_after_12)
weekly_str$before_after_14 <- ifelse(weekly_str$user == 2946556 , 14, weekly_str$before_after_13)
weekly_str$before_after_15 <- ifelse(weekly_str$user == 629695 , 16, weekly_str$before_after_14)
weekly_str$before_after_16 <- ifelse(weekly_str$user == 432189 , 16, weekly_str$before_after_15)

weekly_str$before_after_17 <- ifelse(weekly_str$user == 99657, 24 , weekly_str$before_after_16)
#weekly_str$before_after_18 <- ifelse(weekly_str$user == 'rocketbeanstv' & weekly_str$week >= 29 , 1, weekly_str$before_after_17)
weekly_str$before_after_19 <- ifelse(weekly_str$user == 11477953 , 30, weekly_str$before_after_17)
weekly_str$before_after_20 <- ifelse(weekly_str$user == 65077 , 35, weekly_str$before_after_19)
weekly_str$before_after_21 <- ifelse(weekly_str$user == 5134475 , 36, weekly_str$before_after_20)

#weekly_str$before_after_22 <- ifelse(weekly_str$user == 'power_bada' & weekly_str$week >= 39 , 1, weekly_str$before_after_21)
weekly_str$before_after_23 <- ifelse(weekly_str$user == 9339152, 41, weekly_str$before_after_21)
weekly_str$before_after_24 <- ifelse(weekly_str$user == 76047 , 44, weekly_str$before_after_23)
weekly_str$before_after_25 <- ifelse(weekly_str$user == 20849534 , 42, weekly_str$before_after_24)

weekly_str$before_after_26 <- ifelse(weekly_str$user == 11133898 , 45, weekly_str$before_after_25)
weekly_str$before_after_27 <- ifelse(weekly_str$user == 8536665 ,44, weekly_str$before_after_26)
weekly_str$before_after_28 <- ifelse(weekly_str$user == 720534, 52, weekly_str$before_after_27)
weekly_str$before_after_29 <- ifelse(weekly_str$user == 6779215 , 52, weekly_str$before_after_28)
weekly_str$before_after <- ifelse(weekly_str$user == 137446, 52, weekly_str$before_after_29)




myvariables_2 <- c("user", "week", "avgviewers", "treatment", "before_after","total_avgfollowers", "total_avgviewers" )

newdata_bal <- weekly_str[myvariables_2]

newdata_bal <- newdata_bal %>% filter(before_after != '52')
#Resulting in a balanced panel dataset

######### Garden event study balanced panel #################
library(did2s)
out_bal = event_study(
  data = newdata_bal, yname = "avgviewers", idname = "user",
  tname = "week", gname = "before_after", xformla = ~total_avgviewers + total_avgfollowers,
  estimator = "all"
)
plot_event_study(out_bal, seperate = TRUE, horizon = NULL)
out_bal_mutated <- out_bal %>%
  group_by(estimator) %>%
  summarise(avgstderror = mean(std.error), avgestimate = mean(estimate))

library(stargazer)
stargazer(out_bal_mutated, summary = FALSE)
################# Gardner event study unbalanced panel #######################
library(did2s)
out_unbal = event_study(
  data = newdata_unbal, yname = "avgviewers", idname = "user",
  tname = "week", gname = "before_after", xformla = ~total_avgviewers + total_avgfollowers,
  estimator = "all"
)
plot_event_study(out_unbal, seperate = TRUE, horizon = NULL)
out_unbal_mutated <- out_unbal %>%
  group_by(estimator) %>%
  summarise(avgstderror = mean(std.error))
stargazer(out_bal_mutated, summary = FALSE)
