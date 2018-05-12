#############################################################################################################################
#### Read all data
#############################################################################################################################
rm(list = setdiff(ls(),c('DATADIR','RESDIR','MAIN_DIR')))
#setwd('D:/ICDM11/wikichallenge_data_all/Honourable Mention/AllFeatureSets')
data <- read.csv(paste(RESDIR,"editor_monthly_edits.csv",sep=''), header = TRUE);

#regis_dates = read.csv("Editor_FSDTnum.csv", header = TRUE);
regis_dates <- read.csv(paste(RESDIR,'eid_fsdt_regdate_table.csv',sep=''),header=T)
regis_dates = regis_dates[, 1:2];
data_unique_days = read.csv(paste(RESDIR,"editor_monthly_edits_unique_days.csv",sep=''), header = TRUE);

data_unique_days$last.5.months = rowSums(data_unique_days[,113:117])
data_unique_days$previous.5.months = rowSums(data_unique_days[,108:112])

#### Read features
data_feature_train = read.csv(paste(RESDIR,"Features113_XinP1P2_YinP3.csv",sep=''), header = TRUE);
data_feature_leaderboard = read.csv(paste(RESDIR,"Features113_XinP1P2P3.csv",sep=''), header = TRUE);

#############################################################################################################################
##### Segment the training and leaderboard data
#############################################################################################################################
t0 = strptime("2001-01-01 0:0:0", "%Y-%m-%d %H:%M:%S");    ## beginning of time
t1 = strptime("2010-09-01 0:0:0", "%Y-%m-%d %H:%M:%S");    ## end of time
### t1- t0 = 3530
month_size = as.numeric((t1-t0)/116);
months_in = ceiling((3530 - regis_dates[,2])/month_size);
#### get the index of people joined more than 5 months ago
index = which(months_in <=5) ### 9353
data_old = data[-index,];
data_unique_old = data_unique_days[-index,];
months_old = months_in[-index];

#### segment 1 : people who joined in the previous 5 months
#### segment 2: people who joined in 6-12 ago
#### segment 3: people who joined more than one year ago
seg1 = which(months_old >5 & months_old <=10);
seg2 = which(months_old >10 & months_old <=17);
seg3 = which(months_old >17 & months_old <=116);

#### Segmentation for the leaderboard
seg1.pred = which(months_in <=5);
seg2.pred = which(months_in >5 & months_in <=12);
seg3.pred = which(months_in >12);

#######################################################################################################################
#### Compute the combined feature matrices for training and leaderboard
########################################################################################################################
#### for training
edit = rowSums(data_old[, 113:117]);  ### last 5 months
edit_p = rowSums(data_old[, 108:112]); ### previous 5 months
edit_pp = rowSums(data_old[, 104:108]); ### previous 6-10 months
edit_ppp = rowSums(data_old[, 100:104]); ### previous 11-15 months
seg = rep(0, nrow(data_old));
seg[seg1] =1;
seg[seg2] =2;
seg[seg3] =3;
d1 <- cbind(editor_id =data_old[,1], seg, edit, edit_p, edit_pp, edit_ppp, UniqDays5months = data_unique_old$previous.5.months);
training_features = merge(d1, data_feature_train, 1);
write.csv(training_features, file = paste(RESDIR,"training_features.csv",sep=''), row.names = FALSE);

#### for leaderboard
edit_p = rowSums(data[, 113:117]);  ### previous 5 months
edit_pp = rowSums(data[, 108:112]); ### previous 6-10 months
edit_ppp = rowSums(data[, 104:108]); ### previous 11-15 months
seg = rep(0, nrow(data));
seg[seg1.pred] =1;
seg[seg2.pred] =2;
seg[seg3.pred] =3;

d1 <- cbind(editor_id =data[,1], seg, edit_p, edit_pp, edit_ppp, UniqDays5months = data_unique_days$last.5.months);
leaderboard_features = merge(d1, data_feature_leaderboard, 1);
write.csv(leaderboard_features, file = paste(RESDIR,"leaderboard_features.csv", sep=''),row.names = FALSE);
