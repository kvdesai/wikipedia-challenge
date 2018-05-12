
rm(list = ls(all.names=T))
#setwd('D:/ICDM11/wikichallenge_data_all/Honourable Mention')
source('Models/helper_functions.r')


data <- read.csv("AllFeatureSets/editor_monthly_edits.csv", header = TRUE);
#regis_dates = read.csv("editor_regdate.csv", header = TRUE);
regis_dates = read.csv("AllFeatureSets/eid_fsdt_regdate_table.csv", header = TRUE);

regis_dates = regis_dates[, 1:2];
data_unique_days = read.csv("AllFeatureSets/editor_monthly_edits_unique_days.csv", header = TRUE);
data_unique_days$last.5.months = rowSums(data_unique_days[,113:117])
data_unique_days$previous.5.months = rowSums(data_unique_days[,108:112])

rmsle_fun <- function(beta,y,x){

yhat = cbind(1, x)%*%beta;

r = sqrt(mean((log(y+1) - log(yhat+1))^2));

return(r);
}

############################################################################
#### Compute months on august 31st, 2010
############################################################################

t0 = strptime("2001-01-01 0:0:0", "%Y-%m-%d %H:%M:%S");    ## beginning of time
t1 = strptime("2010-09-01 0:0:0", "%Y-%m-%d %H:%M:%S");    ## end of time

### t1- t0 = 3530
month_size = as.numeric((t1-t0)/116);
months_in = ceiling((3530 - regis_dates[,2])/month_size);
### plot(table(months_in))

####################################################################################
#### Segment the Editors into months since joining and fit a model for each segment
####################################################################################
#### for buliding the model remove new people who joined in last 5 months
index = which(months_in <=5) ### 9353

data_old = data[-index,];
data_unique_old = data_unique_days[-index,];

months_old = months_in[-index];
#### No. of edits in the last 5 months for existing people
edit = rowSums(data_old[, 113:117]);

#### segment 1 : people who joined in the previous 5 months
#### segment 2: people who joined in 6-12 ago
#### segment 3: people who joined more than one year ago
seg1 = which(months_old >5 & months_old <=10);
seg2 = which(months_old >10 & months_old <=17);
seg3 = which(months_old >17 & months_old <=116);

edit11 = rowSums(data_old[seg1, 108:112]);
edit12 = rowSums(data_old[seg1, 104:108]);

edit21 = rowSums(data_old[seg2, 108:112]);
edit22 = rowSums(data_old[seg2, 104:108]);

edit31 = rowSums(data_old[seg3, 108:112]);
edit32 = rowSums(data_old[seg3, 104:108]);

edit1 = rowSums(data_old[seg1, 108:112]);
edit2 = rowSums(data_old[seg2, 108:112]);
edit3 = rowSums(data_old[seg3, 89:112])*5/24;

uniq1 = data_unique_old$previous.5.months[seg1];
uniq2 = data_unique_old$previous.5.months[seg2];
uniq3 = data_unique_old$previous.5.months[seg3];

#### Now fit for each group separately intercept and slope
n1 =  nlm(rmsle_fun, c(0,0.5, 0.5,0.1), edit[seg1],cbind(edit11, edit12, uniq1)); ###0.87
n2 =  nlm(rmsle_fun, c(0,0.5, 0.5,0.1), edit[seg2],cbind(edit21, edit22, uniq2)); ###0.963
n3 =  nlm(rmsle_fun, c(0,0.5, 0.5,0.1), edit[seg3],cbind(edit31, edit32, uniq3)); ###1.173
###########################################################################################
#### Generate prediction to be submitted to the leaderboard
###########################################################################################
seg1.pred = which(months_in <=5);
seg2.pred = which(months_in >5 & months_in <=12);
seg3.pred = which(months_in >12);

edit11.pred = rowSums(data[seg1.pred, 113:117]);
edit12.pred = rowSums(data[seg1.pred, 108:112]);

edit21.pred = rowSums(data[seg2.pred, 113:117]);
edit22.pred = rowSums(data[seg2.pred, 108:112]);

edit31.pred = rowSums(data[seg3.pred, 113:117]);
edit32.pred = rowSums(data[seg3.pred, 108:112]);

uniq1.pred = data_unique_days$last.5.months[seg1.pred];
uniq2.pred = data_unique_days$last.5.months[seg2.pred];
uniq3.pred = data_unique_days$last.5.months[seg3.pred];


n = nrow(data);
pred.leaderboard = rep(0,n);
pred.leaderboard[seg1.pred] = n1$estimate[1]+ n1$estimate[2]*edit11.pred + n1$estimate[3]*edit12.pred+ n1$estimate[4]*uniq1.pred;
pred.leaderboard[seg2.pred] = n2$estimate[1]+ n2$estimate[2]*edit21.pred + n2$estimate[3]*edit22.pred+ n2$estimate[4]*uniq2.pred;
pred.leaderboard[seg3.pred] = n3$estimate[1]+ n3$estimate[2]*edit31.pred + n3$estimate[3]*edit32.pred+ n3$estimate[4]*uniq3.pred;


pred.leaderboard[pred.leaderboard <0] =0;

### store the prediction in a file
mat.leaderboard = cbind(data[,1], pred.leaderboard);
colnames(mat.leaderboard) = c("user_id", "solution");
write.csv(mat.leaderboard, file = "3segemnts-linear-begin-date-uniq days-leaderboard.csv", row.names = FALSE);
