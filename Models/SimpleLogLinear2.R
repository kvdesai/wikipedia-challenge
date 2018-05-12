#source('D:/ICDM11/mygit/Kalpit/not_so_simple9a.r')
rm(list = ls(all.names=T))
#setwd('D:/ICDM11/wikichallenge_data_all/Honourable Mention')
source('Models/helper_functions.r')

data_train = read.csv("AllFeatureSets/Featureskd_XinP1P2_YinP3.csv", header = TRUE);
data_lead = read.csv("AllFeatureSets/Featureskd_XinP1P2P3.csv", header = TRUE);

y.train = data_train$y_next5_edits;

month_size = max(data_lead$LSDT)/116;
data_train$seg = rep(1, nrow(data_train));
data_train$seg[data_train$FSDT < (max(data_train$FSDT) - 5*month_size)]=2;
data_train$seg[data_train$FSDT < (max(data_train$FSDT) - 12*month_size)]=3;

data_lead$seg = rep(1, nrow(data_lead));
data_lead$seg[data_lead$FSDT < (max(data_lead$FSDT) - 5*month_size)]=2;
data_lead$seg[data_lead$FSDT < (max(data_lead$FSDT) - 12*month_size)]=3;

data_train$int1 = sqrt(data_train$edits_p*data_train$edits_pp);
data_lead$int1 = sqrt(data_lead$edits_p)*sqrt(data_lead$edits_pp);

data_train$int2 =  data_train$sum_edit_days_l5*data_train$Edits_mth_cutoff_minus_0;
data_lead$int2 = data_lead$sum_edit_days_l5*data_lead$Edits_mth_cutoff_minus_0;

#######################################################################
##### Fit a multiplicative model using log 
#########################################################################
####################### Gnerate on the leaderboard
y = log(y.train+1);
x = log(abs(data_train)+1);
x = x[, -c(4,46,47)];  ### remove next 5 month edits
xl = log(abs(data_lead)+1);
xl = xl[, -c(4, 46)]

x = as.matrix(x);
mod <- lm(y~x);

####################### Gnerate on the leaderboard

pred.lead = as.matrix(cbind(1, xl))%*%mod$coef;
pred.lead = exp(pred.lead)-1;
pred.lead[pred.lead <0 ] =0;
mat = cbind(editor_id = data_lead[,1], solution = pred.lead);
write.csv(mat, file = "SimpleLogLinear.csv", row.names = FALSE);
