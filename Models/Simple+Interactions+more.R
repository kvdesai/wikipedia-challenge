#source('D:/ICDM11/mygit/Kalpit/not_so_simple9a.r')
rm(list = ls(all.names=T))
#setwd('D:/ICDM11/wikichallenge_data_all/Honourable Mention')
source('Models/helper_functions.r')

data_train = read.csv("AllFeatureSets/training_features.csv", header = TRUE);
data_lead = read.csv("AllFeatureSets/leaderboard_features.csv", header = TRUE);

###################################################################################################
#### Fit our latest best model - 12/08/2011
###################################################################################################
y.train = data_train$edit;
x.train = data_train[, 1:119];
x.lead = data_lead;

#### More features
x.train$rate = data_train$sum_edits/(data_train$LSDT- data_train$FSDT+1);
x.lead$rate = data_lead$sum_edits/(data_lead$LSDT- data_lead$FSDT+1);

### interaction 1
x.train$int1 = sqrt(x.train$edit_p*x.train$edit_pp);
x.lead$int1 = sqrt(x.lead$edit_p)*sqrt(x.lead$edit_pp);

### interaction 2
x.train$int2 =  x.train$UniqDays5months*x.train$Edits_mth_cutoff_minus_0;
x.lead$int2 = x.lead$UniqDays5months*x.lead$Edits_mth_cutoff_minus_0;

model1 =  c("edit_p", "edit_pp",  "int2", "UniqDays5months", "Edits_mth_cutoff_minus_0", "edit_days_month_last", "rate", "sum_articles" );
model2 =  c("edit_p", "edit_pp",  "int1", "int2", "UniqDays5months", "Edits_mth_cutoff_minus_0", "edit_days_month_last", "rate", "sum_articles" );
model3 =  c("edit_p", "edit_pp", "int1", "int2", "UniqDays5months", "Edits_mth_cutoff_minus_0", "edit_days_month_last", "rate", "sum_articles" );

#### Initialize beta
beta1 = rep(0, 9);
beta2 = rep(0, 10);
beta3 = rep(0, 10);

f19 = FitValidatePredictAll(y.train, x.train, x.lead, model1, model2, model3, beta1, beta2, beta3);
l19 = f19$pred.lead;

mat = cbind(editor_id = data_lead[,1], solution = l19);
write.csv(mat, file = "Simple+Interactions+more.csv", row.names = FALSE);



