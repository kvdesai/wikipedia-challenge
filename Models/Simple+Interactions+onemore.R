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

### interaction 1
x.train$int1 = sqrt(x.train$edit_p*x.train$edit_pp);
x.lead$int1 = sqrt(x.lead$edit_p)*sqrt(x.lead$edit_pp);

### interaction 2
x.train$int2 =  x.train$UniqDays5months*x.train$Edits_mth_cutoff_minus_0;
x.lead$int2 = x.lead$UniqDays5months*x.lead$Edits_mth_cutoff_minus_0;

model1 =  c("edit_p", "edit_pp", "int2", "UniqDays5months", "Edits_mth_cutoff_minus_0", "edit_days_month_last");
model2 =  c("edit_p", "edit_pp", "int1", "int2", "UniqDays5months", "Edits_mth_cutoff_minus_0", "edit_days_month_last");
model3 =  c("edit_p", "edit_pp", "int1", "int2", "UniqDays5months", "Edits_mth_cutoff_minus_0", "edit_days_month_last");

#### Initialize beta
beta1 = rep(0.1, 7);
beta2 = rep(0.1, 8);
beta3 = rep(0.1, 8);

f8 = FitValidatePredictAll(y.train, x.train, x.lead, model1, model2, model3, beta1, beta2, beta3);
l8 = f8$pred.lead;

mat = cbind(editor_id = data_lead[,1], solution = l8);
write.csv(mat, file = "Simple+Interactions+onemore.csv", row.names = FALSE);



