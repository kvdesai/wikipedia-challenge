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

#mat = cbind(editor_id = data_lead[,1], solution = l19);
#write.csv(mat, file = "Simple+Interactions+more.csv", row.names = FALSE);

#### Generate training predictions
fit = rep(0, length(y.train));
fit[x.train$seg ==1] = as.matrix(cbind(1, x.train[x.train$seg ==1, model1]))%*%f19$seg1.beta;
fit[x.train$seg ==2] = as.matrix(cbind(1, x.train[x.train$seg ==2, model2]))%*%f19$seg2.beta;
fit[x.train$seg ==3] = as.matrix(cbind(1, x.train[x.train$seg ==3, model3]))%*%f19$seg3.beta;
fit[fit <0] =0;

#### Modify edits of high error editors
err = log(y.train+1) - log(fit +1);
sum(err >3)
high_err_editors = data_train$editor_id[err >3];
high_err_edits  = data_train$edit[err >3];
l20 = l19;
for(j in 1:length(high_err_editors)){
l20[data_lead$editor_id == high_err_editors[j]] = high_err_edits[j];
}

mat = cbind(editor_id = data_lead[,1], solution = l20);
write.csv(mat, file = "Simple+Interactions+more-modified.csv", row.names = FALSE);








