#source('D:/ICDM11/mygit/Kalpit/play_with_rf.r')
rm(list = ls())

#setwd('D:/ICDM11/wikichallenge_data_all/Honourable Mention')
#source('Models/helper_functions.r')


require("randomForest")

rmse<- function(y,yhat){
	return(sqrt(mean((y-yhat)^2)))
}

if (!("TD" %in% ls())){
	TD = read.csv("AllFeatureSets/Features113_XinP1P2_YinP3.csv", header = T)
	LD = read.csv("AllFeatureSets/Features113_XinP1P2P3.csv", header = T)
}
Ntree.new = 50
Ntree.old = 100
# -------------------------------
# labsubs = c("intercept","fsdt_inv","sum_edit_l5","ln_edit_pp","Edits_mth_cutoff_minus_0","sum_edit_days_l5")
labsubs = c("fsdt_inv","lsdt_inv","ln_edit_l5","ln_edit_pp","Edits_mth_cutoff_minus_0","sum_edit_days_l5","edits_week_last","edit_days_week_last","edit_days_month_last","edit_days_month_minus1","edit_days_month_minus2","edits_per_day","edits_per_article","edits_per_day_l5", "edits_per_article_l5","reverts_per_edit","ln_edit_ppp","sum_new_articles_l5","reverts_per_edit_l5","ln_edit_pppp","ln_edit_ppppp","edit_days_pppp","edit_days_ppppp","ln_edit_1314","ln_edit_1516","ln_edit_1718","ln_edit_1920","ln_edit_2122","ln_edit_2324","edit_days_13","edit_days_14","edit_days_15")


xlabs.new = setdiff(labsubs,c("ln_edit_pp","edit_days_month_minus1","edit_days_month_minus2","ln_edit_ppp","reverts_per_edit","ln_edit_pppp","ln_edit_ppppp","edit_days_pppp","edit_days_ppppp","ln_edit_1314","ln_edit_1516","ln_edit_1718","ln_edit_1920","ln_edit_2122","ln_edit_2324","edit_days_13","edit_days_14","edit_days_15"))
xlabs.old = setdiff(labsubs,c("lsdt_inv","edits_per_day_l5", "edits_per_article_l5","edit_days_month_minus2","edit_days_ppppp","ln_edit_1516","ln_edit_1718","ln_edit_1920","ln_edit_2122","ln_edit_2324","edit_days_13","edit_days_14","edit_days_15"))
#xlabs.old = setdiff(labsubs,c("lsdt_inv","lsdt_inv","ln_edit_l5","ln_edit_pp","Edits_mth_cutoff_minus_0","sum_edit_days_l5","edits_week_last","edit_days_week_last","edit_days_month_last","edit_days_month_minus1","edit_days_month_minus2","edits_per_day_l5", "edits_per_article_l5","sum_new_articles_l5","reverts_per_edit_l5"))

Tsub = TD[,labsubs];

Tsub$actual.edits = TD$y_next5_edits;
Tsub$user_id = TD$user_id;


Lsub = LD[,labsubs];

Lsub$user_id = LD$user_id
#rm(list=c('TD','LD'))


Reg = read.csv("AllFeatureSets/editor_regdate.csv", header = T)

t0 = strptime("2001-01-01 0:0:0", "%Y-%m-%d %H:%M:%S"); 
tsb = as.numeric(strptime("2009-09-01 0:0:0", "%Y-%m-%d %H:%M:%S") - t0);
  
user.new = Reg$user_id[which(Reg$reg_date >= tsb)]

inew.t = which(Tsub$user_id %in% user.new)
iold.t = setdiff(1:dim(Tsub)[1],inew.t)

inew.l = which(Lsub$user_id %in% user.new)
iold.l = setdiff(1:dim(Lsub)[1],inew.l)


# -------------------------------


Xt.new = Tsub[inew.t,xlabs.new]
Xt.old = Tsub[iold.t,xlabs.old]

Yt.new = log(Tsub$actual.edits[inew.t]+1)
Yt.old = log(Tsub$actual.edits[iold.t]+1)

XL.new = Lsub[inew.l,xlabs.new]
XL.old = Lsub[iold.l,xlabs.old]

ptm <- proc.time()

rf.new = randomForest(Xt.new, Yt.new, XL.new, keep.forest=T,importance=T,localImp=T,do.trace=T,keep.inbag=F,ntree=Ntree.new, nPerm=ceiling(dim(Xt.new)[2]/6));
err.new = rmse(Yt.new, rf.new$predicted)

rf.old = randomForest(Xt.old, Yt.old, XL.old, keep.forest=T,importance=T,localImp=T,do.trace=T,keep.inbag=F,ntree=Ntree.old,nPerm=ceiling(dim(Xt.old)[2]/6));
err.old = rmse(Yt.old, rf.old$predicted)

print(proc.time() - ptm)

k.t = length(Yt.new)/(length(Yt.new)+length(Yt.old))
oob.rmsle = sqrt(k.t*(err.new^2) + (1-k.t)*(err.old^2))

k.L = length(inew.l)/(length(inew.l)+length(iold.l));
xlead.rmsle = sqrt(k.L*(err.new^2) + (1-k.L)*(err.old^2))

cat(sprintf('SegNew oob.rmsle = %.6f, SegOld oob.rmsle = %.6f \n',err.new, err.old))
cat(sprintf('Total OOB rmsle = %.6f, ExpLeadRMSLE = %.6f \n', oob.rmsle, xlead.rmsle)) 

Submit = TRUE
REPLICATE = TRUE
if (Submit==T){
	y_solution = rep(0,dim(LD)[1])
	if (REPLICATE==T){ #Used the exact RF model that created the submission "RF_29Aug11_2nd.csv"
		load('Models/RFmodels_29Aug11_2nd.rdata') #loads rf.newed and rf.olded variables
		y.rec = y_solution	
		y.rec[inew.l] = predict(rf.newed,XL.new); 
		y.rec[iold.l] = predict(rf.olded,XL.old);
		y_solution = exp(y.rec) - 1
		
	}else{ #Dont try to replicate submitted results. Use the RF model thatwe just trained
		y_solution[iold.l] = exp(rf.old$test$predicted)-1;
		y_solution[inew.l] = exp(rf.new$test$predicted)-1;
	}
	
	y_solution[y_solution < 0] = 0
	mat = cbind(user_id = LD$user_id, solution = y_solution);
	write.csv(mat, file = "RF_29Aug11_2nd.csv", row.names = FALSE);
	#save(rf.new,rf.old,xlabs.new,xlabs.old, file = "RandomForest_29Aug11_2nd.Rdata");
}



