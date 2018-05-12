
rm(list = ls(all.names=T))

#setwd('D:/ICDM11/wikichallenge_data_all/Honourable Mention')
source('Models/helper_functions.r')

TD = read.csv("AllFeatureSets/Features113_XinP1P2_YinP3.csv", header = T)
LD = read.csv("AllFeatureSets/Features113_XinP1P2P3.csv", header = T)
# LD[is.na(LD)] = 0;

Nf = 25

# labsubs = c("intercept","fsdt_inv","sum_edit_l5","ln_edit_pp","Edits_mth_cutoff_minus_0","sum_edit_days_l5")
labsubs = c("intercept","fsdt_inv","lsdt_inv","ln_edit_l5","ln_edit_pp","Edits_mth_cutoff_minus_0","sum_edit_days_l5","edits_week_last","edit_days_week_last","edit_days_month_last")

Tsub = TD[,labsubs];
#Tsub[,6:8] = log(Tsub[,6:8]+1)
Tsub$actual.edits = TD$y_next5_edits;
Tsub$user_id = TD$user_id;


Lsub = LD[,labsubs];
#Lsub[,6:8] = log(Lsub[,6:8]+1)
Lsub$user_id = LD$user_id
rm(list=c('TD','LD'))


Reg = read.csv("AllFeatureSets/editor_regdate.csv", header = T)

t0 = strptime("2001-01-01 0:0:0", "%Y-%m-%d %H:%M:%S"); 
tsb = as.numeric(strptime("2009-09-01 0:0:0", "%Y-%m-%d %H:%M:%S") - t0);
  
user.new = Reg$user_id[which(Reg$reg_date >= tsb)]

inew.t = which(Tsub$user_id %in% user.new)
iold.t = setdiff(1:dim(Tsub)[1],inew.t)

inew.l = which(Lsub$user_id %in% user.new)
iold.l = setdiff(1:dim(Lsub)[1],inew.l)


# inew.t = which(Tsub$fsdt_inv < 365)
# iold.t = which(Tsub$fsdt_inv >= 365)

# inew.l = which(Lsub$fsdt_inv < 365)
# iold.l = which(Lsub$fsdt_inv >= 365)

Tsub$lsdt_inv = Tsub$lsdt_inv/400;
Lsub$lsdt_inv = Lsub$lsdt_inv/400;

Tsub$fsdt_inv = Tsub$fsdt_inv/400;
Lsub$fsdt_inv = Lsub$fsdt_inv/400;

iseg = list()
Nold = 4
segvars = list()
kseg = list()

xlabs.old = setdiff(labsubs,c("lsdt_inv"))
xlabs.new = setdiff(labsubs,c("ln_edit_l5","ln_edit_pp"))


iseg[[1]] = iold.t[which(Tsub$sum_edit_days_l5[iold.t] == 0)]
segvars[[1]] = c("intercept","ln_edit_pp")
kseg[[1]] = c(1,1)

iseg[[2]] = iold.t[which(Tsub$sum_edit_days_l5[iold.t] > 0 & Tsub$sum_edit_days_l5[iold.t] <= 2)]
segvars[[2]] = xlabs.old[c(1,2,4,6,7,8,9)]
kseg[[2]] = c(1,1,1,1,1,1,1)

iseg[[3]] = iold.t[which(Tsub$sum_edit_days_l5[iold.t] > 2 & Tsub$sum_edit_days_l5[iold.t] <= 10)]
segvars[[3]] = xlabs.old
kseg[[3]] = c(1,1,1,1,0.1,1,1,1,1)

iseg[[4]] = iold.t[which(Tsub$sum_edit_days_l5[iold.t] > 10)]
segvars[[4]] = xlabs.old
kseg[[4]] = c(1,1,5,5,10,10,5,5,1)

iseg[[Nold+1]] = inew.t[which(Tsub$sum_edit_days_l5[inew.t] == 0)]
#segvars[[Nold+1]] = setdiff(xlabs.new,c("Edits_mth_cutoff_minus_0","sum_edit_days_l5"))
segvars[[Nold+1]] = c("intercept","fsdt_inv","lsdt_inv")
kseg[[Nold+1]] = c(1,10,10)

iseg[[Nold+2]] = inew.t[which(Tsub$sum_edit_days_l5[inew.t] > 0 & Tsub$sum_edit_days_l5[inew.t] <= 2)]
segvars[[Nold+2]] = c("intercept","fsdt_inv","lsdt_inv","Edits_mth_cutoff_minus_0","edits_week_last","edit_days_week_last")
kseg[[Nold+2]] = c(1,10,10,1,1,1)

iseg[[Nold+3]] = inew.t[which(Tsub$sum_edit_days_l5[inew.t] > 2)]
segvars[[Nold+3]] = labsubs[c(1,2,3,5,7,8,10)]
kseg[[Nold+3]] = c(1,10,10,1,1,5,1)

#  c("intercept","fsdt_inv","lsdt_inv","ln_edit_l5","ln_edit_pp","Edits_mth_cutoff_minus_0","sum_edit_days_l5","edits_week_last","edit_days_week_last","edit_days_month_last")
res = list()

newpred = rep(0,length(Tsub$actual.edits))
segid = rep(-1,length(Tsub$actual.edits))

for (i in 1:length(iseg)){
	
	inds <- iseg[[i]]
	segid[inds]=i
	cat(sprintf('------------ Seg %i [%i]--------------------\n',i,length(inds)))
	#rm('xlabs')
	#if (i <=Nold){ xlabs = xlabs.old; } else{xlabs = xlabs.new;}
	
	X = as.matrix(Tsub[inds,segvars[[i]]])
	for (k in 1:dim(X)[2]){
		X[,k] = X[,k]*kseg[[i]][k]
	}
	init.param = rep(0.01,dim(X)[2])
	Y = Tsub$actual.edits[inds]
	fitdata <- list(X = X, Y= Y)
	res[[i]] <- nfold_jknife(fitdata,Nf,.2,init.param,onresids=F)
	
	yhat_new = apply(X%*%t(res[[i]]$params),1,median)
	
	yhat_new[yhat_new < 0] = 0
	newpred[inds] = yhat_new		
	
	rm(list=c('X','Y','yhat_new','inds'))
	cat(sprintf('=== Seg %i results: median test rmsle = %.6f, median training rmsle = %.6f ====\n',i,
		median(res[[i]]$test.rmsle), median(res[[i]]$train.rmsle)))
}
print(rmsle(newpred,Tsub$actual.edits))
# trainmat = cbind(editor_id = Tsub$user_id, test.pred = newpred, actual.edits = Tsub$actual.edits, segid = segid);
# write.csv(trainmat, file = "withoptim_not_so_Simple9a_nested_segs_training.csv", row.names = FALSE);

predict = TRUE;

if (predict){

	iseg[[1]] = iold.l[which(Lsub$sum_edit_days_l5[iold.l] == 0)]
	segvars[[1]] = c("intercept","ln_edit_pp")
	kseg[[1]] = c(1,1)

	iseg[[2]] = iold.l[which(Lsub$sum_edit_days_l5[iold.l] > 0 & Lsub$sum_edit_days_l5[iold.l] <= 2)]
	segvars[[2]] = xlabs.old[c(1,2,4,6,7,8,9)]
	kseg[[2]] = c(1,1,1,1,1,1,1)

	iseg[[3]] = iold.l[which(Lsub$sum_edit_days_l5[iold.l] > 2 & Lsub$sum_edit_days_l5[iold.l] <= 10)]
	segvars[[3]] = xlabs.old
	kseg[[3]] = c(1,1,1,1,0.1,1,1,1,1)

	iseg[[4]] = iold.l[which(Lsub$sum_edit_days_l5[iold.l] > 10)]
	segvars[[4]] = xlabs.old
	kseg[[4]] = c(1,1,5,5,10,10,5,5,1)

	iseg[[Nold+1]] = inew.l[which(Lsub$sum_edit_days_l5[inew.l] == 0)]
	#segvars[[Nold+1]] = setdiff(xlabs.new,c("Edits_mth_cutoff_minus_0","sum_edit_days_l5"))
	segvars[[Nold+1]] = c("intercept","fsdt_inv","lsdt_inv")
	kseg[[Nold+1]] = c(1,10,10)

	iseg[[Nold+2]] = inew.l[which(Lsub$sum_edit_days_l5[inew.l] > 0 & Lsub$sum_edit_days_l5[inew.l] <= 2)]
	segvars[[Nold+2]] = c("intercept","fsdt_inv","lsdt_inv","Edits_mth_cutoff_minus_0","edits_week_last","edit_days_week_last")
	kseg[[Nold+2]] = c(1,10,10,1,1,1)

	iseg[[Nold+3]] = inew.l[which(Lsub$sum_edit_days_l5[inew.l] > 2)]
	segvars[[Nold+3]] = labsubs[c(1,2,3,5,7,8,10)]
	kseg[[Nold+3]] = c(1,10,10,1,1,5,1)

	solution = rep(0,nrow(Lsub))
	for (i in 1:length(iseg)){
		inds = iseg[[i]]
		Xlead = as.matrix(Lsub[inds,segvars[[i]]])
		for (k in 1:dim(Xlead)[2]){
			Xlead[,k] = Xlead[,k]*kseg[[i]][k]
		}
	
		pred.lead = apply(Xlead%*%t(res[[i]]$params),1,median)
		pred.lead[pred.lead<0] = 0
		solution[inds] = pred.lead
	}
	cat(sprintf('-------- Summary of leaderboard submission ---------------\n'))
	print(summary(solution))
	
	###################################################################
	submat = cbind(editor_id = Lsub$user_id, solution = solution);
	write.csv(submat, file = "withoptim_not_so_Simple9a_nested_segs_LB.csv", row.names = FALSE);
}
