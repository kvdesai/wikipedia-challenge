#source('D:/ICDM11/mygit/Kalpit/Seven_Segs_Nested_v10.r')
rm(list = ls())
#setwd('D:/ICDM11/wikichallenge_data_all/Honourable Mention')
source('Models/helper_functions.r')

if (!("TD" %in% ls())){
  TD = read.csv("AllFeatureSets/Featureskd_wrevs_XinP1P2_YinP3.csv", header = T)
  TD$actual.edits = TD$y_next5_edits
  TD$sess1m_by_sess5m = log(TD$sessions_month_minus0+1) + log(TD$sessions_p+1) 
  LD = read.csv("AllFeatureSets/Featureskd_wrevs_XinP1P2P3.csv", header = T)
  # LD[is.na(LD)] = 0;
}

geomean <- function(vec,w=NA){
  if(is.na(w)){
    w = rep(1.0/length(vec),length(vec))
  }
  vec[vec < 0] = 0;
  wgm = sum(log(vec+1.0)*w)
  gm = exp(wgm) - 1.0
  return(gm)
}

newpred = rep(0,dim(TD)[1])

PrepareXes <- function(inds,segvars,kseg,TD){
	
  	X = as.matrix(TD[inds,segvars])
  	for (k in 1:dim(X)[2]){
		if (kseg[k] == 0){
			X[,k] = log(X[,k] + 1)
		}else{
			X[,k] = X[,k]*kseg[k]
		}	
  	}
	return(X)
}
FitSegments <- function(allinds,segvars,kseg,TD){
  Nf = 25
  res = list()
  for (i in 1:length(allinds)){    
    inds <- allinds[[i]]
    #segid[inds]=i
  	cat(sprintf('------------ Seg %i [%i]--------------------\n',i,length(inds)))
  	X = PrepareXes(inds,segvars[[i]],kseg[[i]],TD)
  	init.param = rep(0.01,dim(X)[2])
  	Y = TD$y_next5_edits[inds]
  	fitdata <- list(X = X, Y= Y)
  	res[[i]] <- nfold_jknife(fitdata,Nf,.33,init.param,onresids=F)
  	
  	# yhat_new = apply(X%*%t(res[[i]]$params),1,median)  	
  	# yhat_new[yhat_new < 0] = 0
  	# newpred[inds] = yhat_new		
  	
  	rm(list=c('X','Y','inds'))
  	cat(sprintf('=== Seg %i results: median training rmsle = %.6f, median test rmsle = %.6f ====\n',i,
  		median(res[[i]]$train.rmsle), median(res[[i]]$test.rmsle)))
  }
  return(res)
}


t0 = strptime("2001-01-01 0:0:0", "%Y-%m-%d %H:%M:%S"); 
te.t = as.numeric(strptime("2009-04-01 0:0:0", "%Y-%m-%d %H:%M:%S") - t0);
te.l = as.numeric(strptime("2009-09-01 0:0:0", "%Y-%m-%d %H:%M:%S") - t0);
  
#user.new = Reg$user_id[which(Reg$reg_date >= tsb)]
user.new.t = TD$user_id[(TD$FSDT >= te.l)]
user.new.l = LD$user_id[(LD$FSDT >= te.l)]
  
inew.t = which(TD$user_id %in% user.new.t)
iold.t = setdiff(1:dim(TD)[1],inew.t)

inew.l = which(LD$user_id %in% user.new.l)
iold.l = setdiff(1:dim(LD)[1],inew.l)

inds.new = list(); inds.old = list()
segvars.new = list(); segvars.old = list();
kseg.new = list(); kseg.old = list();
res.new = list(); res.old = list()


inds.new[[1]] = inew.t[which(TD$sessions_p[inew.t] == 0)]
inds.new[[2]] = inew.t[which(TD$sessions_p[inew.t] > 0 & TD$sessions_p[inew.t] <= 2)]
inds.new[[3]] = inew.t[which(TD$sessions_p[inew.t] > 2)]

# 9a: AggRes:: median(train rmsle) 0.398483, median(test rmsle) 0.409562
segvars.new[[1]] = c("intercept","fsdt_inv","lsdt_inv")
kseg.new[[1]] = rep(1,length(segvars.new[[1]])); kseg.new[[1]][-1] = 0.025; 

# 9a: AggRes:: median(train rmsle) 0.653826, median(test rmsle) 0.643272
segvars.new[[2]] = c('intercept',"fsdt_inv","lsdt_inv",'Edits_mth_cutoff_minus_0','sessions_month_minus0','edits_per_in_day','edits_per_session_last_month')
kseg.new[[2]] = rep(1,length(segvars.new[[2]])); kseg.new[[2]][c(2,3)] = 0.025; 

# 9a: AggRes:: median(train rmsle) 1.362598, median(test rmsle) 1.384223
# c('intercept','fsdt_inv','lsdt_inv','ln_edit_pp','sum_edit_days_l5','edits_week_last','edit_days_month_last')
segvars.new[[3]] = c("intercept","fsdt_inv","lsdt_inv","sessions_p","Edits_mth_cutoff_minus_0","sessions_month_minus0",'edits_per_session_last_month',
						'reverts_l1m_made','reverts_l1m_auto')
kseg.new[[3]] = rep(1,length(segvars.new[[3]])); kseg.new[[3]][c(2,3)] = c(0.03,0.06)
# res3 = FitSegments(inds.new[3],segvars.new[3],kseg.new[3],TD)
# res3[[1]]$params

res.new = FitSegments(inds.new,segvars.new,kseg.new,TD)

inds.old[[1]] = iold.t[which(TD$sessions_p[iold.t] == 0)]
inds.old[[2]] = iold.t[which(TD$sessions_p[iold.t] > 0 & TD$sessions_p[iold.t] <= 2)]
inds.old[[3]] = iold.t[which(TD$sessions_p[iold.t] > 2 & TD$sessions_p[iold.t] <= 10)]
inds.old[[4]] = iold.t[which(TD$sessions_p[iold.t] > 10)]

#Not_so_Simple9a: AggRes:: median(train rmsle) 1.041068, median(test rmsle) 1.035682
segvars.old[[1]] = c("intercept","edits_pp")
kseg.old[[1]] = c(1,0)

#Not_so_Simple9a: AggRes:: median(train rmsle) 0.974334, median(test rmsle) 0.970973
segvars.old[[2]] = c("intercept","sessions_p","sessions_pp","sessions_month_minus0","reverts_got_per_edit_l1","reverts_got_per_edit_l5");
kseg.old[[2]] = rep(1,length(segvars.old[[2]]));# kseg.old[[2]][2] = 0.03
#res = FitSegments(inds.old[2],segvars.old[2],kseg.old[2],TD)
#res[[1]]$params

#Not_so_Simple9a: AggRes:: median(train rmsle) 1.239482, median(test rmsle) 1.249774
segvars.old[[3]] = c("intercept","sessions_month_minus0",'reverts_l5m_made','sessions_p','sessions_pp')
kseg.old[[3]] = rep(1,length(segvars.old[[3]])); #kseg.old[[3]][c(2)] = c(10) 
# res3 = FitSegments(inds.old[3],segvars.old[3],kseg.old[3],TD)
# res3[[1]]$params


#Not_so_Simple9a: AggRes:: median(train rmsle) 1.189620, median(test rmsle) 1.191510
segvars.old[[4]] = c("intercept",'edits_p','edits_pp',"sessions_month_minus0",'Edits_mth_cutoff_minus_0')
kseg.old[[4]] = rep(1,length(segvars.old[[4]])); kseg.old[[4]][c(4,5)] = c(10,5)
# res = FitSegments(inds.old[4],segvars.old[4],kseg.old[4],TD)
# res[[1]]$params

res.old = FitSegments(inds.old,segvars.old,kseg.old,TD)

# print(summary(newpred))
# print(rmsle(newpred,TD$actual.edits))

#trainmat = cbind(editor_id = TD$user_id, test.pred = newpred, actual.edits = TD$actual.edits);
#write.csv(trainmat, file = "withoptim_not_so_Simple9a_nested_segs_training.csv", row.names = FALSE);

predict = TRUE

if (predict){
	
	inds.new[[1]] = inew.l[which(LD$sessions_p[inew.l] == 0)]
	inds.new[[2]] = inew.l[which(LD$sessions_p[inew.l] > 0 & LD$sessions_p[inew.l] <= 2)]
	inds.new[[3]] = inew.l[which(LD$sessions_p[inew.l] > 2)]

	inds.old[[1]] = iold.l[which(LD$sessions_p[iold.l] == 0)]
	inds.old[[2]] = iold.l[which(LD$sessions_p[iold.l] > 0 & LD$sessions_p[iold.l] <= 2)]
	inds.old[[3]] = iold.l[which(LD$sessions_p[iold.l] > 2 & LD$sessions_p[iold.l] <= 10)]
	inds.old[[4]] = iold.l[which(LD$sessions_p[iold.l] > 10)]

	solution = rep(0,nrow(LD))
	segerr = rep(0,nrow(LD))
	for (i in 1:length(inds.new)){
		inds = inds.new[[i]]
		Xlead = PrepareXes(inds,segvars.new[[i]],kseg.new[[i]],LD)		
		
		pred.lead = apply(Xlead%*%t(res.new[[i]]$params),1,geomean)
		pred.lead[pred.lead<0] = 0
		solution[inds] = pred.lead
		segerr[inds] = geomean(res.new[[i]]$test.rmsle)
	}	
	
	for (i in 1:length(inds.old)){
		inds = inds.old[[i]]
		Xlead = PrepareXes(inds,segvars.old[[i]],kseg.old[[i]],LD)
		
		pred.lead = apply(Xlead%*%t(res.old[[i]]$params),1,geomean)
		pred.lead[pred.lead<0] = 0
		solution[inds] = pred.lead
		segerr[inds] = geomean(res.old[[i]]$test.rmsle)
	}

	cat(sprintf('-------- Summary of leaderboard submission ---------------\n'))
	print(summary(solution))
	
	###################################################################
	submat = cbind(editor_id = LD$user_id, solution = solution);
	write.csv(submat, file = "Seven_Segs_v10_corrected_bagging.csv", row.names = FALSE);
	
	# ensemblemat = cbind(editor_id = LD$user_id, prediction = solution, seg.rmsle = segerr);
	# write.csv(ensemblemat, file = "ForEnsemble_Seven_Segs_v10_corrected_bagging.csv", row.names = FALSE);
	
}
