rm(list = setdiff(ls(),c('DATADIR','RESDIR','MAIN_DIR')))

NE = 44514
t0 = strptime("2001-01-01 0:0:0", "%Y-%m-%d %H:%M:%S"); 
tcut.train = as.numeric(strptime("2010-04-01 0:0:0", "%Y-%m-%d %H:%M:%S") - t0);
tcut.lead =  as.numeric(strptime("2010-09-01 0:0:0", "%Y-%m-%d %H:%M:%S") - t0); 

TD = read.csv(paste(RESDIR,'OrigSet_xinp1p2_yinp3.csv',sep=''),header=T)
LD = read.csv(paste(RESDIR,'OrigSet_xinp1p2p3.csv',sep=''),header=T)
RD = read.csv(paste(RESDIR,'reverts_related_features_training_and_LB.csv',sep=''),header=T)

stopifnot(dim(LD)[1] == NE, dim(RD)[1] == NE)
stopifnot(RD$user_id == LD$user_id)

# Add following features
# "intercept"                  "edits_per_in_day"              
# "reverts_per_edit"            "edits_per_article"          
# "reverts_per_edit_l5"        "edits_p" "edits_pp" "edits_ppp" "edits_pppp" "edits_ppppp"
# "fsdt_inv"                   "lsdt_inv"              
# "mean_time_between_edits"    "mean_time_between_edits_l5" "edits_week_minus0"           
# "edits_week_minus1"          "edits_week_minus2"          "edits_week_minus3" 
# "sessions"        
# "sessions_week_minus0"        "sessions_week_minus1"      "sessions_week_minus2"     
# "sessions_week_minus3"      "sessions_month_minus0"       "sessions_month_minus1"    
# "sessions_month_minus2"     "sessions_month_minus3"     
# "sessions_p" "sessions_pp" "sessions_ppp"  "sessions_pppp"  "sessions_ppppp"   
# "edits_per_session"  "edits_per_session_l5" "edits_per_session_week_last" "edits_per_session_month_last"   
# "days_in_p","days_in_pp","days_in_ppp","days_in_pppp","days_in_ppppp"


# Add intercept, fsdt_inv, lsdt_inv
LD$intercept = rep(1,dim(LD)[1])
TD$intercept = rep(1,dim(TD)[1])

LD$fsdt_inv = tcut.lead - LD$FSDT
LD$lsdt_inv = tcut.lead - LD$LSDT
TD$fsdt_inv = tcut.train - TD$FSDT
TD$lsdt_inv = tcut.train - TD$LSDT

# Add reverts_per_edit, reverts_per_edit_l5, edits_per_article
LD$reverts_per_edit = LD$sum_reverts / LD$sum_edits
LD$reverts_per_edit_l5 = (LD$sum_reverts+1) / (LD$sum_edits+1)
LD$edits_per_article = LD$sum_edits / LD$sum_articles

TD$reverts_per_edit = TD$sum_reverts / TD$sum_edits
TD$reverts_per_edit_l5 = (TD$sum_reverts+1) / (TD$sum_edits+1)
TD$edits_per_article = TD$sum_edits / TD$sum_articles


# Add edits_per_in_day, days_in_p, days_in_pp ... in_ppppp
Mlen = tcut.lead / 116

LD$edits_per_in_day = LD$sum_edits / LD$fsdt_inv
LD$days_in_p = pmin(LD$fsdt_inv, Mlen*5)
LD$days_in_pp = pmin(pmax(LD$fsdt_inv-Mlen*5,0), Mlen*5)
LD$days_in_ppp = pmin(pmax(LD$fsdt_inv-Mlen*10,0), Mlen*5)
LD$days_in_pppp = pmin(pmax(LD$fsdt_inv-Mlen*15,0), Mlen*5)
LD$days_in_ppppp = pmin(pmax(LD$fsdt_inv-Mlen*20,0), Mlen*5)

TD$edits_per_in_day = TD$sum_edits / TD$fsdt_inv
TD$days_in_p = pmin(TD$fsdt_inv, Mlen*5)
TD$days_in_pp = pmin(pmax(TD$fsdt_inv-Mlen*5,0), Mlen*5)
TD$days_in_ppp = pmin(pmax(TD$fsdt_inv-Mlen*10,0), Mlen*5)
TD$days_in_pppp = pmin(pmax(TD$fsdt_inv-Mlen*15,0), Mlen*5)
TD$days_in_ppppp = pmin(pmax(TD$fsdt_inv-Mlen*20,0), Mlen*5)

et = scan(paste(RESDIR,'edit_times_unrounded.csv',sep=''),what="character")
meu = read.csv(paste(RESDIR,"editor_monthly_edits_unique_sessions_unrounded.csv",sep=''),header=T)
mea = read.csv(paste(RESDIR,"editor_monthly_edits_unrounded.csv",sep=''),header=T)
stopifnot(dim(meu) == c(NE,117), dim(mea) == c(NE,117), length(et)==NE)


# Add edits_p, _pp ... _ppppp and sessions_p, sessions_pp, ...._ppppp
LD$edits_p = rowSums(mea[,113:117])
LD$edits_pp = rowSums(mea[,108:112])
LD$edits_ppp = rowSums(mea[,103:107])
LD$edits_pppp = rowSums(mea[,98:102])
LD$edits_ppppp = rowSums(mea[,93:97])

LD$sessions = rowSums(meu[,-1])
LD$sessions_p = rowSums(meu[,113:117])
LD$sessions_pp = rowSums(meu[,108:112])
LD$sessions_ppp = rowSums(meu[,103:107])
LD$sessions_pppp = rowSums(meu[,98:102])
LD$sessions_ppppp = rowSums(meu[,93:97])

itrain = which(mea[,1] %in% TD$user_id)
TD$edits_p = LD$edits_pp[itrain]
TD$edits_pp = LD$edits_ppp[itrain]
TD$edits_ppp = LD$edits_pppp[itrain]
TD$edits_pppp = LD$edits_ppppp[itrain]
TD$edits_ppppp = rowSums(mea[itrain,88:92])

TD$sessions = rowSums(meu[itrain,2:112])
TD$sessions_p = LD$sessions_pp[itrain]
TD$sessions_pp = LD$sessions_ppp[itrain]
TD$sessions_ppp = LD$sessions_pppp[itrain]
TD$sessions_pppp = LD$sessions_ppppp[itrain]
TD$sessions_ppppp = rowSums(meu[itrain,88:92])

# Add "mean_time_between_sessions"    "mean_time_between_sessions_l5"
stopifnot(min(LD$sessions) > 0)
LD$mean_time_between_sessions = (LD$LSDT - LD$FSDT)/LD$sessions
iz = which(LD$sessions_p == 0)
LD$mean_time_between_sessions_l5 = (Mlen*5)/(LD$sessions_p)
LD$mean_time_between_sessions_l5[iz] = LD$lsdt_inv[iz]

stopifnot(min(TD$sessions) > 0)
TD$mean_time_between_sessions = (TD$LSDT - TD$FSDT)/TD$sessions
iz = which(TD$sessions_p == 0)
TD$mean_time_between_sessions_l5 = (Mlen*5)/(TD$sessions_p)
TD$mean_time_between_sessions_l5[iz] = TD$lsdt_inv[iz]

#  Add edits_week_minus<0-3> , sessions_week_minus<0-3>, sessions_month_minus<0-3>
Wlen = 7
ewvar = list(); swvar=list(); smvar = list()
newfeatures = list()
for (j in 0:3){
	
	ewvar[j+1] = paste("edits_week_minus",j,sep='')
	swvar[j+1] = paste("sessions_week_minus",j,sep='')
	smvar[j+1] = paste("sessions_month_minus",j,sep='')
	newfeatures[j+1] = smvar[j+1]
	newfeatures[12-(j*2)-1] = ewvar[j+1]
	newfeatures[12-j*2] = swvar[j+1]	
}
ewvar = unlist(ewvar); swvar = unlist(swvar); smvar = unlist(smvar); newfeatures = unlist(newfeatures)

temp.t = matrix(0,dim(TD)[1],length(newfeatures)); colnames(temp.t) <- newfeatures
temp.l = matrix(0,dim(LD)[1],length(newfeatures)); colnames(temp.l) <- newfeatures
TD = as.matrix(TD); LD = as.matrix(LD) #Matrix indexing is much faster than data.frame indexing
TD = cbind(TD,temp.t); LD = cbind(LD,temp.l)

weekref.lead = tcut.lead - 4*Wlen
weekref.train = tcut.train - 4*Wlen


for (ia in 1:NE){
	itr = which(TD[,'user_id'] == LD[ia,'user_id'])
  
	if (ia%%2000 == 0) {cat(sprintf("Parsed %i of %i editors",ia,NE),"\n"); flush.console();}
  
	for (m in 0:3){
		LD[ia,smvar[m+1]] = meu[ia,117-m]
		
		if (length(itr) == 1){
			TD[itr,smvar[m+1]] = meu[ia,112-m]
		}
	}
	splita <- as.numeric(strsplit(et[ia],",",fixed=T)[[1]])
	for (w in 3:0){
		winst = w*Wlen + weekref.lead
		winen = winst + Wlen
		iwithin = (splita > winst & splita <= winen)
		LD[ia,ewvar[w+1]] = sum(iwithin)
		if (LD[ia,ewvar[w+1]] > 0){
			LD[ia,swvar[w+1]] = sum(diff(sort(splita[iwithin])) > 0.5) + 1
		}else{
			LD[ia,swvar[w+1]] = 0
		}
		rm(list=c("winst","winen","iwithin"))
		
		# check if this user also exists in the training set, process if it does
		if (length(itr) == 1){
			winst = w*Wlen + weekref.train
			winen = winst + Wlen
			iwithin = (splita > winst & splita <= winen)
			TD[itr,ewvar[w+1]] = sum(iwithin)
			if (TD[itr,ewvar[w+1]] > 0){
				TD[itr,swvar[w+1]] = sum(diff(sort(splita[iwithin])) > 0.5) + 1
			}else{
				TD[itr,swvar[w+1]] = 0
			}	
		}
	}	
}
TD = data.frame(TD)
LD = data.frame(LD)

# "edits_per_session"  "edits_per_session_l5" "edits_per_session_last_month"  "edits_per_session_last_week" 
LD$edits_per_session = LD$sum_edits / LD$sessions
LD$edits_per_session_l5 = LD$edits_p / pmax(LD$sessions_p,1); 
LD$edits_per_session_last_month = LD$Edits_mth_cutoff_minus_0 / pmax(LD$sessions_month_minus0,1);
LD$edits_per_session_last_week = LD$edits_week_minus0 / pmax(LD$sessions_week_minus0,1); 

TD$edits_per_session = TD$sum_edits / pmax(TD$sessions,1);
TD$edits_per_session_l5 = TD$edits_p / pmax(TD$sessions_p,1); 
TD$edits_per_session_last_month = TD$Edits_mth_cutoff_minus_0 / pmax(TD$sessions_month_minus0,1);
TD$edits_per_session_last_week = TD$edits_week_minus0 / pmax(TD$sessions_week_minus0,1); 

write.csv(LD, file = paste(RESDIR,"Featureskd_XinP1P2P3.csv",sep=''), row.names = FALSE);
write.csv(TD, file = paste(RESDIR,"Featureskd_XinP1P2_YinP3.csv",sep=''), row.names = FALSE);


### -------------- Now Merge reverts related features ------------------------------------------------

itrain = which(RD$user_id %in% TD$user_id)
stopifnot(RD$user_id[itrain] == TD$user_id)

TD = cbind(TD,RD[itrain,2:10])
LD = cbind(LD,RD[,11:19])

TD$reverts_got_per_edit_l5 = (TD$train_l5m_got - TD$train_l5m_auto)/(TD$edits_p + 1)
TD$reverts_made_per_edit_l5 = (TD$train_l5m_made - TD$train_l5m_auto)/(TD$edits_p + 1)

TD$reverts_got_per_edit_l1 = (TD$train_l1m_got - TD$train_l1m_auto)/(TD$Edits_mth_cutoff_minus_0 + 1)
TD$reverts_made_per_edit_l1 = (TD$train_l1m_made - TD$train_l1m_auto)/(TD$Edits_mth_cutoff_minus_0 + 1)

LD$reverts_got_per_edit_l5 = (LD$lead_l5m_got - LD$lead_l5m_auto)/(LD$edits_p + 1)
LD$reverts_made_per_edit_l5 = (LD$lead_l5m_made - LD$lead_l5m_auto)/(LD$edits_p + 1)

LD$reverts_got_per_edit_l1 = (LD$lead_l1m_got - LD$lead_l1m_auto)/(LD$Edits_mth_cutoff_minus_0 + 1)
LD$reverts_made_per_edit_l1 = (LD$lead_l1m_made - LD$lead_l1m_auto)/(LD$Edits_mth_cutoff_minus_0 + 1)

colnames(TD) = gsub('train_','reverts_',colnames(TD))
colnames(LD) = gsub('lead_','reverts_',colnames(LD))

write.csv(LD, file = paste(RESDIR,"Featureskd_wrevs_XinP1P2P3.csv",sep=''), row.names = FALSE);
write.csv(TD, file = paste(RESDIR,"Featureskd_wrevs_XinP1P2_YinP3.csv",sep=''), row.names = FALSE);
