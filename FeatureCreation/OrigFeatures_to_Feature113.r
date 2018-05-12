rm(list = setdiff(ls(),c('DATADIR','RESDIR','MAIN_DIR')))

NE = 44514
t0 = strptime("2001-01-01 0:0:0", "%Y-%m-%d %H:%M:%S"); 
tcut.train = as.numeric(strptime("2010-04-01 0:0:0", "%Y-%m-%d %H:%M:%S") - t0);
tcut.lead =  as.numeric(strptime("2010-09-01 0:0:0", "%Y-%m-%d %H:%M:%S") - t0); 

# Load Original Feature File
TD = read.csv(paste(RESDIR,'OrigSet_xinp1p2_yinp3.csv',sep=''),header=T)
LD = read.csv(paste(RESDIR,'OrigSet_xinp1p2p3.csv',sep=''),header=T)
et = scan(paste(RESDIR,'edit_times.csv',sep=''),what="character")
mea = read.csv(paste(RESDIR,'editor_monthly_edits.csv',sep=''),header=T)
meu = read.csv(paste(RESDIR,'editor_monthly_edits_unique_days.csv',sep=''),header=T)
stopifnot(length(et) == dim(LD)[1], all(dim(mea)==c(NE,117)), all(dim(meu)==c(NE,117)))


newfeatures = c('intercept','ln_edit','edits_per_day','pc_edit_ns0','pc_edit_ns1','pc_edit_ns2','pc_edit_ns3','pc_edit_ns4','pc_edit_ns5','ln_reverts','reverts_per_edit','pc_reverts_ns0','pc_reverts_ns1','pc_reverts_ns2','pc_reverts_ns3','pc_reverts_ns4','pc_reverts_ns5','edits_per_article','ln_edit_l5','edits_per_day_l5','edits_per_article_l5','pc_edit_ns0_l5','pc_edit_ns1_l5','pc_edit_ns2_l5','pc_edit_ns3_l5','pc_edit_ns4_l5','pc_edit_ns5_l5','pc_reverts_ns0_l5','pc_reverts_ns1_l5','pc_reverts_ns2_l5','pc_reverts_ns3_l5','pc_reverts_ns4_l5','pc_reverts_ns5_l5','reverts_per_edit_l5','ln_edit_pp','ln_edit_ppp','fsdt_inv','lsdt_inv','regdt_inv','mean_time_between_edits','mean_time_between_edits_l5',"edits_week_last","edits_week_minus1","edits_week_minus2","edits_week_minus3","edit_days_week_last","edit_days_week_minus1","edit_days_week_minus2","edit_days_week_minus3","edit_days_month_last","edit_days_month_minus1","edit_days_month_minus2","edit_days_month_minus3","edit_days_ppp","ln_edit_pppp","ln_edit_ppppp","edit_days_pppp","edit_days_ppppp","ln_edit_1314","ln_edit_1516","ln_edit_1718","ln_edit_1920","ln_edit_2122","ln_edit_2324","edit_days_13","edit_days_14","edit_days_15")

lead = matrix(0,length(et),length(newfeatures))
colnames(lead) = newfeatures

train = lead

itrain = which(LD$user_id %in% TD$user_id)

stopifnot(dim(LD)[1] == NE)
TD$registration_date[is.na(TD$registration_date)] = -36893
LD$registration_date[is.na(LD$registration_date)] = -(36893+150)


edit_p = rowSums(mea[,113:117]); edit_days_p = rowSums(meu[,113:117])
edit_pp = rowSums(mea[,108:112]); edit_days_pp = rowSums(meu[,108:112])
edit_ppp = rowSums(mea[,103:107])
edit_pppp = rowSums(mea[,98:102])

o1 = 0
o2 = o1+41
o3 = o2+12

train[itrain,'intercept'] = 1
train[itrain,'fsdt_inv'] = tcut.train - floor(TD$FSDT)
train[itrain,'lsdt_inv'] = tcut.train - floor(TD$LSDT)
train[itrain,'regdt_inv'] = tcut.train - TD$registration_date
train[itrain,'ln_edit'] = log(TD$sum_edits+1)
train[itrain,'ln_reverts'] = log(TD$sum_reverts+1)
train[itrain,'edits_per_day'] = TD$sum_edits/TD$sum_edit_days
train[itrain,'reverts_per_edit'] = TD$sum_reverts/TD$sum_edits
train[itrain,'edits_per_article'] = TD$sum_edits/TD$sum_articles
train[itrain,'mean_time_between_edits'] = (train[itrain,'regdt_inv']/TD$sum_edits)
train[itrain,'mean_time_between_edits_l5'] = 150/(edit_pp[itrain]+1)
train[itrain,'ln_edit_l5'] = log(edit_pp[itrain]+1)
train[itrain,'edits_per_day_l5'] = (edit_pp[itrain]+1)/(edit_days_pp[itrain]+1)
train[itrain,'edits_per_article_l5'] = (edit_pp[itrain]+1)/(TD$sum_new_articles_l5+1)
train[itrain,'reverts_per_edit_l5'] = (TD$sum_reverts_l5+1)/(edit_pp[itrain]+1)

# Namesapce based features
train[itrain,'pc_edit_ns0'] = TD$sum_edit_ns0/TD$sum_edits
train[itrain,'pc_reverts_ns0'] = (TD$sum_reverts_ns0+1)/(TD$sum_reverts+1)
train[itrain,'pc_edit_ns0_l5'] = (TD$sum_edit_ns0_l5+1)/(edit_pp[itrain]+1)
train[itrain,'pc_reverts_ns0_l5'] = (TD$sum_reverts_ns0_l5+1)/(TD$sum_reverts_l5+1)

train[itrain,'pc_edit_ns1'] = TD$sum_edit_ns1/TD$sum_edits
train[itrain,'pc_reverts_ns1'] = (TD$sum_reverts_ns1+1)/(TD$sum_reverts+1)
train[itrain,'pc_edit_ns1_l5'] = (TD$sum_edit_ns1_l5+1)/(edit_pp[itrain]+1)
train[itrain,'pc_reverts_ns1_l5'] = (TD$sum_reverts_ns1_l5+1)/(TD$sum_reverts_l5+1)

train[itrain,'pc_edit_ns2'] = TD$sum_edit_ns2/TD$sum_edits
train[itrain,'pc_reverts_ns2'] = (TD$sum_reverts_ns2+1)/(TD$sum_reverts+1)
train[itrain,'pc_edit_ns2_l5'] = (TD$sum_edit_ns2_l5+1)/(edit_pp[itrain]+1)
train[itrain,'pc_reverts_ns2_l5'] = (TD$sum_reverts_ns2_l5+1)/(TD$sum_reverts_l5+1)

train[itrain,'pc_edit_ns3'] = TD$sum_edit_ns3/TD$sum_edits
train[itrain,'pc_reverts_ns3'] = (TD$sum_reverts_ns3+1)/(TD$sum_reverts+1)
train[itrain,'pc_edit_ns3_l5'] = (TD$sum_edit_ns3_l5+1)/(edit_pp[itrain]+1)
train[itrain,'pc_reverts_ns3_l5'] = (TD$sum_reverts_ns3_l5+1)/(TD$sum_reverts_l5+1)

train[itrain,'pc_edit_ns4'] = TD$sum_edit_ns4/TD$sum_edits
train[itrain,'pc_reverts_ns4'] = (TD$sum_reverts_ns4+1)/(TD$sum_reverts+1)
train[itrain,'pc_edit_ns4_l5'] = (TD$sum_edit_ns4_l5+1)/(edit_pp[itrain]+1)
train[itrain,'pc_reverts_ns4_l5'] = (TD$sum_reverts_ns4_l5+1)/(TD$sum_reverts_l5+1)

train[itrain,'pc_edit_ns5'] = TD$sum_edit_ns5/TD$sum_edits
train[itrain,'pc_reverts_ns5'] = (TD$sum_reverts_ns5+1)/(TD$sum_reverts+1)
train[itrain,'pc_edit_ns5_l5'] = (TD$sum_edit_ns5_l5+1)/(edit_pp[itrain]+1)
train[itrain,'pc_reverts_ns5_l5'] = (TD$sum_reverts_ns5_l5+1)/(TD$sum_reverts_l5+1)

# --------------- Leaderboard --------------------------
lead[,'intercept'] = 1
lead[,'fsdt_inv'] = tcut.lead - floor(LD$FSDT)
lead[,'lsdt_inv'] = tcut.lead - floor(LD$LSDT)
lead[,'regdt_inv'] = tcut.lead - LD$registration_date
lead[,'ln_edit'] = log(LD$sum_edits+1)
lead[,'ln_reverts'] = log(LD$sum_reverts+1)
lead[,'edits_per_day'] = LD$sum_edits/LD$sum_edit_days
lead[,'reverts_per_edit'] = LD$sum_reverts/LD$sum_edits
lead[,'edits_per_article'] = LD$sum_edits/LD$sum_articles
lead[,'mean_time_between_edits'] = (lead[,'regdt_inv']/LD$sum_edits)
lead[,'mean_time_between_edits_l5'] = 150/(edit_p+1)
lead[,'ln_edit_l5'] = log(edit_p+1)
lead[,'edits_per_day_l5'] = (edit_p+1)/(edit_days_p+1)
lead[,'edits_per_article_l5'] = (edit_p+1)/(LD$sum_new_articles_l5+1)
lead[,'reverts_per_edit_l5'] = (LD$sum_reverts_l5+1)/(edit_p+1)

# Namesapce based features
lead[,'pc_edit_ns0'] = LD$sum_edit_ns0/LD$sum_edits
lead[,'pc_reverts_ns0'] = (LD$sum_reverts_ns0+1)/(LD$sum_reverts+1)
lead[,'pc_edit_ns0_l5'] = (LD$sum_edit_ns0_l5+1)/(edit_p+1)
lead[,'pc_reverts_ns0_l5'] = (LD$sum_reverts_ns0_l5+1)/(LD$sum_reverts_l5+1)

lead[,'pc_edit_ns1'] = LD$sum_edit_ns1/LD$sum_edits
lead[,'pc_reverts_ns1'] = (LD$sum_reverts_ns1+1)/(LD$sum_reverts+1)
lead[,'pc_edit_ns1_l5'] = (LD$sum_edit_ns1_l5+1)/(edit_p+1)
lead[,'pc_reverts_ns1_l5'] = (LD$sum_reverts_ns1_l5+1)/(LD$sum_reverts_l5+1)

lead[,'pc_edit_ns2'] = LD$sum_edit_ns2/LD$sum_edits
lead[,'pc_reverts_ns2'] = (LD$sum_reverts_ns2+1)/(LD$sum_reverts+1)
lead[,'pc_edit_ns2_l5'] = (LD$sum_edit_ns2_l5+1)/(edit_p+1)
lead[,'pc_reverts_ns2_l5'] = (LD$sum_reverts_ns2_l5+1)/(LD$sum_reverts_l5+1)

lead[,'pc_edit_ns3'] = LD$sum_edit_ns3/LD$sum_edits
lead[,'pc_reverts_ns3'] = (LD$sum_reverts_ns3+1)/(LD$sum_reverts+1)
lead[,'pc_edit_ns3_l5'] = (LD$sum_edit_ns3_l5+1)/(edit_p+1)
lead[,'pc_reverts_ns3_l5'] = (LD$sum_reverts_ns3_l5+1)/(LD$sum_reverts_l5+1)

lead[,'pc_edit_ns4'] = LD$sum_edit_ns4/LD$sum_edits
lead[,'pc_reverts_ns4'] = (LD$sum_reverts_ns4+1)/(LD$sum_reverts+1)
lead[,'pc_edit_ns4_l5'] = (LD$sum_edit_ns4_l5+1)/(edit_p+1)
lead[,'pc_reverts_ns4_l5'] = (LD$sum_reverts_ns4_l5+1)/(LD$sum_reverts_l5+1)

lead[,'pc_edit_ns5'] = LD$sum_edit_ns5/LD$sum_edits
lead[,'pc_reverts_ns5'] = (LD$sum_reverts_ns5+1)/(LD$sum_reverts+1)
lead[,'pc_edit_ns5_l5'] = (LD$sum_edit_ns5_l5+1)/(edit_p+1)
lead[,'pc_reverts_ns5_l5'] = (LD$sum_reverts_ns5_l5+1)/(LD$sum_reverts_l5+1)

train[,'ln_edit_pp'] = log(edit_ppp+1)
train[,'ln_edit_ppp'] = log(edit_pppp+1)

lead[,'ln_edit_pp'] = log(edit_pp+1)
lead[,'ln_edit_ppp'] = log(edit_ppp+1)

m5len = 152
for (i in 1:length(et)){
	edates = as.numeric(strsplit(et[i],',')[[1]])
	
	
	# train[i,'ln_edit_pp'] = log(length(edates[edates > (tcut.train - 2*m5len)  & edates <= (tcut.train - 1*m5len)])+1)
	# train[i,'ln_edit_ppp'] = log(length(edates[edates > (tcut.train - 3*m5len)  & edates <= (tcut.train - 2*m5len)])+1)
		
	# lead[i,'ln_edit_pp'] = log(length(edates[edates > (tcut.lead - 2*m5len)  & edates <= (tcut.lead - 1*m5len)])+1)
	# lead[i,'ln_edit_ppp'] = log(length(edates[edates > (tcut.lead - 3*m5len)  & edates <= (tcut.lead - 2*m5len)])+1)
	# -------------- Adding 12 features more -----------------
	for (w in 1:4){
		ew.train = edates[edates > (tcut.train - w*7)  & edates <= (tcut.train - (w-1)*7)]
		train[i,o2+w] = length(ew.train)
		train[i,o2+w+4] = length(unique(ew.train))	
		
		ew.lead = edates[edates > (tcut.lead - w*7)  & edates <= (tcut.lead - (w-1)*7)]
		lead[i,o2+w] = length(ew.lead)
		lead[i,o2+w+4] = length(unique(ew.lead))	
	}
	for (m in 1:4){
		em.train = edates[edates > (tcut.train - m*30.4)  & edates <= (tcut.train - (m-1)*30.4)]		
		train[i,o2+m+8] = length(unique(em.train))	 
		
		em.lead = edates[edates > (tcut.lead - m*30.4)  & edates <= (tcut.lead - (m-1)*30.4)]		
		lead[i,o2+m+8] = length(unique(em.lead))	 
	}
	
	#----------------- Adding another 14 features	
	
	train[i,o3+2]= length(unique(edates[edates > (tcut.train - 3*m5len)  & edates <= (tcut.train - 2*m5len)]))
	lead[i,o3+1]= length(unique(edates[edates > (tcut.lead - 3*m5len)  & edates <= (tcut.lead - 2*m5len)]))
	for (m5 in 4:5){
		em.train = edates[edates > (tcut.train - m5*m5len)  & edates <= (tcut.train - (m5-1)*m5len)]
		train[i,o3+m5-2] = log(length(em.train)+1)
		train[i,o3+m5] = (length(unique(em.train)))	 
		
		em.lead = edates[edates > (tcut.lead - m5*m5len)  & edates <= (tcut.lead - (m5-1)*m5len)]
		lead[i,o3+m5-2] = log(length(em.lead)+1)
		lead[i,o3+m5] = (length(unique(em.lead)))	 
	}
	m2len = 61
	for (m2 in 1:6){
		em.train = edates[edates > (tcut.train - (6 + m2)*m2len)  & edates <= (tcut.train - (6 + m2-1)*m2len)]
		train[i,o3+5+m2] = log(length(em.train)+1)
		
		em.lead = edates[edates > (tcut.lead - (6 + m2)*m2len)  & edates <= (tcut.lead - (6 + m2-1)*m2len)]
		lead[i,o3+5+m2] = log(length(em.lead)+1)
	}
	mlen = 30.5
	for (m1 in 1:3){
		em.train = edates[edates > (tcut.train - (12+m1)*mlen)  & edates <= (tcut.train - (12+m1-1)*mlen)]
		train[i,o3+11+m1] = length(unique(em.train))
		
		em.lead = edates[edates > (tcut.lead - (12+m1)*mlen)  & edates <= (tcut.lead - (12+m1-1)*mlen)]
		lead[i,o3+11+m1] = length(unique(em.lead))
	}
}

newcolnames.t =  c(colnames(TD)[1],colnames(train)[1],colnames(TD)[-1],colnames(train)[-1]);
newcolnames.l =  c(colnames(LD)[1],colnames(lead)[1],colnames(LD)[-1],colnames(lead)[-1]);

TDnew =  cbind(TD[,1],train[itrain,1],TD[,-1],train[itrain,-1]);
LDnew =  cbind(LD[,1],lead[,1],LD[,-1],lead[,-1]);

colnames(TDnew) <- newcolnames.t
colnames(LDnew) <- newcolnames.l

write.csv(TDnew,file=paste(RESDIR,"Features113_XinP1P2_YinP3.csv",sep=''), row.names = F)
write.csv(LDnew,file=paste(RESDIR,"Features113_XinP1P2P3.csv",sep=''), row.names=F)

