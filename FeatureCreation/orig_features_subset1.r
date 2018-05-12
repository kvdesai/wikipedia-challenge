rm(list = setdiff(ls(),c('DATADIR','RESDIR','MAIN_DIR')))

# Raw Input Files: None
# Derived Input files:
#	'edit_times_unrounded.csv'
#   'eid_fsdt_regdate_table.csv'


NE = 44514
NT = 22126031

t0 = strptime("2001-01-01 0:0:0", "%Y-%m-%d %H:%M:%S"); 
te.lead = as.numeric(strptime("2010-09-01 0:0:0", "%Y-%m-%d %H:%M:%S") - t0); 
te.train = as.numeric(strptime("2010-04-01 0:0:0", "%Y-%m-%d %H:%M:%S") - t0);
tl5.lead = te.train
tl5.train = as.numeric(strptime("2009-11-01 0:0:0", "%Y-%m-%d %H:%M:%S") - t0);

tm0.lead = as.numeric(strptime("2010-08-01 0:0:0", "%Y-%m-%d %H:%M:%S") - t0);
tm1.lead = as.numeric(strptime("2010-07-01 0:0:0", "%Y-%m-%d %H:%M:%S") - t0);
tm2.lead = as.numeric(strptime("2010-06-01 0:0:0", "%Y-%m-%d %H:%M:%S") - t0);
tm3.lead = as.numeric(strptime("2010-05-01 0:0:0", "%Y-%m-%d %H:%M:%S") - t0);
tm4.lead = tl5.lead

tm0.train = as.numeric(strptime("2010-03-01 0:0:0", "%Y-%m-%d %H:%M:%S") - t0);
tm1.train = as.numeric(strptime("2010-02-01 0:0:0", "%Y-%m-%d %H:%M:%S") - t0);
tm2.train = as.numeric(strptime("2010-01-01 0:0:0", "%Y-%m-%d %H:%M:%S") - t0);
tm3.train = as.numeric(strptime("2009-12-01 0:0:0", "%Y-%m-%d %H:%M:%S") - t0);
tm4.train = tl5.train

# First read EID | FSDT | RegDate Table
EFR = read.csv(paste(RESDIR,'eid_fsdt_regdate_table.csv',sep=''),header=T)

# Now create another subset of users whose first edit date (FSDT) was before te.train
# There are 33839 users in this set
uids.train = EFR$user_id[which(EFR$FSDTnum < te.train)]
uids.lead = EFR$user_id

cname.lead = c("user_id","FSDT","LSDT","registration_date","sum_edits","sum_edit_days","sum_edit_ns0","sum_edit_ns1","sum_edit_ns2","sum_edit_ns3","sum_edit_ns4","sum_edit_ns5","sum_reverts","sum_reverts_ns0","sum_reverts_ns1","sum_reverts_ns2","sum_reverts_ns3","sum_reverts_ns4","sum_reverts_ns5","sum_reverts_self","sum_articles","sum_delta","sum_edits_l5","sum_edit_days_l5","sum_edit_ns0_l5","sum_edit_ns1_l5","sum_edit_ns2_l5","sum_edit_ns3_l5","sum_edit_ns4_l5","sum_edit_ns5_l5","sum_reverts_l5","sum_reverts_ns0_l5","sum_reverts_ns1_l5","sum_reverts_ns2_l5","sum_reverts_ns3_l5","sum_reverts_ns4_l5","sum_reverts_ns5_l5","sum_reverts_self_l5","sum_new_articles_l5","sum_delta_l5","Edits_mth_cutoff_minus_4","Edits_mth_cutoff_minus_3","Edits_mth_cutoff_minus_2","Edits_mth_cutoff_minus_1","Edits_mth_cutoff_minus_0")

cname.train = c("user_id","FSDT","LSDT","registration_date","sum_edits","sum_edit_days","sum_edit_ns0","sum_edit_ns1","sum_edit_ns2","sum_edit_ns3","sum_edit_ns4","sum_edit_ns5","sum_reverts","sum_reverts_ns0","sum_reverts_ns1","sum_reverts_ns2","sum_reverts_ns3","sum_reverts_ns4","sum_reverts_ns5","sum_reverts_self","sum_articles","sum_delta","sum_edits_l5","sum_edit_days_l5","sum_edit_ns0_l5","sum_edit_ns1_l5","sum_edit_ns2_l5","sum_edit_ns3_l5","sum_edit_ns4_l5","sum_edit_ns5_l5","sum_reverts_l5","sum_reverts_ns0_l5","sum_reverts_ns1_l5","sum_reverts_ns2_l5","sum_reverts_ns3_l5","sum_reverts_ns4_l5","sum_reverts_ns5_l5","sum_reverts_self_l5","sum_new_articles_l5","sum_delta_l5","Edits_mth_cutoff_minus_4","Edits_mth_cutoff_minus_3","Edits_mth_cutoff_minus_2","Edits_mth_cutoff_minus_1","Edits_mth_cutoff_minus_0","y_next5_edits")

ResMatLead = matrix(-1,NE,45)
colnames(ResMatLead) = cname.lead
ResMatTrain = matrix(-1,length(uids.train),46)
colnames(ResMatTrain) = cname.train
ResMatLead = data.frame(ResMatLead); ResMatTrain = data.frame(ResMatTrain); 

ResMatLead$user_id = EFR[,1]
ResMatLead$FSDT = EFR[,2]
ResMatLead$registration_date = EFR[,3]

itrain = which(EFR$FSDTnum < te.train)
ResMatTrain$user_id = EFR[itrain,1]
ResMatTrain$FSDT = EFR[itrain,2]
ResMatTrain$registration_date = EFR[itrain,3]

rm(list=c("EFR","itrain",'cname.lead','cname.train'))

######## First read-off as many features as we can from the already created eid-time file
ET = scan(paste(RESDIR,'edit_times_unrounded.csv',sep=''),what='character');
# to.export = setdiff(ls(),c('ResMatTrain','ResMatLead'))
# require('snowfall')
# sfInit(parallel=T,12)
# sfExport(list=to.export,local=F)

parWrapper <- function( i )
{
	if (i%%2000 == 0) {cat(sprintf("Parsed %i of %i editors",i,NE),"\n"); flush.console();}
	splita <- as.numeric(strsplit(ET[i],",",fixed=T)[[1]])
	fsa <- floor(splita)
	
	iwithin = which((splita >= tl5.lead) & (splita < te.lead))
	resLead = rep(0,10);
	resTrain = rep(0,10);
	
	resLead[1] = length(splita)
	resLead[2]  = length(unique(fsa))
	resLead[3] = length(iwithin)
	resLead[4] = length(unique(fsa[iwithin]))
	resLead[5] = max(splita)
	
	resLead[6] = sum((splita >= tm4.lead) & (splita < tm3.lead)) 
	resLead[7] = sum((splita >= tm3.lead) & (splita < tm2.lead))
	resLead[8] = sum((splita >= tm2.lead) & (splita < tm1.lead))
	resLead[9] = sum((splita >= tm1.lead) & (splita < tm0.lead))
	resLead[10] = sum((splita >= tm0.lead) & (splita < te.lead))
	
	it = which(uids.train == uids.lead[i])
	if (length(it) > 0){
		iexist =  which(splita < te.train)
		iwithin = which((splita >= tl5.train) & (splita < te.train))
		
		resTrain[1] = length(iexist)
		resTrain[2]  = length(unique(fsa[iexist]))
		resTrain[3] = length(iwithin)
		resTrain[4] = length(unique(fsa[iwithin]))		
		resTrain[5] = max(splita[iexist])
		
		resTrain[6] = sum((splita >= tm4.train) & (splita < tm3.train)) 
		resTrain[7] = sum((splita >= tm3.train) & (splita < tm2.train))
		resTrain[8] = sum((splita >= tm2.train) & (splita < tm1.train))
		resTrain[9] = sum((splita >= tm1.train) & (splita < tm0.train))
		resTrain[10] = sum((splita >= tm0.train) & (splita < te.train))		
	}
	return(list(ilead = i, itrain=it,reslead = resLead, restrain = resTrain))
}
#result = sfLapply(1:NE,parWrapper); sfStop();
result = lapply(1:NE,parWrapper);
varorder = c('sum_edits','sum_edit_days','sum_edits_l5','sum_edit_days_l5','LSDT','Edits_mth_cutoff_minus_4','Edits_mth_cutoff_minus_3','Edits_mth_cutoff_minus_2','Edits_mth_cutoff_minus_1','Edits_mth_cutoff_minus_0');
	
for (ir in 1:length(result)){
	if (ir%%1000 == 0) {cat(sprintf("Aggregated %i of %i editors",ir,NE),"\n"); flush.console();}
	il = result[[ir]]$ilead
	ResMatLead[il,varorder] = result[[ir]]$reslead
	it = result[[ir]]$itrain
	if (length(it) > 0){
		ResMatTrain[it,varorder] = result[[ir]]$restrain
	}
}

write.csv(ResMatLead, file = paste(RESDIR,"OrigSubset1_Lead.csv",sep=''), row.names = FALSE);
write.csv(ResMatTrain, file = paste(RESDIR,"OrigSubset1_Train.csv",sep=''), row.names = FALSE);




