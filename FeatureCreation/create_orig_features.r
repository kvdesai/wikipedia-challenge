rm(list = setdiff(ls(),c('DATADIR','RESDIR','MAIN_DIR')))

# Raw Input Files: NONE
# Derived Input files:
#   'OrigSubset1_Lead.csv'
#   'OrigSubset1_Train.csv'
#   'Parsed_RawDump_Full.csv'

NE = 44514
NT = 22126031

t0 = strptime("2001-01-01 0:0:0", "%Y-%m-%d %H:%M:%S"); 
te.lead = as.numeric(strptime("2010-09-01 0:0:0", "%Y-%m-%d %H:%M:%S") - t0); 
te.train = as.numeric(strptime("2010-04-01 0:0:0", "%Y-%m-%d %H:%M:%S") - t0);
tl5.lead = te.train
tl5.train = as.numeric(strptime("2009-11-01 0:0:0", "%Y-%m-%d %H:%M:%S") - t0);

ResMatLead = read.csv(paste(RESDIR,"OrigSubset1_Lead.csv",sep=''), header=T); ResMatLead[ResMatLead==-1] = 0
ResMatTrain = read.csv(paste(RESDIR,"OrigSubset1_Train.csv",sep=''), header=T); ResMatTrain[ResMatTrain==-1] = 0

ResMatLead = as.matrix(ResMatLead)
ResMatTrain = as.matrix(ResMatTrain)

# load(paste(RESDIR,'Parsed_RawDump_Full.rdata',sep=''))
# A = read.csv(paste(RESDIR,'Parsed_RawDump_Full.csv',sep=''),header=T)
con <- file(paste(DATADIR,'training.tsv',sep=''),'r')
header = readLines(con,n=1); #read the header
stopifnot(header== "user_id\tarticle_id\trevision_id\tnamespace\ttimestamp\tmd5\treverted\treverted_user_id\treverted_revision_id\tdelta\tcur_size")

# Create Name space variable-name list
var_sum_edit = list()
var_sum_reverts = list()
var_sum_edit_l5 = list()
var_sum_reverts_l5 = list()
for (nspace in 0:5){
	var_sum_edit[nspace+1] = paste('sum_edit_ns',nspace,sep='')
	var_sum_reverts[nspace+1] = paste('sum_reverts_ns',nspace,sep='')
	
	var_sum_edit_l5[nspace+1] = paste(var_sum_edit[nspace+1],'_l5',sep='')
	var_sum_reverts_l5[nspace+1] = paste(var_sum_reverts[nspace+1],'_l5',sep='')
}
var_sum_edit = unlist(var_sum_edit);
var_sum_edit_l5 = unlist(var_sum_edit_l5);
var_sum_reverts = unlist(var_sum_reverts);
var_sum_reverts_l5 = unlist(var_sum_reverts_l5);
# THe huge loop
artlist = rep(list(NA),NE)
artlist.train = rep(list(NA),NE)

# Create editor.id->index map for efficient reverse lookup
uid.idx =  matrix(NA,max(ResMatLead[,'user_id']),2)
for (i in 1:dim(ResMatLead)[1]){
	uid.idx[ResMatLead[i,'user_id'],1] = i 
}

for (i in 1:dim(ResMatTrain)[1]){
	uid.idx[ResMatTrain[i,'user_id'],2] = i 
}

time_start = proc.time()

for (i in 2:(NT+1)){
	if (i%%200000 == 0) {
		cat(sprintf("Parsed %i  / %i lines, Elapsed %.2f minutes",i,NT+1,(as.numeric(proc.time()-time_start)[3]/60)),"\n"); 
		flush.console();
	}
	#sline = strsplit(A[i],'\t')[[1]]	
	rline = readLines(con, n=1)
	sline = strsplit(rline,'\t')[[1]]
	
	uid = as.integer(sline[1])
	artid = as.integer(sline[2])
	nspace = as.integer(sline[4]) #namespace
	tstamp = as.numeric(strptime(sline[5], "%Y-%m-%d %H:%M:%S") - t0);
	revflag = as.integer(sline[7])
	revuid = as.integer(sline[8])
	delta = as.integer(sline[10])
	# uid = A[i,1]
	# artid = A[i,2]
	# nspace = A[i,3]
	# tstamp = A[i,4]
	# revflag = A[i,5]
	# revuid = A[i,6]
	# delta = A[i,7]
	#--------------------------------------------
	ilead = uid.idx[uid,1]; stopifnot(!is.na(ilead));		
	itrain = uid.idx[uid,2]
	user.in.training = (!is.na(itrain))	
	edit.in.leadl5 = (tstamp >= tl5.lead)
	edit.in.trainl5 = ((tstamp >= tl5.train) && (tstamp < te.train))
	
	#---------- LEADERBOARD ---------------------
	ResMatLead[ilead,var_sum_edit[nspace+1]] = ResMatLead[ilead,var_sum_edit[nspace+1]] + 1
	if(edit.in.leadl5){
		ResMatLead[ilead,var_sum_edit_l5[nspace+1]] = ResMatLead[ilead,var_sum_edit_l5[nspace+1]] + 1
	}
	if(revflag==1){
		ResMatLead[ilead,'sum_reverts'] = ResMatLead[ilead,'sum_reverts'] + 1
		ResMatLead[ilead,var_sum_reverts[nspace+1]] = ResMatLead[ilead,var_sum_reverts[nspace+1]] + 1
		if(edit.in.leadl5){
			ResMatLead[ilead,'sum_reverts_l5'] = ResMatLead[ilead,'sum_reverts_l5'] + 1
			ResMatLead[ilead,var_sum_reverts_l5[nspace+1]] = ResMatLead[ilead,var_sum_reverts_l5[nspace+1]] + 1
		}
		
		if (revuid == uid){
			ResMatLead[ilead,'sum_reverts_self'] = ResMatLead[ilead,'sum_reverts_self'] + 1
			if(edit.in.leadl5){
				ResMatLead[ilead,'sum_reverts_self_l5'] = ResMatLead[ilead,'sum_reverts_self_l5'] + 1
			}
		}
		# irevid = which(ResMatLead[,'user_id'] == revuid) # uid.idx[revuid,1]
		# if (length(irevid) > 0){ 
			# ResMatLead[irevid,'sum_reverts_self'] = ResMatLead[irevid,'sum_reverts_self'] + 1
			# if(edit.in.leadl5){
				# ResMatLead[irevid,'sum_reverts_self_l5'] = ResMatLead[irevid,'sum_reverts_self_l5'] + 1
			# }
		# }
	}
	
	if (!(artid %in% artlist[[ilead]])){
		ResMatLead[ilead,'sum_articles'] = ResMatLead[ilead,'sum_articles']+1		
		if(edit.in.leadl5){
			ResMatLead[ilead,'sum_new_articles_l5'] = ResMatLead[ilead,'sum_new_articles_l5']+1
		}
		artlist[[ilead]] = c(artlist[[ilead]],artid)		
	}
	
	ResMatLead[ilead,'sum_delta'] = ResMatLead[ilead,'sum_delta'] + delta
	if(edit.in.leadl5){
		ResMatLead[ilead,'sum_delta_l5'] = ResMatLead[ilead,'sum_delta_l5'] + delta
	}
	
	#---------- TRAINING ---------------------
	if(user.in.training && tstamp < te.train){
		ResMatTrain[itrain,var_sum_edit[nspace+1]] = ResMatTrain[itrain,var_sum_edit[nspace+1]] + 1
		if(edit.in.trainl5){
			ResMatTrain[itrain,var_sum_edit_l5[nspace+1]] = ResMatTrain[itrain,var_sum_edit_l5[nspace+1]] + 1
		}
		if(revflag==1){
			ResMatTrain[itrain,'sum_reverts'] = ResMatTrain[itrain,'sum_reverts'] + 1
			ResMatTrain[itrain,var_sum_reverts[nspace+1]] = ResMatTrain[itrain,var_sum_reverts[nspace+1]] + 1
			if(edit.in.trainl5){
				ResMatTrain[itrain,'sum_reverts_l5'] = ResMatTrain[itrain,'sum_reverts_l5'] + 1
				ResMatTrain[itrain,var_sum_reverts_l5[nspace+1]] = ResMatTrain[itrain,var_sum_reverts_l5[nspace+1]] + 1
			}
			
			if (revuid == uid){
				ResMatTrain[itrain,'sum_reverts_self'] = ResMatTrain[itrain,'sum_reverts_self'] + 1
				if(edit.in.trainl5){
					ResMatTrain[itrain,'sum_reverts_self_l5'] = ResMatTrain[itrain,'sum_reverts_self_l5'] + 1
				}
			}
			# irevid = which(ResMatTrain[,'user_id'] == revuid) # uid.idx[revuid,1]
			# if (length(irevid) > 0){ 
				# ResMatTrain[irevid,'sum_reverts_self'] = ResMatTrain[irevid,'sum_reverts_self'] + 1
				# if(edit.in.trainl5){
					# ResMatTrain[irevid,'sum_reverts_self_l5'] = ResMatTrain[irevid,'sum_reverts_self_l5'] + 1
				# }
			# }
		}
		
		if (!(artid %in% artlist.train[[itrain]])){
			ResMatTrain[itrain,'sum_articles'] = ResMatTrain[itrain,'sum_articles']+1		
			if(edit.in.trainl5){
				ResMatTrain[itrain,'sum_new_articles_l5'] = ResMatTrain[itrain,'sum_new_articles_l5']+1
			}
			artlist.train[[itrain]] = c(artlist.train[[itrain]],artid)		
		}
		
		ResMatTrain[itrain,'sum_delta'] = ResMatTrain[itrain,'sum_delta'] + delta
		if(edit.in.trainl5){
			ResMatTrain[itrain,'sum_delta_l5'] = ResMatTrain[itrain,'sum_delta_l5'] + delta
		}	
	}
}

ResMatTrain[,'y_next5_edits'] = ResMatLead[which(ResMatLead[,'user_id'] %in% ResMatTrain[,'user_id']),'sum_edits_l5']

write.csv(ResMatLead, file = paste(RESDIR,"OrigSet_xinp1p2p3.csv",sep=''), row.names = FALSE);
write.csv(ResMatTrain, file = paste(RESDIR,"OrigSet_xinp1p2_yinp3.csv",sep=''), row.names = FALSE);

close(con)


#/ Column	Explanation
#/ user_id	user-id
#/ FSDT	The First timestamp that this user made an edit on
#/ LSDT	The last timestamp (before 31-Oct-2009 midnight) that this  user made an edit
#/ registration_date	The date that the user registered on
#/ sum_edits	Total number of edits that the user made (as on p1 end)
#/ sum_edit_days	Total number of days  that the user made edits on (as on p1 end)
#[ sum_edit_ns0	Total number of edits that this user made (as on p1 end) in the namespace category 0
#[ sum_edit_ns1	Total number of edits that this user made (as on p1 end) in the namespace category 1
#[ sum_edit_ns2	Total number of edits that this user made (as on p1 end) in the namespace category 2
#[ sum_edit_ns3	Total number of edits that this user made (as on p1 end) in the namespace category 3
#[ sum_edit_ns4	Total number of edits that this user made (as on p1 end) in the namespace category 4
#[ sum_edit_ns5	Total number of edits that this user made (as on p1 end) in the namespace category 5
#[ sum_reverts	Total number of reverts that the user had (as on p1 end)
#[ sum_reverts_ns0	Total number of reverts (as on p1 end)  for edits of namespace 0
#[ sum_reverts_ns1	Total number of reverts (as on p1 end)  for edits of namespace 1
#[ sum_reverts_ns2	Total number of reverts (as on p1 end)  for edits of namespace 2
#[ sum_reverts_ns3	Total number of reverts (as on p1 end)  for edits of namespace 3
#[ sum_reverts_ns4	Total number of reverts (as on p1 end)  for edits of namespace 4
#[ sum_reverts_ns5	Total number of reverts (as on p1 end)  for edits of namespace 5
#[ sum_reverts_self	Total number of reverts (as on p1 end) that the user  himself/herself made
# sum_articles	Total number of  unique articles edited by the user (as on p1 end)
# sum_delta	sum of all the deltas of edits madeby the user (as of p1 end)
#/ sum_edits_l5	Total number of edits that the user made (in the last 5 month before p1 ended)
#/ sum_edit_days_l5	Total number of days  that the user made edits on (in the last 5 months before p1 ended)
#[ sum_edit_ns0_l5	Total number of edits that this user made (in the last 5 months before p1 ended) in the namespace category 0
#[ sum_edit_ns1_l5	Total number of edits that this user made (in the last 5 months before p1 ended) in the namespace category 1
#[ sum_edit_ns2_l5	Total number of edits that this user made (in the last 5 months before p1 ended) in the namespace category 2
#[ sum_edit_ns3_l5	Total number of edits that this user made (in the last 5 months before p1 ended) in the namespace category 3
#[ sum_edit_ns4_l5	Total number of edits that this user made (in the last 5 months before p1 ended) in the namespace category 4
#[ sum_edit_ns5_l5	Total number of edits that this user made (in the last 5 months before p1 ended) in the namespace category 5
#[ sum_reverts_l5	Total number of reverts that the user had (in the last 5 months before p1 ended)
#[ sum_reverts_ns0_l5	Total number of reverts (in the last 5 months before p1 ended)  for edits of namespace 0
#[ sum_reverts_ns1_l5	Total number of reverts (in the last 5 months before p1 ended)  for edits of namespace 1
#[ sum_reverts_ns2_l5	Total number of reverts (in the last 5 months before p1 ended)  for edits of namespace 2
#[ sum_reverts_ns3_l5	Total number of reverts (in the last 5 months before p1 ended)  for edits of namespace 3
#[ sum_reverts_ns4_l5	Total number of reverts (in the last 5 months before p1 ended)  for edits of namespace 4
#[ sum_reverts_ns5_l5	Total number of reverts (in the last 5 months before p1 ended)  for edits of namespace 5
#[ sum_reverts_self_l5	Total number of reverts (in the last 5 months before p1 ended) that the user  himself/herself made
# sum_new_articles_l5	Total number of articles that the user edited for the first tiem in the last 5 months before p1 ended
# sum_delta_l5	sum of all the deltas of edits madeby the user (in the last 5 months before p1 ended)
#/ Edits_mth_cutoff_minus_4	Number of edits 4 months before the last month of p1 (i.e., Jun-09)
#/ Edits_mth_cutoff_minus_3	Number of edits 3 months before the last month of p1 (i.e., Jul-09)
#/ Edits_mth_cutoff_minus_2	Number of edits 2 months before the last month of p1 (i.e., Aug-09)
#/ Edits_mth_cutoff_minus_1	Number of edits 1 month before the last month of p1 (i.e., Sep-09)
#/ Edits_mth_cutoff_minus_0	Number of edits 0 months before the last month of p1 (i.e., in the month of Oct-09)
# y_next5_edits	Number of edits that the user made in the 5 months post p1 (Nov-09to Mar-10)
