rm(list = setdiff(ls(),c('DATADIR','RESDIR','MAIN_DIR')))

# Run eid_time_table_unrounded.py to create editor-vs-edit timestamp file before executing this script

# Raw Input Files:
# 	'wikichallenge_example_entry.csv'
#	'regdates.tsv'
# Derived Input files:
#	'edit_times_unrounded.csv'


NE = 44514
NT = 22126031
# DATADIR = 'E:\\public\\ICDM11\\wikichallenge_data_all\\'
# RESDIR = 'D:\\ICDM11\\Honourable Mention\\AllFeatureSets\\'
t0 = strptime("2001-01-01 0:0:0", "%Y-%m-%d %H:%M:%S"); 

# First create EID | FSDT | RegDate Table
X = read.csv(paste(DATADIR,'wikichallenge_example_entry.csv',sep=''),header=T)
RD <- read.csv(paste(DATADIR,'regdates.tsv',sep=''),header=T,sep='\t')
ET = scan(paste(RESDIR,'edit_times_unrounded.csv',sep=''),what='character');
stopifnot(length(ET) == NE, all(dim(X) == (c(NE,2))))

EFR = array(0,c(NE,3))

for (i in 1:NE){
	ix <- which(RD[,1] == X[i,1])
	EFR[i,1] = X[i,1]
	EFR[i,2] <- min(as.numeric(strsplit(ET[i],',',fixed=T)[[1]]))
	EFR[i,3] <- as.numeric(strptime(RD[ix,2],"%Y-%m-%d") - t0)
}

colnames(EFR) = c('user_id','FSDTnum','RegDateNum')
write.csv(EFR,file=paste(RESDIR,'eid_fsdt_regdate_table.csv',sep=''),row.names=F)