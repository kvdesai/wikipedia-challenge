#source('D:/ICDM11/mygit/Kalpit/editors_mnthly_edits.r')
rm(list = setdiff(ls(),c('DATADIR','RESDIR','MAIN_DIR')))

NE = 44514
t0 = strptime("2001-01-01 0:0:0", "%Y-%m-%d %H:%M:%S"); 
te = strptime("2010-09-01 0:0:0", "%Y-%m-%d %H:%M:%S"); 
NM = 116
Mlen = as.numeric((te-t0)/NM)

A <- scan(paste(RESDIR,'edit_times_unrounded.csv',sep=''),'character');
stopifnot(length(A) == NE)

MatAll <- matrix(0,NE,NM+1)
MatUnq <- matrix(0,NE,NM+1)

fsdt = read.csv(paste(RESDIR,'eid_fsdt_regdate_table.csv',sep=''), header = TRUE);

for (ia in 1:NE){

	if (ia%%2000 == 0) {cat(sprintf("Parsed %i of %i editors",ia,NE),"\n")}

	MatAll[ia,1] = fsdt[ia,1]
	MatUnq[ia,1] = fsdt[ia,1]
	
	splita <- as.numeric(strsplit(A[ia],",",fixed=T)[[1]])
	for (m in 1:NM){
		winst = (m-1)*Mlen
		winen = winst + Mlen
		iwithin = (splita > winst & splita <= winen)
		MatAll[ia,m+1] = sum(iwithin)
		if (MatAll[ia,m+1] > 0){
			MatUnq[ia,m+1] = sum(diff(sort(splita[iwithin])) > 0.5) + 1
		}
	}
}
cat(sprintf("Writing tables into CSV files..."),"\n")
colnames(MatAll) = c("user_id",seq(1,NM,1));
colnames(MatUnq) = c("user_id",seq(1,NM,1));

# write(t(MatAll),paste(RESDIR,'editor_monthly_edits_unrounded.csv',sep=''),ncolumns = dim(MatAll)[2],sep = ",")
# write(t(MatUnq),paste(RESDIR,'editor_monthly_edits_unique_sessions_unrounded.csv',sep=''),ncolumns = dim(MatUnq)[2],sep = ",")
write.csv(MatAll, file=paste(RESDIR,'editor_monthly_edits_unrounded.csv',sep=''),row.names=F)
write.csv(MatUnq, file=paste(RESDIR,'editor_monthly_edits_unique_sessions_unrounded.csv',sep=''),row.names=F)

alarm()
