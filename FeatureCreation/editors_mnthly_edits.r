rm(list = setdiff(ls(),c('DATADIR','RESDIR','MAIN_DIR')))

NE = 44514
t0 = strptime("2001-01-01 0:0:0", "%Y-%m-%d %H:%M:%S"); 
te = strptime("2010-09-01 0:0:0", "%Y-%m-%d %H:%M:%S"); 
NM = 116
Mlen = (te-t0)/NM

A <- scan(paste(RESDIR,'edit_times.csv',sep=''),'character');
stopifnot(length(A) == NE)

MatAll <- array(0,c(NE,NM+1))
MatUnq <- array(0,c(NE,NM+1))

fsdt = read.csv(paste(RESDIR,'eid_fsdt_regdate_table.csv',sep=''), header = TRUE);

for (ia in 1:NE){

	if (ia%%2000 == 0) {cat(sprintf("Parsed %i of %i editors",ia,NE),"\n")}
	MatAll[ia,1] = fsdt[ia,1]
	MatUnq[ia,1] = fsdt[ia,1]

	splita <- as.integer(strsplit(A[ia],",",fixed=T)[[1]])
	for (m in 1:NM){
		winst = (m-1)*Mlen
		winen = winst + Mlen
		iwithin = (splita > winst & splita <= winen)
		MatAll[ia,m+1] = sum(iwithin)
		MatUnq[ia,m+1] = length(unique(splita[iwithin]))
	}
}
cat(sprintf("Writing tables into CSV files..."),"\n")
colnames(MatAll) = c("user_id",seq(1,NM,1));
colnames(MatUnq) = c("user_id",seq(1,NM,1));

# write(t(MatAll),paste(RESDIR,'editor_monthly_edits.csv',sep=''),ncolumns = dim(MatAll)[2],sep = ",")
# write(t(MatUnq),paste(RESDIR,'editor_monthly_edits_unique_days.csv',sep=''),ncolumns = dim(MatUnq)[2],sep = ",")
write.csv(MatAll, file=paste(RESDIR,'editor_monthly_edits.csv',sep=''),row.names=F)
write.csv(MatUnq, file=paste(RESDIR,'editor_monthly_edits_unique_days.csv',sep=''),row.names=F)
alarm()

RD <- read.csv(paste(DATADIR,'regdates.tsv',sep=''),header=T,sep='\t')
EL <- scan(paste(RESDIR,'editor_list.txt',sep=''),what='integer')
EL = as.integer(EL)
ELT = array(0,c(length(EL),2))
t0 = strptime("2001-01-01 0:0:0", "%Y-%m-%d %H:%M:%S"); 
ELT[,1] = EL
for (i in 1:length(EL)){
	ix <- which(RD[,1] == EL[i])
	ELT[i,2] <- strptime(RD[ix,2],"%Y-%m-%d") - t0
}
ibad = which(is.na(ELT[,2]))
for (ib in ibad){
	ELT[ib,2] <- min(as.integer(strsplit(A[ib],',',fixed=T)[[1]]))
}
#write(t(ELT),file=paste(RESDIR,'editor_regdate.csv',sep=''),sep=',',ncolumns=2)
colnames(ELT) = c("user_id","reg_date")
write.csv(ELT, file=paste(RESDIR,'editor_regdate.csv',sep=''),row.names=F)


