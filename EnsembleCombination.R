#####################################################################################
### Final Model File
#####################################################################################
### Run 8 individual Models
#####################################################################################
setwd(MAIN_DIR)

source("Models/3segemnts-linear-begin-date-uniq days.R")
source("Models/not_so_simple9a.R")
source("Models/RFsub_29Aug11_2nd.R")
source("Models/Seven_Segs_Nested_v10.R")
source("Models/Simple+Interactions+more.R")
source("Models/Simple+Interactions+more-modified.R")
source("Models/Simple+Interactions+onemore.R")
source("Models/SimpleLogLinear2.R")


#########################################################################################
#### Ensemble combination using Geometric Means
#########################################################################################
#### File to combine the Predictions
fnames = list.files(pattern = ".csv");

p = length(fnames);
n = 44514;
pred = matrix(0, n,p);
for(i in 1:p){
fname = fnames[i];
data = read.csv(fname, header = TRUE);
pred[,i] = data[,2];
}


pred_final = exp(rowMeans(log(pred+1)))-1;
mat = cbind(editor_id = data[,1], solution = pred_final);
write.csv(mat, file = "Pick3.csv", row.names  =FALSE);

##############################################################################################


