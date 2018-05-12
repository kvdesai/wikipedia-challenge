##### rmse function
rmse <- function(y, yhat){

	yhat[yhat < 0] = 0
	r = sqrt(mean((y - yhat)^2));
	return(r);
}
##### rmsle function
rmsle <- function(y, yhat){

	yhat[yhat < 0] = 0
	r = sqrt(mean((log(y+1) - log(yhat+1))^2));
	return(r);
}
#################################################################
# RMSLE based Objective function t
rmsle_objfun <- function(beta,y,x){
	
	yhat = x%*%beta;
	yhat[yhat <0] =0;
	res <- rmsle(y,yhat)
	#n = length(y);
	#attr(res, "gradient") <- -t((log(y+1) - log(yhat+1))/(yhat+1))%*%x/(n*res); 
	return(res);
}
# RMSLog_Yratio based Objective function
rmslyr_objfun <- function(beta,yr,x){
	
	yrhat = x%*%beta;
	
	res <- sqrt(mean((log(yr) - log(yrhat))^2));	
	return(res);
}
# RMSE based Objective function
rmse_objfun <- function(beta,y,x){
	
	yhat = x%*%beta;
	
	res <- sqrt(mean((y - yhat)^2));	
	return(res);
}

###################################################################
single_test <- function(dtest,params,onresids=F){
	test.x = as.matrix(dtest$X)
	if (onresids){
		
		yratio_hat <- test.x %*% params
		test.yhat <- yratio_hat*dtest$yhat_prev + yratio_hat - 1
	}
	else{
		test.yhat <- test.x %*% params
	}
  test.yhat[test.yhat < 0] = 0
	test.rmsle <- rmsle(dtest$Y, test.yhat)
	return(list(rmsle = test.rmsle,yhat=test.yhat))

}
single_fit <- function(dtrain,init.param,onresids=F){
	
	train.x = as.matrix(dtrain$X)
	if(onresids){
		optim.y = dtrain$yratio	
		# oout = nlm(rmslyr_objfun, init.param, optim.y,train.x);
		# oout = nlm(rmse_objfun, init.param, optim.y,train.x);
		
		oout =  optim(init.param, rmslyr_objfun, gr = NULL, optim.y, train.x, method = "BFGS", control=list((maxit = 1E3)));
	}
	else{
		optim.y = dtrain$Y
		# oout = nlm(rmsle_objfun, init.param, optim.y,train.x);
		
		oout =  optim(init.param, rmsle_objfun, gr = NULL, optim.y, train.x, method = "BFGS", control=list((maxit = 1E3)));
	}	
	
	# fminval = oout$minimum
	# params = oout$estimate
	fminval = oout$value
	params = oout$par
	
	if (onresids){
		yratio_hat <- train.x %*% params
		train.yhat <- yratio_hat*dtrain$yhat_prev + yratio_hat - 1
		
	}
	else{
		train.yhat <- train.x %*% params
	}
	train.rmsle <- rmsle(dtrain$Y, train.yhat)
	
	res = list(params = params, fminval = fminval, train.rmsle = train.rmsle)
	return(res)
}
mycortest <- function(X,Y){
	randind = sample(1:length(Y),length(Y))
	pY = Y[randind]
	
	par(mfrow=c(2,1))
	
	plot(X,Y,main=round(cor(X,Y),2));
	abline(h=0,lwd=2,col='red')
	#mod = lm(formula = Y[X>300] ~ X[X>300])
	#abline(a=mod$coef[1],b=mod$coef[2],col='green',lwd=2)
	
	plot(X,pY,main=round(cor(X,pY),2))
	abline(h=0,lwd=2,col='red')
	
}

###################################################################
nfold_jknife <- function(dall,Nf,test_frac,init.param,onresids=F,fullres = F){
	N = length(dall$Y)
	Ntest = floor(N*test_frac)
	set.seed(59);
	
	params = matrix(0,Nf,length(init.param))	
	fminval = rep(0,Nf)
	train.rmsle = rep(0,Nf)
	test.rmsle = rep(0,Nf)
  if(fullres==T){
    test.inds = matrix(0,Ntest,Nf)
    test.pred = matrix(0,Ntest,Nf)
  }
	for (i in 1:Nf){
		test_inds = sample(1:N, Ntest);	
		test.X = dall$X[test_inds,]
		test.Y = dall$Y[test_inds]
		train.X = dall$X[-test_inds,]
		train.Y = dall$Y[-test_inds]
		
		if(onresids){
			test.yhat_prev = dall$yhat_prev[test_inds]
			test.yratio = dall$yratio[test_inds]
			train.yhat_prev = dall$yhat_prev[-test_inds]
			train.yratio = dall$yratio[-test_inds]
			
			dtrain = list(X = train.X, Y = train.Y, yhat_prev = train.yhat_prev, yratio = train.yratio)
			dtest = list(X = test.X, Y = test.Y, yhat_prev = test.yhat_prev, yratio = test.yratio)
		}else{
			dtrain = list(X = train.X, Y = train.Y)
			dtest = list(X = test.X, Y = test.Y)
		}
		r <- single_fit(dtrain,init.param,onresids)
		
		# Store the results
		params[i,] <- r$params
		fminval[i] <- r$fminval
		train.rmsle[i] <- r$train.rmsle
    rtest = single_test(dtest,r$params,onresids)
		test.rmsle[i] <- rtest$rmsle
    if(fullres==T){
      test.pred[,i] = rtest$yhat
      test.inds[,i] = test_inds
    }
		cat(sprintf('JK-iter %i, train rmsle %.6f, test rmsle %.6f\n',i,train.rmsle[i],test.rmsle[i]))
		flush.console()
	}
  if(fullres==T){
    res = list(params=params,fminvals = fminval, train.rmsle = train.rmsle, test.rmsle = test.rmsle, test.inds = test.inds, test.pred = test.pred)
  }else{
	  res = list(params=params,fminvals = fminval, train.rmsle = train.rmsle, test.rmsle = test.rmsle)
  }
  cat(sprintf('AggRes:: median(train rmsle) %.6f, median(test rmsle) %.6f\n',median(train.rmsle),median(test.rmsle)))
  flush.console()
	return(res)
}


rmsle_fun <- function(beta,y,x){
n = length(y);
yhat = x%*%beta;
yhat[yhat <0] =0;
res <- sqrt(mean((log(y+1) - log(yhat+1))^2));
#attr(res, "gradient") <- -t((log(y+1) - log(yhat+1))/(yhat+1))%*%x/(n*res); 
return(res);
}

###################################################################

FitValidatePredictAll <- function(y.train, x.train, x.lead, model1, model2, model3, beta1, beta2, beta3){

n = length(y.train);
loss.train = rep(0,4); ### seg1, seg2, seg3, overall
loss.test = rep(0,4);  ### seg1, seg2, seg3, overall
### do a training-test split (80-20)
### fit on training, validate on test and return joint fit coefficients
set.seed(39);
samp = sample(1:n, floor(0.8*n));
x.tr = x.train[samp,];
y.tr = y.train[samp];
x.te = x.train[-samp,];
y.te = y.train[-samp];

#####################################################################
### Fit on the training -seg1
#####################################################################
x1 = as.matrix(cbind(1, x.tr[x.tr$seg==1, model1]));
y1 = y.tr[x.tr$seg ==1];
p1 = length(model1)+1;
mod1 =  optim(beta1, rmsle_fun, gr = NULL, y1, x1 , method = "BFGS");

### Predict on test -seg1
x1.te = as.matrix(cbind(1, x.te[x.te$seg==1, model1]));
y1.te = y.te[x.te$seg ==1];
pred1.te = x1.te%*%mod1$par;
pred1.te[pred1.te <0] =0;

### compute loss
#loss.train[1] = mod1$minimum;
loss.test[1] = rmsle(y1.te, pred1.te);

#################################################################33
### Fit on the training -seg2
###################################################################
x2 = as.matrix(cbind(1, x.tr[x.tr$seg==2, model2]));
y2 = y.tr[x.tr$seg ==2];
p2 = length(model2)+1;
mod2 =  optim(beta2, rmsle_fun, gr = NULL, y2, x2, method = "BFGS");

### Predict on test -seg2
x2.te = as.matrix(cbind(1, x.te[x.te$seg==2, model2]));
y2.te = y.te[x.te$seg ==2];
pred2.te = x2.te%*%mod2$par;
pred2.te[pred2.te <0] =0;

### compute loss
#loss.train[2] = mod2$minimum;
loss.test[2] = rmsle(y2.te, pred2.te);

#################################################################33
### Fit on the training -seg3
###################################################################
x3 = as.matrix(cbind(1, x.tr[x.tr$seg==3, model3]));
y3 = y.tr[x.tr$seg ==3];
p3 = length(model3)+1;
mod3 =  optim(beta3, rmsle_fun, gr = NULL, y3, x3, method = "BFGS");

### Predict on test -seg3
x3.te = as.matrix(cbind(1, x.te[x.te$seg ==3, model3]));
y3.te = y.te[x.te$seg ==3];
pred3.te = x3.te%*%mod3$par;
pred3.te[pred3.te <0] =0;

### compute loss
#loss.train[3] = mod3$minimum;
loss.test[3] = rmsle(y3.te, pred3.te);

#######################################################################
### Compute Overall loss
#######################################################################
fit = rep(0,nrow(x.tr));
fit[x.tr$seg ==1] = x1%*%mod1$par;
fit[x.tr$seg ==2] = x2%*%mod2$par;
fit[x.tr$seg ==3] = x3%*%mod3$par;
fit[fit <0] =0;

pred.te = rep(0,nrow(x.te));
pred.te[x.te$seg ==1] = x1.te%*%mod1$par;
pred.te[x.te$seg ==2] = x2.te%*%mod2$par;
pred.te[x.te$seg ==3] = x3.te%*%mod3$par;
pred.te[pred.te <0] =0;

loss.train[1] = rmsle(y.tr[x.tr$seg ==1], fit[x.tr$seg ==1]);
loss.train[2] = rmsle(y.tr[x.tr$seg ==2], fit[x.tr$seg ==2]);
loss.train[3] = rmsle(y.tr[x.tr$seg ==3], fit[x.tr$seg ==3]);
loss.train[4] = rmsle(y.tr, fit);
loss.test[4] = rmsle(y.te, pred.te);

##########################################################################
### Generate prediction for the leaderboard
##########################################################################
x1.lead = as.matrix(cbind(1, x.lead[x.lead$seg==1, model1]));
x2.lead = as.matrix(cbind(1, x.lead[x.lead$seg==2, model2]));
x3.lead = as.matrix(cbind(1, x.lead[x.lead$seg==3, model3]));

pred.lead = rep(0, nrow(x.lead));
pred.lead[x.lead$seg ==1] = x1.lead%*%mod1$par;
pred.lead[x.lead$seg ==2] = x2.lead%*%mod2$par;
pred.lead[x.lead$seg ==3] = x3.lead%*%mod3$par;
pred.lead[pred.lead <0] =0;


r = list(loss.train = loss.train, loss.test = loss.test, seg1.beta = mod1$par, seg2.beta = mod2$par,
seg3.beta = mod3$par, pred.lead = pred.lead);
return(r);
}
