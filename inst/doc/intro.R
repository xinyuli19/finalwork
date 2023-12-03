## ----eval=FALSE---------------------------------------------------------------
#  DGP1<-function(N,P,R2){
#    X<-matrix(rep(0,N*P),nrow=N)
#    #alpha~(U(0,1),1,alphai2)
#    Alpha<-cbind(runif(N),rep(1,N),mvrnorm(N,rep(0,R2),Sigma=diag(R2)))
#    #gamma~(1,U(0,1),alphai2)
#    Gamma<-cbind(rep(1,P),runif(P),mvrnorm(P,rep(0,R2),Sigma=diag(R2)))
#    Z<-exp(Alpha%*%t(Gamma))
#    X<-matrix(rpois(N*P,lambda=Z),N,P)
#    return(X)
#  }

## ----eval=FALSE---------------------------------------------------------------
#  DGP2<-function(N,P,R2){
#    X<-matrix(rep(0,N*P),nrow=N)
#    Alpha<-matrix(rnorm(N*R2,0,0.5),nrow=N)
#    Gamma<-mvrnorm(P,rep(1,R2),Sigma=diag(R2))
#    mu<-matrix(0.5*runif(P),N,P,byrow = TRUE)
#    Z<-exp(Alpha%*%t(Gamma)+mu)
#    X<-matrix(rpois(N*P,lambda=Z),N,P)
#    return(X)
#  }

## ----eval=FALSE---------------------------------------------------------------
#  DGP_miss<-function(Nm,Pm,X){
#    N<-nrow(X);P<-ncol(X)
#    row_index<-sample(1:N,Nm,replace=FALSE)
#    col_index<-sample(1:P,Pm,replace=FALSE)
#    X_miss<-X
#    for(rindex in row_index){ # Construct column missing
#      num<-sample(1:(Pm/4),size=1,replace=FALSE) #the num of missing columns in row rindex
#      col_missindex<-sample(col_index,size=num,replace = FALSE)
#      X_miss[rindex,col_missindex]<-NA
#    }
#    for(cindex in col_index){ # Construct row missing
#      num<-sample(1:(Nm/4),size=1,replace=FALSE) #the num of missing rows in column cindex
#      row_missindex<-sample(row_index,size=num,replace = FALSE)
#      X_miss[row_missindex,cindex]<-NA
#    }
#  
#    return(X_miss)
#  }

## ----eval=FALSE---------------------------------------------------------------
#  GFM_TW<-function(X_miss,num_factor,type="poission",
#                   detail="no intercept"){
#    N<-nrow(X_miss);P<-ncol(X_miss)
#    #mark matrix,mij=1(TRUE) denotes missing
#    missing <- is.na(X_miss)
#    #row index with non missing values
#    goodN <- rowSums(is.na(X_miss)) == 0
#    #column index with non missing values
#    goodP <- colSums(is.na(X_miss)) == 0
#    X_TALL<-X_miss[,goodP]
#    X_WIDE<-X_miss[goodN,]
#  
#    #data is the type of poisson
#    if(type=="poisson"){
#      if(detail=="no intercept"){
#        #use TALL to estimate factors
#        gfm_TALL <- gfm(list(X_TALL), 'poisson',  q=num_factor, verbose = FALSE)
#        H_hat1<-gfm_TALL$hH
#        #use WIDE to estimate loadings and intercept
#        gfm_WIDE <- gfm(list(X_WIDE), 'poisson',  q=num_factor, verbose = FALSE)
#        B_hat1<-gfm_WIDE$hB
#        mu_hat1<-gfm_WIDE$hmu
#        Mu_hat1<-t(matrix(rep(mu_hat1,N),ncol=N))
#  
#        #the first estimate, take the rounded value of the mean as an estimate
#        X_hat1<-matrix(rep(0,N*P),nrow=N)
#        X_hat1<-H_hat1%*%t(B_hat1)+Mu_hat1
#        X_hat1<-round(exp(X_hat1))
#        #impute the matrix
#        X_hat_tmp<-matrix(rep(t(X_miss),1),ncol=P,byrow=TRUE)
#        X_hat_tmp[missing]<-X_hat1[missing]
#  
#        #reestimate
#        gfm_ALL<-gfm(list(X_hat_tmp), 'poisson',  q=num_factor, verbose = FALSE)
#        H_hat2<-gfm_ALL$hH
#        B_hat2<-gfm_ALL$hB
#        mu_hat2<-gfm_ALL$hmu
#        Mu_hat2<-t(matrix(rep(mu_hat2,N),ncol=N))
#  
#        X_hat2<-matrix(rep(0,N*P),nrow=N)
#        X_hat2<-H_hat2%*%t(B_hat2)+Mu_hat2
#        X_hat2<-round(exp(X_hat2))
#        X_complete<-matrix(rep(t(X_miss),1),ncol=P,byrow=TRUE)
#        X_complete[missing]<-X_hat2[missing]
#      }
#      if(detail=="intercept"){
#        #use TALL to estimate factors
#        gfm_TALL <- gfm(list(X_TALL), 'poisson',  q=num_factor, verbose = FALSE)
#        H_hat1<-gfm_TALL$hH
#        #use WIDE to estimate loadings and intercept
#        gfm_WIDE <- gfm(list(X_WIDE), 'poisson',  q=num_factor, verbose = FALSE)
#        B_hat1<-gfm_WIDE$hB
#        mu_hat1<-gfm_WIDE$hmu
#        Mu_hat1<-t(matrix(rep(mu_hat1,N),ncol=N))
#  
#        #the first estimate, take the rounded value of the mean as an estimate
#        X_hat1<-matrix(rep(0,N*P),nrow=N)
#        X_hat1<-H_hat1%*%t(B_hat1)+Mu_hat1
#        X_hat1<-round(exp(X_hat1))
#        X_hat_tmp<-matrix(rep(t(X_miss),1),ncol=P,byrow=TRUE)
#        X_hat_tmp[missing]<-X_hat1[missing]
#  
#        #reestimate
#        gfm_ALL<-gfm(list(X_hat_tmp), 'poisson',  q=num_factor, verbose = FALSE)
#        H_hat2<-gfm_ALL$hH
#        B_hat2<-gfm_ALL$hB
#        mu_hat2<-gfm_ALL$hmu
#        Mu_hat2<-t(matrix(rep(mu_hat2,N),ncol=N))
#  
#        X_hat2<-matrix(rep(0,N*P),nrow=N)
#        X_hat2<-H_hat2%*%t(B_hat2)+Mu_hat2
#        X_hat2<-round(exp(X_hat2))
#        X_complete<-matrix(rep(t(X_miss),1),ncol=P,byrow=TRUE)
#        X_complete[missing]<-X_hat2[missing]
#      }
#    }
#  
#    #return the complete matrix
#    return(X_complete)
#  }

## ----eval=FALSE---------------------------------------------------------------
#  double RMSEC(const NumericMatrix& Xhat,
#               const NumericMatrix& X,int missnum) {
#    double sum = 0;
#    int nrow = X.nrow();
#    int ncol = X.ncol();
#    double rmse=0;
#    double diff=0;
#    for (int i = 0; i < nrow; i++) {
#      for (int j = 0; j < ncol; j++) {
#        diff = X(i, j) - Xhat(i, j);
#        sum += diff * diff;
#      }
#    }
#    rmse=std::sqrt(sum / missnum);
#    return rmse;
#  }

## ----eval=TRUE----------------------------------------------------------------
library(finalwork)
library(fbi)
compare_method<-function(N,P,Nm,Pm,itermax,R2,types='poisson',details="no intercept"){
  #generate a list to reserve the result of each iter
  #list[[iter]] contains X_true,missnum,X_complete for three methods
  result<-list()
  for(iter in 1:itermax){
    if(types=="poisson"){
      if(details=="no intercept"){
        #generate data matrix and artificially
        #creating missing values
        X_true<-DGP1(N,P,R2)
        X_true_miss<-DGP_miss(Nm,Pm,X_true)
        missnum<-sum(is.na(X_true_miss))
        ##GFM-TW
        X_complete_GFMTW<-GFM_TW(X_true_miss,R2+2,types,details)
        ##TW-sd
        X_complete_TWsd<-tw_apc(X_true_miss,R2+2,center=TRUE,standardize = TRUE,re_estimate = TRUE)$data
        X_complete_TWsd<-round(X_complete_TWsd)
        ##TW-raw
        X_complete_TWraw<-tw_apc(X_true_miss,R2+2,center=FALSE,standardize = FALSE,re_estimate = TRUE)$data
        X_complete_TWraw<-round(X_complete_TWraw)
      }
      if(details=="intercept"){
        X_true<-DGP2(N,P,R2)
        X_true_miss<-DGP_miss(Nm,Pm,X_true)
        missnum<-sum(is.na(X_true_miss))
        ##GFM-TW
        X_complete_GFMTW<-GFM_TW(X_true_miss,R2,types,details)
        ##TW-sd
        X_complete_TWsd<-tw_apc(X_true_miss,R2,center=TRUE,standardize = TRUE,re_estimate = TRUE)$data
        X_complete_TWsd<-round(X_complete_TWsd)
        ##TW-raw
        X_complete_TWraw<-tw_apc(X_true_miss,R2,center=FALSE,standardize = FALSE,re_estimate = TRUE)$data
        X_complete_TWraw<-round(X_complete_TWraw)
      }
    }
    #reserve results
    result[[iter]]<-list(X_true=X_true,missnum=missnum,
                         X_complete_GFMTW=X_complete_GFMTW,
                         X_complete_TWsd=X_complete_TWsd,
                       X_complete_TWraw=X_complete_TWraw)
  }
  return(result)
}

## ----eval=TRUE----------------------------------------------------------------
library(finalwork)
report_RMSE<-function(res,iternum){
  RMSE_result<-matrix(0,nrow=iternum,ncol=3)
  for(iter in 1:iternum){
  RMSE_result[iter,1]<-RMSEC(res[[iter]]$X_complete_GFMTW,res[[iter]]$X_true,res[[iter]]$missnum)
  RMSE_result[iter,2]<-RMSEC(res[[iter]]$X_complete_TWsd,res[[iter]]$X_true,res[[iter]]$missnum)
  RMSE_result[iter,3]<-RMSEC(res[[iter]]$X_complete_TWraw,res[[iter]]$X_true,res[[iter]]$missnum)
  }
  return(RMSE_result)
}

## ----eval=TRUE----------------------------------------------------------------
#setting
library(finalwork)
library(fbi)
iter1=10
N1=200;P1=300;R1=1;Nm1<-80;Pm1<-80
#simulation
result1<-compare_method(N1,P1,Nm1,Pm1,iter1,R1,'poisson','no intercept')

#report
report_result<-report_RMSE(result1,iter1)
report_result

## ----eval=TRUE----------------------------------------------------------------
#setting
iter2=10
N2=200;P2=300;R=3;Nm2<-80;Pm2<-80
#simulation
result1<-compare_method(N2,P2,Nm2,Pm2,iter2,R,'poisson','intercept')
#report
report_result<-report_RMSE(result1,iter2)
report_result

