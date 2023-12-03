#' @title A function that constructs matrices with missing values
#' @description A function that constructs matrices with missing values, the result will contain Nm rows with missing values and Pm columns with missing values.
#' @param X a N times P data matrix 
#' @param Nm the number of rows with missing values
#' @param Pm the number of columns with missing values
#' @return a matrix with missing values
#' @examples
#' \dontrun{
#' X<-matrix(rnorm(20*30),nrow=20,ncol=30)
#' X_miss<-DGP_miss(10,10,X)
#' X_miss
#' }
#' @export
DGP_miss<-function(Nm,Pm,X){
  N<-nrow(X);P<-ncol(X)
  row_index<-sample(1:N,Nm,replace=FALSE)
  col_index<-sample(1:P,Pm,replace=FALSE)
  X_miss<-X 
  for(rindex in row_index){ # Construct column missing  
    num<-sample(1:(Pm/4),size=1,replace=FALSE) #the num of missing columns in row rindex
    col_missindex<-sample(col_index,size=num,replace = FALSE)
    X_miss[rindex,col_missindex]<-NA
  }
  for(cindex in col_index){ # Construct row missing
    num<-sample(1:(Nm/4),size=1,replace=FALSE) #the num of missing rows in column cindex
    row_missindex<-sample(row_index,size=num,replace = FALSE)
    X_miss[row_missindex,cindex]<-NA
  }
  
  return(X_miss)   
}

#' @title A function that generate the poisson data matrices with no intercepts
#' @description A function that generate the poisson data matrices which don't have the intercept term. Details are described in the simulation section.
#' @param N the number of rows 
#' @param P the number of columns
#' @param R2 the number of factors for alphai2
#' @return a data matrix named X
#' @examples
#' \dontrun{
#' X<-DGP1(200,300,1)
#' X
#' }
#' @export
DGP1<-function(N,P,R2){
  X<-matrix(rep(0,N*P),nrow=N)
  #alpha~(U(0,1),1,alphai2)
  Alpha<-cbind(runif(N),rep(1,N),mvrnorm(N,rep(0,R2),Sigma=diag(R2))) 
  #gamma~(1,U(0,1),alphai2)
  Gamma<-cbind(rep(1,P),runif(P),mvrnorm(P,rep(0,R2),Sigma=diag(R2))) 
  Z<-exp(Alpha%*%t(Gamma))
  X<-matrix(rpois(N*P,lambda=Z),N,P)
  return(X)
}

#' @title A function that generate the poisson data matrices with intercepts 
#' @description A function that generate the poisson data matrices which have the intercept term. Details are described in the simulation section.
#' @param N the number of rows 
#' @param P the number of columns
#' @param R2 the number of factors 
#' @return a data matrix named X
#' @examples
#' \dontrun{
#' X<-DGP2(200,300,3)
#' X
#' }
#' @export
DGP2<-function(N,P,R2){
  X<-matrix(rep(0,N*P),nrow=N)
  Alpha<-matrix(rnorm(N*R2,0,0.5),nrow=N)
  Gamma<-mvrnorm(P,rep(1,R2),Sigma=diag(R2))
  mu<-matrix(0.5*runif(P),N,P,byrow = TRUE)
  Z<-exp(Alpha%*%t(Gamma)+mu)
  X<-matrix(rpois(N*P,lambda=Z),N,P)
  return(X)
}

#' @title A function that perform GFM-TW algorithm 
#' @description A function that perform GFM-TW algorithm for the poisson data. Details see the section simulation.
#' @param X_miss the data matrix with missing values 
#' @param num_factor the number of factors for alphai2
#' @param type the type of data
#' @param detail the details about the data type
#' @return a complete data matrix
#' @examples
#' \dontrun{
#' X<-DGP1(200,300,1)
#' X_miss<-DGP_miss(80,80,X)
#' X_complete<-GFM_TW(X_miss,3,'poisson','no intercept')
#' }
#' @export  
GFM_TW<-function(X_miss,num_factor,type="poission",
                 detail="no intercept"){
  N<-nrow(X_miss);P<-ncol(X_miss)
  #mark matrix,mij=1(TRUE) denotes missing
  missing <- is.na(X_miss)        
  #row index with non missing values
  goodN <- rowSums(is.na(X_miss)) == 0   
  #column index with non missing values
  goodP <- colSums(is.na(X_miss)) == 0
  X_TALL<-X_miss[,goodP]
  X_WIDE<-X_miss[goodN,]
  
  #data is the type of poisson
  if(type=="poisson"){
    if(detail=="no intercept"){
      #use TALL to estimate factors
      gfm_TALL <- gfm(list(X_TALL), 'poisson',  q=num_factor, verbose = FALSE)
      H_hat1<-gfm_TALL$hH
      #use WIDE to estimate loadings and intercept
      gfm_WIDE <- gfm(list(X_WIDE), 'poisson',  q=num_factor, verbose = FALSE)
      B_hat1<-gfm_WIDE$hB
      mu_hat1<-gfm_WIDE$hmu
      Mu_hat1<-t(matrix(rep(mu_hat1,N),ncol=N))
      
      #the first estimate, take the rounded value of the mean as an estimate
      X_hat1<-matrix(rep(0,N*P),nrow=N)
      X_hat1<-H_hat1%*%t(B_hat1)+Mu_hat1
      X_hat1<-round(exp(X_hat1))
      #impute the matrix
      X_hat_tmp<-matrix(rep(t(X_miss),1),ncol=P,byrow=TRUE)
      X_hat_tmp[missing]<-X_hat1[missing]
      
      #reestimate
      gfm_ALL<-gfm(list(X_hat_tmp), 'poisson',  q=num_factor, verbose = FALSE)
      H_hat2<-gfm_ALL$hH
      B_hat2<-gfm_ALL$hB
      mu_hat2<-gfm_ALL$hmu
      Mu_hat2<-t(matrix(rep(mu_hat2,N),ncol=N))
      
      X_hat2<-matrix(rep(0,N*P),nrow=N)
      X_hat2<-H_hat2%*%t(B_hat2)+Mu_hat2
      X_hat2<-round(exp(X_hat2))
      X_complete<-matrix(rep(t(X_miss),1),ncol=P,byrow=TRUE)
      X_complete[missing]<-X_hat2[missing]
    }
    if(detail=="intercept"){
      #use TALL to estimate factors
      gfm_TALL <- gfm(list(X_TALL), 'poisson',  q=num_factor, verbose = FALSE)
      H_hat1<-gfm_TALL$hH
      #use WIDE to estimate loadings and intercept
      gfm_WIDE <- gfm(list(X_WIDE), 'poisson',  q=num_factor, verbose = FALSE)
      B_hat1<-gfm_WIDE$hB
      mu_hat1<-gfm_WIDE$hmu
      Mu_hat1<-t(matrix(rep(mu_hat1,N),ncol=N))
      
      #the first estimate, take the rounded value of the mean as an estimate
      X_hat1<-matrix(rep(0,N*P),nrow=N)
      X_hat1<-H_hat1%*%t(B_hat1)+Mu_hat1
      X_hat1<-round(exp(X_hat1))
      X_hat_tmp<-matrix(rep(t(X_miss),1),ncol=P,byrow=TRUE)
      X_hat_tmp[missing]<-X_hat1[missing]
      
      #reestimate
      gfm_ALL<-gfm(list(X_hat_tmp), 'poisson',  q=num_factor, verbose = FALSE)
      H_hat2<-gfm_ALL$hH
      B_hat2<-gfm_ALL$hB
      mu_hat2<-gfm_ALL$hmu
      Mu_hat2<-t(matrix(rep(mu_hat2,N),ncol=N))
      
      X_hat2<-matrix(rep(0,N*P),nrow=N)
      X_hat2<-H_hat2%*%t(B_hat2)+Mu_hat2
      X_hat2<-round(exp(X_hat2))
      X_complete<-matrix(rep(t(X_miss),1),ncol=P,byrow=TRUE)
      X_complete[missing]<-X_hat2[missing]
    }
  }
  
  #return the complete matrix
  return(X_complete)
  
}

#' @import stats
#' @import graphics
#' @import bootstrap
#' @import boot
#' @import DAAG
#' @import GFM
#' @import fbi
#' @import datasets
#' @import coda
#' @import lubridate
#' @import microbenchmark
#' @importFrom Rcpp evalCpp
#' @importFrom MASS mvrnorm
#' @importFrom utils combn
#' @useDynLib finalwork
NULL

#' @title A dataset used for HW0 in vignettes.
#' @name HW0data1
#' @description This dataset is used in HW0 for plots.
#' @examples
#' \dontrun{
#' data(HW0data1)
#' attach(HW0data1)
#' xt <- ts(t(HW0data1), start = c(2000,1), frequency = 4) 
#' ts.plot(xt, main="The average income in USA", ylab="income")
#' }
NULL

#' @title A dataset used for HW0 in vignettes.
#' @name HW0data2
#' @description This dataset is used in HW0 for plots.
#' @examples
#' \dontrun{
#' data(HW0data2)
#' attach(HW0data2)
#' data_time <- data.frame(day = ymd(HW0data2[,1]), lowtemp =HW0data2[,3])
#' ggplot(data_time, aes(x=day, y=lowtemp))+
#' geom_line(color="blue") + xlab("")+
#' scale_x_date(date_breaks = "2 years")+
#' labs(title="The lowest temperature in Hefei")+
#' theme(axis.text.x=element_text(angle=30,hjust=0.8,vjust=0.8))
#' }
NULL