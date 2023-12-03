#include <Rcpp.h>
using namespace Rcpp;

//' @title A Rcpp function to calculate RMSE
//' @description A Rcpp function to calculate RMSE
//' @param Xhat the estimated matrix
//' @param X the origin data matrix
//' @param missnum the number of missing values in X_miss
//' @return the value of RMSE. Details see Section Simulation.
//' @examples
//' \dontrun{
//' X1<-matrix(rnorm(20*30),nrow=20,ncol=30)
//' X1hat<-matrix(rnorm(20*30),nrow=20,ncol=30)
//' missnum1<-10*10
//' rmse<-RMSEC(X1hat,X1,missnum1)
//' }
//' @export
// [[Rcpp::export]]
double RMSEC(const NumericMatrix& Xhat, 
             const NumericMatrix& X,int missnum) {
  double sum = 0;
  int nrow = X.nrow();
  int ncol = X.ncol();
  double rmse=0;
  double diff=0;
  for (int i = 0; i < nrow; i++) {
    for (int j = 0; j < ncol; j++) {
      diff = X(i, j) - Xhat(i, j);
      sum += diff * diff;
    }
  }
  rmse=std::sqrt(sum / missnum);
  return rmse;
}




