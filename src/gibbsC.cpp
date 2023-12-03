#include <Rcpp.h>
using namespace Rcpp;

//' @title A Gibbs sampler using Rcpp
//' @description A Gibbs sampler using Rcpp
//' @param a the first parameter in rbeta(a,b)
//' @param b the second parameter in rbeta(a,b)
//' @param n the parameter in rbinom(1,n,p)
//' @param initial the initial values of chain
//' @param N the number of samples
//' @return a random sample 
//' @examples
//' \dontrun{
//' N_0 <- 10000;burn <- 1000
//' a_0<-3;b_0<-4;n_0<-20
//' init<-c(sample(0:n_0,1),runif(1))
//' matC <- gibbsC(a_0,b_0,n_0,init,N_0)
//' }
//' @export
// [[Rcpp::export]]
NumericMatrix gibbsC(double a,double b,
                     int n,NumericVector initial, int N) {
  NumericMatrix mat(N, 2);
  double x,y;
  mat(0, 0) = initial(0);
  mat(0, 1) = initial(1);
  for(int i = 1; i < N; i++) {
    y = mat(i - 1, 1);
    mat(i, 0) = rbinom(1,n,y)[0];
    x = mat(i, 0);
    mat(i, 1) = rbeta(1,(x+a),(n-x+b))[0];
  }
  return(mat);
}