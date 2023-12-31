// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// RMSEC
double RMSEC(const NumericMatrix& Xhat, const NumericMatrix& X, int missnum);
RcppExport SEXP _finalwork_RMSEC(SEXP XhatSEXP, SEXP XSEXP, SEXP missnumSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const NumericMatrix& >::type Xhat(XhatSEXP);
    Rcpp::traits::input_parameter< const NumericMatrix& >::type X(XSEXP);
    Rcpp::traits::input_parameter< int >::type missnum(missnumSEXP);
    rcpp_result_gen = Rcpp::wrap(RMSEC(Xhat, X, missnum));
    return rcpp_result_gen;
END_RCPP
}
// gibbsC
NumericMatrix gibbsC(double a, double b, int n, NumericVector initial, int N);
RcppExport SEXP _finalwork_gibbsC(SEXP aSEXP, SEXP bSEXP, SEXP nSEXP, SEXP initialSEXP, SEXP NSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< double >::type a(aSEXP);
    Rcpp::traits::input_parameter< double >::type b(bSEXP);
    Rcpp::traits::input_parameter< int >::type n(nSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type initial(initialSEXP);
    Rcpp::traits::input_parameter< int >::type N(NSEXP);
    rcpp_result_gen = Rcpp::wrap(gibbsC(a, b, n, initial, N));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_finalwork_RMSEC", (DL_FUNC) &_finalwork_RMSEC, 3},
    {"_finalwork_gibbsC", (DL_FUNC) &_finalwork_gibbsC, 5},
    {NULL, NULL, 0}
};

RcppExport void R_init_finalwork(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
