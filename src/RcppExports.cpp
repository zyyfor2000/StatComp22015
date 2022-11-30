// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// gibbsC
NumericMatrix gibbsC(int N, int burn, int thin, double mu1, double mu2, double sigma1, double sigma2, double rho);
RcppExport SEXP _StatComp22015_gibbsC(SEXP NSEXP, SEXP burnSEXP, SEXP thinSEXP, SEXP mu1SEXP, SEXP mu2SEXP, SEXP sigma1SEXP, SEXP sigma2SEXP, SEXP rhoSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< int >::type N(NSEXP);
    Rcpp::traits::input_parameter< int >::type burn(burnSEXP);
    Rcpp::traits::input_parameter< int >::type thin(thinSEXP);
    Rcpp::traits::input_parameter< double >::type mu1(mu1SEXP);
    Rcpp::traits::input_parameter< double >::type mu2(mu2SEXP);
    Rcpp::traits::input_parameter< double >::type sigma1(sigma1SEXP);
    Rcpp::traits::input_parameter< double >::type sigma2(sigma2SEXP);
    Rcpp::traits::input_parameter< double >::type rho(rhoSEXP);
    rcpp_result_gen = Rcpp::wrap(gibbsC(N, burn, thin, mu1, mu2, sigma1, sigma2, rho));
    return rcpp_result_gen;
END_RCPP
}
// rwMetropolisC
NumericVector rwMetropolisC(int n, double sigma, double x0, int N, int burn);
RcppExport SEXP _StatComp22015_rwMetropolisC(SEXP nSEXP, SEXP sigmaSEXP, SEXP x0SEXP, SEXP NSEXP, SEXP burnSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< int >::type n(nSEXP);
    Rcpp::traits::input_parameter< double >::type sigma(sigmaSEXP);
    Rcpp::traits::input_parameter< double >::type x0(x0SEXP);
    Rcpp::traits::input_parameter< int >::type N(NSEXP);
    Rcpp::traits::input_parameter< int >::type burn(burnSEXP);
    rcpp_result_gen = Rcpp::wrap(rwMetropolisC(n, sigma, x0, N, burn));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_StatComp22015_gibbsC", (DL_FUNC) &_StatComp22015_gibbsC, 8},
    {"_StatComp22015_rwMetropolisC", (DL_FUNC) &_StatComp22015_rwMetropolisC, 5},
    {NULL, NULL, 0}
};

RcppExport void R_init_StatComp22015(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
