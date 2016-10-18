// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// gridToIndex3DCpp
IntegerVector gridToIndex3DCpp(IntegerVector array_dim, NumericMatrix voxmat);
RcppExport SEXP neuroim_gridToIndex3DCpp(SEXP array_dimSEXP, SEXP voxmatSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< IntegerVector >::type array_dim(array_dimSEXP);
    Rcpp::traits::input_parameter< NumericMatrix >::type voxmat(voxmatSEXP);
    rcpp_result_gen = Rcpp::wrap(gridToIndex3DCpp(array_dim, voxmat));
    return rcpp_result_gen;
END_RCPP
}
// indexToGridCpp
NumericMatrix indexToGridCpp(IntegerVector idx, IntegerVector array_dim);
RcppExport SEXP neuroim_indexToGridCpp(SEXP idxSEXP, SEXP array_dimSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< IntegerVector >::type idx(idxSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type array_dim(array_dimSEXP);
    rcpp_result_gen = Rcpp::wrap(indexToGridCpp(idx, array_dim));
    return rcpp_result_gen;
END_RCPP
}
