// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// concat
String concat(IntegerVector my_vector);
RcppExport SEXP _nn2poly_concat(SEXP my_vectorSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< IntegerVector >::type my_vector(my_vectorSEXP);
    rcpp_result_gen = Rcpp::wrap(concat(my_vector));
    return rcpp_result_gen;
END_RCPP
}
// rm_null
List rm_null(List x);
RcppExport SEXP _nn2poly_rm_null(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< List >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(rm_null(x));
    return rcpp_result_gen;
END_RCPP
}
// select_allowed_partitions
List select_allowed_partitions(std::string coeff_label, double q_previous_layer, List all_partitions);
RcppExport SEXP _nn2poly_select_allowed_partitions(SEXP coeff_labelSEXP, SEXP q_previous_layerSEXP, SEXP all_partitionsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< std::string >::type coeff_label(coeff_labelSEXP);
    Rcpp::traits::input_parameter< double >::type q_previous_layer(q_previous_layerSEXP);
    Rcpp::traits::input_parameter< List >::type all_partitions(all_partitionsSEXP);
    rcpp_result_gen = Rcpp::wrap(select_allowed_partitions(coeff_label, q_previous_layer, all_partitions));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_nn2poly_concat", (DL_FUNC) &_nn2poly_concat, 1},
    {"_nn2poly_rm_null", (DL_FUNC) &_nn2poly_rm_null, 1},
    {"_nn2poly_select_allowed_partitions", (DL_FUNC) &_nn2poly_select_allowed_partitions, 3},
    {NULL, NULL, 0}
};

RcppExport void R_init_nn2poly(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
