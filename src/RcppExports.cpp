// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <RcppArmadillo.h>
#include <Rcpp.h>

using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// select_allowed_partitions
std::vector<ListOf<IntegerVector>> select_allowed_partitions(IntegerVector equivalent_label, int q_previous_layer, ListOf<IntegerVector> labels, List partitions);
RcppExport SEXP _nn2poly_select_allowed_partitions(SEXP equivalent_labelSEXP, SEXP q_previous_layerSEXP, SEXP labelsSEXP, SEXP partitionsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< IntegerVector >::type equivalent_label(equivalent_labelSEXP);
    Rcpp::traits::input_parameter< int >::type q_previous_layer(q_previous_layerSEXP);
    Rcpp::traits::input_parameter< ListOf<IntegerVector> >::type labels(labelsSEXP);
    Rcpp::traits::input_parameter< List >::type partitions(partitionsSEXP);
    rcpp_result_gen = Rcpp::wrap(select_allowed_partitions(equivalent_label, q_previous_layer, labels, partitions));
    return rcpp_result_gen;
END_RCPP
}
// alg_non_linear
arma::mat alg_non_linear(arma::mat coeffs_input, ListOf<IntegerVector> labels_input, ListOf<IntegerVector> labels_output, IntegerVector q_taylor_vector, int current_layer, arma::vec g, ListOf<IntegerVector> partitions_labels, List partitions);
RcppExport SEXP _nn2poly_alg_non_linear(SEXP coeffs_inputSEXP, SEXP labels_inputSEXP, SEXP labels_outputSEXP, SEXP q_taylor_vectorSEXP, SEXP current_layerSEXP, SEXP gSEXP, SEXP partitions_labelsSEXP, SEXP partitionsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::mat >::type coeffs_input(coeffs_inputSEXP);
    Rcpp::traits::input_parameter< ListOf<IntegerVector> >::type labels_input(labels_inputSEXP);
    Rcpp::traits::input_parameter< ListOf<IntegerVector> >::type labels_output(labels_outputSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type q_taylor_vector(q_taylor_vectorSEXP);
    Rcpp::traits::input_parameter< int >::type current_layer(current_layerSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type g(gSEXP);
    Rcpp::traits::input_parameter< ListOf<IntegerVector> >::type partitions_labels(partitions_labelsSEXP);
    Rcpp::traits::input_parameter< List >::type partitions(partitionsSEXP);
    rcpp_result_gen = Rcpp::wrap(alg_non_linear(coeffs_input, labels_input, labels_output, q_taylor_vector, current_layer, g, partitions_labels, partitions));
    return rcpp_result_gen;
END_RCPP
}
// combinations_with_repetition
IntegerMatrix combinations_with_repetition(int n, int k);
RcppExport SEXP _nn2poly_combinations_with_repetition(SEXP nSEXP, SEXP kSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< int >::type n(nSEXP);
    Rcpp::traits::input_parameter< int >::type k(kSEXP);
    rcpp_result_gen = Rcpp::wrap(combinations_with_repetition(n, k));
    return rcpp_result_gen;
END_RCPP
}
// generate_partitions
List generate_partitions(int p, int q_max);
RcppExport SEXP _nn2poly_generate_partitions(SEXP pSEXP, SEXP q_maxSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< int >::type p(pSEXP);
    Rcpp::traits::input_parameter< int >::type q_max(q_maxSEXP);
    rcpp_result_gen = Rcpp::wrap(generate_partitions(p, q_max));
    return rcpp_result_gen;
END_RCPP
}
// generate_partitions_full
List generate_partitions_full(int p, int q_max);
RcppExport SEXP _nn2poly_generate_partitions_full(SEXP pSEXP, SEXP q_maxSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< int >::type p(pSEXP);
    Rcpp::traits::input_parameter< int >::type q_max(q_maxSEXP);
    rcpp_result_gen = Rcpp::wrap(generate_partitions_full(p, q_max));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_nn2poly_select_allowed_partitions", (DL_FUNC) &_nn2poly_select_allowed_partitions, 4},
    {"_nn2poly_alg_non_linear", (DL_FUNC) &_nn2poly_alg_non_linear, 8},
    {"_nn2poly_combinations_with_repetition", (DL_FUNC) &_nn2poly_combinations_with_repetition, 2},
    {"_nn2poly_generate_partitions", (DL_FUNC) &_nn2poly_generate_partitions, 2},
    {"_nn2poly_generate_partitions_full", (DL_FUNC) &_nn2poly_generate_partitions_full, 2},
    {NULL, NULL, 0}
};

RcppExport void R_init_nn2poly(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
