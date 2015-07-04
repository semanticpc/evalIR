#ifndef _evalIR_DIVERSITY_H
#define _evalIR_DIVERSITY_H

#include <RcppArmadillo.h>

// Subtopic Recall 
RcppExport SEXP srecall_ideal(SEXP _qrels_RelDocMatrix, SEXP _rank) ;


// Alpha-nDCG and Alpha-DCG
RcppExport SEXP andcg_ideal(SEXP _qrels_RelDocMatrix, SEXP _rank, SEXP _alpha) ;
RcppExport SEXP compute_dcg(SEXP _mat, SEXP _rank, SEXP _alpha) ;


#endif
