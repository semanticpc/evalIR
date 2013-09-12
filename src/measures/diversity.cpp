
#include "diversity.h"
#include <RcppArmadillo.h>


using namespace Rcpp;
using namespace std;

/* *****************************************************************************
*  Subtopic Recall Functions
*******************************************************************************/
SEXP srecall_ideal(SEXP _qrels_RelDocMatrix, SEXP _rank) {
    arma::mat matrix = as<arma::mat > (_qrels_RelDocMatrix);
    int rank = as<int>(_rank);
    arma::rowvec seenSubtopics = arma::zeros<arma::rowvec > (matrix.n_cols);
    arma::rowvec zeroVector = arma::zeros<arma::rowvec>(matrix.n_cols);
    vector<int> idealRankList;
    vector<int>::iterator it;


    // Use a greedy algorithm to determine the idea rank list
    for (int i = 0; i < matrix.n_rows; i++) {
        if(i > rank) break;
        int pickedDoc = -1;
        int maxNewSubtopics = 0;
        // Iterate through the set of relevant documents to find
        //   the best document
        for(int j = 0; j < matrix.n_rows; j++){

            // Ignore the document already picked
            it =  find (idealRankList.begin(), idealRankList.end(), j);
            if(it != idealRankList.end() )
                continue;

            // Compute the number of new subtopics the document contains
            int numOfNewSubtopics = arma::sum(arma::sum((matrix.row(j) +
                                                seenSubtopics) > zeroVector));

            // Keep track of the best document/ max number of subtopics
            if(numOfNewSubtopics > maxNewSubtopics){
                maxNewSubtopics = numOfNewSubtopics;
                pickedDoc = j;
            }
        }
        // Add the best document to the ideal rank list
        //  and keep track of subtopics seen
        seenSubtopics += matrix.row(pickedDoc);
        idealRankList.push_back(pickedDoc);

    }
    return Rcpp::wrap(idealRankList);
}





/* *****************************************************************************
*  Alpha-nDCG Functions
******************************************************************************/


static arma::rowvec compute_andcg_discount(arma::rowvec subtopics, double alpha){
    // Iterate through the elements in the vector and compute (1- alpha)^x
    arma::rowvec discount = arma::zeros<arma::rowvec>(subtopics.n_cols);
    int i =0;
    for(arma::rowvec::iterator it=subtopics.begin(); it!=subtopics.end(); ++it){
        discount(i++) = pow((1 - alpha),(*it));
    }
    return discount;
}

SEXP andcg_ideal(SEXP _qrels_RelDocMatrix, SEXP _rank, SEXP _alpha) {
    arma::mat matrix = as<arma::mat > (_qrels_RelDocMatrix);
    int rank = as<int>(_rank);
    double alpha = as<double>(_alpha);
    arma::rowvec seenSubtopics = arma::zeros<arma::rowvec > (matrix.n_cols);
    arma::rowvec zeroVector = arma::zeros<arma::rowvec>(matrix.n_cols);
    vector<int> idealRankList;
    vector<int>::iterator it;
    // Use a greedy algorithm to determine the idea rank list
    for (int i = 0; i < matrix.n_rows; i++) {
        if(i >= rank) break;
        int pickedDoc = -1;
        double maxGain = 0;
        // Iterate through the set of relevant documents to find
        //   the best document
        for(int j = 0; j < matrix.n_rows; j++){

            // Ignore the document already picked
            it =  find (idealRankList.begin(), idealRankList.end(), j);
            if(it != idealRankList.end() )
                continue;

            arma::rowvec J_d = matrix.row(j);
            // Compute the discount
            arma::rowvec discount = compute_andcg_discount(seenSubtopics, alpha);

            double gain = arma::sum(arma::sum(J_d % discount)) / ((log(2 + i)/log(2)));
            if(gain > maxGain){
                maxGain = gain;
                pickedDoc = j;
            }
        }
        // Add the best document to the ideal rank list
        //  and keep track of subtopics seen
        seenSubtopics += matrix.row(pickedDoc);
        idealRankList.push_back(pickedDoc);
    }
    return Rcpp::wrap(idealRankList);
}


SEXP compute_dcg(SEXP _mat, SEXP _rank, SEXP _alpha){
    arma::mat matrix = as<arma::mat > (_mat);
    int rank = as<int>(_rank);
    double alpha = as<double>(_alpha);

    vector<double> dcg_vector;
    arma::rowvec seenSubtopics = arma::zeros<arma::rowvec>(matrix.n_cols);
    set<int>::iterator it;
    double dcg = 0;
    for(int i = 0; i < rank; i++){
        arma::rowvec J_d = matrix.row(i);
        arma::rowvec discount = compute_andcg_discount(seenSubtopics, alpha);
        double gain = arma::sum(arma::sum(J_d % discount)) / ((log(2 + i)/log(2)));
        dcg += gain;
        seenSubtopics += matrix.row(i);
        dcg_vector.push_back(dcg);
    }
    return Rcpp::wrap(dcg_vector);
}


