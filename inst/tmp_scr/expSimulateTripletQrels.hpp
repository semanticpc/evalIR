/*
 * File:   SimulateTripletQrels.hpp
 * Author: praveen
 *
 * Created on May 17, 2014, 1:45 AM
 */

#ifndef SIMULATETRIPLETQRELS_HPP
#define	SIMULATETRIPLETQRELS_HPP


#include <iostream>
#include <fstream>
#include <RcppArmadillo.h>
#include "TripletQrels.hpp"
#include "DiversityQrels.hpp"
using namespace std;
using namespace Rcpp;
using namespace arma;

class SimulateTripletQrels {
private:

    typedef TripletQrels::COND_SET COND_SET;
    typedef TripletQrels::QUERY_ANNOTATOR_QRELS QUERY_ANNOTATOR_QRELS;
    typedef DivQrels::QUERY_DIV_QRELS QUERY_DIV_QRELS;

    QUERY_DIV_QRELS div_qrels;
    QUERY_ANNOTATOR_QRELS triplet_qrels;
    map<int, COND_SET> cachedCond;
    Rcpp::List opts;
    string simulationMethod;
    string resolveTiesMethod;

private:



    string sim_Subtopic(int query, vector<string> top, string left, string right);

    string sim_SubtopicLR23(int query, vector<string> top, string left, string right);

    string ties_random(string left, string right);


    void simulate(int query, TripletQrels::COND &condition);

    bool isCached(int query, TripletQrels::COND &condition);

    bool docInCondition(string doc, TripletQrels::COND & condition);




public:

    SimulateTripletQrels(string qrelsPath, SEXP opts);

    vector<int> getQueries();

    SEXP getCondPrefCount(int queryID, vector<string> topdoc, vector<string> doc);

    SEXP getCondApperanceCount(int queryID, vector<string> topdoc, vector<string> doc);

    string judgeTriplet(int query, vector<string> top, string left, string right);


};





#endif	/* SIMULATETRIPLETQRELS_HPP */

