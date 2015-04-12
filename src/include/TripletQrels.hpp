/*
 * File:   TripletQrels.hpp
 * Author: praveen
 *
 * Created on May 16, 2014, 4:03 PM
 */

#ifndef TRIPLETQRELS_HPP
#define	TRIPLETQRELS_HPP

#include <iostream>
#include <fstream>
#include <RcppArmadillo.h>
#include "include/Utils.hpp"
#include "include/PairwiseQrels.hpp"
using namespace std;
using namespace Rcpp;

class TripletQrels {
private:
    typedef map<string, int> DOC_COUNTS;
    typedef PairwiseQrels::PairwiseJudgments PAIRS;

public:

    struct COND {
        vector<string> top;

        COND() {
        }

        COND(const string t) {
            top.push_back(t);
        }

        COND(const vector<string> t) :
        top(t) {
        }


    };

    struct Cond_Comparer {

        bool operator()(COND const& Left, COND const& Right) const {
            return Left.top < Right.top;
        }
    };

    struct TripletJudgments {
        int queryID;
        map< COND, PAIRS, Cond_Comparer> preferences;
        map< COND, PAIRS >::iterator topdoc_iter;

        DOC_COUNTS::iterator pref_iter;
        set<string> documents;
        DOC_COUNTS nonrel_pref;

        void addJudgment(COND condition, string left, string right, string pref) {

            if (pref == "-2") {
                nonrel_pref[left] += 1;
                nonrel_pref[right] += 1;
            } else if (pref == "-3") {
                nonrel_pref[left] += 1;
                nonrel_pref[right] += 1;
                for (int i = 0; i < condition.top.size(); i++)
                    nonrel_pref[condition.top[i]] += 1;
            }
            preferences[condition].addJudgment(left, right, pref);
            documents.insert(left);
            documents.insert(right);

        }

        int getCondPref(COND condition, string doc) {
            return preferences[condition].getPref(doc);
        }

        int getApprearance(COND condition, string doc) {
            return preferences[condition].getAppearance(doc);
        }

    };


    typedef set<TripletQrels::COND, TripletQrels::Cond_Comparer> COND_SET;
    typedef map<string, TripletJudgments> ANNOTATORS;
    typedef map<int, ANNOTATORS> QUERY_ANNOTATOR_QRELS;
    QUERY_ANNOTATOR_QRELS qrels;

private:
    bool hasQuery(int query, string annotatorID);

public:

    TripletQrels(string qrelsPath);

    vector<int> getQueries();

    vector<string> getAnnotators(int query);


    SEXP getTriplets(int queryID);

    SEXP getCondPrefCount(int queryID, vector<string> topdoc, vector<string> doc);

    SEXP getCondApperanceCount(int queryID, vector<string> topdoc, vector<string> doc);

    SEXP getDocuments(int query);

    SEXP getConditions(int query);

};





#endif	/* TRIPLETQRELS_HPP */

