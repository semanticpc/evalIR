/*
 * File:   PairwiseQrels.hpp
 * Author: praveen
 *
 * Created on May 18, 2014, 4:47 PM
 */

#ifndef PAIRWISEQRELS_HPP
#define	PAIRWISEQRELS_HPP

#include <iostream>
#include <fstream>
#include <RcppArmadillo.h>
#include "Utils.hpp"
using namespace std;
using namespace Rcpp;

class PairwiseQrels {
private:
    typedef map<string, int> DOC_COUNTS;

public:

    struct Pair {
        string left, right;

        Pair(string l, string r) : left(l), right(r) {
        }
    };

    struct PairwiseJudgments {
        int queryID;
        vector< pair<Pair, string> > raw_judgments;
        DOC_COUNTS preferences;
        DOC_COUNTS appearance;
        DOC_COUNTS nonrel_docs;

        void addJudgment(string left, string right, string pref) {
            Pair p(left, right);
            raw_judgments.push_back(make_pair(p, pref));
            appearance[left] += 1;
            appearance[right] += 1;
            if (pref == "-2" || pref == "BOTH_NONREL") {
                nonrel_docs[left] += 1;
                nonrel_docs[right] += 1;
            } else if (pref == "LEFT_NONREL") {
                nonrel_docs[left] += 1;
                preferences[right] += 1;
            } else if (pref == "RIGHT_NONREL") {
                nonrel_docs[right] += 1;
                preferences[left] += 1;
            } else if (pref == right)
                preferences[right] += 1;
            else if (pref == left)
                preferences[left] += 1;

        }

        int getPref(string doc) {
            return Utils::find(preferences, doc, 0);
        }

        int getAppearance(string doc) {
            return Utils::find(appearance, doc, 0);
        }

        SEXP getAllPairs() {
            int pairCount = raw_judgments.size();
            CharacterMatrix pairs(pairCount, 3);
            vector< pair<Pair, string> >::iterator iter;
            iter = raw_judgments.begin();
            int i = 0;
            while (iter != raw_judgments.end()) {
                pairs(i, 0) = iter->first.left;
                pairs(i, 1) = iter->first.right;
                pairs(i, 2) = iter->second;
                i++;
            }
            return pairs;
        }
    };

private:
    typedef map<string, PairwiseJudgments> ANNOTATORS;
    typedef map<int, ANNOTATORS> QUERY_ANNOTATOR_QRELS;
    QUERY_ANNOTATOR_QRELS qrels;
    bool hasQuery(int query, string annotatorID);

public:

    PairwiseQrels(string qrelsPath);

    vector<int> getQueries();

    vector<string> getAnnotators(int query);

    vector<int> getPrefCounts(int queryID, vector<string> doc);

    vector<int> getApperanceCounts(int queryID, vector<string> doc);


};




#endif	/* PAIRWISEQRELS_HPP */

