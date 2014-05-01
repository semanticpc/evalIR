/*
 * File:   pref_rels.h
 * Author: praveen
 *
 * Created on October 9, 2013, 2:29 PM
 */

#ifndef PREF_RELS_H
#define	PREF_RELS_H
#include <iostream>
#include <fstream>
#include <RcppArmadillo.h>
using namespace std;
using namespace Rcpp;

class PrfJudgments {
private:

    struct Edges {
        set<pair<string, string> > pairs;

        void addEdge(string left, string right) {
            pairs.insert(make_pair(left, right));
        }

    };

    struct Qrels {
        int queryID;
        Edges prfMatrix;
        set<string> rel_docs;
        set<string> non_rel_docs;

        void add_judgment_4options(string left, string right, string preference) {
            if (preference == "LEFT_NONREL") non_rel_docs.insert(left);
            else if (preference == "RIGHT_NONREL") non_rel_docs.insert(right);
            else if (preference == "BOTH_NONREL") {
                non_rel_docs.insert(left);
                non_rel_docs.insert(right);
            } else {
                rel_docs.insert(left);
                rel_docs.insert(right);
                if (preference == "LEFT")
                    prfMatrix.addEdge(right, left);
                else if (preference == "RIGHT")
                    prfMatrix.addEdge(left, right);
                else {
                    prfMatrix.addEdge(right, left);
                    prfMatrix.addEdge(left, right);
                }
            }
        }

        void add_judgment_2options(string left, string right, string preference) {
            if (preference == "LEFT") {
                prfMatrix.addEdge(right, left);
                rel_docs.insert(left);

            } else if (preference == "RIGHT") {
                prfMatrix.addEdge(left, right);
                rel_docs.insert(right);
            } else {
                rel_docs.insert(left);
                rel_docs.insert(right);
                prfMatrix.addEdge(right, left);
                prfMatrix.addEdge(left, right);
            }

        }

    };

    map<int, Qrels> qrels_list;
    string type;



public:

    PrfJudgments(string type);

    PrfJudgments(string type, string qrelsPath);

    vector<int> getQueries();

    SEXP getNonRelDocuments(int queryID);

    SEXP getEdges(int queryID);

    string getJudgment(int queryID, string left, string right);

    void addPair(int queryID, string left, string right, string preference);

    void addPairs(int queryID, CharacterMatrix pairs);

private:
    string getJudgment_2options(int queryID, string left, string right);

    string getJudgment_4options(int queryID, string left, string right);

};


#endif	/* PREF_RELS_H */

