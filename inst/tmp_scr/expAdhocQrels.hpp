/*
 * File:   adhoc_qrels.h
 * Author: praveen
 *
 * Created on January 21, 2014, 11:10 AM
 */

#ifndef ADHOC_QRELS_H
#define	ADHOC_QRELS_H


#include <iostream>
#include <fstream>
#include <RcppArmadillo.h>
using namespace std;
using namespace Rcpp;

class AdhocQrels {
private:

    struct Qrels {
        int queryID;
        map<string, double> grades;
        vector<string> non_rel_docs;

        void addDocument(string _docid, double _grade) {
            if (_grade > 0) grades.insert(make_pair(_docid, _grade));
            else non_rel_docs.push_back(_docid);
        }
    };

    map<int, Qrels> qrels_list;

public:

    AdhocQrels(string qrelsPath);

    vector<int> getQueries();

    SEXP getGrades(int queryID);

    SEXP getNonRelDocuments(int queryID);

    void setGrades(int queryID, vector<string> documents, vector<double> grades);

    SEXP judgePair(int queryID, CharacterMatrix pairs, string ties);

    SEXP judgeQuery(int queryID, vector<string> run);

};



#endif	/* ADHOC_QRELS_H */

