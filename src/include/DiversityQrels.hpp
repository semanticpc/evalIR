/*
 * File:   diversity_qrels.h
 * Author: praveen
 *
 * Created on October 9, 2013, 3:19 PM
 */

#ifndef DIVERSITY_QRELS_H
#define	DIVERSITY_QRELS_H

#include <iostream>
#include <fstream>
#include <RcppArmadillo.h>
using namespace std;
using namespace Rcpp;

class DivQrels {
private:

    struct Qrels {
        int queryID;
        map<string, vector<string> > grades;
        map<string, string> docid_index;
        vector<string> non_rel_docs;
        map<string, double> st_probability;

        void addDocument(string _docid, double _grade, string _subtopic) {
            if (_grade > 0 && _subtopic != "0") {
                grades[_docid].push_back(_subtopic);

                // Keep track of a list of subtopics
                map<string, double>::iterator it = st_probability.find(_subtopic);
                if (it == st_probability.end())
                    st_probability.insert(make_pair(_subtopic, 1.0));
            } else
                non_rel_docs.push_back(_docid);
        }

    };

    map<int, Qrels> qrels_list;

public:

    DivQrels(string qrelsPath);

    vector<int> getQueries();

    SEXP getSubtopicProbabilties(int queryID);

    SEXP getSubtopicMatrix(int queryID);

    SEXP getNonRelDocuments(int queryID);

    SEXP judgeQuery(int queryID, vector<string> run);

};



#endif	/* DIVERSITY_QRELS_H */

