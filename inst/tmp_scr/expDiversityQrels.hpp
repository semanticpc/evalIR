/*
 * File:   DiversityQrels.hpp
 * Author: praveen
 *
 * Created on May 17, 2014, 3:37 PM
 */

#ifndef DIVERSITYQRELS_HPP
#define	DIVERSITYQRELS_HPP

#include <iostream>
#include <fstream>
#include <RcppArmadillo.h>
#include "Utils.hpp"
using namespace std;
using namespace Rcpp;
using namespace arma;

class DivQrels {
public:

    struct SubtopicGrade {
        map<string, int> subtopic_grades;
        map<string, int>::iterator subtopic_grades_iter;

        void addSubtopic(string name, int grade) {
            subtopic_grades[name] = grade;
        }

        int getSTGrade(string name) {
            if (hasSubtopic(name))
                return subtopic_grades[name];
            else
                return 0;
        }

        bool hasSubtopic(string name) {
            subtopic_grades_iter = subtopic_grades.find(name);
            if (subtopic_grades_iter != subtopic_grades.end()) return true;
            else return false;
        }
    };

    struct SubtopicJudgments {
        int queryID;
        map<string, SubtopicGrade > grades;
        map<string, SubtopicGrade >::iterator grades_iter;
        vector<string> non_rel_docs;
        map<string, double> st_prob;
        map<string, double>::iterator st_prob_iter;

        void addJudgment(string _docid, double _grade, string _subtopic) {
            if (_grade > 0 && _subtopic != "0") {
                grades[_docid].addSubtopic(_subtopic, _grade);
                if (Utils::find(st_prob, _subtopic, -1.00) == -1)
                    st_prob.insert(make_pair(_subtopic, 1.0));
            } else
                non_rel_docs.push_back(_docid);
        }

        bool hasGrade(string docid) {
            grades_iter = grades.find(docid);
            if (grades_iter != grades.end()) return true;
            else return false;
        }

        vec getGrade(string docid) {
            string st_name;
            vec st_grade = arma::zeros(st_prob.size());
            int i = 0;
            st_prob_iter = st_prob.begin();
            if (hasGrade(docid)) {
                while (st_prob_iter != st_prob.end()) {
                    st_name = st_prob_iter->first;
                    st_grade(i) = grades[docid].getSTGrade(st_name);
                    ++st_prob_iter;
                    ++i;
                }
            } else
                st_grade = arma::zeros(st_prob.size());
            return st_grade;
        }

        int getSTCount() {
            return st_prob.size();
        }

        vector<string> getSTNames() {
            vector<string> names;
            st_prob_iter = st_prob.begin();
            while (st_prob_iter != st_prob.end()) {
                names.push_back(st_prob_iter->first);
                ++st_prob_iter;
            }
            return names;
        }

    };


    typedef map<int, SubtopicJudgments> QUERY_DIV_QRELS;
    QUERY_DIV_QRELS qrels;


public:

    DivQrels(string qrelsPath);

    vector<int> getQueries();

    SEXP getSubtopicProbabilties(int queryID);

    SEXP getSubtopicMatrix(int queryID);

    SEXP getNonRelDocuments(int queryID);

    SEXP judgeQuery(int queryID, vector<string> run);

};

#endif	/* DIVERSITYQRELS_HPP */

