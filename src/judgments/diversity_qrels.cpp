#include "diversity_qrels.h"

using namespace Rcpp;
using namespace std;

DivQrels::DivQrels(string qrelsPath) {
    ifstream qrelsFile(qrelsPath.c_str(), ios_base::in);
    string docid, line, subtopic;
    int query, curQuery;
    double rel;
    Qrels qrels;

    bool first = true;
    while (std::getline(qrelsFile, line)) {
        std::istringstream iss(line);
        iss >> query >> subtopic >> docid >> rel;
        if (first) {
            curQuery = query;
            qrels.queryID = curQuery;
            qrels.grades.clear();
            qrels.non_rel_docs.clear();
            qrels.st_probability.clear();
            first = false;
        }
        if (curQuery != query) {

            qrels_list.insert(make_pair(curQuery, qrels));
            curQuery = query;
            qrels.queryID = curQuery;
            qrels.grades.clear();
            qrels.non_rel_docs.clear();
            qrels.st_probability.clear();
        }
        qrels.addDocument(docid, rel, subtopic);
    }
    qrels_list.insert(make_pair(curQuery, qrels));

}

SEXP DivQrels::getSubtopicProbabilties(int queryID) {
    map<int, Qrels>::iterator it = qrels_list.find(queryID);
    if (it != qrels_list.end())
        return Rcpp::wrap(qrels_list[queryID].st_probability);
    else {
        Rprintf("Query ID does not exist in the Qrels\n");
        return R_NilValue;
        ;
    }

}

SEXP DivQrels::getSubtopicMatrix(int queryID) {
    map<int, Qrels>::iterator it = qrels_list.find(queryID);
    if (it != qrels_list.end()) {
        Rcpp::NumericMatrix matrix(qrels_list[queryID].grades.size(),
                qrels_list[queryID].st_probability.size());

        vector<string> subtopics;

        map<string, int> subtopic_indexes;
        int i = 0;
        for (map<string, double>::iterator it = qrels_list[queryID].st_probability.begin();
                it != qrels_list[queryID].st_probability.end(); ++it) {
            subtopic_indexes[it->first] = i++;
            subtopics.push_back(it->first);
        }



        int r = -1;
        map<string, vector<string> >::iterator documents;
        vector<string> docids;
        for (documents = qrels_list[queryID].grades.begin();
                documents != qrels_list[queryID].grades.end(); ++documents) {
            ++r;
            docids.push_back(documents->first);
            for (vector<string>::iterator it = (documents->second).begin();
                    it != (documents->second).end(); ++it) {
                matrix(r, subtopic_indexes[(*it)]) = 1;
            }
        }

        matrix.attr("dimnames") =
                Rcpp::List::create(docids, subtopics);
        return matrix;
    } else {
        Rprintf("Query ID does not exist in the Qrels\n");
        return R_NilValue;
        ;
    }
}

vector<int> DivQrels::getQueries() {
    vector<int> queries;
    for (map<int, Qrels>::iterator it = qrels_list.begin(); it != qrels_list.end(); ++it)
        queries.push_back(it->first);
    return queries;
}

SEXP DivQrels::getNonRelDocuments(int queryID) {
    map<int, Qrels>::iterator it = qrels_list.find(queryID);
    if (it != qrels_list.end())
        return Rcpp::wrap(qrels_list[queryID].non_rel_docs);
    else {
        Rprintf("Query ID does not exist in the Qrels\n");
        return R_NilValue;
        ;
    }

}

SEXP DivQrels::judgeQuery(int queryID, vector<string> run) {
    map<int, Qrels>::iterator it = qrels_list.find(queryID);
    NumericVector judgedGrades(run.size());
    if (it != qrels_list.end()) {

        NumericMatrix matrix(run.size(),
                qrels_list[queryID].st_probability.size());

        vector<string> subtopics;

        map<string, int> subtopic_indexes;
        int i = 0;
        for (map<string, double>::iterator it = qrels_list[queryID].st_probability.begin();
                it != qrels_list[queryID].st_probability.end(); ++it) {
            subtopic_indexes[it->first] = i++;
            subtopics.push_back(it->first);
        }

        for (int i = 0; i < run.size(); i++) {
            map<string, vector<string> >::iterator it =
                    qrels_list[queryID].grades.find(run.at(i));
            if (it != qrels_list[queryID].grades.end()) {
                for (vector<string>::iterator st = (it->second).begin();
                        st != (it->second).end(); ++st) {
                    matrix(i, subtopic_indexes[(*st)]) = 1;
                }
            } else
                matrix.row(i) = rep(0, qrels_list[queryID].st_probability.size());
        }
        matrix.attr("dimnames") = List::create(run, subtopics);
        return matrix;
    } else {
        Rprintf("Query ID does not exist in the Qrels\n");
        return R_NilValue;
        ;
    }
}

RCPP_MODULE(DivQrels) {
    using namespace Rcpp;

    class_<DivQrels > ("DivQrels")
            .constructor<string>()
            // read-only property
            .method("getQueries", &DivQrels::getQueries)
            .method("getNonRelDocs", &DivQrels::getNonRelDocuments)
            .method("getMatrix", &DivQrels::getSubtopicMatrix)
            .method("getSubtopicProbabilties", &DivQrels::getSubtopicProbabilties)
            .method("judgeQuery", &DivQrels::judgeQuery);
}

