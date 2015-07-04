#include "DiversityQrels.hpp"

DivQrels::DivQrels(string qrelsPath) {
    ifstream qrelsFile(qrelsPath.c_str(), ios_base::in);
    string docid, line, subtopic;

    int query;
    double rel;
    while (std::getline(qrelsFile, line)) {
        std::istringstream iss(line);
        iss >> query >> subtopic >> docid >> rel;
        qrels[query].addJudgment(docid, rel, subtopic);
    }

}

SEXP DivQrels::getSubtopicProbabilties(int queryID) {
    QUERY_DIV_QRELS::iterator it = qrels.find(queryID);
    if (it != qrels.end())
        return Rcpp::wrap(qrels[queryID].st_prob);
    else {
        Rprintf("Query ID does not exist in the Qrels\n");
        return R_NilValue;
        ;
    }

}

SEXP DivQrels::getSubtopicMatrix(int queryID) {
    QUERY_DIV_QRELS::iterator it = qrels.find(queryID);
    if (it != qrels.end()) {
        Rcpp::NumericMatrix matrix(qrels[queryID].grades.size(),
                qrels[queryID].getSTCount());

        vector<string> docids;
        vector<string> subtopics = qrels[queryID].getSTNames();
        map<string, SubtopicGrade>::iterator documents;
        documents = qrels[queryID].grades.begin();
        arma::mat mat(qrels[queryID].grades.size(), subtopics.size());
        int i = 0;
        while (documents != qrels[queryID].grades.end()) {
            docids.push_back(documents->first);
            mat.row(i++) = arma::trans(qrels[queryID].getGrade(documents->first));
            ++documents;
        }
        matrix = Rcpp::wrap(mat);
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
    QUERY_DIV_QRELS::iterator it = qrels.begin();
    while (it != qrels.end()) {
        queries.push_back(it->first);
        ++it;
    }


    return queries;
}

SEXP DivQrels::getNonRelDocuments(int queryID) {
    QUERY_DIV_QRELS::iterator it = qrels.find(queryID);
    if (it != qrels.end())
        return Rcpp::wrap(qrels[queryID].non_rel_docs);
    else {
        Rprintf("Query ID does not exist in the Qrels\n");
        return R_NilValue;
        ;
    }

}

SEXP DivQrels::judgeQuery(int queryID, vector<string> run) {
    QUERY_DIV_QRELS::iterator it = qrels.find(queryID);
    if (it != qrels.end()) {
        Rcpp::NumericMatrix matrix(qrels[queryID].grades.size(),
                qrels[queryID].getSTCount());

        vector<string> docids;
        vector<string> subtopics = qrels[queryID].getSTNames();
        map<string, SubtopicGrade>::iterator documents;
        arma::mat mat(run.size(), subtopics.size());
        int row_index = 0;

        for (int i = 0; i < run.size(); i++) {
            docids.push_back(run[i]);
            mat.row(row_index++) = arma::trans(qrels[queryID].getGrade(run[i]));
        }
        matrix = Rcpp::wrap(mat);
        matrix.attr("dimnames") =
                Rcpp::List::create(docids, subtopics);
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


