#include "include/TripletQrels.hpp"

using namespace Rcpp;
using namespace std;

TripletQrels::TripletQrels(string qrelsPath) {

    ifstream qrelsFile(qrelsPath.c_str(), ios_base::in);
    string top, left, right, pref, annotatorID, line;
    int query, q0, curQuery;
    double rel;

    while (std::getline(qrelsFile, line)) {
        std::istringstream iss(line);

        iss >> query >> annotatorID >> top >> left >> right >> pref;

        COND condition(top);
        qrels[query][annotatorID].addJudgment(condition, left, right, pref);

        COND pref_condition("");
        qrels[query][annotatorID].addJudgment(pref_condition, left, right, pref);

    }
}

bool TripletQrels::hasQuery(int query, string annotatorID) {
    QUERY_ANNOTATOR_QRELS::iterator findQueryIter = qrels.find(query);
    map<string, TripletJudgments>::iterator annotatorIter;
    if (findQueryIter == qrels.end())
        return false;
    else if (annotatorID == "") return true;
    else {
        annotatorIter = qrels[query].find(annotatorID);
        if (annotatorIter == qrels[query].end())
            return false;
        else
            return true;
    }


}

vector<string> TripletQrels::getAnnotators(int query) {
    vector<string> annotators;
    if (hasQuery(query, "")) {
        for (ANNOTATORS::iterator it = qrels[query].begin();
                it != qrels[query].end(); ++it)
            annotators.push_back(it->first);
    }

    return annotators;
}

vector<int> TripletQrels::getQueries() {
    vector<int> queries;
    QUERY_ANNOTATOR_QRELS::iterator it = qrels.begin();
    for (; it != qrels.end(); ++it)
        queries.push_back(it->first);
    return queries;
}

SEXP TripletQrels::getCondApperanceCount(int query, vector<string> topdoc,
        vector<string> docids) {
    vector<int> scores;
    ANNOTATORS::iterator an_iter;
    if (hasQuery(query, "")) {
        for (int i = 0; i < topdoc.size(); i++) {
            int s = 0;
            for (an_iter = qrels[query].begin();
                    an_iter != qrels[query].end(); ++an_iter) {
                COND condition(topdoc[i]);
                s += qrels[query][an_iter->first].getApprearance(condition,
                        docids[i]);
            }
            scores.push_back(s);
        }

        // Convert to a R Vector  to return
        Rcpp::NumericVector results(docids.size());
        results = Rcpp::wrap(scores);
        results.attr("names") = Rcpp::wrap(docids);
        return results;
    } else {
        Rprintf("Query ID does not exist in the Qrels\n");
        return R_NilValue;
    }
}

SEXP TripletQrels::getCondPrefCount(int query, vector<string> topdoc,
        vector<string> docids) {
    vector<int> scores;
    ANNOTATORS::iterator an_iter;
    if (hasQuery(query, "")) {
        for (int i = 0; i < topdoc.size(); i++) {
            int s = 0;
            for (an_iter = qrels[query].begin();
                    an_iter != qrels[query].end(); ++an_iter) {
                COND condition(topdoc[i]);
                s += qrels[query][an_iter->first].getCondPref(condition,
                        docids[i]);
            }
            scores.push_back(s);
        }

        // Convert to a R Vector  to return
        Rcpp::NumericVector results(docids.size());
        results = Rcpp::wrap(scores);
        results.attr("names") = Rcpp::wrap(docids);
        return results;
    } else {
        Rprintf("Query ID does not exist in the Qrels\n");
        return R_NilValue;
    }
}

SEXP TripletQrels::getConditions(int query) {
    set<string> conds;
    if (hasQuery(query, "")) {
        for (ANNOTATORS::iterator it = qrels[query].begin();
                it != qrels[query].end(); ++it) {
            map< COND, PAIRS, Cond_Comparer>::iterator cond_iter =
                    it->second.preferences.begin();

            while (cond_iter != it->second.preferences.end()) {
                string condition_code;
                for (int i = 0; i < cond_iter->first.top.size(); i++) {
                    condition_code += cond_iter->first.top[i];
                    if (i != cond_iter->first.top.size() - 1)
                        condition_code += ":";
                }
                conds.insert(condition_code);
                ++cond_iter;
            }
        }
    }

    return wrap(conds);
}

SEXP TripletQrels::getDocuments(int query) {
    set<string> documents;
    if (hasQuery(query, "")) {
        for (ANNOTATORS::iterator it = qrels[query].begin();
                it != qrels[query].end(); ++it) {
            set<string>::iterator dociter = it->second.documents.begin();
            while (dociter != it->second.documents.end()) {
                documents.insert(*dociter);
                ++dociter;
            }
        }
    }

    return wrap(documents);
}

RCPP_MODULE(TripletQrels) {
    using namespace Rcpp;

    class_<TripletQrels > ("TripletQrels")
            .constructor<string>("Default Constructor")
            // read-only property
            .method("getQueries", &TripletQrels::getQueries)
            .method("getDocuments", &TripletQrels::getDocuments)
            .method("getConditions", &TripletQrels::getConditions)
            .method("getAnnotators", &TripletQrels::getAnnotators)
            .method("getCondPrefCount", &TripletQrels::getCondPrefCount)
            .method("getCondApperanceCount", &TripletQrels::getCondApperanceCount);

}


