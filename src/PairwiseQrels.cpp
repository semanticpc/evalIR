#include "include/PairwiseQrels.hpp"

using namespace Rcpp;
using namespace std;

PairwiseQrels::PairwiseQrels(string qrelsPath) {

    ifstream qrelsFile(qrelsPath.c_str(), ios_base::in);
    string top, left, right, pref, annotatorID, line;
    int query, q0, curQuery;
    double rel;


    while (std::getline(qrelsFile, line)) {
        std::istringstream iss(line);

        iss >> query >> top >> left >> right >> pref >> annotatorID;
        if (annotatorID == "") annotatorID = "single";
        qrels[query][annotatorID].addJudgment(left, right, pref);

    }
}

bool PairwiseQrels::hasQuery(int query, string annotatorID) {
    QUERY_ANNOTATOR_QRELS::iterator findQueryIter = qrels.find(query);
    map<string, PairwiseJudgments>::iterator annotatorIter;
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

vector<string> PairwiseQrels::getAnnotators(int query) {
    vector<string> annotators;
    if (hasQuery(query, "")) {
        for (ANNOTATORS::iterator it = qrels[query].begin();
                it != qrels[query].end(); ++it)
            annotators.push_back(it->first);
    }

    return annotators;
}

vector<int> PairwiseQrels::getQueries() {
    vector<int> queries;
    QUERY_ANNOTATOR_QRELS::iterator it = qrels.begin();
    for (; it != qrels.end(); ++it)
        queries.push_back(it->first);
    return queries;
}

vector<int> PairwiseQrels::getPrefCounts(int query, vector<string> docids) {
    vector<int> scores;
    if (hasQuery(query, "")) {
        for (int i = 0; i < docids.size(); i++) {
            int s = 0;
            for (ANNOTATORS::iterator an = qrels[query].begin();
                    an != qrels[query].end(); ++an) {
                s += qrels[query][an->first].getPref(docids[i]);
            }
            scores.push_back(s);
        }
    }
    return scores;
}

vector<int> PairwiseQrels::getApperanceCounts(int query, vector<string> docids) {
    vector<int> scores;
    if (hasQuery(query, "")) {
        for (int i = 0; i < docids.size(); i++) {
            int s = 0;
            for (ANNOTATORS::iterator an = qrels[query].begin();
                    an != qrels[query].end(); ++an) {
                s += qrels[query][an->first].getAppearance(docids[i]);
            }
            scores.push_back(s);
        }
    }
    return scores;
}

RCPP_MODULE(PairwiseQrels) {
    using namespace Rcpp;

    class_<PairwiseQrels > ("TripletQrels")
            .constructor<string>("Default Constructor")
            // read-only property
            .method("getQueries", &PairwiseQrels::getQueries)
            .method("getAnnotators", &PairwiseQrels::getAnnotators)
            .method("getApperanceCounts", &PairwiseQrels::getApperanceCounts)
            .method("getPrefCounts", &PairwiseQrels::getPrefCounts);
}



