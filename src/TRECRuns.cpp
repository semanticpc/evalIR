#include "include/Runs.hpp"


using namespace Rcpp;
using namespace std;

class Runs {
private:

    struct Document {
        int query;
        vector<string> runids;
        vector<int> ranks;
        vector<int> docPos;
        vector<double> scores;

        void add(string runid, int rank, int pos, double score) {
            runids.push_back(runid);
            ranks.push_back(rank);
            docPos.push_back(pos);
            scores.push_back(score);
        }
    };
    map<int, map<string, Document> > query_docs;
    map<int, int> query_docCounts;
    map<string, int> runids;
    int numberOfRuns;


public:

    Runs() {
        numberOfRuns = 0;
    }

    Runs(vector<string> runsPath, vector<string> runids, int docLimit = 1000) {
        numberOfRuns = 0;
        if (runsPath.size() != runids.size())
            throw (Rcpp::exception(" Runs and Runids must be equal"));
        for (int i = 0; i < runsPath.size(); ++i)
            addRun(runsPath.at(i), runids.at(i), docLimit);
    }

    void addRun(string path, string runid, int docLimit = 1000) {
        runids[runid] = numberOfRuns++;
        query_docCounts.clear();
        ifstream qrelsFile(path.c_str(), ios_base::in);
        string docid, q0, id, line;
        int query, rank, curQuery, docCount = 0;
        double score;
        bool first = true;

        while (getline(qrelsFile, line)) {
            istringstream iss(line);
            iss >> query >> q0 >> docid >> rank >> score >> id;

            if (q0 != "Q0") continue;

            if (first) {
                curQuery = query;
                first = false;
                query_docCounts[query] = 0;
                docCount = 0;
            }
            docCount = query_docCounts[query];

            if (curQuery != query) {

                curQuery = query;
                map<int, int>::iterator findQueryIter = query_docCounts.find(query);
                if (findQueryIter != query_docCounts.end()) {
                    docCount = query_docCounts[query];
                } else {
                    docCount = 0;
                    query_docCounts[query] = 0;
                }

            }

            if (docCount < docLimit) {
                query_docs[query][docid].add(runid, rank, docCount + 1, score);
            }
            query_docCounts[query] = ++docCount;
        }
    }

    vector<string> getRunids() {
        vector<string> run;
        for (map<string, int>::iterator it = runids.begin();
                it != runids.end(); ++it)
            run.push_back(it->first);
        return run;
    }

    vector<int> getQueries() {
        vector<int> queries;
        for (map<int, map<string, Document> >::iterator it = query_docs.begin();
                it != query_docs.end(); ++it)
            queries.push_back(it->first);
        return queries;
    }

    SEXP getGivenRankMatrix(int queryID) {
        return getRankingMatrix(queryID, "givenrank");
    }

    SEXP getRankMatrix(int queryID) {
        return getRankingMatrix(queryID, "pos");
    }

    SEXP getRankingMatrix(int queryID, string type) {
        map<int, map<string, Document> >::iterator it = query_docs.find(queryID);
        if (it != query_docs.end()) {
            Rcpp::NumericMatrix matrix(query_docs[queryID].size(),
                    runids.size());

            int r = -1;
            vector<string> docids;
            map<string, Document>::iterator doc;
            for (doc = query_docs[queryID].begin();
                    doc != query_docs[queryID].end(); ++doc) {

                ++r;
                docids.push_back(doc->first);
                for (int runs = 0; runs < doc->second.ranks.size(); runs++)
                    if (type == "pos")
                        matrix(r, runids[doc->second.runids.at(runs)]) = doc->second.docPos.at(runs);
                    else
                        matrix(r, runids[doc->second.runids.at(runs)]) = doc->second.ranks.at(runs);
            }

            vector<string> runid_names;
            for (map<string, int>::iterator it = runids.begin();
                    it != runids.end(); ++it)
                runid_names.push_back(it->first);

            matrix.attr("dimnames") =
                    Rcpp::List::create(docids, runid_names);
            return matrix;
        } else {
            Rprintf("Query ID does not exist in the Qrels\n");
            return R_NilValue;
            ;
        }
    }

    SEXP getScoreMatrix(int queryID) {
        map<int, map<string, Document> >::iterator it = query_docs.find(queryID);
        if (it != query_docs.end()) {
            Rcpp::NumericMatrix matrix(query_docs[queryID].size(),
                    runids.size());

            int r = -1;
            vector<string> docids;
            map<string, Document>::iterator doc;
            for (doc = query_docs[queryID].begin();
                    doc != query_docs[queryID].end(); ++doc) {

                ++r;
                docids.push_back(doc->first);
                for (int runs = 0; runs < doc->second.ranks.size(); runs++)
                    matrix(r, runids[doc->second.runids.at(runs)]) = doc->second.scores.at(runs);
            }

            vector<string> runid_names;
            for (map<string, int>::iterator it = runids.begin();
                    it != runids.end(); ++it)
                runid_names.push_back(it->first);

            matrix.attr("dimnames") =
                    Rcpp::List::create(docids, runid_names);
            return matrix;
        } else {
            Rprintf("Query ID does not exist in the Qrels\n");
            return R_NilValue;
            ;
        }
    }

};

RCPP_MODULE(Runs) {
    using namespace Rcpp;

    class_<Runs> ("Runs")
            .constructor<vector<string>, vector<string>, int>("Default Constructor")
            .method("getQueries", &Runs::getQueries)
            .method("getRunids", &Runs::getRunids)
            .method("getRankMatrix", &Runs::getRankMatrix)
            .method("getScoreMatrix", &Runs::getScoreMatrix)
            .method("getGivenRankMatrix", &Runs::getGivenRankMatrix);
}