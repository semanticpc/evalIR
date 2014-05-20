#include <map>
#include "include/SimulateTripletQrels.hpp"

SimulateTripletQrels::SimulateTripletQrels(string _qrelsPath,
        SEXP _opts) {
    opts = as<Rcpp::List>(_opts);
    if (opts.containsElementNamed("simulateType"))
        simulationMethod = Rcpp::as<std::string>(opts["simulateType"]);
    else
        simulationMethod = "subtopic";

    if (opts.containsElementNamed("resolveTies"))
        resolveTiesMethod = Rcpp::as<std::string>(opts["resolveTies"]);
    else
        resolveTiesMethod = "random";


    ifstream qrelsFile(_qrelsPath.c_str(), ios_base::in);
    string docid, line, subtopic;
    int query;
    double rel;


    while (std::getline(qrelsFile, line)) {
        std::istringstream iss(line);
        iss >> query >> subtopic >> docid >> rel;
        div_qrels[query].addJudgment(docid, rel, subtopic);
    }

    //Simulate basic condition(preferences with no top doc) for all queries.
    QUERY_DIV_QRELS::iterator qrels_iter = div_qrels.begin();
    TripletQrels::COND condition;
    while (qrels_iter != div_qrels.end()) {
        simulate(qrels_iter->first, condition);
        ++qrels_iter;
    }
}

string SimulateTripletQrels::judgeTriplet(int query,
        vector<string> top_docid, string left_docid, string right_docid) {
    string pref;
    if (simulationMethod == "subtopic") {
        pref = simulateJudgment_Subtopic(query, top_docid, left_docid, right_docid);
    } else if (simulationMethod == "subtopic_LR-2-3")
        pref = simulateJudgment_SubtopicLR23(query, top_docid, left_docid, right_docid);

    if (pref == "tie") {
        if (resolveTiesMethod == "random")
            return resolveTies_random(left_docid, right_docid);
        else if (resolveTiesMethod == "alphabetical")
            return (left_docid < right_docid ? left_docid : right_docid);
        else
            return resolveTies_random(left_docid, right_docid);
    } else
        return pref;
}

string SimulateTripletQrels::simulateJudgment_SubtopicLR23(int query,
        vector<string> top_docid, string left_docid, string right_docid) {

    vec left = div_qrels[query].getGrade(left_docid);
    vec right = div_qrels[query].getGrade(right_docid);
    vec top = arma::zeros(div_qrels[query].st_prob.size());
    for (int i = 0; i < top_docid.size(); i++)
        top += div_qrels[query].getGrade(top_docid[i]);

    double alpha = 0.5;
    double scoreA = 0, scoreB = 0;
    for (int i = 0; i < left.n_rows; i++) {
        if (left(i) == 1)
            scoreA += pow((1 - alpha), top(i));
    }
    for (int i = 0; i < right.n_rows; i++) {
        if (right(i) == 1)
            scoreB += pow((1 - alpha), top(i));
    }


    if (arma::sum(right) && arma::sum(left) && arma::sum(top))
        return "-3";
    else if (arma::sum(right) && arma::sum(left))
        return "-2";
    else if (scoreA > scoreB)
        return left_docid;
    else if (scoreA < scoreB)
        return right_docid;
    else
        return "tie";
}

string SimulateTripletQrels::simulateJudgment_Subtopic(int query,
        vector<string> top_docid, string left_docid, string right_docid) {
    vec left = div_qrels[query].getGrade(left_docid);
    vec right = div_qrels[query].getGrade(right_docid);
    vec top = arma::zeros(div_qrels[query].st_prob.size());

    for (int i = 0; i < top_docid.size(); i++)
        top += div_qrels[query].getGrade(top_docid[i]);

    double alpha = 0.5;
    double scoreA = 0, scoreB = 0;
    for (int i = 0; i < left.n_rows; i++) {
        if (left(i) == 1)
            scoreA += pow((1 - alpha), top(i));
    }
    for (int i = 0; i < right.n_rows; i++) {
        if (right(i) == 1)
            scoreB += pow((1 - alpha), top(i));
    }
    if (scoreA > scoreB)
        return left_docid;
    else if (scoreA < scoreB)
        return right_docid;
    else
        return "tie";
}

string SimulateTripletQrels::resolveTies_random(string left, string right) {
    // Resolve ties randomly
    int number = rand() % 10;
    if (number < 5)
        return left;
    else
        return right;
}

bool SimulateTripletQrels::isCached(int query, TripletQrels::COND & condition) {
    map<int, COND_SET >::iterator qIter;
    COND_SET::iterator condIter;
    qIter = cachedCond.find(query);
    if (qIter != cachedCond.end()) {
        condIter = cachedCond[query].find(condition);
        if (condIter != cachedCond[query].end())
            return true;
    }
    return false;
}

bool SimulateTripletQrels::docInCondition(string doc, TripletQrels::COND & condition) {
    for (int i = 0; i < condition.top.size(); i++) {
        if (doc == condition.top[i])
            return true;
    }
    return false;
}

void SimulateTripletQrels::simulate(int query, TripletQrels::COND & condition) {
    map<string, DivQrels::SubtopicGrade>::iterator iterDocsA;
    map<string, DivQrels::SubtopicGrade>::iterator iterDocsB;

    if (isCached(query, condition)) return;
    iterDocsA = div_qrels[query].grades.begin();
    while (iterDocsA != div_qrels[query].grades.end()) {
        iterDocsB = div_qrels[query].grades.find(iterDocsA->first);
        if (docInCondition(iterDocsA->first, condition)) {
            ++iterDocsA;
            continue;
        }
        while (iterDocsB != div_qrels[query].grades.end()) {
            if (iterDocsB->first == iterDocsA->first ||
                    docInCondition(iterDocsB->first, condition)) {
                ++iterDocsB;
                continue;
            }
            string pref = judgeTriplet(query, condition.top,
                    iterDocsA->first, iterDocsB->first);
            triplet_qrels[query]["TREC"].addJudgment(condition.top,
                    iterDocsA->first, iterDocsB->first, pref);
            ++iterDocsB;
        }
        ++iterDocsA;
    }
    cachedCond[query].insert(condition);

}

vector<int> SimulateTripletQrels::getQueries() {
    vector<int> queries;
    QUERY_DIV_QRELS::iterator qrels_iter = div_qrels.begin();
    for (; qrels_iter != div_qrels.end(); ++qrels_iter)
        queries.push_back(qrels_iter->first);

    return queries;
}

SEXP SimulateTripletQrels::getPrefCount(int query, vector<string> docids) {
    vector<int> scores;
    QUERY_DIV_QRELS::iterator qrels_iter;
    if (qrels_iter != div_qrels.end()) {
        for (int i = 0; i < docids.size(); i++) {
            TripletQrels::COND condition;
            scores.push_back(triplet_qrels[query]["TREC"].getCondPref(condition, docids[i]));
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

SEXP SimulateTripletQrels::getCondPrefCount(int query, vector<string> topdoc,
        vector<string> docids) {
    vector<int> scores;
    QUERY_DIV_QRELS::iterator qrels_iter;
    if (qrels_iter != div_qrels.end()) {
        for (int i = 0; i < docids.size(); i++) {
            if (arma::sum(div_qrels[query].getGrade(topdoc[i])) == 0) {
                scores.push_back(0);
            } else {
                TripletQrels::COND condition(topdoc[i]);
                simulate(query, condition);
                scores.push_back(triplet_qrels[query]["TREC"].getCondPref(condition, docids[i]));
            }
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

SEXP SimulateTripletQrels::getApperanceCount(int query, vector<string> docids) {
    vector<int> scores;
    QUERY_DIV_QRELS::iterator qrels_iter;
    if (qrels_iter != div_qrels.end()) {
        for (int i = 0; i < docids.size(); i++) {
            TripletQrels::COND condition;
            scores.push_back(triplet_qrels[query]["TREC"].getApprearance(condition, docids[i]));
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

SEXP SimulateTripletQrels::getCondApperanceCount(int query, vector<string> topdoc,
        vector<string> docids) {
    vector<int> scores;
    QUERY_DIV_QRELS::iterator qrels_iter;
    if (qrels_iter != div_qrels.end()) {
        for (int i = 0; i < docids.size(); i++) {
            TripletQrels::COND condition(topdoc[i]);
            scores.push_back(triplet_qrels[query]["TREC"].getApprearance(condition, docids[i]));
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

RCPP_MODULE(SimulateTripletQrels) {
    using namespace Rcpp;

    class_<SimulateTripletQrels > ("SimulateTripletQrels")
            .constructor<string, SEXP>()
            .method("getQueries", &SimulateTripletQrels::getQueries)
            .method("getPrefCount", &SimulateTripletQrels::getPrefCount)
            .method("getCondPrefCount", &SimulateTripletQrels::getCondPrefCount)
            .method("getApperanceCount", &SimulateTripletQrels::getApperanceCount)
            .method("getCondApperanceCount", &SimulateTripletQrels::getCondApperanceCount);
}


