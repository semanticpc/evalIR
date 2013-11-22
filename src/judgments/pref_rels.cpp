#include "pref_rels.h"



//' RunQuery wrapper
//'
//' @param string an indri query
//' @param string query number
//' @param int number of results to be returned
//' @param string runid for the retrieved results
//'
//' @return DataFrame results in the TREC format

PrfJudgments::PrfJudgments(string _type) {
    if (_type == "2options" || _type == "4options")
        type = _type;
    else {
        Rprintf(" Allowed type:\n 2options or 4options");
        exit(0);
    }

}

PrfJudgments::PrfJudgments(string type, string qrels) {
}

vector<int> PrfJudgments::getQueries() {
    vector<int> queries;
    map<int, Qrels>::iterator it = qrels_list.begin();

    for (; it != qrels_list.end(); ++it)
        queries.push_back(it->first);
    return queries;
}

SEXP PrfJudgments::getNonRelDocuments(int queryID) {
    map<int, Qrels>::iterator it = qrels_list.find(queryID);
    if (it != qrels_list.end())
        return Rcpp::wrap(qrels_list[queryID].non_rel_docs);
    else {
        Rprintf("Query ID does not exist in the Qrels\n");
        return R_NilValue;
        ;
    }
}

SEXP PrfJudgments::getEdges(int queryID) {
    map<int, Qrels>::iterator it = qrels_list.find(queryID);
    if (it != qrels_list.end()) {
        vector<string> left;
        vector<string> right;

        set<pair<string, string> >::iterator it =
                qrels_list[queryID].prfMatrix.pairs.begin();

        for (; it != qrels_list[queryID].prfMatrix.pairs.end(); ++it) {
            left.push_back(it->first);
            right.push_back(it->second);
        }
        return Rcpp::DataFrame::create(Named("left_docid") = left,
                Named("right_docid") = right);
    } else {
        Rprintf("Query ID does not exist in the Qrels\n");
        return R_NilValue;
    }
}

string PrfJudgments::getJudgment(int queryID, string left, string right) {
    map<int, Qrels>::iterator it;


    it = qrels_list.find(queryID);

    if (it != qrels_list.end()) {
        if (type == "2options")
            return getJudgment_2options(queryID, left, right);
        else
            return getJudgment_4options(queryID, left, right);

    } else {
        Rprintf("Query ID %d does not exist in the Qrels\n", queryID);
        return "ERROR";
    }

}

string PrfJudgments::getJudgment_2options(int queryID, string left, string right) {
    set < pair<string, string> > ::iterator iter_lr;
    set < pair<string, string> > ::iterator iter_rl;
    pair<string, string > lr_pair;
    pair<string, string > rl_pair;

    lr_pair = make_pair(left, right);
    rl_pair = make_pair(right, left);

    iter_lr = qrels_list[queryID].prfMatrix.pairs.find(lr_pair);
    iter_rl = qrels_list[queryID].prfMatrix.pairs.find(rl_pair);
    if (iter_lr == qrels_list[queryID].prfMatrix.pairs.end() &&
            iter_rl == qrels_list[queryID].prfMatrix.pairs.end()) {
        return "BOTH_NONREL";
    } else if (iter_lr == qrels_list[queryID].prfMatrix.pairs.end())
        return "LEFT";
    else if (iter_rl == qrels_list[queryID].prfMatrix.pairs.end())
        return "RIGHT";
    else
        return "BOTH_REL";
}

string PrfJudgments::getJudgment_4options(int queryID, string left, string right) {
    set < pair<string, string> > ::iterator iter_lr;
    set < pair<string, string> > ::iterator iter_rl;
    pair<string, string > lr_pair;
    pair<string, string > rl_pair;
    // Check if both documents are relevant
    if (qrels_list[queryID].rel_docs.find(left) != qrels_list[queryID].rel_docs.end()) {
        if (qrels_list[queryID].rel_docs.find(right) != qrels_list[queryID].rel_docs.end()) {
            lr_pair = make_pair(left, right);
            rl_pair = make_pair(right, left);

            iter_lr = qrels_list[queryID].prfMatrix.pairs.find(lr_pair);
            iter_rl = qrels_list[queryID].prfMatrix.pairs.find(rl_pair);
            if (iter_lr != qrels_list[queryID].prfMatrix.pairs.end() &&
                    iter_rl != qrels_list[queryID].prfMatrix.pairs.end())
                return "BOTH";
            else if (iter_lr != qrels_list[queryID].prfMatrix.pairs.end())
                return "RIGHT";
            else
                return "LEFT";

        } else
            return "RIGHT_NONREL";
    } else if (qrels_list[queryID].rel_docs.find(right) != qrels_list[queryID].rel_docs.end())
        return "LEFT_NONREL";
    else
        return "BOTH_NONREL";
}

void PrfJudgments::addPair(int queryID, string left, string right, string preference) {
    map<int, Qrels>::iterator it = qrels_list.find(queryID);
    if (it == qrels_list.end()) {
        Qrels q;
        qrels_list.insert(make_pair(queryID, q));
    }
    if (type == "2options")
        qrels_list[queryID].add_judgment_2options(left, right, preference);
    else if (type == "4options")
        qrels_list[queryID].add_judgment_4options(left, right, preference);

}

void PrfJudgments::addPairs(int queryID, CharacterMatrix pairs) {
    map<int, Qrels>::iterator it = qrels_list.find(queryID);
    if (it == qrels_list.end()) {
        Qrels q;
        qrels_list.insert(make_pair(queryID, q));
    }
    for (int i = 0; i < pairs.nrow(); i++) {
        string left(pairs(i, 0));
        string right(pairs(i, 1));
        string preference(pairs(i, 2));
        if (type == "2options")
            qrels_list[queryID].add_judgment_2options(left, right, preference);
        else if (type == "4options")
            qrels_list[queryID].add_judgment_4options(left, right, preference);
    }



}

RCPP_MODULE(PrfJudgments) {
    using namespace Rcpp;

    class_<PrfJudgments > ("PrfJudgments")
            .constructor<string>()
            .constructor<string, string>()
            // read-only property
            .method("addPair", &PrfJudgments::addPair)
            .method("addPairs", &PrfJudgments::addPairs)
            .method("getQueries", &PrfJudgments::getQueries)
            .method("getNonRelDocs", &PrfJudgments::getNonRelDocuments)
            .method("getJudgment", &PrfJudgments::getJudgment)
            .method("getEdges", &PrfJudgments::getEdges);
}

