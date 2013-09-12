#include <iostream>
#include <fstream>
#include <RcppArmadillo.h>
using namespace Rcpp;
using namespace std;


class PrfJudgments{
private:
    struct PreferenceMatrix{
        vector<pair<string, string> > pairs;

        void addPair(string left, string right, string preference){
            if(preference == "left"){
                pairs.push_back(make_pair(right, left));
            } else if(preference == "right"){
                pairs.push_back(make_pair(left, right));
            } else if(preference == "both"){
                pairs.push_back(make_pair(left, right));
                pairs.push_back(make_pair(right, left));
            }
        }
    };

    struct Qrels{
        int queryID;
        PreferenceMatrix prfMatrix;
        map<string, int> docid_index;
        vector<string> non_rel_docs;
        //map<string, int> rel_docs;

        void add_pairwise_judgment(string left, string right, string preference){
            if(preference == "LEFT_NONREL" ) non_rel_docs.push_back(left);
            else if(preference == "RIGHT_NONREL") non_rel_docs.push_back(right);
            else if(preference == "BOTH_NONREL") {
                non_rel_docs.push_back(left);
                non_rel_docs.push_back(right);
            }else{
                prfMatrix.addPair(left, right, preference);
                //rel_docs[left] += 1;
                //rel_docs[right] += 1;
            }
        }

    };

    map<int, Qrels> qrels_list;

public:
    PrfJudgments(){}

    PrfJudgments(string qrelsPath){}

    vector<int> getQueries(){
        vector<int> queries;
        for(map<int,Qrels>::iterator it = qrels_list.begin(); it != qrels_list.end(); ++it)
          queries.push_back(it->first);
        return queries;
    }

    SEXP getNonRelDocuments(int queryID){
        map<int,Qrels>::iterator it = qrels_list.find(queryID);
        if(it != qrels_list.end())
            return Rcpp::wrap(qrels_list[queryID].non_rel_docs);
        else{
            Rprintf("Query ID does not exist in the Qrels\n");
            return R_NilValue;;
        }
    }

    SEXP getPairwiseJudgments(int queryID){
        map<int,Qrels>::iterator it = qrels_list.find(queryID);
        if(it != qrels_list.end()){
            vector<string> left;
            vector<string> right;
            for(vector<pair<string, string> >::iterator it = qrels_list[queryID].prfMatrix.pairs.begin();
                    it != qrels_list[queryID].prfMatrix.pairs.end(); ++it){
                left.push_back(it->first);
                right.push_back(it->second);
            }
            return Rcpp::DataFrame::create(Named("Left") =left,
                                           Named("Right")= right,
                                           Named("prf") = rep(1, right.size()));
        }
        else{
            Rprintf("Query ID does not exist in the Qrels\n");
            return R_NilValue;;
        }
    }

    void addPair(int queryID, string left, string right, string preference){
        map<int,Qrels>::iterator it = qrels_list.find(queryID);
        if(it == qrels_list.end()){
            Qrels q;
            qrels_list.insert(make_pair(queryID,q));
        }
        qrels_list[queryID].add_pairwise_judgment(left, right, preference);
    }


    void addPairs(int queryID, CharacterMatrix pairs){
        map<int,Qrels>::iterator it = qrels_list.find(queryID);
        if(it == qrels_list.end()){
            Qrels q;
            qrels_list.insert(make_pair(queryID,q));
        }
        for(int i= 0 ; i < pairs.nrow(); i++){
            string left(pairs(i,0));
            string right(pairs(i,1));
            string preference(pairs(i,2));
            qrels_list[queryID].add_pairwise_judgment(left, right, preference);
        }



    }
};




RCPP_MODULE(PrfJudgments) {
    using namespace Rcpp;

    class_<PrfJudgments > ("PrfJudgments")
        .constructor()
        .constructor<string>()
        // read-only property
        .method("addPair", &PrfJudgments::addPair)
        .method("addPairs", &PrfJudgments::addPairs)
        .method("getQueries", &PrfJudgments::getQueries)
        .method("getNonRelDocs", &PrfJudgments::getNonRelDocuments)
        .method("getPairwiseJudgments", &PrfJudgments::getPairwiseJudgments);
}

