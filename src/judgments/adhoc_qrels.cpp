#include "utils.h"

using namespace Rcpp;
using namespace std;

class AdhocQrels{
private:

    struct Qrels{
        int queryID;
        map<string, double> grades;
        vector<string> non_rel_docs;

        void addDocument(string _docid, double _grade){
            if (_grade > 0)
                grades.insert(make_pair(_docid, _grade));
            else
                non_rel_docs.push_back(_docid);
        }
    };

    map<int, Qrels> qrels_list;

public:

    AdhocQrels(string qrelsPath){
        ifstream qrelsFile(qrelsPath.c_str(), ios_base::in);
        string docid, line;
        int query, q0, curQuery;
        double rel;
        Qrels qrels;

        bool first = true;
        while (std::getline(qrelsFile, line))
        {
            std::istringstream iss(line);
            iss >> query >> q0 >> docid >> rel;
            if(first){
                curQuery = query;
                qrels.queryID = curQuery;
                qrels.grades.clear();
                qrels.non_rel_docs.clear();
                first = false;
            }
            if(curQuery != query){

                qrels_list.insert(make_pair(curQuery, qrels));
                curQuery = query;
                qrels.queryID = curQuery;
                qrels.grades.clear();
                qrels.non_rel_docs.clear();
            }
            qrels.addDocument(docid, rel);
        }
        qrels_list.insert(make_pair(curQuery, qrels));

    }

    vector<int> getQueries(){
        vector<int> queries;
        for(map<int,Qrels>::iterator it = qrels_list.begin(); it != qrels_list.end(); ++it)
          queries.push_back(it->first);
        return queries;
    }

    SEXP getGrades(int queryID){
        map<int,Qrels>::iterator it = qrels_list.find(queryID);
        if(it != qrels_list.end())
            return Rcpp::wrap(qrels_list[queryID].grades);
        else{
            Rprintf("Query ID does not exist in the Qrels\n");
            return R_NilValue;;
        }
    }


    void setGrades(int queryID, vector<string> documents, vector<double> grades){
        map<int,Qrels>::iterator it = qrels_list.find(queryID);
        if(it != qrels_list.end()){
            qrels_list[queryID].grades.clear();
            for(int i=0;i < documents.size(); i++){
              qrels_list[queryID].grades[documents.at(i)] = grades.at(i);
            }
        }
        else
            Rprintf("Query ID does not exist in the Qrels\n");
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

    SEXP judgePair(int queryID, CharacterMatrix pairs, string ties){
        map<int,Qrels>::iterator it = qrels_list.find(queryID);
        srand (time(NULL));
        CharacterVector judgedGrades(pairs.nrow());
        if(it != qrels_list.end()){

            for(int i= 0 ; i < pairs.nrow(); i++){
                string docLeft(pairs(i,0));
                string docRight(pairs(i,1));
                map<string, double>::iterator itL =
                                qrels_list[queryID].grades.find(docLeft);
                map<string, double>::iterator itR =
                                qrels_list[queryID].grades.find(docRight);

                if(itL == qrels_list[queryID].grades.end() &&
                        itR == qrels_list[queryID].grades.end())
                    judgedGrades(i) = "BOTH_NONREL";
                else if(itL == qrels_list[queryID].grades.end())
                    judgedGrades(i) = "LEFT_NONREL";
                else if(itR == qrels_list[queryID].grades.end())
                    judgedGrades(i) = "RIGHT_NONREL";
                else{

                    if(itL->second == itR->second){
                        if(ties == "random"){
                            int num = rand() % 10 + 1;
                            if(num <= 5)
                                judgedGrades(i) = "left";
                            else
                                judgedGrades(i) = "right";
                        }
                        else
                            judgedGrades(i) = "both";
                    }
                    else if(itL->second > itR->second) judgedGrades(i) = "left";
                    else if(itL->second < itR->second) judgedGrades(i) = "right";
                }
            }
            return judgedGrades;
        }
        else{
            Rprintf("Query ID does not exist in the Qrels\n");
            return R_NilValue;;
        }
    }

    SEXP judgeQuery(int queryID, vector<string> run){
        map<int,Qrels>::iterator it = qrels_list.find(queryID);
        NumericVector judgedGrades(run.size());
        if(it != qrels_list.end()){
            for(int i=0; i < run.size(); i++){
                map<string, double>::iterator it =
                                qrels_list[queryID].grades.find(run.at(i));
                if(it != qrels_list[queryID].grades.end())
                    judgedGrades(i) = qrels_list[queryID].grades[run.at(i)];
                else
                    judgedGrades(i) = 0;
            }
            judgedGrades.attr("names") = run;
            return judgedGrades;
        }
        else{
            Rprintf("Query ID does not exist in the Qrels\n");
            return R_NilValue;;
        }
    }
};


RCPP_MODULE(AdhocQrels) {
    using namespace Rcpp;

    class_<AdhocQrels > ("AdhocQrels")
        .constructor<string>("Default Constructor")
        // read-only property
        .method("getQueries", &AdhocQrels::getQueries)
        .method("getNonRelDocs", &AdhocQrels::getNonRelDocuments)
        .method("getGrades", &AdhocQrels::getGrades)
        .method("setGrades", &AdhocQrels::setGrades)
        .method("judgeQuery", &AdhocQrels::judgeQuery)
        .method("judgePair", &AdhocQrels::judgePair);
}

