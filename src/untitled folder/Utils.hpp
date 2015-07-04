/*
 * File:   Utils.hpp
 * Author: praveen
 *
 * Created on May 17, 2014, 3:57 AM
 */

#ifndef UTILS_H
#define	UTILS_H


#include <iostream>
#include <fstream>
#include <RcppArmadillo.h>
using namespace std;
using namespace Rcpp;

class Utils {
public:

    template <typename K, typename V>
    static V find(const std::map <K, V> & m, const K & key, const V & defval) {
        typename std::map<K, V>::const_iterator it = m.find(key);
        if (it == m.end()) {
            return defval;
        } else {
            return it->second;
        }
    }
};


#endif	/* UTILS_H */

