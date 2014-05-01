#!/usr/bin/env python
import subprocess 
import sys
import os
import collections


def run_trec_eval(qrels_path, run_path):
    process = subprocess.Popen(['trec_eval', '-q', '-mofficial' , qrels_path,
                                run_path], stdout=subprocess.PIPE)
    output = process.communicate()[0]

    return output

def trec_eval2table(trec_output):
    measures = collections.defaultdict(dict)
    queries = set()
    summary = {}
    runid = ''
    for line in trec_output.split('\n'):
        if len(line.split()) <= 0:
            continue
        measureName =  line.split()[0]
        
        if measureName in ['gm_bpref', 'num_q', 'gm_map', 'relstring']:
            continue
        elif measureName == 'runid':
            runid = line.split()[2]
            continue

        qid =  line.split()[1]
        score = float(line.split()[2])        
        
        if qid == 'all':
            summary[measureName] = score
        else:
            queries.add(int(qid))
            measures[measureName][int(qid)] = score
    
    header = 'runid, qid'
    for measure in sorted(measures.keys()):
        header +=  ', ' + measure
    header += '\n'

    results = ''    
    for qid in sorted(queries):
        results += runid.strip() + ', ' + str(qid)
        for measure in sorted(measures.keys()):
            results +=  ', ' + str(measures[measure][qid])
        results += '\n'
    print '\n'.join(header.split(','))        
    return (header + results)
        
        
    
if __name__ == '__main__':
    if len(sys.argv) <= 2:
        print 'Usage: python trec_eval_test.py qrels_path run_path'
        sys.exit()
        
    qrels_path = sys.argv[1]
    run_path = sys.argv[2]
    
    trec_eval_output = run_trec_eval(qrels_path, run_path)
    
    res = trec_eval2table(trec_eval_output)
    
    