library(evalIR, quietly=T)
library(MTurkR, quietly=T)
require(plyr, quietly=T)


source('paths.R')

pool.documents <- function(x, runs, pooling_depth=5){
  pooled_docs <- data.frame(docID=pooling.topk(runs$getRankMatrix(x), 
                                               pooling_depth))
  return(pooled_docs)
}

generate.triplets <- function(documents, triplet_folder){
  query <- unique(documents$query)
  documents$docID <- as.character(documents$docID)
  doc_ids <- documents$docID
  triplets <- ddply(documents, 
                    .(docID), 
                    function(x)  
                      t(combn(doc_ids[!doc_ids %in% c(x$docID)],2)))
  colnames(triplets) <- c('top_doc','left_doc','right_doc')
  triplets <- cbind(query= rep(query, nrow(triplets)),
                    tripletID= seq(1, nrow(triplets)),
                    triplets)
  write.table(triplets, file=paste0(triplet_folder, '/', query, '.dat'),
              quote=F, sep=' ', row.names=F, col.names=T, append=F)
}

load_nonreldocs <- function(nonrel_path, data_runs){
  runFiles <- list.files(path=nonrel_path, full.names=T)
  runIDs <- basename(runFiles)
  
  runs <- read.runs(runPaths= runFiles, runids= runIDs, limit= 5)
  nonrel <- do.call(c, sapply(runs$getQueries(), function(x) 
    rownames(runs$getRankMatrix(x))))
  
  
  rel <- do.call(c, sapply(data_runs$getQueries(), function(x) 
    rownames(data_runs$getRankMatrix(x))))
  nonrel_docs <- setdiff(nonrel, rel)
  
  write.table(nonrel, file=paste0(data_home,'/nonrel_documents.dat'),
              quote=F, sep=' ', row.names=F, col.names=T, append=F)
  
}

initializeTriplets <- function(runs_path, nonrel_path){
  # Intiliaziation
  runFiles <- list.files(path=runs_path, full.names=T)
  runIDs <- basename(runFiles)
  pooling_depth <- 5
  
  runs <- read.runs(runPaths= runFiles, runids= runIDs, limit= 1000)
  
  # Document Pooling
  pooled_docs <- ddply(data.frame(query=runs$getQueries()), .(query), 
                       function(x) pool.documents(x$query, runs, pooling_depth))
  

  
  # Generate Triplets and store to file
  triplet_folder <- paste0(data_home, '/tripletIDs')
  d_ply(pooled_docs, .(query), generate.triplets, triplet_folder)
  
  # Create a list of non-relevant documents for the traps
  load_nonreldocs(nonrel_path, runs)
}  

################################################################################


sample_basic <- function(query, sample_size){
  # Read the triplets from the Triplets Folder
  triplet_folder <- paste0(data_home, '/tripletIDs')
  triplets <- read.table(paste0(triplet_folder, '/', query, '.dat'), header=T)
  
  sample_size <- min(nrow(triplets), sample_size)
  sample_size <- sample_size - (sample_size %% 4)
  sampled_triplets <- triplets[sample(triplets$tripletID, size=sample_size),]
  return(sampled_triplets)
}

addTrap <- function(curTriplet){
  # Pick one randomly and make it a trap 
  trap <- curTriplet[sample(1:nrow(curTriplet), 1),]
  
  trap$tripletID <- -1
  
  if(runif(1) < 0.5){ # Identical Trap
    # Randomly pick left of right
    ifelse(runif(1) < 0.5, trap$left_doc <- trap$top_doc, 
           trap$right_doc <- trap$top_doc)
  }else{ # Irrelevant Trap
    irrel <- sample(nonrel_documents, 1)
    ifelse(runif(1) < 0.5, trap$left_doc <- irrel, trap$right_doc <- irrel)
  }
  return(trap)
}

generateHIT <- function(sampled_triplets){
  query <- unique(sampled_triplets$query)
  print(paste(query,nrow(sampled_triplets),  sep=' '))
  
  hits <- adply(seq(1,nrow(sampled_triplets),4), 1, function(x) 
    rbind(sampled_triplets[x:(x+3),],addTrap(sampled_triplets[x:(x+3),])))
  
  colnames(hits)[1] <- c('hitID')
  sampled_folder <- paste0(data_home, '/sampledTriplets/', batch_number)
  write.table(hits, file=paste0(sampled_folder, '/', query, '.dat'),
              quote=F, sep=' ', row.names=F, col.names=T, append=F)
}

createBatch <- function(batch_number, sample_size=100){  
  
  sampled_folder <- paste0(data_home, '/sampledTriplets/', batch_number)
  if (file.exists(sampled_folder)){
    print(paste("A directory already exists for this batch.",
                "Script will exit to protect data loss."))
  } else {
    dir.create(file.path(sampled_folder))
  }
  
  triplet_folder <- paste0(data_home, '/tripletIDs')
  queries <- as.numeric(gsub('.dat', '', list.files(triplet_folder)))
  
  
  
  # Create HITs
  
  #   1. Open Triplets file and sample triplets
  sampled_triplets <- adply(queries, 1, sample_basic, sample_size)
  # Removing unnecessary columns created by adply
  sampled_triplets <- sampled_triplets[, 2:ncol(sampled_triplets)]
  
  
  #   2. Add trap to the HIT and assign HIT ID
  #   3. Store tripletIDs of the sampled triplets
  #      Triplet IDs for the traps are set to -1
  
  nonrel_documents <- read.table(paste0(data_home,'/nonrel_documents.dat'),
                                 header=T)[,1]
  nonrel_documents <<- as.character(nonrel_documents)
  hits <- ddply(sampled_triplets, .(query), generateHIT)
  
}

createParam <- function(hit_dataframe, topicDesc){  
  query <- unique(hit_dataframe$query)
  return(c(paste0(baseURL, hit_dataframe$left_doc, '.html'),
           paste0(baseURL, hit_dataframe$right_doc, '.html'),
           paste0(baseURL, hit_dataframe$top_doc, '.html'),
           as.character(topicDesc[[as.character(query)]]$text),
           as.character(topicDesc[[as.character(query)]]$desc)))
}

getHitType <- function(){
  qualReqs <- paste0(GenerateQualificationRequirement("2XPGKCCVLL3CO7K5MGGQ7C9NQVT6WC",">=","30"),
                     # Worker_PercentAssignmentsApproved
                     GenerateQualificationRequirement("000000000000000000L0", 
                                                      ">", "90",qual.number=2))
  
  hittype <- RegisterHITType(keypair= credentials(amtKey),
                             title="Document Preference",
                             description=paste("Read the document at the top and pick the document from the two documents shown below that gives most new information"),
                             reward="0.8",
                             duration=seconds(hours=2), 
                             qual.req=qualReqs,
                             keywords="web pages, search, preference, opinion",
                             auto.approval.delay=seconds(days=15),
                             sandbox = T)
  return(hittype)
}

submitQuery <- function(batch_number, query){
  # Submit HITs to Mechanical Turk and store the AMT HIT details 
  amt_hit_folder <- paste0(data_home, '/submittedHITS/', batch_number)
  ifelse (file.exists(amt_hit_folder),  
          print("A directory already exists for this batch."),  
          dir.create(file.path(amt_hit_folder)))
  
  
  sampled_folder <- paste0(data_home, '/sampledTriplets/', batch_number)
  
  sampled_hits <- read.table(paste0(sampled_folder, '/',  query, '.dat'),
                             header=T)
  hittype <- getHitType()
  topicDesc <- parseWebTopicDesc(topicsFile)
  
  submit.HIT <- function(hit_triplets, topicDesc, batch_number){
    query <- unique(hit_triplets$query)
    hit_triplets <- hit_triplets[sample(nrow(hit_triplets)),]
    nameText <- c('leftdoc1', 'leftdoc2', 'leftdoc3', 'leftdoc4', 'leftdoc5', 
               'rightdoc1', 'rightdoc2', 'rightdoc3', 'rightdoc4', 'rightdoc5', 
               'topdoc1', 'topdoc2', 'topdoc3', 'topdoc4', 'topdoc5',
               'query', 'queryExp')
    
    values <- createParam(hit_triplets, topicDesc)
    hit_id <- CreateHIT(hitlayoutid="2I3NMHGYQ4J31ESPGLDFIOYYIVOM9P",
                        hit.type=hittype$HITTypeId,
                        sandbox=T,
                        hitlayoutparameters= GenerateHITLayoutParameter(nameText, 
                                                                        values),
                        keypair= credentials(amtKey),
                        annotation = batch_number,
                        assignments = "3",
                        expiration=seconds(days=10))  
    
    res <-   t(data.frame(c(values[1:15], as.character(query))))
    colnames(res) <- nameText[1:16]
    res <- cbind(res, AMT_HITId=c(hit_id$HITId), AMT_HITTypeID=c(hit_id$HITTypeId))
    return(res)
  }
  hitids <- ddply(sampled_hits, .(hitID), submit.HIT, topicDesc, batch_number)
  
  
  write.table(hitids, file=paste0(amt_hit_folder, '/', query, '.dat'),
              quote=F, sep=' ', row.names=F, col.names=T, append=F)
  
}



## Running the experiment
#initializeTriplets(paste0(data_home,'/data/trec2009'), paste0(data_home, '/data/trec2010'))


# initializeTriplets('~/work/data/runs/diversity/trec2012', 
#                   paste0(data_home, '/data/trec2009'))


# batch_number <- 1
# createBatch(batch_number, sample_size=100)
# sumbitQuery(batch_number, 1)