library(RCurl)

fetchClueweb09_docs <- function(docs_list, output_folder){
  h = getCurlHandle(header = TRUE, userpwd = "udel-irl:udblue&gold", 
                    netrc = TRUE)
  base <- 'http://boston.lti.cs.cmu.edu:8085/clueweb09/render/renderpage.cgi?id='
  for(url in docs_list){
    write(getURL(paste(base,'',url,sep=''),  
                 verbose = F,
                 userpwd = "udel-irl:udblue&gold"), 
          file=paste(output_folder, '/', url,'.html', sep=''))
    
  }
}
