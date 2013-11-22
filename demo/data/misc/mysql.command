mysqlimport  -u praveen --fields-terminated-by=' '   --lines-terminated-by='\n' test clueweb_urls.txt --delete --local  --columns=url,id
