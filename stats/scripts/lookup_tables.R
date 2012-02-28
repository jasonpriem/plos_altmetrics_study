plos_journals = c("PLoS ONE",
    "PLoS Biology",
    "PLoS Medicine",
    "PLoS Genetics",
    "PLoS Computational Biology",
    "PLoS Pathogens",
    "PLoS Neglected Tropical Diseases")
names(plos_journals) = c("pone", "pbio", "pmed", "pgen", "pcbi", "ppat", "pntd")

WINDOW_WIDTH_IN_DAYS = 365


altmetricsColumns = c( "wosCountThru2010", "wosCountThru2011",
"almScopusCount",
"almPubMedCentralCount",
"almCrossRefCount",
"pdfDownloadsCount",        
"htmlDownloadsCount",    
"mendeleyReadersCount",     
"almCiteULikeCount",        
"plosCommentCount",         
"plosCommentResponsesCount",
"deliciousCount",
"almBlogsCount",            
"facebookCommentCount",          
"facebookLikeCount",          
"facebookShareCount",          
"facebookClickCount",          
"f1000Factor",              
"wikipediaCites",           
"backtweetsCount")

prettyAltmetricsColumns = c(
    "Web of Science cites thru 2010", 
    "Web of Science cites thru 2011", 
    "Scopus cites",
    "PubMed Central cites",
    "CrossRef cites",
    "PDF downloads",
    "HTML pageviews", 
    "Mendeley saves", 
    "CiteULike saves", 
    "PLoS comments", 
    "PLoS comment responses", 
    "Delicious bookmarks", 
    "Blog mentions", 
    "Facebook comments", 
    "Facebook likes",
    "Facebook shares", 
    "Facebook clicks",
    "F1000 rating", 
    "Wikipedia cites", 
    "Twitter mentions")
names(prettyAltmetricsColumns) = altmetricsColumns

prettyETypeColumns = c("HTML pageviews", 
    "PDF downloads",
    "PLoS comments", 
    "CiteULike saves", 
    "Twitter mentions",
    "Delicious bookmarks")
names(prettyETypeColumns) = c("html views", "pdf views", "native comments", "citeulike", "backtweets", "delicious")
