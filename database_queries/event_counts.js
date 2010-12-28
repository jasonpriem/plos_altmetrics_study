function(doc){
 totalViews = function(viewsObj, viewType){
  var total = 0;
  for (i in viewsObj) {
   total += parseInt(viewsObj[i][viewType]);
  }
  return total;
 }
 
 count = function(obj){
  if (typeof obj == "number") {
   return obj;
  }
  else if (typeof obj == "string") {
   return obj;
  }
  else if (typeof obj == "array") {
   return obj.length;
  }
  else if (typeof obj == "object") {
   var size = 0, key;
    for (key in obj) {
     if (obj.hasOwnProperty(key)) size++;
    }
    return size;   
	}
  else {
   return 0;
  }
 }

 cleanText = function(dirty){
   dirtRegex = /\n|\r|\t|\| +/gi
   clean = [];
   if (typeof dirty == "object"){
    for (var i in dirty) {
     clean.push(dirty[i].replace(dirtRegex, " ").replace(/ +/g, " "));
    }
   }
   else if (typeof dirty == "string"){
	   clean = dirty.replace(dirtRegex, " ").replace(/ +/g, " ");
   }
   else {
	    clean = " ";
   }
   return clean;
 }
 
 var ret = {};
 if (doc.pubDate) {
  ret.doi                         = doc._id;
  ret.pubDate                     = doc.pubDate;
  ret.journal                     = doc.namespace.slice(-4)
  ret.articleType                 = (doc.articleType) ? cleanText(doc.articleType) : "NA";
  ret.authorsCount                = (doc.authors) ? count(doc.authors) : "NA";
  ret.f1000Factor                 = doc.F1000.factor;
  ret.backtweetsCount             = (doc.backtweets) ? count(doc.backtweets.tweets) : "NA";
  ret.deliciousCount              = (doc.delicious) ? count(doc.delicious.bookmarks) : "NA";
  ret.pmid                        = (doc.pmid) ? doc.pmid : "NA";
  ret.plosSubjectTags             = (doc.subjects) ? cleanText(doc.subjects).join("|") : "NA";
  ret.plosSubSubjectTags          = (doc.subSubjects) ? cleanText(doc.subSubjects).join("|") : "NA";
  
    
  // Facebook
  if (doc.facebook.current_stats) {
   ret.facebookShareCount         = count(doc.facebook.current_stats.share_count);
   ret.facebookLikeCount          = count(doc.facebook.current_stats.like_count);
   ret.facebookCommentCount       = count(doc.facebook.current_stats.comment_count);
   ret.facebookClickCount         = count(doc.facebook.current_stats.click_count);
  }
  else {
   ret.facebookShareCount         = "NA";
   ret.facebookLikeCount          = "NA";
   ret.facebookCommentCount       = "NA";
   ret.facebookClickCount         = "NA"; 
  }
  
  
  // Mendeley
  try {
   if (doc.mendeley.article_info == {}) {
       ret.mendeleyReadersCount   = 0;
	}
   else {
	    ret.mendeleyReadersCount   = count(doc.mendeley.article_info.stats.readers);
	}
  } catch(e) {
       ret.mendeleyReadersCount   = "NA";
  }
  
  
  // PLoS ALM
  if (doc.plos_alm.current_stats){
   ret.almBlogsCount              = count(doc.plos_alm.current_stats.blogs);
   ret.pdfDownloadsCount          = totalViews(doc.plos_alm.current_stats.views, "pdf");
   ret.xmlDownloadsCount          = totalViews(doc.plos_alm.current_stats.views, "xml");
   ret.htmlDownloadsCount         = totalViews(doc.plos_alm.current_stats.views, "html");
   ret.almCiteULikeCount          = count(doc.plos_alm.current_stats.citeulike);
   ret.almScopusCount             = count(doc.plos_alm.current_stats.scopus);
   ret.almPubMedCentralCount      = count(doc.plos_alm.current_stats.pub_med);
   ret.almCrossRefCount           = count(doc.plos_alm.current_stats.crossref);
  }
  else {
   ret.almBlogsCount              = "NA";
   ret.pdfDownloadsCount          = "NA";
   ret.xmlDownloadsCount          = "NA";
   ret.htmlDownloadsCount         = "NA";
   ret.almCiteULikeCount          = "NA";
   ret.almScopusCount             = "NA";
   ret.almPubMedCentralCount      = "NA";
   ret.almCrossRefCount           = "NA";
  }
  
  // PLoS comments
  if (doc.plos_comments.comments){
   ret.plosCommentCount           = count(doc.plos_comments.comments);
   ret.plosCommentResponsesCount  = count(doc.plos_comments.comments.filter(function(x){ return x.position > 0; }));
  }
  else {
   ret.plosCommentCount           = "NA";
   ret.plosCommentResponsesCount  = "NA";
  }
  
  // wikipedia
  if (doc.wikipedia) {
   ret.wikipediaCites             = count(doc.wikipedia.latest_stats.filter(function(x){ return x.ns === 0; }));
  }
  else {
   ret.wikipediaCites             = "NA";
  }
  
  emit(doc._id, ret);
 }
}

