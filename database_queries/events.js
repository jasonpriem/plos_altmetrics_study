function(doc) {

 pubEvent = function(eventType, doi, creator, date, value, count){
	articlePubDateObj = new Date(doc.pubDate.replace(' ', 'T'));
   eventPubDateObj = new Date(date.replace(' ', 'T'));
	
  this.eventType = eventType;
  this.doi       = doi;
  this.creator   = creator;
  this.date      = date;
  this.latency   = (eventPubDateObj.getTime() - articlePubDateObj.getTime()) / 1000; //in seconds, not milliseconds
  if (value === null) {
   this.value    = "NA";
  }
  else if (typeof(value) == "string") {
   this.value = '"' + value.replace(/"/g, '""').replace(/\n|\r|\c/g,"") + '"';
  } 
  else {
   this.value = value.join('|'); // we're assuming it's an array
  }
  this.count     = count;
 }
 
 
 if (doc.pubDate) {
  for (i in doc.backtweets.tweets){
   var thisTweet = doc.backtweets.tweets[i];
	var tweetDate = thisTweet.tweet_created_at.replace(' ', 'T');
	var tweetText = thisTweet.tweet_text;
	thisEvent = new pubEvent("backtweets", doc._id, thisTweet.tweet_from_user_id, tweetDate, tweetText, 1 );
   emit(thisEvent.eventType, thisEvent);
  }
  
  for (i in doc.delicious.bookmarks){
   var thisDeliciousBookmark = doc.delicious.bookmarks[i];
   thisEvent = new pubEvent("delicious", doc._id, thisDeliciousBookmark.a, thisDeliciousBookmark.dt, thisDeliciousBookmark.t, 1 );
   emit(thisEvent.eventType, thisEvent);
  }
  
  if (doc.plos_alm.current_stats){
   for (i in doc.plos_alm.current_stats.citeulike) {
	 thisCiteULikeBookmark = doc.plos_alm.current_stats.citeulike[i]
    thisEvent = new pubEvent("citeulike", doc._id, thisCiteULikeBookmark.creator, thisCiteULikeBookmark.pubDate, thisCiteULikeBookmark.tags, 1);
    emit(thisEvent.eventType, thisEvent);
	}
	
	for (i in doc.plos_alm.current_stats.blogs) {
	 thisBlogPost = doc.plos_alm.current_stats.blogs[i];
    thisEvent = new pubEvent(thisBlogPost.from, doc._id, thisBlogPost.blogName, thisBlogPost.pubDate, thisBlogPost.uri, 1 );
    emit(thisEvent.eventType, thisEvent);
	}
	
	for (yearMonth in doc.plos_alm.current_stats.views){
	 for (viewType in doc.plos_alm.current_stats.views[yearMonth]){
     thisYear = yearMonth.slice(0,4);
     thisMonth = yearMonth.slice(5);
     thisMonthTs = new Date(thisYear, thisMonth, 0, 23, 59, 59).toISOString(); // last second in month	 
	  thisEvent = new pubEvent(viewType + " views", doc._id, "", thisMonthTs, null, doc.plos_alm.current_stats.views[yearMonth][viewType]); 
     emit(thisEvent.eventType, thisEvent);
	 }
   }
  }

  for  (i in doc.plos_comments.comments) {
   thisComment = doc.plos_comments.comments[i];
   thisEvent = new pubEvent("native comments", doc._id, thisComment.author, thisComment.pub_date, thisComment.content, 1 );
   
	emit(thisEvent.eventType, thisEvent);
  }
 }
}

