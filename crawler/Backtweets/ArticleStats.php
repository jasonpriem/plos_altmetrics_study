<?php

class Backtweets_ArticleStats {
  	 protected $resultsObj;
	 protected $getter;
	 protected $urls;
	 protected $searchResults;

	 function __construct(UrlGetter $getter, Urls $urls) {
		 $this->getter = $getter;
		 $this->urls = $urls;
	 }

	 public function setResultsObj(stdClass $resultsObj) {
		 if (!isset($resultsObj->_rev) || !isset($resultsObj->_id)) {
			 throw new Exception("No CouchDb document to put results into.");
		 }
		 $this->resultsObj = $resultsObj;
	 }

	 public function setSearchResults(Array $results){
		 if (!$results) throw new Exception("trying to set search results with empty array.");
		 foreach ($results as &$result){
			 $this->addTargetsPropertyToSearchResult($result);
		 }
		 $this->searchResults = $results;
	 }

	 public function getInfo() {
		 if (!isset($this->resultsObj)) {
			 throw new Exception("No document to put results into.");
		 }

		 if (!isset($this->searchResults)) {
			 throw new Exception ('no search results set.');
		 }

		 if (!isset($this->resultsObj->backtweets)){
			 $this->resultsObj->backtweets = new stdClass();
		 }
		 
		 if (!isset($this->resultsObj->backtweets->tweets)){
			 $this->resultsObj->backtweets->tweets = array();
		 }		 

		 unset($this->resultsObj->backtweets->lastUpdate); // houskeeping; remove after one run.
		 unset($this->resultsObj->backtweets->latestChanges); // houskeeping; remove after one run.
		 
		 $this->resultsObj->backtweets->last_update = date('c');
		 $citingTweets = $this->findTweetsTargetingDoi($this->resultsObj->_id); // shouldn't rely on the id to be the doi.
		 $citingTweets = $this->keyTweetsByNativeId($citingTweets);
		 $this->resultsObj->backtweets->tweets =
					array_merge((array)$this->resultsObj->backtweets->tweets, (array)$citingTweets);
		 $this->resultsObj->latest_changes = count($citingTweets) . " tweets added.";
		 return $this->resultsObj;
	 }

	 private function findTweetsTargetingDoi($doi){
		 $hits = array();
		 foreach ($this->searchResults as $tweet) {
			 $targets = array_flip($tweet->targets); // an array keyed by targets
			 if (isset($targets[$doi])) {
				$hits[] = $tweet;
			 }
		 }
		 return $hits;
	 }
	 private function keyTweetsByNativeId(Array $tweets){
		 $newTweets = array();
		 foreach ($tweets as $k => $tweet) {
			 $newTweets[(string)$tweet->tweet_id] = $tweet;
		 }
		 return $newTweets;
	 }

	 private function addTargetsPropertyToSearchResult(stdClass $searchResult) {
		 $searchResult->targets = array();
		 // this wil return the "href='...'" along with the links, but that's ok.
		 $regex = '#\bhref\s*=\s*(?:"[^"]*"|\'[^\']\'|\S+)#';
		 $links = preg_match_all($regex, $searchResult->tweet_text, $m);
		 foreach ($m[0] as $url){
			 $uri = $this->urls->plosDoiFromUrl($url);
			  if ($uri) $searchResult->targets[] = $uri;
		 }
//		 if (!$searchResult->targets) throw new Exception("Can't find a PLoS doi in the tweet: '{$searchResult->tweet_text}'");
		 return $searchResult;


	 }


}
?>
