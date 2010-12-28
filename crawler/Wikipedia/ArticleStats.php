<?php

class Wikipedia_ArticleStats {
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
		 $this->searchResults = $results;
	 }

	 public function getInfo() {
		 if (!isset($this->resultsObj)) {
			 throw new Exception("No document to put results into.");
		 }

		 if (!isset($this->searchResults)) {
			 throw new Exception ('no search results set.');
		 }

		 if (!isset($this->resultsObj->wikipedia)){
			 $this->resultsObj->wikipedia = new stdClass();
		 }

		 if (!isset($this->resultsObj->wikipedia->old_stats)){
			 $this->resultsObj->wikipedia->old_stats = new stdClass();
		 }
		 $timestamp = date('c');
		 $this->resultsObj->wikipedia->last_update = $timestamp;

		 $citations = $this->findArticlesThatCiteDoi($this->resultsObj->_id); // shouldn't rely on the id to be the doi.
		 $this->resultsObj->wikipedia->old_stats->{$timestamp} =	$citations;
		 $this->resultsObj->wikipedia->latest_stats = $citations;

		 $this->resultsObj->latest_changes = count($citations) . " wikipedia citations added.";
		 return $this->resultsObj;
	 }

	 private function findArticlesThatCiteDoi($doi){
		 if (!isset($this->searchResults)) {
			 throw new Exception ('no search results set.');
		 }
		 $citations = array();
		 $urlEncodedDoi = urlencode($doi);
		 foreach ($this->searchResults as $k => $wikiArticle) {
			 if (isset($wikiArticle->url)) {
				 $urlEncodedDoiPos = strpos($wikiArticle->url, $urlEncodedDoi);
				 $doiPos = strpos($wikiArticle->url, $doi);
				 if ($urlEncodedDoiPos || $doiPos) {
					 $citations[] = $wikiArticle;
				 }
			 }
		 }
		 return $citations;
	 }

}
?>
