<?php
/**
 * This is different from the other ArticleStats classes becuase it doesn't
 * add anything; it's just to remove stuff from documents.
 *
 */
class Curator_ArticleStats {
  	 protected $resultsObj;
	 protected $getter;
	 protected $urls;

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

	 public function getInfo() {
		$oldPubDate = $this->resultsObj->pubDate;
		if (is_int($oldPubDate)) { // oldPubDate is a timestamp
			$newPubDate = date('c', $oldPubDate);
			$this->resultsObj->pubDate = $newPubDate;
			$this->resultsObj->latest_changes = "changed pubDate to ISO 8601";
		}
		else {
			throw new Exception("pubDate '$oldPubDate' isn't a Unix timestamp.");

		}
		return $this->resultsObj;
	 }
}
?>
