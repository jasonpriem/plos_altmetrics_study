<?php
/**
 *
 */
class Mendeley_ArticleStats {
  	 protected $resultsObj;
	 protected $getter;
	 protected $urls;
	 protected $urlTemplate;

	 function __construct(UrlGetter $getter, Urls $urls) {
		 $this->getter = $getter;
		 $this->urls = $urls;
		 $this->urlTemplate = 'http://www.mendeley.com/oapi/documents/details/[doi]?type=doi&consumer_key=' .MENDELEY_KEY;
	 }
	 
	 
	 public function setResultsObj(stdClass $resultsObj) {
		 if (!isset($resultsObj->_rev) || !isset($resultsObj->_id)) {
			 throw new Exception("No CouchDb document to put results into.");
		 }
		 $this->resultsObj = $resultsObj;
	 }
	 
	 public function getInfo() {
		 if (!$this->resultsObj) {
			 throw new Exception("No document to put results into.");
		 }
		 if (!isset($this->resultsObj->mendeley)){
			 $this->resultsObj->mendeley = new stdClass();
		 }
		 if (!isset($this->resultsObj->mendeley->stats)){
			 $this->resultsObj->mendeley->stats = new stdClass();
		 }

		 $doi = $this->resultsObj->_id; // fix this. can't always assume _id will be doi.
		 $doubleUrlEncodedUri = urlencode( urlencode( $doi ) ); // for some readon the mendeley api needs this
		 $url = $this->urls->makeUrl($doubleUrlEncodedUri, $this->urlTemplate);
		 $this->getter->setUrl( $url );

		 $timestamp = date('c');
		 $this->resultsObj->mendeley->last_update = $timestamp;

		 $json = $this->getter->curlGet();
		 $mendeleyResult = json_decode($json);
		 if ($this->getter->getHttpReturnCode() < 400 && !isset($mendeleyResult->error)) { // good response
			 unset($mendeleyResult->abstract); // no point in storing this, and it's big.
				$this->resultsObj->mendeley->stats->{$timestamp} = $mendeleyResult->stats;
		 }
		 else { // mendeley returned an error code
			 if (in_array($this->getter->getHttpReturnCode(), array(404, 200))) { // not found, but that's ok; it's an empty object
				 $mendeleyResult = new stdClass();
			 }
			 else { // this is an error that should stop things--probably bumped against the rate limit.
				 throw new Exception("Mendeley didn't return stats for "
						. $url . "\n"
						. "Mendeley response: "
						. $this->getter->getHttpReturnCode() . ": "
						. print_r($mendeleyResult, true));
			 }
		 }

		 $this->resultsObj->mendeley->article_info = $mendeleyResult;

		 $changes = (count((array)$mendeleyResult)) ? "added full Mendeley results" : "added blank Mendeley results";
		 $this->resultsObj->latest_changes = $changes;

		 return $this->resultsObj;
	 }

	 public function deleteMendeley() {
		 unset($this->resultsObj->mendeley);
		 $this->resultsObj->latest_changes = "deleted mendeley.";
		 return $this->resultsObj;
	 }
		
}
?>
