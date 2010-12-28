<?php
/**
 *
 */
class Facebook_ArticleStats {
  	 protected $resultsObj;
	 protected $getter;
	 protected $urls;
	 protected $urlTemplate;

	 function __construct(UrlGetter $getter, Urls $urls) {
		 $this->getter = $getter;
		 $this->urls = $urls;
		 $this->urlTemplate = 'http://api.facebook.com/restserver.php?method=links.getStats&urls=http://www.[stub].org/article/info%3Adoi%2F[doi]';
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
		 if (!isset($this->resultsObj->facebook)){
			 $this->resultsObj->facebook = new stdClass();
		 }
		 if (!isset($this->resultsObj->facebook->old_stats)){
			 $this->resultsObj->facebook->old_stats = new stdClass();
		 }

		 $doi = $this->resultsObj->_id; // fix this. can't always assume _id will be doi.
		 $encodedDoi = urlencode($doi);
		 $url = $this->urls->makeUrl($encodedDoi, $this->urlTemplate);
		 $this->getter->setUrl( $url );

		 $timestamp = date('c');
		 $this->resultsObj->facebook->last_update = $timestamp;

		 // get the data from the api:
		 try {
			$simpleXmlResponse = $this->getter->get(); // returns a SimpleXMLElement obj

		 } catch(Exception $e) {
			 throw new Exception("Facebook didn't return stats for "
					. $url . "\n"
					. "Facebook response: "
					. (isset($simpleXmlResponse)) ? (string)$simpleXmlResponse : "no response recorded"
						);
		 }
		 
		 // parse the stats from the xml:
		 $stats = new stdClass();
		 $linkStat = $simpleXmlResponse->link_stat;
		 $stats->share_count = (int)$linkStat->share_count;
		 $stats->like_count = (int)$linkStat->like_count;
		 $stats->comment_count = (int)$linkStat->comment_count;
		 $stats->total_count = (int)$linkStat->total_count;
		 $stats->click_count = (int)$linkStat->click_count;

		 // put the stats in the object
		 $this->resultsObj->facebook->old_stats->{$timestamp} = $stats;
		 $this->resultsObj->facebook->current_stats = $stats;

		 $changes = (array_sum((array)$stats)) ? "added full Facebook results" : "added blank Facebook results";
		 $this->resultsObj->latest_changes = $changes;

		 return $this->resultsObj;
	 }

}
?>
