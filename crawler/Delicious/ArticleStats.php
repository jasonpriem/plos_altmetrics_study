<?php
/**
 *
 */
class Delicious_ArticleStats {
  	 protected $resultsObj;
	 protected $getter;
	 protected $urls;
	 protected $urlTemplate;
	 protected $plosUrlTemplate;

	 function __construct(UrlGetter $getter, Urls $urls) {
		 $this->getter = $getter;
		 $this->urls = $urls;
		 $this->urlTemplate = 'http://feeds.delicious.com/v2/json/url/[plosUrlHash]';
		 $this->plosUrlTemplate = 'http://www.[stub].org/article/[prefixPlusDoi]';
	 }


	 public function setResultsObj(stdClass $resultsObj) {
		 if (!isset($resultsObj->_rev) || !isset($resultsObj->_id)) {
			 throw new Exception("No CouchDb document to put results into.");
		 }
		 $this->resultsObj = $resultsObj;
	 }

	 public function getInfo() {

		 // set up the object
		 if (!$this->resultsObj) {
			 throw new Exception("No document to put results into.");
		 }
		 if (!isset($this->resultsObj->delicious)){
			 $this->resultsObj->delicious = new stdClass();
		 }
		 if (!isset($this->resultsObj->delicious->bookmarks)){
			 $this->resultsObj->delicious->bookmarks = array();
		 }
		 $OrigBookmarksCount = count($this->resultsObj->delicious->bookmarks);

		 // setup the dois.  some people bookmark the urlencoded PLoS url version, some the plain one,
		 //	so we have to search for both.
		 $doi = $this->resultsObj->_id; // fix this. can't always assume _id will be doi.
		 $doiPlusPrefix = "info:doi/" . $doi;
		 $doiVersions = array($doiPlusPrefix, urlencode($doiPlusPrefix));

		 foreach ($doiVersions as $doiPlus){

			 //setup the url
			 $plosUrl = $this->urls->makeUrl($doiPlus, $this->plosUrlTemplate);
			 $deliciousUrl = $this->urls->makeUrl(md5($plosUrl), $this->urlTemplate);

			 //get the data from the url
			 $this->getter->setUrl( $deliciousUrl );
			 $json = $this->getter->curlGet();
			 $deliciousResult = json_decode($json);


			 // add the new bookmarks
			 if ($this->getter->getHttpReturnCode() < 400 && is_array($deliciousResult)) {
				 $allBookmarks = array_merge(
							$this->resultsObj->delicious->bookmarks,
							$deliciousResult
							);
				 $this->resultsObj->delicious->bookmarks =
							array_values(
								array_intersect_key(
										  $allBookmarks,
										  array_unique( array_map('serialize', $allBookmarks) )
										  )
									  );
			 }
			 else {
				 throw new Exception("no result returned from delicious for "
							. $deliciousUrl
							. " ($plosUrl). "
							. "Response: "
							. $this->getter->getHttpReturnCode() . ": "
							. print_r($deliciousResult, true)
							);
			 }
		 }

		 $this->resultsObj->delicious->last_update = date('c');
		 $newBookmarksCount = count($this->resultsObj->delicious->bookmarks) - $OrigBookmarksCount;

		 $this->resultsObj->latest_changes = "Added $newBookmarksCount new delicious bookmarks.";

		 return $this->resultsObj;
	 }


}
?>
