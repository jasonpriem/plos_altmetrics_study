<?php
/**
 *
 */
class F1000_ArticleStats {
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

		 // set up the object
		 if (!$this->resultsObj) {
			 throw new Exception("No document to put results into.");
		 }
		 if (!isset($this->resultsObj->F1000)){
			 $this->resultsObj->F1000 = new stdClass();
		 }

		 // this is a bit of a hack, as this set of F1000 factors is a one-off.
		 // It's just a serialized array of PMID => F1000_Factor
		 $factorsList = file_get_contents('./F1000/f1000-factors.txt');
		 $factorsArr = unserialize($factorsList);
		 $pmid = (isset($this->resultsObj->pmid)) ? $this->resultsObj->pmid : 0;

		 $factor = (isset($factorsArr[$pmid])) ? $factorsArr[$pmid] : false;
		 $this->resultsObj->F1000->factor = $factor;

		 $this->resultsObj->F1000->last_update = date('c');
		 $this->resultsObj->latest_changes = "Added F1000 factor: $factor";

		 return $this->resultsObj;
	 }


}
?>
