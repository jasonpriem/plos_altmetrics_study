<?php
/**
 * Delivers tweet objects suitable for putting in the db
 *
 */
class Wikipedia_TermSearch implements Iterator{
	 private $getter;
	 private $searchUrlTemplate;
	 private $key;
	 private $rowIndex;
	 private $rows;
	 private $offset;
	 private $searchTerm;
	 private $searchUrlTemplateWithTerm;
	 private $logFile;
	 private $itemsPerPage;

	 function __construct(UrlGetter $getter) {
		 $this->getter = $getter;
		 $this->itemsPerPage = 500;
		 $this->searchUrlTemplate = "http://en.wikipedia.org/w/api.php"
					.'?action=query'
					.'&list=exturlusage'
					.'&euquery=[searchTerm]'
					."&eulimit={$this->itemsPerPage}"
					.'&format=json'
					.'&euoffset=[offset]';
	 }
	 public function setSearchUrlTemplate($searchUrlTemplate) {
		 $this->searchUrlTemplate = $searchUrlTemplate;
	 }


	 public function setSearchTerm($searchTerm) {
		 $this->searchTerm = $searchTerm;
		 $this->searchUrlTemplateWithTerm = str_replace('[searchTerm]', $searchTerm, $this->searchUrlTemplate);
		 $this->rewind();
	 }

	 public function current() {
		 return $this->rows[$this->rowIndex];
	 }
	 
	 public function key() {
		 return $this->key;
	 }

	 public function next() {
		 $this->key++;
		 $this->rowIndex++;

		 // if we're past the last row, get another page.
		 if (!isset($this->rows[$this->rowIndex])) {
			 $this->rows = $this->getNewRows();
			 $this->rowIndex = 0;
		 }
	 }

	 public function rewind() {
		 if (!isset($this->searchUrlTemplateWithTerm)){
			 throw new Exception("Searcher doesn't have a search term loaded");
		 }
		 $this->key = 0;
		 $this->rowIndex = 0;
		 $this->rows = null;
		 $this->offset = '0';
		 $this->rows = $this->getNewRows();

	 }

	 public function valid() {
		 return ($this->rows) ? true : false;
	 }

	 public function getNewRows(){
		 if ($this->offset === false) { // there are no more results pages
			 return false;
		 }
		 else { // there is another page of results; get it.
			 $url = str_replace('[offset]', $this->offset, $this->searchUrlTemplateWithTerm);
			 echo "$url\n";
			 $this->getter->setUrl($url);
			 $resultsPage = $this->getter->curlGet();
		 
			 $resultsPageObj = json_decode($resultsPage);
			 if (!$resultsPageObj) {
				 throw new Exception("no response object from Wikipedia API for $url; returned $resultsPage");
			 }
			 if (isset($resultsPageObj->error)){
				 throw new Exception("Wikipedia threw an error for $url: " . print_r($resultsPageObj->error, true));
			 }

			 // set the offset for the next set of results
			 if (isset($resultsPageObj->{"query-continue"})) {
				$this->offset = $resultsPageObj->{"query-continue"}->exturlusage->euoffset;
			 }
			 else{
				 $this->offset = false;
			 }

			 return $resultsPageObj->query->exturlusage;
		 }

	 }





}
?>
