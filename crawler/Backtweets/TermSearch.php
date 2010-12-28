<?php
/**
 * Delivers tweet objects suitable for putting in the db
 *
 */
class Backtweets_TermSearch implements Iterator{
	 private $getter;
	 private $searchUrlTemplate;
	 private $key;
	 private $rowIndex;
	 private $rows;
	 private $currentPage;
	 private $searchTerm;
	 private $label;
	 private $searchUrlTemplateWithTerm;
	 private $logFile;

	 function __construct(UrlGetter $getter) {
		 $this->getter = $getter;
		 $itemsPerPage = 100;
		 $this->searchUrlTemplate = "http://backtweets.com/search.json?q=[searchTerm]&key=<key>&itemsperpage=$itemsPerPage&page=[page]";
		 $this->label = "backtweets";
	 }
	 public function setSearchUrlTemplate($searchUrlTemplate) {
		 $this->searchUrlTemplate = $searchUrlTemplate;
	 }

	 public function setSearchTerm($searchTerm) {
		 $this->searchTerm = $searchTerm;
		 $this->searchUrlTemplateWithTerm = str_replace('[searchTerm]', $searchTerm, $this->searchUrlTemplate);
		 $this->rewind();
	 }

	 public function getLabel() {
		 return $this->label;
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
		 
		 // if there are no more rows, get another page.
		 if (!isset($this->rows[$this->rowIndex])) {
			 $this->getNewRows();
			 $this->rowIndex = 0;			 
		 }		 
	 }

	 public function rewind() {
		 if (strpos($this->searchUrlTemplateWithTerm, '[searchTerm]')){
			 throw new Exception("Searcher doesn't have a search term loaded");
		 }
		 $this->key = 0;
		 $this->rowIndex = 0;
		 $this->rows = null;
		 $this->currentPage = 0;
		 $this->getNewRows();

	 }

	 public function valid() {
		 return (isset($this->rows[$this->rowIndex])) ? true : false;
	 }
	 
	 public function getNewRows(){
		 $this->currentPage++;
		 $url = str_replace('[page]', $this->currentPage, $this->searchUrlTemplateWithTerm);
		 $this->getter->setUrl($url);
		 try {
			 $resultsPage = $this->getter->get(false);
		 }	catch (Exception $e) {
			 $this->rows = false;
			 return false; 
		 }
		 $resultsPageObj = json_decode($resultsPage);
		 if (!$resultsPageObj) {
			 throw new Exception("no response object from Backtweets API for $url; returned $resultsPage");
		 }
		 $this->rows = $resultsPageObj->tweets;

		 return true;
	 }


	 


}
?>
