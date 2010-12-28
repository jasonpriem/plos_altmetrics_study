<?php

/**
 * Manages a full search of an external source, based on an array of search terms.
 * @todo: This should call rewind() on the searcher obj.
 */
class TermsSearch implements Iterator {
    private $searcher;
	 private $searchTerms;
	 private $log;

	 function __construct(Iterator $searcher, Array $searchTerms, $log) {
		 $this->searcher = $searcher;
		 $this->searchTerms = $searchTerms;
		 $this->log = $log;
	 }


	 public function current() {
		 $current = $this->searcher->current();
		 fwrite($this->log, $this->key . ": Returning matching item: " . print_r($current, true));
		 return $current;
	 }

	 public function key() {
		 return $this->key;
	 }

	 public function next() {
		 $this->searcher->next();
		 if (!$this->searcher->valid()) { // the searcher is out of results for this term
			$this->searchTermsKey++;
			if (isset($this->searchTerms[$this->searchTermsKey])) {  // is there another search term to try?
				fwrite($this->log, "setting next search term: " . $this->searchTerms[$this->searchTermsKey]. "\n");
				$this->searcher->setSearchTerm($this->searchTerms[$this->searchTermsKey]);
			}
		 }
	 }

	 public function rewind() {
		 $this->key = 0;
		 $this->searchTermsKey = 0;
		 $this->searcher->setSearchTerm($this->searchTerms[0]);
	 }
	 
	 public function valid() {
		 return (isset($this->searchTerms[$this->searchTermsKey]));
	 }

}
?>
