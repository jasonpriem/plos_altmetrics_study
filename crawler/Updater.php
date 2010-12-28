<?php
/**
 * Description of Updater
 *
 */
class Updater {
    private $db;
	 private $articleStats;
	 private $numConsecFailsAllowed;
	 private $sleepBetweenUpdates;
	 private $searchResults;
	 private $log;
	 private $viewName;
	 private $updateIfOlderThan;
	 
	 
	 function __construct($db, $log) {
		 $this->db = $db;
		 $this->log = $log;
		 $this->numConsecFailsAllowed = 2;
		 $this->secondsBetweenUpdates = 0;
		 $this->searchResults = false;

	 }
	 
	 public function setDb($db) {
		 $this->db = $db;
	 }
	 
	 public function setArticleStats($articleStats) {
		 $this->articleStats = $articleStats;
	 }
	 
	 public function setNumConsecFailsAllowed($numConsecFailsAllowed) {
		 $this->numConsecFailsAllowed = $numConsecFailsAllowed;
	 }
	 
	 
	 public function setSecondsBetweenUpdates($secondsBetweenUpdates) {
		 $this->secondsBetweenUpdates = $secondsBetweenUpdates;
	 }
	 
	 public function setSearchResults($searchResults) {
		 $this->searchResults = $searchResults;
	 }
	 public function setViewName(Array $viewName) {
		 $this->viewName = $viewName;
	 }

	 public function setUpdateIfOlderThan($updateIfOlderThan) {
		 $this->updateIfOlderThan = $updateIfOlderThan;
	 }


	 
	 
	 
	 public function update(){
		$startTime = time();
		fwrite($this->log, "\n\nstarting update at " . date("H:i") . "\n");
		$fails = 0;
		while ($fails < $this->numConsecFailsAllowed) {
			sleep($this->secondsBetweenUpdates);
			
			// get a document
			try {
				$result = $this->db->descending(true)
						  ->limit(1)->startkey($this->updateIfOlderThan)
						  ->include_docs(true)
						  ->getView($this->viewName[0], $this->viewName[1]);
			} catch (Exception $e) {
				fwrite($this->log, "Error getting a record from database: " . $e->getMessage() . "\n");
			}

			// update the document
			if (isset($result->rows[0])) { // we got a document back

				// update the article's stats
				try {
					$this->articleStats->setResultsObj($result->rows[0]->doc);
					if ($this->searchResults) {
						$this->articleStats->setSearchResults($this->searchResults);
					}					
					$updatedDoc = $this->articleStats->getInfo();
				} catch (Exception $e){
					fwrite($this->log,  "Error adding info to doc "
//							  . $updatedDoc->_id .": "
							  . $e->__toString() . "\n"
							  );
					$fails++;
					continue;
				}

				// store the updated doc
				try {
					$res = $this->db->storeDoc($updatedDoc);
					fwrite($this->log,  $res->id . " updated. {$updatedDoc->latest_changes}\n");
				} catch (Exception $e) {
					fwrite($this->log,  "storing {$updatedDoc->_id} failed: " .$e->getMessage(). "\n\n");
					$fails++;
					continue;
				}					
			}
			else { // there are no records to update
				fwrite($this->log, "didn't get any records back for "
						  . $this->viewName[0] . "/"
						  . $this->viewName[1] . " with startkey "
						  . $this->updateIfOlderThan . "\n"
						  );
				break;
				
			}
		}
		$endTime = time();
		$elapsed = round(($endTime - $startTime) / 60);
		fwrite($this->log, "Done at " .date("H:i"). " ($elapsed minutes)\n\n");
		fclose($this->log);
	 }

}
?>
