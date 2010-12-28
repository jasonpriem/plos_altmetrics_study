<?php

/**
 * A bunch of functions used to manually edit the database
 *
 */
class Porters_DbCurator {
    private $db;

	function __construct(Db $db) {
		$this->db = $db;
	}
	public function changeDateFormats(){

		$continue = true;
		while ($continue){
				$response = $this->db
						  ->include_docs(true)
						  ->limit(200)
						  ->getView('crawl', 'pubdates');

				$docsToStore = array();
				foreach ($response->rows as $k => $row){
					$doc = $row->doc;
					$ts = strtotime(implode('/', $doc->pubDate));
					$doc->pubDate = $ts;
					$docsToStore[] = $doc;
				}
				$this->db->storeDocs($docsToStore);
				$continue = count($response->rows);
		}
	}
	public function addNamespaceAndNativeId(){

		$continue = true;
		while ($continue){
				$response = $this->db
						  ->include_docs(true)
						  ->limit(200)
						  ->getView('crawl', 'plosone_articles_without_nativeId');

				$docsToStore = array();
				foreach ($response->rows as $k => $row){
					$doc = $row->doc;
					$doc->namespace = '10.1371/journal.pone';
					$doc->nativeId = substr($doc->_id, -7);
					$docsToStore[] = $doc;
				}
				$this->db->storeDocs($docsToStore);
				$continue = count($response->rows);
		}
	}
	public function removePctr() {
		$response = $this->db
				  ->include_docs(true)
				  ->startkey("10.1371/journal.pctr.0000000")
				  ->endkey("10.1371/journal.pctr.9999999")
				  ->getAllDocs();
		$toDel = array();
		foreach ($response->rows as $k => $row) {
			$toDel[] = $row->doc;
		}
		$delResponse = $this->db->deleteDocs($toDel);
		var_dump($delResponse);
	}
	
	
}
?>
