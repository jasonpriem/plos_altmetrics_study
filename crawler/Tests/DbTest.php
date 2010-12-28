<?php
require_once 'PHPUnit/Framework.php';
require_once dirname(__FILE__).'/../PHP-on-couch/couch.php';
require_once dirname(__FILE__).'/../PHP-on-couch/couchClient.php';
require_once dirname(__FILE__).'/../Db.php';

class DbTest extends PHPUnit_Framework_TestCase {

	protected $db;

	protected function setUp() {
		date_default_timezone_set("UTC");
		$this->db = new Db("http://localhost:5984", "test");
		$this->db->deleteDatabase();
		$this->db->createDatabase();
		// create some article objects
		$docsToStore = array();
		for ($i=0; $i<9; $i++){
			$doc = new stdClass();
			$doc->_id = "10.1371/journal.pone.000000" .$i;
			$doc->namespace = "10.1371/journal.pone";
			$doc->nativeId = "000000" .$i;
			if ($i % 2) { // some docs have these properties, some don't
				$doc->pmid = "123456" .$i;
				if ($i < 5) {
					$doc->lastUpdates = array('comments'=>1284508800); //sept 15
				}
				else {
					$doc->lastUpdates = array('comments'=>1284940800); //sept 20
				}
			}
			$docsToStore[] = $doc;
		}

		// create some tweet objects
		for ($i=0; $i<5; $i++){
			$doc = new stdClass();
			$doc->_id = "twitter.com/2459650009" .$i;
			$doc->namespace = "twitter.com";
			$doc->nativeId = "2459650009" .$i;
			$doc->content = "content for number $i";
			$docsToStore[] = $doc;
		}

		// create the views needed
		$byIdView =
			"function(doc){
				emit([doc.namespace, doc.nativeId], doc.nativeId);
				}";
		$byRandView =
			'function(doc) {
				if (!doc.pmid && doc._id.indexOf("10.1371/journal") == 0){
				  emit(Math.random(),null);
				}
			}';
		$byLastUpdatesCommentsView =
			"function(doc) {
				if (doc._id.indexOf('10.1371/journal') == 0) { // plos articles only
				  if (!doc.lastUpdates) {
					 emit(null, null);
				  }
				  else if (!doc.lastUpdates.comments) {
					 emit(null, null);
				  }
				  else {
					 emit(doc.lastUpdates.comments, null);
				  }
				}
			}";
		$designDoc = new stdClass();
		$designDoc->_id = "_design/crawl";
		$designDoc->language = "javascript";
		$designDoc->views = array(
			"nativeIds_by_namespace"					=> array("map" => $byIdView),
			"rand_plos_article_without_pmid"			=> array("map" => $byRandView),
			"plos_articles_by_lastUpdate_comments" => array("map" => $byLastUpdatesCommentsView)
			);
		$docsToStore[] = $designDoc;

		$this->db->storeDocs($docsToStore);
	}


	public function testGetHighestId() {
		$this->assertEquals(
				  "0000008",
				  $this->db->getHighestId("10.1371/journal.pone")
				  );
	}

	public function testGetRandomDoc() {
		$doc = $this->db->getRandomDoc("crawl", "rand_plos_article_without_pmid" );
		$this->assertEquals(
				  "10.1371/journal.pone",
				  $doc->namespace
				  );
		$this->assertFalse(
				  isset($doc->pmid)
				  );
	}

	public function testGetDocWithRandomSkip() {
		$doc = $this->db->getDocWithRandomSkip(
				  "crawl", "plos_articles_by_lastUpdate_comments", 1284940800, 5 );
		var_dump($doc);
		if (isset($doc->lastUpdates)){
			$this->assertLessThanOrEqual(
					  $doc->lastUpdates->comments,
					  1284940800
					  );
		}
	}
}
?>
