<?php

/**
 * Scrapes the list of Plos article dois at alm.plos.org/articles
 * @todo only get articles from the 7 main journals, not the clinical hubs.
 *		This means an expression to test for one of the 7 allowed 4-digit ids.
 *
 */
class PlosArticles_ArticlesScraper {
    private $baseUrl;
	 private $db;
	 private $getter;
	 
	 function __construct(Db $db, UrlGetter $getter) {
		 $this->baseUrl = 'http://alm.plos.org/articles?page='; 
		 $this->db = $db;
		 $this->getter = $getter;
	 }
	 
	 public function update() {
		 $isMore = true;
		 for ($i = 1 ; $isMore == true; $i++){
			 echo "<h2>scraping page $i</h2>";
			 $this->getter->setUrl($this->baseUrl . $i);
			 $xml = $this->getter->get();
			 $this->saveArticles($xml);
			 $isMore = $this->checkIfMore($xml);
		 }
	 }
	 /**
	  * Saves all the articles listed on a page.
	  * Note the hack to not store plos one articles I already have in the db right now.
	  *
	  * @param SimpleXMLElement $xml
	  */
	 public function saveArticles(SimpleXMLElement $xml) {
		 $dois = $xml->xpath("//span[@class='doi']/a");
		 foreach ($dois as $doi){
			 if (strpos($doi, '10.1371') === 0) { // only store PLoS articles
				 $doiStr = (string)$doi;
				 $doc = new stdClass();
				 $doc->_id = $doiStr;
				 $doc->namespace = substr($doiStr, 0, -8); // all but the last 8 chars
				 $doc->nativeId = substr($doiStr, -7); // the last seven chars
				 $doc->doi = $doiStr;


//			 if ( $doc->namespace == "10.1371/journal.pone" && (int)$doc->nativeId < 12649) {
//				 continue;
//			 }

				 try {
					 $response = $this->db->storeDoc($doc);
					 echo $response->id . " stored.<br>\n";
				 } catch (Exception $e){
					 echo "Storing $doiStr failed: " .$e->getMessage(). "(" .$e->getCode(). ")<br>\n";
				 }
			 }

		 }
	 }

	 public function checkIfMore(SimpleXMLElement $xml){
		 $nexts = $xml->xpath("//span[@class='disabled next_page']");
		 return (isset($nexts[0])) ? false : true;
	 }
	 
	 
}
?>
