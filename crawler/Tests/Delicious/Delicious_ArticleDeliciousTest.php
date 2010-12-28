<?php
require_once 'PHPUnit/Framework.php';

require_once dirname(__FILE__).'/../../Delicious/ArticleDelicious.php';
require_once dirname(__FILE__).'/../../Urls.php';
require_once dirname(__FILE__).'/../../UrlGetter.php';


class Delicious_ArticleDeliciousTest extends PHPUnit_Framework_TestCase {

	protected $obj;
	protected $urlEncodedResp;
	protected $urlNotEncodedResp;
	protected $bothResponses;

	protected function setUp() {
		$this->obj = new Delicious_ArticleDelicious(new UrlGetter(), new Urls());
		$this->urlEncodedResp = json_decode(
				  file_get_contents(
							 dirname(__FILE__) . '/delicious-reponse-urlencoded.json'
							 )
				  );
		$this->urlNotEncodedResp = json_decode(
				  file_get_contents(
							 dirname(__FILE__) . '/delicious-response-not-urlencoded.json'
							 )
				  );
		$this->bothResponses = array_merge($this->urlEncodedResp, $this->urlNotEncodedResp);
	}

	public function testGetInfo() {
		$doc = new stdClass();
		$doc->_id = "10.1371/journal.pone.0007595";
		$doc->_rev = "123456789";
		$this->obj->setResultsObj($doc);
		$updatedDoc = $this->obj->getInfo();
		$this->assertEquals(
				  asort($this->bothResponses),
				  asort($doc->delicious->bookmarks)
				  );
	}



}
?>
