<?php
require_once 'PHPUnit/Framework.php';

require_once dirname(__FILE__).'/../Urls.php';

/**
 * Test class for PlosScraper_Urls.
 * Generated by PHPUnit on 2010-09-21 at 15:20:59.
 */
class UrlsTest extends PHPUnit_Framework_TestCase {

	protected $obj;

	protected function setUp() {
		$this->obj = new Urls;
		$this->testDoi ='10.1371%2Fjournal.pone.0007595';
		$this->xmlTemplate = 'http://www.[stub].org/article/fetchObjectAttachment.action?uri=info%3Adoi%2F10.1371%2Fjournal.[id]&representation=XML';
		$this->xmlUrl = 'http://www.plosgenetics.org/article/fetchObjectAttachment.action?uri=info%3Adoi%2F10.1371%2Fjournal.pgen.0030115&representation=XML';
		$this->almXmlTemplate = 'http://alm.plos.org/articles/info%3Adoi%2F10.1371%2Fjournal.[id].xml?citations=1';
		$this->almXmlUrl = 'http://alm.plos.org/articles/info%3Adoi%2F10.1371%2Fjournal.pgen.0030115.xml?citations=1';
		$this->mendeleyUrlTemplate = 'http://www.mendeley.com/oapi/documents/details/[doi]?type=doi&consumer_key=f951efc67d3992264d9264baad7496c304c9badcc';
		$this->mendeleyUrl = 'http://www.mendeley.com/oapi/documents/details/10.1371%252Fjournal.pone.0007595?type=doi&consumer_key=f951efc67d3992264d9264baad7496c304c9badcc';


	}
	


	public function testMakeUrl() {
		$url = $this->obj->makeUrl('doi%2F10.1371%2Fjournal.pgen.0030115', $this->xmlTemplate);
		$this->assertEquals(
				  $this->xmlUrl,
				  $url
				  );
	}
	public function testMakeUrlWorksForAlmXmlUrl() {
		$url = $this->obj->makeUrl('doi%2F10.1371%2Fjournal.pgen.0030115', $this->almXmlTemplate);
		$this->assertEquals(
				  $this->almXmlUrl,
				  $url
				  );
	}
	
	public function testPlosDoiFromUrl(){
		$this->assertEquals(
				  "10.1371/journal.pone.0012893",
				  $this->obj->plosDoiFromUrl("http://www.plosone.org/article/info:doi/10.1371/journal.pone.0012893")
				  );
		$this->assertEquals(
				  "10.1371/journal.pone.0012806",
				  $this->obj->plosDoiFromUrl("http://www.plosone.org/article/info:doi/10.1371/journal.pone.0012806?utm_source=feedburner&utm_medium=feed&utm_campaign=Feed:+plosone/NeurologicalDisorders+(PLoS+ONE+Alerts:+Neurological+Disorders)")
				  );
	}
	public function testMendeleyUrlFromDoi(){
		$url = $this->obj->makeUrl('10.1371%252Fjournal.pone.0007595', $this->mendeleyUrlTemplate);
		$this->assertEquals(
				  $this->mendeleyUrl,
				  $url
				  );
	}
	public function testDeliciousUrlFromDoi(){
		$deliciousUrlTemplate = 'http://feeds.delicious.com/v2/json/url/[plosUrlHash]';
		$deliciousUrl = 'http://feeds.delicious.com/v2/json/url/e89ddcc0527148b9598536e5a1565efc';
		$this->assertEquals(
				  $deliciousUrl,
				  $this->obj->makeUrl("e89ddcc0527148b9598536e5a1565efc", $deliciousUrlTemplate)
				  );
	}
	public function testPlosUrlFromPrefixPlusDoi() {
		$plosUrlTemplate = 'http://www.[stub].org/article/[prefixPlusDoi]';
		$plosUrl = 'http://www.plosone.org/article/info%3Adoi%2F10.1371%2Fjournal.pone.0007595';
		$this->assertEquals(
				  $plosUrl,
				  $this->obj->makeUrl('info%3Adoi%2F10.1371%2Fjournal.pone.0007595', $plosUrlTemplate)
				  );
	}
}
?>