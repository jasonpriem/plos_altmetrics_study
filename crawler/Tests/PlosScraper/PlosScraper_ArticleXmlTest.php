<?php
require_once 'PHPUnit/Framework.php';

require_once dirname(__FILE__).'/../../PlosScraper/ArticleXml.php';
require_once dirname(__FILE__).'/../../UrlGetter.php';

class PlosScraper_ArticleXmlTest extends PHPUnit_Framework_TestCase {

	protected $object;

	protected function setUp() {
		$doc = new stdClass();
		$doc->_rev = "09871024878914";
		$this->xmlUrl = dirname(__FILE__) . '/../journal.pone.0012504.xml';

		$this->getter = new UrlGetter();
		$this->getter->setUrl($this->xmlUrl);

		$this->obj = new PlosScraper_ArticleXml();
		$this->obj->setGetter($this->getter);
		$this->obj->setResultsObj($doc);
	}

	public function testGetInfoGetsArticleType() {
		$doc = $this->obj->getInfo();
		$this->assertEquals(
				  "Research Article",
				  $doc->articleType
				  );
	}


	public function testGetInfoGetsMainSubjects() {
		$doc = $this->obj->getInfo();
		$this->assertEquals(
				  array('Developmental Biology', 'Infectious Diseases'),
				  $doc->subjects
				  );
	}
		public function testGetInfoGetsSubSubjects(){
		$doc = $this->obj->getInfo();
		$this->assertEquals(
				  array('Microbial Growth and Development', 'Morphogenesis and Cell Biology', 'Bacterial Infections'),
				  $doc->subSubjects
				  );
	}
	public function testGetInfoGetsAuthors() {
		$doc = $this->obj->getInfo();
		$this->assertEquals(
				  array(
						'Olcott, Marika H.',
						'Henkels, Marcella D.',
						'Rosen, Kise L.',
						'L.Walker, Francesca',
						'Sneh, Baruch',
						'Loper, Joyce E.',
						'Taylor, Barbara J.'
					  ),
				  $doc->authors
				  );
		}
		public function testGetInfoGetsPubDate(){
			$doc = $this->obj->getInfo();
			$this->assertEquals(
					  1284336000,
					  $doc->pubDate
					  );
		}
		public function testGetInfoGetsTitle() {
			$doc = $this->obj->getInfo();
			$this->assertEquals(
					  'Lethality and Developmental Delay in Drosophila melanogaster Larvae after Ingestion of Selected Pseudomonas fluorescens Strains',
					  $doc->title
					  );
		}
		public function testGetInfoGetsAuthorNamesWhenAuthorIsOrganization(){
			$xmlUrl = dirname(__FILE__) . '/../journal.pone.0001275.xml';
			$this->getter->setUrl($xmlUrl);
			$this->obj->setGetter($this->getter);
			$doc = $this->obj->getInfo();

			$this->assertEquals(
					  array('REX Consortium'),
					  $doc->authors
					  );
		}
}
?>
