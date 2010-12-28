<?php
require_once 'PHPUnit/Framework.php';

require_once dirname(__FILE__).'/../../PlosScraper/ArticleMetricsXml.php';
require_once dirname(__FILE__).'/../../init.php';


class PlosScraper_ArticleMetricsXmlTest extends PHPUnit_Framework_TestCase {

	protected $obj;
	protected $xmlObj;
	protected $getter;
	protected $urls;



	protected function setUp() {
		$getter = new UrlGetter();
		$urls = new PlosScraper_Urls();
		$this->obj = new PlosScraper_ArticleMetricsXml($getter, $urls);
		$this->xmlObj = simplexml_load_file('../metrics.xml');

	}

	function testGetPmid() {
		$this->assertEquals(
				  '12929205',
				  $this->obj->getPmid($this->xmlObj)
				  );
	}
	function testGetBlogPosts() {
		$blogs = array(
			array(
				'uri'			=>	'http://duncan.hull.name/2009/09/18/plos/',
				'from'		=>	'Nature via Plos',
				'pubDate'	=> 1253271011,
				'title'		=>	"Popular, personal and public data: Article-level metrics at PLoS",
				'natureBlogId'	=>	551
				),
			array(
				'uri'			=>	'http://the-mouse-trap.blogspot.com/2008/10/maudsely-debates-anti-depressants-and.html',
				'from'		=>	'Nature via Plos',
				'pubDate'	=> 1224505200,
				'title'		=>	"The Maudsely debates: anti-depressants and placebo",
				'natureBlogId'	=>	122
				),
			array(
				'uri'			=>	"http://blog.openwetware.org/scienceintheopen/2008/02/27/a-quick-update/",
				'from'		=>	'Postgenomic via Plos',
				'pubDate'	=> 1204147816,
				'title'		=>	"A quick update",
				'blogName'	=>	"Science in the open"
				),
			array(
				'uri'			=>	"http://feedproxy.google.com/~r/brontossauros/~3/PDh89YJ4nFI/sexo_animal_sexo_oral_em_morce.php",
				'from'		=>	'Research Blogging via Plos',
				'pubDate'	=> 1256808514,
				'title'		=>	"Sexo Animal: sexo oral em morcegos",
				'blogName'	=>	"Brontossauros em meu Jardim"
				)
		);
		$this->assertEquals(
				  $blogs,
				  $this->obj->getBlogPosts($this->xmlObj)
				 );
	}

	function testGetViews() {
		$views = array(
			'2003/10'	=>		array('pdf'	=> 0, 'xml' => 0, 'html' => 1445),
			'2010/8'		=>		array('pdf'	=> 29, 'xml' => 0, 'html' => 104),
			'2010/9'		=>		array('pdf'	=> 23, 'xml' => 1, 'html' => 96)
		);
		$this->assertEquals(
				  $views,
				  $this->obj->getViews($this->xmlObj)
				);
	}

	function testGetCiteULike() {
		$bookmark = array(
			array(
				'uri'			=>	"http://www.citeulike.org/user/chica/article/687411",
				'pubDate'	=>	1247846766,
				'tags'		=> array('pfalciparum', 'transcriptome'),
				'creator'	=> 'chica'
				)
		);
		$this->assertEquals(
				  $bookmark,
				  $this->obj->getCiteULike($this->xmlObj)
				  );

	}
	function testScopusCiteCount() {
		$this->assertEquals(
				  498,
				  $this->obj->getScopusCiteCount($this->xmlObj)
					);
	}
	function testGetPubMedCites() {
		$cites = array(
			array(
				'uri'	=> 'http://www.pubmedcentral.nih.gov/articlerender.fcgi?artid=1079819'
				),
			array(
				'uri'	=> 'http://www.pubmedcentral.nih.gov/articlerender.fcgi?artid=1084361'
				)
		);
		$this->assertEquals(
				  $cites,
				  $this->obj->getPubMedCites($this->xmlObj)
					);
	}
	function testGetCrossRefCites(){
		$cites = array(
			array(
				'uri'					=> 'http://dx.doi.org/10.1002/bies.20025',
				'doi'					=> '10.1002/bies.20025',
				'journalTitle'		=> 'BioEssays',
				'title'				=> 'The transcriptome: malariologists ride the wave',
				'pubYear'			=> 2004,
				'creator'			=> array( 'Wilson R' )
			),
			array(
				'uri'					=> 'http://dx.doi.org/10.1002/cm.20055',
				'doi'					=> '10.1002/cm.20055',
				'journalTitle'		=> 'Cell Motility and the Cytoskeleton',
				'title'				=> '&lt;i&gt;Plasmodium falciparum&lt;/i&gt; myosins: Transcription and translation during asexual parasite development',
				'pubYear'			=> 2005,
				'creator'			=> array('Chaparro-Olaya J','Margos G','Coles DJ','Dluzewski AR','Mitchell GH','Wasserman MM', 'Pinder JC')
			)
		);
		$this->assertEquals(
				  $cites,
				  $this->obj->getCrossRefCites($this->xmlObj)
				  );
	}



}
?>
