<?php
require_once 'PHPUnit/Framework.php';

require_once dirname(__FILE__).'/../../PlosComments/ArticleStats.php';
require_once dirname(__FILE__).'/../../Urls.php';
require_once dirname(__FILE__).'/../../UrlGetter.php';


class PlosComments_ArticleStatsTest extends PHPUnit_Framework_TestCase {


	protected $obj;
	protected $plosComments;

	protected function setUp() {
		date_default_timezone_set("UTC");
		$this->obj = new PlosComments_ArticleStats(new UrlGetter(), new Urls());
		$this->plosComments = array(
			(object)array(
				'id'			=> "annotation/de69412c-06fe-4797-9138-05733db59325",
				'thread'		=> "annotation/de69412c-06fe-4797-9138-05733db59325",
				'position'	=> 0,
				'title'		=> 'Correction on species',
				'author'		=> '6489',
				'pub_date'	=> '2007-07-29T15:41:00+00:00',
				'content'	=> htmlentities('<p>This dewlap belongs to A. chlorocyanus cyanostictus, not A. reconditus.  I obtained the wrong name for this photo file in error!</p>', ENT_QUOTES, "UTF-8", false)
			),
			(object)array(
				'id'			=> "annotation/452a32e5-19b7-4c44-9648-2c42e722d8fe",
				'thread'		=> "annotation/452a32e5-19b7-4c44-9648-2c42e722d8fe",
				'position'	=> 0,
				'title'		=> 'Correction on direction',
				'author'		=> '6489',
				'pub_date'	=> '2007-07-29T15:38:00+00:00',
				'content'	=> htmlentities('<p>Clearly this is meant to be left to right, not right to left</p>', ENT_QUOTES, "UTF-8", false)
			),
			(object)array(
				'id'			=> "annotation/407",
				'thread'		=> "annotation/407",
				'position'	=> 0,
				'title'		=> 'Science(?)Daily',
				'author'		=> '38747',
				'pub_date'	=> '2007-03-08T12:33:00+00:00',
				'content'	=> htmlentities('<p>I would love to see the press release on this study which resulted in ScienceDaily creating the following headline and leadin paragraph:<br /><br />&quot;Lizards May Help Unlock Secrets of Evolution<br /><br />Science Daily &mdash; Hundreds of species of anoles roam the Caribbean Islands and parts of North and South America, a highly diverse and colorful small lizard that scientists have studied in hopes of unlocking the secrets of evolution.&quot;<br /><br />Kip Hansen<br /><br /></p>', ENT_QUOTES, "UTF-8", false)
			),
			(object)array(
				'id'			=> "reply/118",
				'thread'		=> "annotation/407",
				'position'	=> 1,
				'title'		=> 'RE: Science(?)Daily',
				'author'		=> '6422',
				'pub_date'	=> '2007-03-13T17:35:00+00:00',
				'content'	=> htmlentities("<p>PLoS ONE distributes press releases through EurekAlert which archives them and makes them publicly available once their embargo is past. The URL of the press release for this paper is: <br /><br /><a href=\"http://www.eurekalert.org/pub_releases/2007-03/plos-crp030607.php\" title=\"http://www.eurekalert.org/pub_releases/2007-03/plos-crp030607.php\">http://www.eurekalert.org...</a><br /><br />It was written by the authors/the authors' institutional press office.</p>", ENT_QUOTES, "UTF-8", false)
			)
		);
	}

	function testGetInfo(){
		$doc = new stdClass();
		$doc->_id = "10.1371/journal.pone.0000274";
		$doc->_rev = "123456789";
		$this->obj->setResultsObj($doc);
		$updatedDoc = $this->obj->getInfo();

		$this->assertEquals(
				  $this->plosComments,
				  $doc->plos_comments->comments
				  );
	}


}
?>
