<?php
require_once 'PHPUnit/Framework.php';

require_once dirname(__FILE__).'/../../Backtweets/ArticleBacktweets.php';
require_once dirname(__FILE__).'/../../init.php';

class Backtweets_ArticleBacktweetsTest extends PHPUnit_Framework_TestCase {

	protected $obj;
	protected $doc;
	protected $searchResults;

	protected function setUp() {
		$this->obj = new Backtweets_ArticleBacktweets(new UrlGetter(), new Urls());

		$serializedResults = file_get_contents(dirname(__FILE__) . '/../tweetsFromSearch.json');
		$this->searchResults = json_decode($serializedResults);

		$this->doc = new stdClass();
		$this->doc->_id = '10.1371/journal.pone.0012866';
		$this->doc->_rev = '12345';

		$tweet = new stdClass();
		$tweet->tweet_id = "25273161141";
		$tweet->tweet_from_user_id = 17567533;
		$tweet->tweet_from_user = "BoraZ";
		$tweet->tweet_profile_image_url = "http://a1.twimg.com/profile_images/548766433/BoraZ191124_normal.jpg";
		$tweet->tweet_created_at = '2010-09-23 03:12:36';
		$tweet->tweet_text = 'Size, Rarity and Charisma: Valuing African Wildlife Trophies <a href="http://www.plosone.org/article/info%3Adoi%2F10.1371%2Fjournal.pone.0012866">http://bit.ly/9cVvgA</a>';
		$tweet->targets = array("10.1371/journal.pone.0012866");
		$this->tweet = $tweet;


//		$this->docWithTweet = $this->doc;
//		$this->docWithTweet->backtweets = new stdClass();
//		$this->docWithTweet->backtweets->tweets = array($tweet);


		}

	public function testGetInfo() {
		$this->obj->setSearchResults($this->searchResults);
		$this->obj->setResultsObj($this->doc);
		$resultDoc = $this->obj->getInfo();
		var_dump($resultDoc);
		$this->assertEquals(
				  $this->tweet,
				  $resultDoc->backtweets->tweets[0]
				  );



	}

}
?>
