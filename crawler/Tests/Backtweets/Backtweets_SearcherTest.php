<?php
require_once 'PHPUnit/Framework.php';

require_once dirname(__FILE__).'/../../Backtweets/Searcher.php';
require_once dirname(__FILE__).'/../../init.php';

class Backtweets_SearcherTest extends PHPUnit_Framework_TestCase {

	protected $obj;
	protected $tweets;
	protected $testFileLoc;

	protected function setUp() {
		$getter = new UrlGetter();
		$this->obj = new Backtweets_Searcher($getter); // dummy search term, we won't use it.

		$this->testFileLoc = dirname(__FILE__) . '/../backtweets.json.page';
		$this->obj->setSearchUrlTemplate($this->testFileLoc. '[page]');
		$this->obj->setSearchTerm('foo');

		// make the test array
		$this->tweets = array();
		for ($i = 1; $i <= 2; $i++){
			$thisPage = file_get_contents($this->testFileLoc . $i);
			$thisPageObj = json_decode($thisPage);
			foreach ($thisPageObj->tweets as $tweet) {
				$this->tweets[] = $tweet;
			}
		}


	}


	public function testSearcherLoopGetsAllTweets() {
		$tweets = array();

		foreach ($this->obj as $k => $tweet) {
			$tweets[] = $tweet;
		}
		var_dump($tweets);

		$this->assertEquals(
				  count($this->tweets),
				  count($tweets)
				  );
	}




}
?>
