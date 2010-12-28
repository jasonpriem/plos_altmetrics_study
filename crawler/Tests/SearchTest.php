<?php
require_once 'PHPUnit/Framework.php';

require_once dirname(__FILE__).'/../SearchTerms.php';
require_once dirname(__FILE__).'/../init.php';

class SearchTest extends PHPUnit_Framework_TestCase {

	protected $obj;


	protected function setUp() {
		$searchTerms = array('foo','bar','baz');
		$searcher = new Backtweets_Searcher(new Urls(), new UrlGetter());
		$this->obj = new SearchTerms($searcher, $searchTerms);
	}


}
?>
