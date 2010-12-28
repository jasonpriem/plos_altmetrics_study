<?php
require_once 'PHPUnit/Framework.php';

require_once dirname(__FILE__).'/../../Wikipedia/TermSearch.php';
require_once dirname(__FILE__).'/../../UrlGetter.php';

class Wikipedia_TermSearchTest extends PHPUnit_Framework_TestCase {

	protected $obj;

	protected function setUp() {
		$this->obj = new Wikipedia_TermSearch(new UrlGetter());
	}

	function testGetsRows(){
		$this->obj->setSearchTerm('www.plosntds.org');
		foreach($this->obj as $k => $row){
			echo "row $k:";
			var_dump($row);
			echo "\n";
		}
	}
}
?>
