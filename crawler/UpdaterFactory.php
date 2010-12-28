<?php

/**
 * Create an updater for a given type of article stats
 * The runSearch method be part of a searchFactory at some point.
 */
class UpdaterFactory {
    
	private $viewNameBase;
	private $updateIfOlderThan;
	private $secondsBetweenDocUpdates;
	private $log;
	private $searchTerms;
	private $testSearchTerms;
	
	function __construct() {
		$this->updateIfOlderThan = array(
			"Mendeley"		=> date('c', time() - 1209600),	// 2 wks
			"Backtweets"	=> date('c', time() - 43200),		// 12 hrs
			"Delicious"		=> date('c', time() - 1209600),	// 2 wks
			"PlosAlm"		=> date('c', time() - 604800),	// 1 wk
			"PlosComments"	=> date('c', time() - 604800),	// 1 wk
			"Curator"		=> "\u9999",							// anything
			"Facebook"		=> date('c', time() - 604800),	// 1 wk
			"Wikipedia"		=> date('c', time() - 1209600),	// 2 wk
			"PlosArticles"	=> date('c', time() - 43200),		// 12 hrs
			"F1000"			=> date('c', time() - 1209600)	// 2 wks
		);
		$this->secondsBetweenDocUpdates = array(
			"Mendeley"		=> 0,
			"Backtweets"	=> 0,
			"Delicious"		=> 10,
			"PlosAlm"		=> 0,
			"PlosComments"	=> 0,
			"Curator"		=> 0,
			"Facebook"		=> 1,
			"Wikipedia"		=> 0,
			"PlosArticles"	=> 0,
			"F1000"			=> 0
		);
		$this->searchTerms = array(
			"Backtweets"	=> array(
				 'http%3A%2F%2Fplosntds.org',
				 'http%3A%2F%2Fplospathogens.org',
				 'http%3A%2F%2Fplosgenetics.org',
				 'http%3A%2F%2Fploscompbiol.org',
				 'http%3A%2F%2Fplosmedicine.org',
				 'http%3A%2F%2Fplosbiology.org',
				 'http%3A%2F%2Fplosone.org'
				 ),
			"Wikipedia"		=> array(
				'www.plosntds.org',
				'www.plospathogens.org',
				'www.plosgenetics.org',
				'www.ploscompbiol.org',
				'www.plosmedicine.org',
				'www.plosbiology.org',
				'www.plosone.org',
				'dx.doi.org/10.1371'
				)
			);
		$this->testSearchTerms = array(
			"Wikipedia"	=> array('www.plosntds.org')
		);
		$this->viewNameBase = "articles_by_last_update_";
		$this->log = fopen('./logs/' . date("Y-m-d"), 'a+');
	}

	
	public function create($statsType) {

		// prepare things unique to this statsType
		$ucStatsType = ucfirst($statsType);
		if (!isset($this->updateIfOlderThan[$ucStatsType])){
			throw new Exception("unknown type of article stats type given: '$statsType'");
			exit();
		}
		$viewName = $this->viewNameBase . strtolower($statsType);
		$articleStatsClassName = $ucStatsType . "_ArticleStats";

		// get search results for statsTypes that need them
		$searchResults = $this->runSearch($ucStatsType);
		
		// setup the objects we need
		$db = new Db('http://'.AM_CRAWLER_USER.':'.AM_CRAWLER_PW.'@'.AM_CRAWLER_DB_URL, AM_CRAWLER_DB_NAME);
		$urls = new Urls();
		$getter = new UrlGetter();

		$articleStats = new $articleStatsClassName($getter, $urls);
		$updater = new Updater($db, $this->log);

		// setup update options
		$updater->setViewName(array("crawl", $viewName));
		$updater->setArticleStats($articleStats);
		$updater->setSecondsBetweenUpdates( $this->secondsBetweenDocUpdates[$ucStatsType] );
		$updater->setUpdateIfOlderThan( $this->updateIfOlderThan[$ucStatsType] );
		$updater->setSearchResults($searchResults);
		return $updater;
	}

	private function runSearch($statsType){
		if (isset( $this->searchTerms[$statsType] )) {
			$className = $statsType . "_TermSearch";
			$searcher = new $className(new UrlGetter());
			$termsSearch = new TermsSearch($searcher, $this->searchTerms[$statsType], $this->log);
//			$termsSearch = new TermsSearch($searcher, $this->testSearchTerms[$statsType], $this->log);
			$searchResults = array();
			foreach ($termsSearch as $v) {
				$searchResults[] = $v;
			}
			return $searchResults;
		}
		else {
			return false;
		}
	}
}
?>
