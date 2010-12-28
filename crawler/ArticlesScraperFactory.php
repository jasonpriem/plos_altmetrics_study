<?php

class ArticlesScraperFactory {
    
	private $log;
	
	function  __construct() {
//		$this->log = fopen('./logs/' . date("Y-m-d"), 'a+');
	}

	public function create($articlesSource){
		// prepare things unique to this articlesSource
		$ucArticlesSource = ucfirst($articlesSource);
		$articlesSourceClassName = $ucArticlesSource . "Articles_ArticlesScraper";

		// setup the objects we need for all articleSources
		$db = new Db('http://'.AM_CRAWLER_USER.':'.AM_CRAWLER_PW.'@'.AM_CRAWLER_DB_URL, AM_CRAWLER_DB_NAME);
		$getter = new UrlGetter();

		// instantiate the scraper
		$scraper = new $articlesSourceClassName($db, $getter);

		return $scraper;

	}
}
?>
