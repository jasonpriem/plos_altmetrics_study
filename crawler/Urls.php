<?php

/**
 * Makes urls to scrape stuff with
 *
 */
class Urls {

	private $template;
	private $urlStubs;
	private $plosUrlTemplate;
	private $plosUrl;
	
	public function  __construct() {
		$this->urlStubs = array(
			 'pone'	=> 'plosone',
			 'pbio'	=> 'plosbiology',
			 'pmed'	=> 'plosmedicine',
			 'pcbi'	=> 'ploscompbiol',
			 'pgen'	=> 'plosgenetics',
			 'ppat'	=> 'plospathogens',
			 'pntd'	=> 'plosntds'
		 );
	}
	
	
	 /**
	  * makes a url out of a doi, using a template specific to each ArticleInfo type
	  * Template url should have [stub] and [id] fields.
	  *
	  * @param string $key a doi
	  */
	 public function makeUrl($key, $template){

		 $url = $template;
		 if ( preg_match('#journal.\w{4}.\d{7}#', $key) ) { // key is a doi
			 $id = substr($key, -12);
			 $idParts = explode('.', $id);
			 $journalId = $idParts[0];
			 $articleId = $idParts[1];
			 $url = str_replace('[stub]', $this->urlStubs[$journalId], $url);
			 $url = str_replace('[id]', $journalId .'.'. $articleId, $url);
			 $url = str_replace('[doi]', $key, $url);
		 }

		 $url = str_replace('[prefixPlusDoi]', $key, $url); // hack so you can urlencode the whole "info:doi/<doi>"
		 $url = str_replace('[plosUrlHash]', $key, $url);

		 return $url;
	 }

	 
	 public function plosDoiFromUrl($url){
		 $doiPrefix = "10.1371/journal.";
		 $stubsStr = implode('|', array_keys($this->urlStubs));
		 $plosArticleRegex = "/($stubsStr)\.\d{7}/";
		 preg_match($plosArticleRegex, $url, $m);

		 if (!$m) return false;

		 return $doiPrefix . $m[0];
	 }
	 
}
?>
