<?php

/**
 * Works with the article-level metrics xml files served for each PLoS article
 *
 */
class PlosAlm_ArticleStats {
  	 protected $resultsObj;
	 protected $getter;
	 protected $urls;
	 protected $urlTemplate;

	 function __construct(UrlGetter $getter, Urls $urls) {
		 $this->getter = $getter;
		 $this->urls = $urls;
		 $this->urlTemplate = 'http://alm.plos.org/articles/info%3Adoi%2F10.1371%2Fjournal.[id].xml?citations=1';
	 }

	 public function setResultsObj(stdClass $resultsObj) {
		 if (!isset($resultsObj->_rev) || !isset($resultsObj->_id)) {
			 throw new Exception("No CouchDb document to put results into.");
		 }
		 $this->resultsObj = $resultsObj;
	 }
	 
	 private function setUrl() {
		 if (!$this->getter) {
			 throw new Exception("No getter object to fetch results with.");
		 }
		 if (!$this->resultsObj) {
			 throw new Exception("No document to put results into.");
		 }
		 $doi = $this->resultsObj->_id; // fix this. can't always assume _id will be doi.
		 $xmlUrl = $this->urls->makeUrl($doi, $this->urlTemplate);
		 $this->getter->setUrl($xmlUrl);		 
	 }

	 public function getInfo() {

		 $this->setUrl();

		 if (!isset($this->resultsObj->plos_alm)) {
					$this->resultsObj->plos_alm = new stdClass();
		 }
		 if (!isset($this->resultsObj->plos_alm->past_stats)) {
					$this->resultsObj->plos_alm->past_stats = new stdClass();
		 }
		 $timestamp = date('c');
		 $this->resultsObj->plos_alm->last_update = $timestamp;

		 try {
			$xml = $this->getter->get(true); // return a SimpleXmlElement object
		 } catch (Exception $e) {
			 // do nothing
		 }

		 if (isset($xml)) {
			 $this->resultsObj->pmid = $this->getPmid($xml);
			 $this->resultsObj->plos_alm->current_stats = (object)array(
				 'blogs' => $this->getBlogPosts($xml),
				 'views' => $this->getViews($xml),
				 'citeulike' => $this->getCiteULike($xml),
				 'scopus' => $this->getScopusCiteCount($xml),
				 'pub_med' => $this->getPubMedCites($xml),
				 'crossref' => $this->getCrossRefCites($xml)
			 );
			 $this->resultsObj->plos_alm->past_stats->{$timestamp} = (object)array(
				 'scopus' => $this->getScopusCiteCount($xml),
				 'pub_med' => $this->getPubMedCites($xml)
			 );
		 }
					
			
		 $this->resultsObj->latest_changes = "added plos alms";
		 return $this->resultsObj;
	 }

	 public function getPmid(SimpleXMLElement $xml){
		 $pmid = (string)$xml->attributes()->pub_med;
		 return ($pmid) ? $pmid : false;
	 }
	 public function getBlogPosts(SimpleXmlElement $xml){
		 $blogs = array();
		 $natureBlogs = $xml->xpath("//source[@source='Nature']/citations/citation");
		 if ($natureBlogs){
			 foreach ($natureBlogs as $citation){
				 $blog = array();
				 $blog['uri'] = (string)$citation->attributes()->uri;
				 $blog['from'] = "Nature via Plos";
				 $blog['pubDate'] = date('c', strtotime($citation->post[0]->attributes()->created_at));
				 $blog['title'] = (string)$citation->post[0]->attributes()->title;
				 $blog['natureBlogId'] = (string)$citation->post[0]->attributes()->blog_id;
				 $blogs[] = $blog;
			 }
		 }

		 $postGenomicBlogs = $xml->xpath("//source[@source='Postgenomic']/citations/citation");
		 if ($postGenomicBlogs){
			 foreach ($postGenomicBlogs as $citation){
				 $blog = array();
				 $blog['uri'] = (string)$citation->attributes()->uri;
				 $blog['from'] = "Postgenomic via Plos";
				 $blog['pubDate'] = date('c', strtotime($citation->pubdate[0]));
				 $blog['title'] = (string)$citation->title[0];
				 $blog['blogName'] = (string)$citation->blog_name[0];
				 $blogs[] = $blog;
			 }
		 }

		 $researchBlogsBlogs = $xml->xpath("//source[@source='Research Blogging']/citations/citation");
		 if ($researchBlogsBlogs){
			 foreach ($researchBlogsBlogs as $citation){
				 $blog = array();
				 $blog['uri'] = (string)$citation->attributes()->uri;
				 $blog['from'] = "Research Blogging via Plos";
				 $blog['pubDate'] = date('c', strtotime($citation->details[0]->attributes()->receiveddate));
				 $blog['title'] = (string)$citation->details[0]->attributes()->title;
				 $blog['blogName'] = (string)$citation->details[0]->attributes()->name;
				 $blogs[] = $blog;
			 }
		 }
		 return ($blogs) ?  $this->escArray($blogs) : false;

	 }

	 public function getViews(SimpleXMLElement $xml){
		 $allViewsArr = array();
		 $views = $xml->xpath("//source[@source='Counter']/citations/citation/details/views");
		 if ($views) {
			 foreach ($views as $viewRow) {
				 $month = (string)$viewRow->attributes()->month;
				 $year = (string)$viewRow->attributes()->year;
				 $pdf = (string)$viewRow->attributes()->pdf_views;
				 $xml = (string)$viewRow->attributes()->xml_views;
				 $html = (string)$viewRow->attributes()->html_views;

				 $allViewsArr[$year . '/' . $month] = array( 'pdf' => $pdf, 'xml' => $xml, 'html' => $html);
			 }
		 return  $allViewsArr;
		 }
		 else {
			 return false;
		 }
	 }

	 public function getCiteULike(SimpleXMLElement $xml) {
		 $allBookmarks = array();
		 $citations = $xml->xpath("//source[@source='CiteULike']/citations/citation");
		 if ($citations) {
			 foreach ($citations as $citation) {
				 $bookmark = array();
				 $bookmark['uri'] = (string)$citation->attributes()->uri;
				 $bookmark['pubDate'] = date('c', strtotime($citation->post_time[0]));
				 $bookmark['tags'] =
					$this->arrayTrim(explode(',', (string)$citation->tags[0]));
				 $bookmark['creator'] = (string)$citation->username;
				 $allBookmarks[] = $bookmark;
			 }
		 return  $this->escArray($allBookmarks);
		 }
		 else {
			 return false;
		 }
	 }

	 public function getScopusCiteCount(SimpleXMLElement $xml){
		 $scopuses = $xml->xpath('//source[@source="Scopus"]');
		 if ($scopuses) {
			 return  (string)$scopuses[0]->attributes()->count;
		 }
		 else {
			 return false;
		 }
	 }

	 public function getPubMedCites(SimpleXMLElement $xml) {
		 $allCites = array();
		 $citations = $xml->xpath("//source[@source='PubMed Central']/citations/citation");
		 if ($citations) {
			 foreach ($citations as $citation) {
				 $cite = array();
				 $cite['uri'] = (string)$citation->attributes()->uri;
				 $allCites[] = $cite;
			 }
		  return $allCites;
		 }
		 else {
			 return false;
		 }
	 }

	 public function getCrossRefCites(SimpleXMLElement $xml) {
		 $allCites = array();
		 $citations = $xml->xpath("//source[@source='CrossRef']/citations/citation");
		 if ($citations) {
			 foreach ($citations as $citation) {
				 $cite = array();
				 $cite['uri'] = (string)$citation->attributes()->uri;
				 $cite['doi'] = (string)$citation->doi[0];
				 $cite['journalTitle'] = (string)$citation->journal_title[0];
				 $cite['title'] = (string)$citation->article_title[0];
				 $cite['pubYear'] = (int)$citation->year[0];
				 $cite['creator'] = $this->arrayTrim(explode(',', (string)$citation->contributors[0]));
				 $allCites[] = $cite;
			 }
		 return $this->escArray($allCites);
		 }
		 else {
			 return false;
		 }
	 }



	 private function arrayTrim($arr)
	 {
		 foreach ($arr as &$v){
			 $v = trim($v);
		 }
		 return $arr;
	 }

	 
	 private function escArray($value)
	 {
		 return is_array($value) ?
			array_map(array($this, 'escArray'), $value) :
			htmlspecialchars($value, ENT_NOQUOTES, 'UTF-8', false);
	 }


}
?>