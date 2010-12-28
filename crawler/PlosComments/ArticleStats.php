<?php
/**
 * Gathers comments from the PLos commenting system
 *
 */
class PlosComments_ArticleStats {
  	 protected $resultsObj;
	 protected $getter;
	 protected $urls;
	 protected $urlTemplate;
	 protected $baseUrlTemplate;

	 function __construct(UrlGetter $getter, Urls $urls) {
		 $this->getter = $getter;
		 $this->urls = $urls;
		 $this->urlTemplate = 'http://www.[stub].org/article/comments/info%3Adoi%2F[doi]';
		 $this->baseUrlTemplate = 'http://www.[stub].org';
	 }


	 public function setResultsObj(stdClass $resultsObj) {
		 if (!isset($resultsObj->_rev) || !isset($resultsObj->_id)) {
			 throw new Exception("No CouchDb document to put results into.");
		 }
		 $this->resultsObj = $resultsObj;
	 }

	 public function getInfo() {

		 // set up the object
		 if (!$this->resultsObj) {
			 throw new Exception("No document to put results into.");
		 }
		 if (!isset($this->resultsObj->plos_comments)){
			 $this->resultsObj->plos_comments = new stdClass();
		 }
		 if (!isset($this->resultsObj->plos_comments->comments)){
			 $this->resultsObj->plos_comments->comments = array();
		 }
		 $OrigCommentsCount = count($this->resultsObj->plos_comments->comments);
		 $this->resultsObj->plos_comments->last_update = date("c");

		 // get the list of comment threads for this article
		 $doi = $this->resultsObj->_id; // fix this. can't always assume _id will be doi.
		 $threadListUrl = $this->urls->makeUrl($doi, $this->urlTemplate);
		 try {
			 $this->getter->setUrl($threadListUrl);
			 $threadListXmlObj = $this->getter->get($threadListUrl);
			 if (get_class($threadListXmlObj) != 'SimpleXMLElement') {
				 throw new Exception("couldn't get any xml back from threadlist page at $threadListUrl");
			 }
		 } catch (Exception $e){
			 // do nothing.  if there's no $threadListXmlObj, the comments logic won't run.
		 }
		 if (isset($threadListXmlObj)) {
			 // using the list of threads, get all the comments for this article
			 $newComments = $this->getComments($threadListXmlObj, $doi);
			 $allComments = array_merge(
						$this->resultsObj->plos_comments->comments,
						$newComments
						);

			 // clean out duplicate comments
			 $this->resultsObj->plos_comments->comments = array_values(
						array_intersect_key(
								  $allComments,
								  array_unique( array_map('serialize', $allComments) )
								  )
					);
		 }


		 $totalCommentsCount = count($this->resultsObj->plos_comments->comments);
		 $newCommentsCount = $totalCommentsCount - $OrigCommentsCount;


		 $this->resultsObj->latest_changes = "Added $totalCommentsCount comments ($newCommentsCount new).";
		 return $this->resultsObj;

	 }

	 private function getComments(SimpleXMLElement $xml, $doi){
		 $allComments = array();
		 $threadUrls = $xml->xpath("//td[@class='title']/a/@href");
		 if ($threadUrls){
			 foreach ($threadUrls as $threadUrl){
				 $threadUrl = (string)$threadUrl;

				 // construct the url for the thread
				 $baseUrl = $this->urls->makeUrl($doi, $this->baseUrlTemplate);
				 $fullUrl = $baseUrl . $threadUrl;

				 // get the thread xml
				 $this->getter->setUrl($fullUrl);
				 $threadXml = $this->getter->get($threadUrl);
				 if (get_class($threadXml) != 'SimpleXMLElement') {
					 throw new Exception("couldn't get any xml back from comment thread page at $fullUrl");
				 }

				 //parse and add the comments
				 try {
					$parsedComments = $this->parseComments($threadXml);
				 } catch (Exception $e){
					 throw new Exception("couldn't parse comments for thread at $fullUrl: " . $e->getMessage());
				 }
				 $allComments = array_merge($allComments, $parsedComments);
			 }
		 }
		 return $allComments;
	 }
	 
	 private function parseComments(SimpleXMLElement $xml){
		$hdDivs = $xml->xpath('//div[@class="hd"]');
		$bodies = $xml->xpath('//div[contains(@class,"response")]/blockquote');
		if (!$hdDivs || !$bodies){
			throw new Exception("no comments to parse");
		}
		$origThreadId = $this->getCommentId($hdDivs[0]);
		$comments = array();

		foreach ($hdDivs as $k => $hd){ // loop for each comment
			$comment = new stdClass();
			$comment->id = $this->getCommentId($hd);
			$comment->thread = $origThreadId;
			$comment->position = $k;
			
			$comment->title = htmlentities($hd->h3[0]->a[0], ENT_QUOTES, "UTF-8");
			
			preg_match("#account%2F(.+)#", $hd->div[0]->a[0]->attributes()->href, $m);
			$comment->author = $m[1];
			
			$timestampStr = $hd->div[0]->strong[0] . " " . $hd->div[0]->strong[1];
			$comment->pub_date = date('c', strtotime($timestampStr));
			
			$comment->content = trim(htmlentities($bodies[$k]->children()->asXML(), ENT_QUOTES, "UTF-8"));
			$comments[] = $comment;
		}

		return $comments;
	 }

	 private function getCommentId(SimpleXMLElement $hd){
		 preg_match("#doi/10\.1371/(.+)#", $hd->h3[0]->a[0]->attributes()->name, $m);
		 return $m[1];

	 }


    
}
?>
