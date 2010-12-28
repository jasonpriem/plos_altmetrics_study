<?php

/**
 * gets and parses xml for an article
 *
 */
class PlosArticles_ArticleStats {
	 protected $resultsObj;
	 protected $getter;
	 protected $urls;
	 protected $urlTemplate;

	 function __construct(UrlGetter $getter, Urls $urls) {
		 $this->getter = $getter;
		 $this->urls = $urls;
		 $this->urlTemplate = 'http://www.[stub].org/article/fetchObjectAttachment.action?uri=info%3Adoi%2F10.1371%2Fjournal.[id]&representation=XML';;
		 date_default_timezone_set("UTC");
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

	 public function getInfo(){

		 $this->setUrl();

		 try {
			$xml = $this->getter->get(true);
		 } catch (Exception $e) {
			 // throw new Exception ("Couldn't fetch XML: " .$e->getMessage());
			 // this is just to contain the file_get_contents exception for 500 errors.
			 // yes, it is ugly.
		 }

		 if (isset($xml)){
			 $articleTypes = $xml->xpath("//subj-group[@subj-group-type='heading']/subject");
			 if ($articleTypes) {
				$this->resultsObj->articleType = (string)$articleTypes[0];
			 }
			 else {
				 throw new Exception("Couldn't find article type.");
			 }


			 $mainSubjects = array();
			 $subSubjects = array();
			 $subjects = $xml->xpath("//subj-group[@subj-group-type='Discipline']/subject");
			 if ($subjects){
				 foreach ($subjects as $compoundSubject){
					 if (strpos($compoundSubject, '/')){
						$splitSubjects = explode('/', $compoundSubject);
						$mainSubjects[] = $splitSubjects[0]; // the subject ahead of the slash is the broader one
						$subSubjects[] = $splitSubjects[1];
					 }
					 else {
						 $mainSubjects[] = (string)$compoundSubject;
					 }
				 }
				$this->resultsObj->subjects = array_keys(array_flip($mainSubjects)); // remove duplicates & reindex
				$this->resultsObj->subSubjects = array_keys(array_flip($subSubjects));
			 }
			 // if there are no subjects, that's ok; we dont' throw an exception.

			 if ($this->resultsObj->articleType == "Research Article") {
				 $authors = array();
				 $names = $xml->xpath("//contrib[@contrib-type='author']/name|//contrib[@contrib-type='author']/collab");
				 if ($names){
					 foreach ($names as $name){
						 if ($name->surname){
							$thisAuthor = $name->surname .', '. $name->{'given-names'};
						 } else {
							 $thisAuthor = (string)$name;
						 }
						 $authors[] = $thisAuthor;
					 }
					 $this->resultsObj->authors = $authors;
				 } else {
					 throw new Exception("Couldn't find any author names.");
				 }
			 }

			 $pubDates = $xml->xpath("//pub-date[@pub-type='epub']");
			 date_default_timezone_set("UTC");
			 if ($pubDates){
				 $dateStr =
							(string)$pubDates[0]->year
							. '/' . (string)$pubDates[0]->month
							. '/' . (string)$pubDates[0]->day;

				 $this->resultsObj->pubDate =  date("c", strtotime($dateStr));
			 }
			 else{
				 throw new Exception("Couldn't find any pub date.");
			 }

			 $titles = $xml->xpath("//article-title");
			 if ($titles) {
				$this->resultsObj->title = strip_tags($titles[0]->asXML());
			 }
			 else {
				 throw new Exception("Couldn't find title.");
			 }
		 }
		 $changes = (isset($this->resultsObj->title)) ? "Added PlosArticle metadata" : "Attempted to add empty PlosArticle metadata.";
		 $this->resultsObj->latest_changes = $changes;
		 $this->resultsObj->metadata_last_update = date("c");


		 return $this->resultsObj;
	 }

}
?>
