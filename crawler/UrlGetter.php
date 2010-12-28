<?php
/**
 * Gets a file specified by a url
 *
 */
class UrlGetter {
    protected $url;
	 protected $numRetries;
	 protected $httpReturnCode;
	 protected $userAgent;
	 
	 function __construct($numRetries = 0) {
		 $this->numRetries = $numRetries;
		 $this->userAgent = "alt-metrics crawler (+http://altmetrics.org)";
	 }

	 public function setUrl($url) {
		 $this->url = $url;
	 }

	 public function setNumRetries($numRetries) {
		 $this->numRetries = $numRetries;
	 }
	 public function getUrl() {
		 return $this->url;
	 }
	 public function getHttpReturnCode() {
		 return $this->httpReturnCode;
	 }
	 public function setUserAgent($userAgent) {
		 $this->userAgent = $userAgent;
	 }



	 
	 
	 
	 
	 public function get($returnSimpleXml = true, $i = 1)
	 {
		 if (!$this->url) {
			 throw new Exception("No url set.");
		 }
		 
		 try  {
			 $res = file_get_contents($this->url);
		 } catch (Exception $e){
			 if ($i <= $this->numRetries) {// calls self again until number of retries met
				 return $this->get($returnSimpleXml, $i+1);
			 }
			 else {
				 throw new Exception("Failed to get url: '" .$e->getMessage(). "'. Giving up.");
			 }
		 }
		 
		 return ($returnSimpleXml) ? $this->asSimpleXml($res) : $res;
	 }

	 public function curlGet(){
		 $options = array(
				  CURLOPT_RETURNTRANSFER => true,
				  CURLOPT_HEADER         => false,
				  CURLOPT_FOLLOWLOCATION => true,
				  CURLOPT_USERAGENT      => $this->userAgent,
				  CURLOPT_AUTOREFERER    => true,
				  CURLOPT_CONNECTTIMEOUT => 60,
				  CURLOPT_TIMEOUT        => 60,
				  CURLOPT_MAXREDIRS      => 10,
				  CURLOPT_VERBOSE        => 1            
			 );

		$ch      = curl_init($this->url);
		curl_setopt_array($ch,$options);
		$content = curl_exec($ch);
		if (curl_errno($ch)){
		 throw new Exception("cURL error: " . curl_error($ch));
		}
		$info  = curl_getinfo($ch);
		$this->httpReturnCode = $info['http_code'];
		curl_close($ch);
		return $content;
	 }
	 
	 private function asSimpleXml($str)
	 {
		 if (stripos($str, '<!DOCTYPE html') !== false)	 { // we've got to parse the html first
			$dom = new domDocument;
			// hack needed or else loadHTML won't work with utf-8:
			@$dom->loadHTML('<?xml encoding="UTF-8">' . $str);
			$simpleXmlObj = simplexml_import_dom($dom);
		 }
		 elseif (stripos($str, '<?xml') === 0){
			 $simpleXmlObj = simplexml_load_string($str);
		 }
		 else {
			 throw new Exception("Can't parse as XML or HTML.");
		 }
		 return $simpleXmlObj;
	 }
	 
}
?>
