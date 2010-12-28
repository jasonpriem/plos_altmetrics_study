<?php
class Tools_Getter {
	private $httpCode;
	private $response;

	/**
	 * Gets a url
	 * Retries errors $numTries times; also load html response codes in 
	 * the httpCode property 
	 * 
	 * @param string $url The url to get
	 * @param int $numTries the number of time to retry a connection that gives an error
	 * @param int $i the number of retries so far
	 * @return string the body of the GET-request response
	 */
	public function getUrl($url, $numTries = 3, $i = 1)
	{
		$ch = curl_init( $url );
		curl_setopt($ch, CURLOPT_RETURNTRANSFER, true); // return web page
		curl_setopt($ch, CURLOPT_HEADER, false); // don't return headers
		curl_setopt($ch, CURLOPT_FOLLOWLOCATION, true); // follow redirects
		curl_setopt($ch, CURLOPT_ENCODING, "utf-8");    
		curl_setopt($ch, CURLOPT_AUTOREFERER, true); // set referer on redirect
		curl_setopt($ch, CURLOPT_CONNECTTIMEOUT, 20); // timeout on connect   
		curl_setopt($ch, CURLOPT_TIMEOUT, 20); // timeout on response   
		curl_setopt($ch, CURLOPT_MAXREDIRS, 10);
		
		$page = curl_exec( $ch );
		$info = curl_getinfo($ch);
		if (curl_errno($ch) && curl_errno($ch) != 18){ // hack; PLoS causes a bunch of incorrect '18' errors.
			if ($i >= $numTries) {
				throw new Exception("Error getting $url: " . curl_errno($ch). ": " . curl_error($ch));
				curl_close($ch);
			}
			else { // recurse $numTries times
				echo "Error getting $url: " . curl_errno($ch). ": " . curl_error($ch) . "; on try $i; trying again.\n";
				curl_close($ch);
				return $this->getUrl($url, $numTries, $i+1);
			}
		}
		curl_close( $ch );
		
		$this->httpCode = $info['http_code'];
		$this->response = $page;
		return true;
	}

	public function getHttpCode() {
		return $this->httpCode;
	}
	public function getResponse() {
		return $this->response;
	}

}



?>