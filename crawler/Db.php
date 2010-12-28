<?php
/**
 * Description of Db
 *
 */
class Db extends couchClient {
	private $viewToGet;
	private $persistantStartKey;

	public function setViewToGet($designDoc, $viewName) {
		$this->viewToGet = array('designDoc' => $designDoc, 'viewName' => $viewName);
	}
	public function setPersistantStartKey($persistantStartKey) {
		$this->persistantStartKey = $persistantStartKey;
	}

	



	/**
	 * Gets the hightest _id in a particular namespace
	 *
	 * @param string $namespace	the namespace you're looking for
	 * @return string	The highest id in the specified namespace
	 */
	public function getHighestId($namespace)
	{
		$result = $this
			->startkey(array($namespace,"abc\ufff0"))
			->descending(true)
			->limit(1)
			->getView('crawl', 'nativeIds_by_namespace');
		return $result->rows[0]->value;
	}



	public function getDocWithRandomSkip($maxToSkip=25, $skip = true)
	{

		$rowsToSkip = ($skip) ? mt_rand(1, $maxToSkip) : 0;
		$this->skip($rowsToSkip);
		// the "persistant startkey" is a huge hack to cover the way php-on-couch
		//		resets the startkey every time it calls getView.
		if (isset($this->persistantStartKey)) $this->startkey($this->persistantStartKey);

		// note that this uses the NEW skip behavior (v1.0.1).  Will break on v1.0.0.
		$this->descending(true)->include_docs(true)->limit(1);
		$result = $this->getView($this->viewToGet['designDoc'], $this->viewToGet['viewName']);

		if ($result->total_rows) {
			if (isset($result->rows[0])) { // we got something back. yay!
				return $result->rows[0]->doc;
			}
			elseif ($rowsToSkip > 0) { // maybe there are more rows, but we're skipping over them
				return $this->getDocWithRandomSkip($maxToSkip, false);
			}
		}

		// either there are no rows, or our startkey puts us at the end of all rows.
		throw new Exception("No documents returned.");
	}

}
?>
