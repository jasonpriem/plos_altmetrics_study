<pre>
<?php
require_once '../PHP-on-couch/couch.php';
require_once '../PHP-on-couch/couchClient.php';
require_once '../PHP-on-couch/couchDocument.php';
require_once '../Db.php';
require_once '../../creds/main.php'; // database creds
set_time_limit(0);



$db = new Db(
		  'http://'
		  .AM_CRAWLER_USER.':'
		  .AM_CRAWLER_PW.'@'
		  .AM_CRAWLER_DB_URL
		  , AM_CRAWLER_DB_NAME
		  );
$externalDb = new Db(
		  'http://'
		  .EXTERNAL_AM_CRAWLER_USER.':'
		  .EXTERNAL_AM_CRAWLER_PW.'@'
		  .EXTERNAL_AM_CRAWLER_DB_URL
		  , EXTERNAL_AM_CRAWLER_DB_NAME
		  );
var_dump( $externalDb->listDatabases());

$chunkSize = 25;
$moreRows = true;
$start = null;
$i = 0;

while ($moreRows = true){
	echo ($i * $chunkSize) . " documents processed...\n";
	$response = $db
			  ->include_docs(true)
			  ->startkey($start)
			  ->limit($chunkSize)
			  ->getView('crawl', 'all');
	$moreRows = count($response->rows) - 1;
	$start = end($response->rows)->key;
	$i++;

	echo "storing docs to remote db\n";
	$docsToStore = array();
	foreach ($response->rows as $k => $row){
		$doc = $row->doc;
		$doc->doi = $doc->_id;
		$docsToStore[] = $doc;
	}
	$storeRes = $externalDb->storeDocs($docsToStore);
	var_dump($storeRes);
}




?>
</pre>