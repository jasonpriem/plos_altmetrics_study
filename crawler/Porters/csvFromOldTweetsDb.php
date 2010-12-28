<?php
/*
 * This makes a csv file from the old sql tweets database i had. This can
 * in turn be read into the new CouchDb database more easily.
 */
require_once '../../creds/main.php';

try {
	$db = new PDO("mysql:host=" .SQL_HOST. ";dbname=" .SQL_DB, SQL_USER, SQL_PW);
	$db->setAttribute(PDO::ATTR_ERRMODE, PDO::ERRMODE_EXCEPTION);
	}
catch (PDOException $e) {
	echo $e->getMessage();
	}

/*
$stmt = $db->query("SELECT * FROM tweets");
$tweets = array();
while ($row = $stmt->fetch(PDO::FETCH_NUM)) {
	$tweet = new stdClass();
	$tweet->tweet_id = $row[0];
	$tweet->tweet_from_user_id = $row[1];
	$tweet->tweet_from_user = $row[2];
	$tweet->tweet_profile_image_url = $row[3];
	$tweet->tweet_created_at = date('c', $row[4]);
	$tweet->tweet_text = $row[5];
	$tweets[] = $tweet;

}
var_dump($tweets);
file_put_contents('./tweetsFromOldDb.json', json_encode($tweets));

*/
$tweets = json_decode(file_get_contents('./tweetsFromOldDb.json'));
echo count($tweets);
var_dump($tweets);









?>
