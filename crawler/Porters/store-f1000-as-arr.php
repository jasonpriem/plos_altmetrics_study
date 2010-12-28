<?php
/* 
 * Takes a F1000-generated TSV file with a row for each article
 * Creates a saves a text file containing a serialized PHP array of the form
 * NLM_ID => F1000_Factor
 */

$inputFileLoc = "plos-extract-3-article-factors.txt";
$outputFileLoc = "f1000-factors.txt";
$handle = fopen($inputFileLoc, "r");
$res = array();
while ($row = fgetcsv($handle, 5000, "\t")){
	$nlmId = $row[2];
	$res[$nlmId] = (int)$row[3];
}
//print_r($res);
fclose($handle);
$serializedRes = serialize($res);
file_put_contents($outputFileLoc, $serializedRes);


?>
