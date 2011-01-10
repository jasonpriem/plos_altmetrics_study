<?php
/*
 * These takes care of any environmental settings
 */

define('LIBRARY_ROOT',dirname(__FILE__));
require_once(LIBRARY_ROOT . '/' . 'Tools'	. '/' . 'Autoloader.php'
	);
spl_autoload_register(array(new Tools_Autoloader,'loadClass'));

// log exceptions:
function exception_handler($exception) {
	echo (string)$exception;
	file_put_contents('exceptions.txt', date('r') . "\n" . (string)$exception. "\n\n",FILE_APPEND);
}
set_exception_handler('exception_handler');

/*
// turn all errors into exceptions:
function handleError($errno, $errstr, $errfile, $errline, array $errcontext)
{
    // error was suppressed with the @-operator
    if (0 === error_reporting()) {
        return false;
    }

    throw new ErrorException($errstr, 0, $errno, $errfile, $errline);
}
set_error_handler('handleError');
*/

require_once './PHP-on-couch/couch.php';
require_once './PHP-on-couch/couchClient.php';
require_once './PHP-on-couch/couchDocument.php';


// define constants for credentials
// You can define the constants in a file outside this project
require_once '../creds/main.php'; // database creds

// if you don't load the '../creds/main.php' file, you need to set the constants below:
/* 
define("AM_CRAWLER_USER", "CouchDB user");
define("AM_CRAWLER_PW", "password for CouchDB user");
define("AM_CRAWLER_DB_URL", "url for CouchDB; can be localhost or on a different server");
define("AM_CRAWLER_DB_NAME", "CouchDB database name");
define("BACKTWEETS_KEY", "consumer key for the backtweets API");
define("MENDELEY_KEY", "consumer key for the Mendeley API");
*/









?>
