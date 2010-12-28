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

require_once './PHP-on-couch/couch.php';
require_once './PHP-on-couch/couchClient.php';
require_once './PHP-on-couch/couchDocument.php';
require_once '../creds/main.php'; // database creds

?>
