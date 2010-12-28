<?php
/**
 * Does what it says on the tin.
 */
class Tools_Autoloader {

	public function loadClass($class)
	{
		if(!defined('LIBRARY_ROOT'))
		{
			throw new Exception('LIBRARY_ROOT must be defined');
		}

		$classPath=preg_replace('/_/','/',$class);
		$classFile=LIBRARY_ROOT.'/'.$classPath.'.php';
		if(file_exists($classFile) && is_readable($classFile)) {
			require($classFile);	
		}
	}
}


?>
