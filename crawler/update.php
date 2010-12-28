<pre>
<?php
require_once './init.php';
date_default_timezone_set("UTC");
set_time_limit(0);

if (isset($_GET['articleStatsType'])){
	$updaterFactory = new UpdaterFactory();
	$updater = $updaterFactory->create($_GET['articleStatsType']);
	$updater->update();
}


if (isset($_GET['articlesSource'])) {
	$articlesScraperFactory = new ArticlesScraperFactory();
	$scraper = $articlesScraperFactory->create($_GET['articlesSource']);
	$scraper->update();
}

?>
</pre>