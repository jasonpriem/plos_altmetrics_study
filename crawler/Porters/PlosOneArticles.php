<?php

/**
 * Moves PLoS ONE articles from the old MySQL database to the new
 * CouchDB one.
 *
 */
class Porters_PlosOneArticles {
	private $pdo;
	private $article;
	private $sql;
	
	function __construct(PDO $pdo, Resources_PlosOneArticle $article) {
		$this->pdo = $pdo;
		$this->article = $article;
		$this->sql = "
			SELECT articles.id, articles.title, articles.pubDate, GROUP_CONCAT( authorNames.fullName
			ORDER BY authorNames.fullName
			SEPARATOR '|' )
			FROM articles
			LEFT JOIN articles_authorNames ON articlesPlosID = articles.id
			LEFT JOIN authorNames ON authorNames.id = authorNamesId
			WHERE articles.title IS NOT NULL
			GROUP BY articles.id, articles.title, articles.pubDate
			  ";
	}

	function port() {
		$sth = $this->pdo->prepare($this->sql);
		$sth->execute();
		while ($row = $sth->fetch(PDO::FETCH_NUM)){
			$this->article->setID($row[0]);
			$this->article->setTitle($row[1]);
			$this->article->setTimestamp($row[2]);
			$this->article->setAuthorsArr(explode("|", $row[3]));
			$this->article->saveNew();
		}
	}

}
?>
