SELECT
	source_doi,
    target_doi,
    citations.type,
    year AS source_year,
    title_slug AS source_title_slug
FROM citations
LEFT JOIN doidata
	ON doidata.doi = citations.source_doi
WHERE target_doi IN (SELECT doi FROM medicine_publication_record)
	AND year IS NOT NULL;
    