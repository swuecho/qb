-- Queries for the schema in schema/, turned into Queries_gen by the dune rule.

-- name: account_by_email
SELECT * FROM accounts WHERE email = :email;

-- name: articles_with_authors
-- Every article of at least this rating, with its author.
SELECT ar.*, ac.*
FROM articles ar
JOIN accounts ac ON ac.id = ar.account_id
WHERE ar.rating >= :min_rating
ORDER BY ar.title;

-- name: accounts_and_articles
SELECT ac.*, ar.*
FROM accounts ac
LEFT JOIN articles ar ON ar.account_id = ac.id
ORDER BY ac.id, ar.title;

-- name: titles
SELECT ac.email, ac.nickname, ar.title, ar.rating AS stars
FROM accounts ac
LEFT JOIN articles ar ON ar.account_id = ac.id AND ar.title LIKE :pattern
WHERE ac.active = :active AND ac.login_count BETWEEN :low AND :high
ORDER BY ac.email, ar.title
LIMIT :limit;

-- name: article_count
SELECT count(*) FROM articles WHERE account_id = :account_id;

-- name: counts
SELECT ac.email, count(ar.id) AS n, coalesce(max(ar.rating), 0::float8) AS best
FROM accounts ac
LEFT JOIN articles ar ON ar.account_id = ac.id
GROUP BY ac.email
ORDER BY n DESC, ac.email;

-- name: by_nickname
SELECT email FROM accounts
WHERE (:nickname?::text IS NULL AND nickname IS NULL) OR nickname = :nickname
ORDER BY email;
