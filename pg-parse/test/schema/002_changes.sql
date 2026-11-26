-- Replayed after 001: the generated modules show the end state.
ALTER TABLE accounts ADD COLUMN login_count INTEGER NOT NULL DEFAULT 0;
ALTER TABLE accounts RENAME COLUMN display_name TO nickname;
ALTER TABLE posts ALTER COLUMN body SET NOT NULL, DROP COLUMN score;
ALTER TABLE posts ADD COLUMN rating DOUBLE PRECISION;
ALTER TABLE posts RENAME TO articles;
DROP TABLE scratch;
INSERT INTO accounts (email) VALUES ('seed@example.com');
ALTER TABLE articles ADD COLUMN "values" TEXT;
