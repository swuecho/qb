\restrict a1b2c3
CREATE TABLE accounts (
  id BIGSERIAL PRIMARY KEY,
  email TEXT NOT NULL UNIQUE,
  display_name VARCHAR(80),
  active BOOLEAN NOT NULL DEFAULT true,
  created_at TIMESTAMPTZ NOT NULL DEFAULT now()
);

CREATE TABLE posts (
  id UUID PRIMARY KEY,
  account_id BIGINT NOT NULL REFERENCES accounts (id),
  title TEXT NOT NULL,
  body TEXT,
  score REAL,
  meta JSONB,
  published_on DATE,
  "type" TEXT
);

CREATE INDEX posts_account ON posts (account_id);

CREATE TABLE scratch (x INT);

-- Another schema, written the way pg_dump writes a serial column.
CREATE SCHEMA audit;

CREATE TABLE audit.accounts (
  event_id INTEGER NOT NULL,
  account_id BIGINT NOT NULL,
  at TIMESTAMPTZ NOT NULL
);

CREATE SEQUENCE audit.accounts_event_id_seq;
ALTER TABLE ONLY audit.accounts
  ALTER COLUMN event_id SET DEFAULT nextval('audit.accounts_event_id_seq'::regclass);
ALTER TABLE ONLY audit.accounts ADD CONSTRAINT accounts_pkey PRIMARY KEY (event_id);
ALTER TABLE audit.accounts ENABLE ROW LEVEL SECURITY;
\unrestrict a1b2c3
