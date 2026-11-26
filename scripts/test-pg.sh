#!/bin/sh
# Run the test suite against a throwaway Postgres in Docker.
#   scripts/test-pg.sh            # all tests
#   scripts/test-pg.sh ./test     # extra arguments go to `dune test`
# QB_PG_IMAGE and QB_PG_PORT override the image and host port.
set -eu

image=${QB_PG_IMAGE:-postgres:17-alpine}
port=${QB_PG_PORT:-54329}
name=qb-test-pg-$$

docker run -d --rm --name "$name" \
  -e POSTGRES_USER=qb -e POSTGRES_PASSWORD=qb -e POSTGRES_DB=qb \
  -p "127.0.0.1:$port:5432" "$image" >/dev/null
trap 'docker stop "$name" >/dev/null' EXIT INT TERM

# Over TCP, so this waits past the image's init-time server (socket only).
tries=0
until docker exec "$name" pg_isready -q -h 127.0.0.1 -U qb -d qb; do
  tries=$((tries + 1))
  if [ "$tries" -gt 60 ]; then
    echo "Postgres did not start" >&2
    exit 1
  fi
  sleep 0.5
done

cd "$(dirname "$0")/.."
QB_TEST_PG_URL="postgresql://qb:qb@127.0.0.1:$port/qb" dune test --root . --force "$@"
