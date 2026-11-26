# Contributing to qb

Thanks for taking the time to contribute. Bug reports, questions, examples and
pull requests are all welcome.

## Reporting a bug

Open an issue with:

- what you expected and what happened instead, including any error output;
- a small query or code sample that reproduces the problem;
- your OCaml version (`ocaml -version`), the qb version or commit, and the
  database and driver you are using.

If the problem is in the parser or the ppx, include the migration that defines
the affected table and the SQL that fails.

## Development setup

You need [opam](https://opam.ocaml.org/) 2.1 or later and OCaml 4.14 or later.
OCaml 5.4 is needed to run `example/users.exe`, which demonstrates labeled
tuples; the libraries themselves work on 4.14.

```sh
git clone https://github.com/swuecho/qb.git
cd qb
opam install . --deps-only --with-test --with-doc
```

`qb-pg-parse`, `qb-ppx` and `qb-migrate` need
[libpg_query](https://github.com/pganalyze/libpg_query). On macOS:

```sh
brew install libpg_query
```

On Linux, build and install a tagged release:

```sh
git clone --depth 1 --branch 18.1.0 https://github.com/pganalyze/libpg_query.git
make -C libpg_query -j"$(nproc)"
sudo make -C libpg_query install
```

If libpg_query is somewhere else, set `LIBPG_QUERY_PREFIX` to the prefix that
contains its `include/` and `lib/` directories.

The test suites also link against `libsqlite3` and `libpq` (via
`caqti-driver-sqlite3` and `caqti-driver-postgresql`); `opam install . --deps-only --with-test`
installs their system packages automatically where depexts are enabled (e.g.
`libsqlite3-dev` and `libpq-dev` on Debian/Ubuntu, or `brew install libpq sqlite`
on macOS).

PostgreSQL is only needed for the Postgres test suite. The simplest way to get
one is Docker:

```sh
scripts/test-pg.sh
```

The script starts a throwaway `postgres:17-alpine` container, runs the full
suite against it, and removes it afterwards. To use an existing database:

```sh
QB_TEST_PG_URL=postgresql://user:pass@localhost/db dune test --force
```

## Build, test and format

```sh
dune build                 # build everything
dune test                  # unit tests and cram tests (SQLite; Postgres if configured)
dune build @fmt            # check formatting
dune build @fmt --auto-promote  # apply formatting
dune build @doc            # build the odoc documentation
```

The core tests live in `test/`, the Lwt tests in `test-lwt/`, and each package
has its own tests. Cram tests under `ppx/test/` check the exact compiler errors
produced by `[%sql]`; run them with `dune test` and update them with
`dune test --auto-promote` after an intentional message change.

Formatting is pinned to ocamlformat 0.29.0 by `.ocamlformat`. Install that
version (`opam install ocamlformat.0.29.0`) before running `@fmt`.

## Project layout

| Directory | Contents |
| --- | --- |
| `src/` | the `qb` builder and its `.mli` documentation |
| `lwt/` | `qb-lwt` |
| `pg-parse/` | `qb-pg-parse`: libpg_query bindings, schema import and code generation |
| `ppx/` | `qb-ppx`: the `[%sql]` rewriter and its cram tests |
| `migrate/` | `qb-migrate`: migration runner, verifier and CLI |
| `doc/` | tutorial and references |
| `example/` | runnable examples |
| `test/`, `test-lwt/` | the core test suites |

## Pull requests

- Keep each change focused, and add a test for a bug fix or a new feature.
- Update the `.mli` documentation and the relevant page under `doc/` when the
  public API or SQL behaviour changes.
- Add a line to the `Unreleased` section of `CHANGELOG.md` for user-visible
  changes.
- Run `dune build @fmt` and `dune test` (or let CI do it) before asking for a
  review.

## Release process

The five packages are released together and share one version, because
`qb-lwt`, `qb-ppx` and `qb-migrate` depend on the exact version of `qb` or
`qb-pg-parse`.

1. Move the `Unreleased` entries in `CHANGELOG.md` under the new version.
2. Make sure CI is green on `main`.
3. Tag the release and push the tag:

   ```sh
   git tag -a v0.1.0 -m "qb 0.1.0"
   git push origin main --tags
   ```

4. Publish the packages with [opam-publish](https://github.com/ocaml/opam-publish):

   ```sh
   opam publish
   ```

## License

By contributing, you agree that your contributions are licensed under the
[MIT License](LICENSE).
