ubg4
=====

Uwspółcześniona Biblia Gdańska - WEB

Build
-----

```bash
./rebar3 compile
```

Then to start in dev mode:

```bash
./rebar3 shell
```

Build a docker release
-------

```bash
./release.sh
```

Start the release
----------------

```bash
docker run -p "8080:8080" -d --restart=always finalclass/ubg4
```
