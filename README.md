ubg4
=====

Uwspółcześniona Biblia Gdańska - WEB

Build
-----

```bash
rebar3 compile
```

Build a docker release
-------

```bash
./release.sh
```

Start the release
----------------

```bash
docker run -p "8080:8080" -d finalclass/ubg4
```
