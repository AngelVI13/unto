### Building

#### Local

This assumes you already have opam installed and a switch installed and active

0. Install dune if not available
```sh
opam install -y . --deps-only
```

1. Install OS deps 

```sh
sudo apt-get update && sudo apt-get install -y \
libssl3 \
libpcre3 \
libev4 \
sqlite3 \
libcurl3-gnutls \
libcurl4-gnutls-dev \
libgmp-dev \
libev-dev \
libgmp-dev \
pkg-config \
liblz4-dev \
libpcre3-dev \
libsqlite3-dev \
libssl-dev \
ca-certificates 
```

2. Install opam deps
```sh
opam install -y . --deps-only
```

3. Build

```sh
opam exec -- dune build
```

#### Docker

```sh
docker build -t unto .
```

NOTE: If you want to remove cache pass in the flag `--no-cache-filter "*"`

### Running

#### Local

```sh
./_build/default/bin/main.exe run-app -t new_tokens.json -d app.db
```

NOTE: this assumes you have the slack client and client secret variables
available in a `.env` file or already loaded as env variables

#### Docker

```sh
docker run --env-file .env -p 8080:8080 unto
```

NOTE: here the slack client and client secrets are loaded into the docker
container from a `.env` file with the `--env-file` flag.

### Deploy

#### Fly.io

Deploying to fly.io is very simple. Just run the command

```sh
fly deploy
```

It will look for a Dockerfile in the repository, it will build it and send the image to your app's machine.

> NOTE: to set environment variables for the app's machine (so you don't expose
> any tokens etc.) use the fly secrets command.
```sh
fly secrets VAR=VALUE
```
