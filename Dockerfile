# ---- Build stage ----
FROM ocaml/opam:debian-12-ocaml-5.2 AS build

# Switch to opam user (preconfigured)
USER opam
WORKDIR /home/opam/app

# Install dune if not already installed
RUN opam install -y dune

# Install dependencies
RUN sudo apt-get update && sudo apt-get install -y \
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
RUN sudo rm -rf /var/lib/apt/lists/*

# Copy opam file
COPY --chown=opam:opam ./unto.opam .

# Install all dependencies declared in the .opam file
RUN opam install -y . --deps-only

# Copy project files
COPY --chown=opam:opam . .

# Build the project
# RUN dune build @install
RUN opam exec -- dune build 

# ---- Runtime stage ----
FROM debian:bookworm-slim

WORKDIR /app

# Install dependencies
# NOTE: this is the same as above
RUN apt-get update && apt-get install -y \
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
RUN rm -rf /var/lib/apt/lists/*

# Copy built binaries from the builder
COPY --from=build /home/opam/app/_build/default/bin/main.exe /usr/local/bin/unto
COPY --from=build /home/opam/app/static ./static
COPY ./new_tokens.json .

# Expose port Dream listens on
EXPOSE 8080

# Run the executable
CMD ["unto", "run-app", "-t", "new_tokens.json"]

