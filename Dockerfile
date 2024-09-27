FROM docker.io/debian:bookworm-slim
ENV LANG=C.UTF-8
RUN apt-get update && apt-get install -y libpq5 zlib1g
RUN adduser --system --group --no-create-home crossmap-dev
USER crossmap-dev
ADD target/crossmap-dev /usr/local/bin/crossmap-dev
ADD target/crossmap-dev-server /usr/local/bin/crossmap-dev-server
ADD migrations /usr/local/share/crossmap-dev/migrations
ENV MIGRATION_DIR=/usr/local/share/crossmap-dev/migrations
ENTRYPOINT ["/usr/local/bin/crossmap-dev-server"]
