FROM docker.io/debian:bookworm-slim
ENV LANG=C.UTF-8
RUN apt-get update && apt-get install -y libpq5 zlib1g
RUN adduser --system --group --home /var/crossmap crossmap
USER crossmap
ADD target/crossmap /usr/local/bin/crossmap
ADD target/crossmap-server /usr/local/bin/crossmap-server
ADD migrations /usr/local/share/crossmap/migrations
ENV MIGRATION_DIR=/usr/local/share/crossmap/migrations
ENV HOME=/var/crossmap
ENTRYPOINT ["/usr/local/bin/crossmap-server"]
