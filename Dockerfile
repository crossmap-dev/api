FROM docker.io/debian:bookworm-slim
ENV LANG=C.UTF-8
RUN apt-get update && apt-get install -y libpq-dev zlib1g-dev
RUN adduser --system --group --no-create-home crossmap-dev
USER crossmap-dev
ADD bin/crossmap-dev /usr/local/bin/crossmap-dev
ADD bin/crossmap-dev-server /usr/local/bin/crossmap-dev-server
ADD migrations /usr/local/share/crossmap-dev/migrations
ENTRYPOINT ["/usr/local/bin/crossmap-dev-server"]
