FROM docker.io/ubuntu:20.04
RUN apt-get update && apt-get install -y libpq-dev
ADD bin/crossmap-dev /usr/local/bin/crossmap-dev
ADD bin/crossmap-dev-server /usr/local/bin/crossmap-dev-server
ENTRYPOINT ["/usr/local/bin/crossmap-dev-server"]
