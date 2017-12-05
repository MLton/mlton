FROM ubuntu:latest

# Install the dependencies. We'll use the ubuntu provided mlton to bootstrap our local build.
RUN apt-get update -y -qq \
 && apt-get install -y -qq git build-essential libgmp-dev libtool automake mlton mlton-tools 

# Copy the current directory (MLton source root) to a location within the container & move there
COPY . /root/mlton
WORKDIR /root/mlton

# Build from source & install
RUN autoreconf -vfi \
 && ./configure \
 && make \
 && make install PREFIX=

ENTRYPOINT ["make", "check"]
