FROM ubuntu:latest

# Install the dependencies. We'll use the ubuntu provided mlton to bootstrap our local build.
RUN apt-get update -qq \
 && apt-get install -qq git build-essential libgmp-dev mlton mlton-tools

# Copy the current directory (MLton source root) to a location within the container & move there
COPY . /root/mlton
WORKDIR /root/mlton

# Build from source & install
RUN make \
 && make install

ENTRYPOINT ["make", "check"]
