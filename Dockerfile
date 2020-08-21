FROM fpco/stack-build:lts as build
RUN mkdir -p /opt/build/bin
COPY . /opt/build
RUN cd /opt/build && stack install --system-ghc
FROM ubuntu:latest
RUN mkdir -p /opt/lqpl/bin
WORKDIR /opt/lqpl/bin
RUN apt-get update && apt-get install -y ca-certificates libgmp-dev
COPY --from=build /opt/build/bin/lqpl-emulator .
COPY --from=build /opt/build/bin/lqpl-compiler-server .
COPY --from=build /opt/build/scripts/run-lqpl-servers.sh .
# default to running the emulator
ENV LQPL_SERVICE="EMULATOR"
EXPOSE 9502
EXPOSE 7683
CMD ["/opt/lqpl/bin/run-lqpl-servers.sh"]
