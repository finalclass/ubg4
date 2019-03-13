FROM alpine:3.5

RUN apk add -uUv \
        erlang \
	erlang-xmerl \
	erlang-tools \
	erlang-typer \
	erlang-snmp \
	erlang-syntax-tools \
	erlang-ssl \
	erlang-sasl \
	erlang-runtime-tools \
	erlang-ssh \
	erlang-stdlib \
	erlang-otp-mibs \
	erlang-reltool \
	erlang-mnesia \
	erlang-percept \
	erlang-parsetools \
	erlang-orber \
	erlang-public-key \
	erlang-odbc \
	erlang-os-mon \
	erlang-observer \
	erlang-et \
	erlang-ic \
	erlang-megaco \
	erlang-kernel \
	erlang-hipe \
	erlang-inets \
	erlang-jinterface \
	erlang-erts \
	erlang-gs \
	erlang-eunit \
	erlang-debugger \
	erlang-costime \
	erlang-costransaction \
	erlang-erl-interface \
	erlang-edoc \
	erlang-dialyzer \
	erlang-eldap \
	erlang-diameter \
	erlang-erl-docgen \
	erlang-crypto \
	erlang-cosevent \
	erlang-cosnotification \
	erlang-asn1 \
	erlang-cosfiletransfer \
	erlang-coseventdomain \
	erlang-dev \
	erlang-common-test \
	erlang-compiler \
	erlang-cosproperty \
        gcc \
        g++ \
        git \
        make \
        bash \
        && rm -rf /var/cache/apk/*

RUN git clone https://github.com/erlang/rebar3.git /rebar3 && \
     cd /rebar3 && \
     git checkout 3.3.6 && \
     ./bootstrap

COPY . /ubg4

WORKDIR /ubg4

EXPOSE 8080 8080

ENV PATH="$PATH:/rebar3" \
        TERM="xterm"

RUN rm -rf _build \
        && rebar3 as prod release

ENTRYPOINT ["_build/prod/rel/ubg4/bin/ubg4"]

CMD ["foreground"]
