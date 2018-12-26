###############################################################################
# Compile app into dynamically linked binary
###############################################################################

FROM debian:stable AS builder

WORKDIR /root/quicklisp/local-projects/cl-snake/

COPY cl-snake.asd ./
COPY src src

RUN apt-get update && apt-get install -y --no-install-recommends \
    sbcl cl-quicklisp libncurses5-dev gcc \
    && sbcl --noinform --end-runtime-options \
            --no-sysinit --no-userinit \
	    --load $(find / -name 'quicklisp.lisp' -type f) \
	    --non-interactive \
	    --eval '(quicklisp-quickstart:install)' \
	    --eval '(ql:quickload :cl-snake)' \
	    --eval '(asdf:operate :program-op :cl-snake)' \
    && mkdir /app \
    && mv $(find / -name 'cl-snake' -type f) /app/cl-snake

###############################################################################
# Copy app into final image
###############################################################################

FROM debian:stable

WORKDIR /app

COPY --from=builder /app/cl-snake ./

RUN apt-get update && apt-get install -y --no-install-recommends \
    libncurses5

CMD ["60", "18"]
ENTRYPOINT ["/app/cl-snake"]
