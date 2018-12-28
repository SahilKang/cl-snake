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
	    --eval '(sb-ext:save-lisp-and-die \
                      "cl-snake" \
                      :toplevel (function main:entry-point) \
                      :executable t \
                      :purify t \
                      :compression 9)' \
            --end-toplevel-options \
    && mkdir /app \
    && mv ./cl-snake /app/cl-snake

###############################################################################
# Copy app into final image
###############################################################################

FROM debian:stable

ENV TERM='xterm-256color'

WORKDIR /app

COPY --from=builder /app/cl-snake ./

ENTRYPOINT ["/bin/bash"]
