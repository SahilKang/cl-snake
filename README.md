# cl-snake

A terminal based snake game in Common Lisp; written over the holiday weekend.

## Building and Running

`cd` into project root directory:

### docker

1. build image: `docker build . -t cl-snake:v1`
2. get inside container: `docker run -it --rm cl-snake:v1`
3. start game inside container: `./cl-snake`
4. try and enjoy yourself
5. exit container: `exit`

### lisp

```lisp
(ql:quickload :cl-snake)
(asdf:operate :program-op :cl-snake)
```

If you're running SBCL and want to ensure compression
(uiop doesn't seem to handle it in the asd):

```lisp
(sb-ext:save-lisp-and-die
 "cl-snake"
 :toplevel #'main:entry-point
 :executable t
 :purify t
 :compression 9)
```
