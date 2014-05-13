#! /bin/sh

sbcl --eval "(require :cl-wiki)" \
     --eval "(wiki:start)"
