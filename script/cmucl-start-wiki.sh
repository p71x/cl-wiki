#! /bin/sh

lisp -eval "(asdf:operate 'asdf:load-op :cl-wiki)" \
     -eval "(setf *print-pretty* nil)" \
     -eval "(wiki:start)" \
     -eval "(mp::startup-idle-and-top-level-loops)"
