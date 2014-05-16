;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-

;;; This software is Copyright (c) Stefan Scholl, 2005-2008.
;;; Modified by Piotr Chamera, 2014.
;;; Stefan Scholl grants you the rights to distribute
;;; and use this software as governed by the terms
;;; of the Lisp Lesser GNU Public License
;;; (http://opensource.franz.com/preamble.html),
;;; known as the LLGPL.


(in-package #:cl-user)

(defpackage #:cl-wiki.system
  (:use #:cl
        #:asdf))

(in-package #:cl-wiki.system)

(defsystem #:cl-wiki
    :version "0.4.0"
    :author "Piotr Chamera <piotr_chamera@poczta.onet.pl>"
    :author "Stefan Scholl <stesch@no-spoon.de>"
    :licence "Lesser Lisp General Public License"
    :depends-on (#:hunchentoot #:cl-emb #:cl-ppcre #:cl-who #:bordeaux-threads #:colorize)
    :components ((:file "packages")
                 (:file "util")
                 (:file "codes")
                 (:file "codes1")
                 (:file "wiki" :depends-on ("packages" "util" "codes" "codes1"))))
