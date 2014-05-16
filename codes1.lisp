;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-

;;; This software is Copyright (c) Piotr Chamera, 2014.
;;; 
;;; Lisp Lesser GNU Public License
;;; (http://opensource.franz.com/preamble.html),
;;; known as the LLGPL.

;; Handling wiki text

(defpackage #:cl-wiki.sxml-text
  (:nicknames #:st)
  (:use #:cl #:colorize)
  (:export #:translate-wiki-codes))

(in-package :cl-wiki.sxml-text)

(defun con (&rest args)
  "Concatenate args into string."
  (apply #'concatenate 'string args))

(defun wrap-into-tag (tag in)
  "Wraps processed children into html tag."
  (con "<" tag ">" (process-list in) "</" tag ">"))

(defun empty-tag (tag)
  "Generate empty html tag."
  (con "<" tag "/>"))


(defvar *test-structure* nil
  "List structure for testing.")
(setf *test-structure*
      '(:section 
	(:title "test tytułu" " ABC.") 
	(:p "To jest " (:em "akapit") " z " (:link "linkiem") ".")
	(:p "Drugi akapit e = m c" (:^ "2") ".")))

(defvar *section-level* 1
  "Level of section nesting. Top level is 1, and up for next levels.")

(defvar *page-source* nil
  "Source tree of whole page.")


(defun process-list (in)
  "Process list of nodes."
  (apply #'concatenate 'string (mapcar #'process-child in)))


(defun process-child (in)
  "Process single node."
  (cond ((stringp in)
	 in)
	((listp in)
	 (let ((name (first in))
	       (children (rest in)))
	   (case name
	     (:section (section children))
	     (:title   (title children))
	     (:source  (source children))
	     (:p       (wrap-into-tag "p" children))
	     (:ul      (wrap-into-tag "ul" children))
	     (:ol      (wrap-into-tag "ol" children))
	     (:li      (wrap-into-tag "li" children))
	     (:em      (wrap-into-tag "em" children))
	     (:strong  (wrap-into-tag "strong" children))
	     (:sup     (wrap-into-tag "sup" children))  ;; superscript
	     (:sub     (wrap-into-tag "sub" children))  ;; subscript
	     (:s       (wrap-into-tag "s" children))    ;; strike out text
	     (:u       (wrap-into-tag "u" children))    ;; underline text
	     (:del     (wrap-into-tag "del" children))  ;; mark text as deleted
	     (:ins     (wrap-into-tag "ins" children))  ;; mark text as inserted
	     (:hr     (empty-tag "hr"))
	     (:br     (empty-tag "br"))
	     (:link   (link-external children))
	     (:wiki   (link-wiki children))
	     (:page-source (page-source))
	     (otherwise (format nil "~s" in)))))))
	 

(defun section (in)
  (let ((*section-level* (1+ *section-level*)))
    (process-list in)))
  
(defun title (in)
  (let ((tag (case *section-level*
	       (1 "h1")
	       (2 "h2")
	       (3 "h3")
	       (4 "h4")
	       (5 "h5")
	       (6 "h6")
	       (T "h6"))))
    (con "<" tag ">" (process-list in) "</" tag ">")))
 
(defun source (in)
  (let* ((lang-name (first in))
	 (lang (first (rassoc lang-name (colorize:coloring-types) :test #'string-equal)))
	 (string (apply #'concatenate 'string (rest in))))
    (if lang
	(concatenate 'string "<div class=\"code\">" (colorize:html-colorization lang string 'colorize::encode-for-tt) "</div>")
	(concatenate 'string "<pre class=\"code\">" string "</pre>"))))

(defun page-source ()
  (let* ((lang (first (rassoc "Common Lisp" (colorize:coloring-types) :test #'string-equal)))
	 (string (format nil "~a" *page-source*)))
    (concatenate 'string "<div class=\"code\">" 
		 (colorize:html-colorization lang string 'colorize::encode-for-tt) 
		 "</div>")))

(defun link-external (in)
  (let* ((url (first in))
	 (text (if (rest in)
		   (process-list (rest in))
		   (first in))))
    (con "<a href=\"" url "\">" text "</a>")))

(defun link-wiki (in)
  (let* ((name (first in))
	 (text (if (rest in)
		   (process-list (rest in))
		   (first in))))
    (con "<a href=\"" (tbnl:url-encode name) "\">" text "</a>")))


(defun translate-wiki-codes (string)
   "Translate wiki codes."
   (let ((*read-eval* nil)
	 (*package* (find-package :keyword))
	 (*page-source* string))
     (labels ((read-objects (string &optional (pos 0) (acc nil))
		"Read sequence of objects from string."
		(multiple-value-bind (object last-position)
		    (read-from-string string nil nil :start pos)
		  (if object
		      (progn 
			(push object acc)
			(read-objects string last-position acc))
		      (progn
			(push (subseq string last-position (length string)) acc)
			(reverse acc))))))
       (process-list (read-objects string)))))

;(in-package :cl-wiki)

(defun translate (string)
  "Translates the wiki codes inside STRING into HTML."
  (cl-wiki.sxml-text:translate-wiki-codes string))
