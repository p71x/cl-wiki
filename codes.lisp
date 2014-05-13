;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-

;;; This software is Copyright (c) Stefan Scholl, 2005-2008.
;;; Stefan Scholl grants you the rights to distribute
;;; and use this software as governed by the terms
;;; of the Lisp Lesser GNU Public License
;;; (http://opensource.franz.com/preamble.html),
;;; known as the LLGPL.

(in-package :cl-wiki)

;; Handling wiki codes


(defparameter *marker* (map 'string #'code-char #(#x1e #xff #xfe #x1e))
  "To mark the place in the string where preserved HTML should be restored.")

(defun rep-url-encoder-1 (target-string start end match-start match-end
                          reg-starts reg-ends)
  "Replacement function for CL-PPCRE:REGEX-REPLACE-ALL. Match in register 1 (0 in array)
gets url encoded."
  (declare (ignore start end match-start match-end))
  (tbnl:url-encode (subseq target-string (aref reg-starts 0) (aref reg-ends 0))))

(defun rep-nowiki (target-string start end match-start match-end
                   reg-starts reg-ends)
  "Replacement function for CL-PPCRE:REGEX-REPLACE-ALL. Match in register 1 (0 in array)
will be escaped and stored in *preserved-html*."
  (declare (special *preserved-html*) (ignore start end match-start match-end))
  (prog1
      (format nil "~A~D~A" *marker* (fill-pointer *preserved-html*) *marker*)
    (vector-push-extend
     (escape-string (subseq target-string (or (aref reg-starts 0) 0) (or (aref reg-ends 0) 0)))
     *preserved-html*)))

(defun rep-list (target-string start end match-start match-end reg-starts reg-ends)
  "Replacement function for CL-PPCRE:REGEX-REPLACE-ALL. Handles lists."
  (declare (ignore start end reg-starts reg-ends))
  (with-output-to-string (s)
    (write-string "<ul>" s)
    (cl-ppcre:do-matches-as-strings (line "(?ims)(?:<br>)?.*?(?=(?:<br>|\\z))"
                                          (subseq target-string match-start match-end)
                                          nil :sharedp t)
      (write-string (ppcre:regex-replace "(?:<br>|^)\\*\\s*(.*)" line "<li>\\1</li>") s))
    (write-string "</ul>" s)))

(defun make-source-snippet (lang string)
  "Make a source snippet (colorized or just plain PRE) out of string."
  (if lang
      (concatenate 'string "<div class=\"code\">" (colorize:html-colorization lang string 'colorize::encode-for-tt) "</div>")
      (concatenate 'string "<pre class=\"code\">" string "</pre>")))

(defun rep-source (target-string start end match-start match-end
                   reg-starts reg-ends)
  "Replacement function for CL-PPCRE:REGEX-REPLACE-ALL. Match in register 1 (0 in array)
is the language, match in register 2 becomes a source snippet. Stored in *preserved-html*."
  (declare (special *preserved-html*) (ignore start end match-start match-end))
  (prog1
      (format nil "~A~D~A" *marker* (fill-pointer *preserved-html*) *marker*)
    (vector-push-extend
     (let* ((lang-string (subseq target-string (or (aref reg-starts 0) 0) (or (aref reg-ends 0) 0)))
            (lang (first (rassoc lang-string (colorize:coloring-types) :test #'string-equal))))
       (make-source-snippet lang (subseq target-string (aref reg-starts 1) (aref reg-ends 1))))
     *preserved-html*)))

(defun rep-restore-preserved (target-string start end match-start match-end
                              reg-starts reg-ends)
  "Replacemen function for CL-PPCRE:REGEX-REPLACE-ALL. Match in register 1 (0 in array)
is an index for the array *preserved-html*. Restores preserved HTML."
  (declare (special *preserved-html*) (ignore start end match-start match-end))
  (aref *preserved-html* (parse-integer (subseq target-string (aref reg-starts 0) (aref reg-ends 0)))))
  
(defparameter *unescaped-replace-list* ; re-evaluate *unescaped-replace-list-compiled* if you change this
  `(
    ;; Remove the marker first, so there will be no trouble at the end.
    (,*marker* . "")

    ;; <nowiki> .. </nowiki> ==> Exclude portions of text from wiki converting
    ("(?ims)<nowiki>(.*?)</nowiki>" . ,'rep-nowiki)

    ;; <!-- comment --> ==> Removes everything between <!-- and -->. Allows commenting.
    ("(?ims)<!--(.*?)-->" . "")

    ;; <source> .. </source> ==> Sourcecode
    ("(?ims)<source(?:\\s*lang=\"(.*?)\")?>(?:\\x0d?\\x0a)*(.*?)</source>" . ,'rep-source))
  "List of conses. CAR is regexp and CDR replace-string for
CL-PPCRE:REGEX-REPLACE-ALL. This list is used before string gets escaped
for HTML.")

(defparameter *replace-list* ; re-evaluate *replace-list-compiled* if you change this
  `(
    ;; CR or LF ==> <br>
    ("\\x0d?\\x0a" . "<br>")

    ;; <br> or <br/> ==> Start a new line
    ("&lt;br\s*/?&gt;(<br>)?" . "<br/>")

    ;; '''text''' ==> stronger emphasize text (<strong>text</strong>),
    ;;                on most browsers bold.
    ("'''(.*?)'''" . "<strong>\\1</strong>")

    ;; ''text'' ==> emphasize text (<em>text</em>), on most browsers italic.
    ("''(.*?)''" . "<em>\\1</em>")

    ;; <small>text</small> ==> text in small font
    ("&lt;small&gt;(.*?)&lt;/small&gt;" . "<small>\\1</small>")

    ;; <big>text</big> ==> text in big font
    ("&lt;big&gt;(.*?)&lt;/big&gt;" . "<big>\\1</big>")

    ;; <sub>x</sub> ==> subscripting x
    ("&lt;sub&gt;(.*?)&lt;/sub&gt;" . "<sub>\\1</sub>")

    ;; <sup>x</sup> ==> superscripting x
    ("&lt;sup&gt;(.*?)&lt;/sup&gt;" . "<sup>\\1</sup>")

    ;; <s>text</s> ==> strike out text
    ("&lt;s&gt;(.*?)&lt;/s&gt;" . "<s>\\1</s>")

    ;; <u>text</u> ==> underline text
    ("&lt;u&gt;(.*?)&lt;/u&gt;" . "<u>\\1</u>")

    ;; <del>text</del> ==> Mark text as deleted
    ("&lt;del&gt;(.*?)&lt;/del&gt;" . "<del>\\1</del>")

    ;; <ins>text</ins> ==> Mark text as inserted
    ("&lt;ins&gt;(.*?)&lt;/ins&gt;" . "<ins>\\1</ins>")

    ;; [[Page|Text]] ==> Generates a link to named page and links Text.
    ("\\[\\[([^]\">|]*?)\\|([^]\">]*?)\\]\\]"
     . ,(list "<a href=\"" 'rep-url-encoder-1 "\" title=\"" 0 "\">" 1 "</a>"))

    ;; [[Page]] ==> Generates a link to named page.
    ("\\[\\[(.*?)\\]\\]"
     . ,(list "<a href=\"" 'rep-url-encoder-1 "\" title=\"" 0 "\">" 0 "</a>"))

    ;; [http://www.somepage.example/] ==> Inserts external link.
    ("\\[(http|https|ftp|mailto|gopher):([^\">].*?)\\]"
     . "<a rel=\"nofollow\" class=\"external \\1\" href=\"\\1:\\2\" title=\"\\1:\\2\">\\1:\\2</a>")

    ;; ====== Foo ======  ==> Level 6 header (etc.)
    ("(?ims)(<br>|^)======\\s*(.*?)\\s*=*(?=(?:<br>|\\z))" . "\\1<h6>\\2</h6>")
    ("(?ims)(<br>|^)=====\\s*(.*?)\\s*=*(?=(?:<br>|\\z))" . "\\1<h5>\\2</h5>")
    ("(?ims)(<br>|^)====\\s*(.*?)\\s*=*(?=(?:<br>|\\z))" . "\\1<h4>\\2</h4>")
    ("(?ims)(<br>|^)===\\s*(.*?)\\s*=*(?=(?:<br>|\\z))" . "\\1<h3>\\2</h3>")
    ("(?ims)(<br>|^)==\\s*(.*?)\\s*=*(?=(?:<br>|\\z))" . "\\1<h2>\\2</h2>")
    ("(?ims)(<br>|^)=\\s*(.*?)\\s*=*(?=(?:<br>|\\z))" . "\\1<h1>\\2</h1>")

    ;; ---- ==> Horizontal divider
    ("(?ims)(<br>|^)----.*?(?=(?:<br>|\\z))" . "\\1<hr>")

    ;; * Item ==> List item
    ("(?ims)(?:<br>|^)\\*.*?(?=(?:<br>|^)[^\\*]|\\z)" . ,'rep-list)

    ;; <br><br><br>* ==> <p>
    ("(?ims)(<br>){2,}" . "<p>")

    ;; <br> => " "
    ("(?ims)(<br>)" . " ")

    ;; <br/> => "<br>" - Cleanup for <br> wiki tag
    ("(?ims)<br/>" . "<br>")


    ;; Restore preserved HTML
    (,(concatenate 'string *marker* "(\\d+)" *marker*) . ,'rep-restore-preserved)
    )
  ;;
  "List of conses. CAR is regexp and CDR replace-string for
CL-PPCRE:REGEX-REPLACE-ALL.")

(defparameter *unescaped-replace-list-compiled*
  (mapcar #'(lambda (reg-rep)
              (destructuring-bind (regexp . replace) reg-rep
                (cons (cl-ppcre:create-scanner regexp) replace)))
          *unescaped-replace-list*))

(defparameter *replace-list-compiled*
  (mapcar #'(lambda (reg-rep)
              (destructuring-bind (regexp . replace) reg-rep
                (cons (cl-ppcre:create-scanner regexp) replace)))
          *replace-list*))

(defun translate-wiki-codes (string)
   "Translate wiki codes."
   (let ((*preserved-html* (make-array 3 :element-type 'string
                                       :adjustable t :fill-pointer 0)))
     (declare (special *preserved-html*)) ; To temporary store HTML code ;
    ;; First with the unescaped string  ;
     (loop for (regexp . replace) in *unescaped-replace-list-compiled*
        do (setf string (ppcre:regex-replace-all regexp string replace)))
    ;; Replacement in the escaped string ;
     (setf string (escape-string string))
     (loop for (regexp . replace) in *replace-list-compiled*
        do (setf string (ppcre:regex-replace-all regexp string replace)))
     string))


(defun translate (string)
  "Translates the wiki codes inside STRING into HTML."
  (translate-wiki-codes string))
