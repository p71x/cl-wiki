;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-

;;; This software is Copyright (c) Stefan Scholl, 2005-2008.
;;; Modified by Piotr Chamera, 2014.
;;; Stefan Scholl grants you the rights to distribute
;;; and use this software as governed by the terms
;;; of the Lisp Lesser GNU Public License
;;; (http://opensource.franz.com/preamble.html),
;;; known as the LLGPL.

(in-package :cl-wiki)

(defvar *wiki-port* 5757
  "Port CL-WIKI listens for http connections.")

(defvar *wiki-directory* nil
  "Base directory for data.")

(defvar *wiki-template-directory*
  (merge-pathnames "templates/"
                   (asdf:component-pathname (asdf:find-system :cl-wiki)))
  "Base directory for templates.")

(defvar *wiki-page-list* '((:title "Home" :link "Home")
                           (:title "About" :link "About")
                           (:title "Contact" :link "Contact"))
  "Plist of pages displayed on every page. :LINK can have parameters.")

(defvar *wiki-home-page* "Home"
  "Default page when root directory of the site is accessed.")

(defvar *behind-proxy-p* nil
  "Indicates if CL-WIKI runs behind a reverse proxy.")

(defvar *page-index* (make-hash-table :test #'equal)
  "Global index over all wiki pages.")

(defvar *edit-textarea-size* '(:rows 25 :cols 80)
  "List of two integer values containing number of rows and columns of textarea
of edit form.")

;; -------------------------------------------------------------------

(defvar *wiki-server* nil
  "Hunchentoot server instance")

(defvar *emb-lock* (bordeaux-threads:make-lock "emb-lock")
  "Lock for CL-EMB.")

(defvar *page-index-lock* (bordeaux-threads:make-lock "page-index-lock")
  "Lock for *PAGE-INDEX*")

;; -------------------------------------------------------------------

(defun current-page ()
  "Extracts the current page from the URL. 
Returns *WIKI-HOME-PAGE* when / is accessed."
  (let ((url (hunchentoot:script-name*)))
    (hunchentoot:url-decode (if (string= "/" url)
                          *wiki-home-page*
                          (subseq url (+ 1 (position #\/ url :from-end t)))))))

(defun page-version (page)
  "Current version of PAGE in *PAGE-INDEX*"
  (bordeaux-threads:with-lock-held (*page-index-lock*)
    (gethash page *page-index*)))


(defun contents-of-file (pathname)
  "Returns a string with the entire contents of the specified file."
  (with-open-file (in pathname :direction :input)
    (contents-of-stream in)))

;; From lemonodor's LSP
(defun contents-of-stream (stream)
  "Returns a string with the entire contents of the specified stream."
  (with-output-to-string (contents)
    (let* ((buffer-size 4096)
           (buffer (make-string buffer-size)))
      (labels ((read-chunks ()
                 (let ((size (read-sequence buffer stream)))
                   (if (< size buffer-size)
                       (princ (subseq buffer 0 size) contents)
                       (progn
                         (princ buffer contents)
                         (read-chunks))))))
        (read-chunks)))))

(defun meta (pathname)
  "Reads and returns meta data of the page with given PATHNAME."
  (with-open-file (in pathname :direction :input)
    (read in)))

(defun meta-and-content (pathname)
  "Reads and returns meta data and content of the page with given PATHNAME."
  (with-open-file (in pathname :direction :input)
    (values (read in)
            (contents-of-stream in))))

(defun ncomplete-meta (meta)
  "Completes the META data with a few additional values such as the string
at :LAST-MODIFIED, which is the ISO 8601 representation of the universaltime at
:TIME."
  (nconc meta (list :last-modified (iso-date-time (getf meta :time)))))

(defun index-path ()
  "Filesystem path to page index file."
  (merge-pathnames (make-pathname :name "all" :type "index") *wiki-directory*))

(defun set-page-version (page version)
  "Sets the VERSION of given PAGE in the *PAGE-INDEX*. Don't forget to lock!"
  (setf (gethash page *page-index*) version)
  (with-open-file (out (index-path) :direction :output :if-exists :supersede)
    (loop for key being the hash-keys in *page-index* using (hash-value value)
          do (format out "~A~%~A~%" key value))))

(defun save (page)
  "Save the submitted PAGE and display it."
  (bordeaux-threads:with-lock-held (*page-index-lock*)
    (let* ((version (aif (gethash page *page-index*)
                         (1+ it)
                         1))
           (path (page-version-path page version)))
      (ensure-directories-exist path)
      (with-open-file (out path :direction :output :if-exists :supersede)
        (prin1 (list :address (if *behind-proxy-p*
                                  (hunchentoot:header-in* "X-Forwarded-For")
                                  (hunchentoot:remote-addr*))
                     :user nil      ; XXX just a test value
                     :time (get-universal-time))
               out)
        (write-string (hunchentoot:parameter "content") out))
      (set-page-version page version)))
  (display page (page-version page)))

(defun page-version-path (page version)
  "Filesystem path for given PAGE and VERSION number."
  (merge-pathnames (make-pathname :directory (list :relative page)
                                  :name (princ-to-string version)
                                  :type "page")
                   *wiki-directory*))

(defun page-path (page version)
  "Filesystem path for given PAGE. NIL if non existant."
  (let ((path (page-version-path page version)))
    (when (probe-file path)
      path)))
  
(defun execute-main-template (page version body &key edit meta)
  "Execute the main template with all data needed. EDIT is t if you display the
edit form. The plist META contains the meta data like :TIME for last modified time."
  (emb:execute-emb (merge-pathnames "main.template" *wiki-template-directory*)
                   :env `(:edit ,edit
                          :page-list ,*wiki-page-list*
                          :edit-url ,(format nil "~A?version=~D&action=edit"
                                             page version)
                          :prev-link ,(when (and version (> version 1))
                                            (format nil "~A?version=~D"
                                                    page (1- version)))
                          :version ,(princ-to-string version)
                          :title ,page
                          :page ,page
                          :meta ,meta
                          :body ,body)))

(defun edit (page version &key preview)
  "Edit the PAGE."
  (multiple-value-bind (meta content)
      (aif (page-path page version)
           (if preview
               (values (meta it) (hunchentoot:post-parameter "content"))
               (meta-and-content it))
           (values nil 
                   (if preview
                       (hunchentoot:post-parameter "content")
                       (format nil "Describe ~A here." page)))); XXX All texts must be configurable. L10N.
    (let ((body (with-html-output-to-string (s)
                  (when preview
                    (htm 
                     (:h2 "Preview")
                     (:hr)
                     (:strong "This is only a preview! Changes aren't saved!")
                     (:hr)
                     (str (translate content))
                     (:hr)))
                  (:form :action (format nil "~A" page) :method "POST"
                         (:fieldset
                          (:legend "Edit page")
                          (:p
                           (:textarea :name "content"
                                      :rows (getf *edit-textarea-size* :rows)
                                      :cols (getf *edit-textarea-size* :cols)
                                      (esc content)))
                          (:input :type "submit" :name "action-preview" :value "Preview")
                          (str " ")
                          (:input :type "submit" :name "action-save" :value "Save"))))))
      (execute-main-template page version body :edit t :meta meta))))

(defun display (page version)
  "Display PAGE."
  (let ((path (page-path page version)))
    (if path
        (multiple-value-bind (meta content)
            (meta-and-content path)
          (execute-main-template page version (translate content) :meta (ncomplete-meta meta)))
        (edit page version))))

(defun debug-header ()
  "Shows the request header for debugging purpose."
  (with-html-output-to-string (s)
    (:html (:head (:title "Request header - DEBUG - CL-WIKI"))
           (:body (:h1 "Request header")
                  (:table :border 1
                   (:tr
                    (:th "Name") (:th "Value"))
                   (loop for (name . value) in (hunchentoot:headers-in*)
                         do (htm (:tr (:td (str name)) (:td (str value))))))))))

(defun wiki ()
  "Main command dispatching function for CL-WIKI."
  (let* ((action (hunchentoot:parameter "action"))
         (action-save (hunchentoot:parameter "action-save"))
         (action-preview (hunchentoot:parameter "action-preview"))
         (page (current-page))
         (max-version (page-version page))
         (param-version (aif (hunchentoot:parameter "version")
                             (and it (parse-integer it :junk-allowed t))))
         (version (if (and param-version (< 0 param-version max-version))
                      param-version
                      max-version)))
    (cond
      ((string= "debug-header" action) (debug-header))
      ((or (string= "edit" action) action-preview) (edit page version :preview action-preview))
      (action-save (save page))
      (t (display page version)))))

(defun emb-lock-function (func)
  "Lock function for CL-EMB."
  (bordeaux-threads:with-lock-held (*emb-lock*)
    (funcall func)))

;; -------------------------------------------------------------------

(defun read-config (&optional path)
  (with-open-file (plist-stream (or path
                                    (make-pathname :name "wiki" :type "conf")))
    (let ((*read-eval* nil)
          (*package* (find-package :keyword)))
      (read plist-stream))))

(defun get-config-value (config section key &optional default)
  (getf (getf config section) key default))

;; Partially taken from PCL 
(defun string-to-directory-pathname (string)
  (when string
    (let ((pathname (pathname string)))
      (make-pathname 
       :directory (append (or (pathname-directory pathname) (list :relative))
                          (list (file-namestring pathname)))
       :name      nil
       :type      nil
       :defaults pathname))))

(defun read-page-index ()
  "Filling *PAGE-INDEX*."
  (bordeaux-threads:with-lock-held (*page-index-lock*)
    (clrhash *page-index*)
    (with-open-file (in (index-path) :direction :input :if-does-not-exist nil)
      (when in
        (loop for key = (read-line in nil nil)
              while key
              for value = (parse-integer (read-line in t))
              do (setf (gethash key *page-index*) value))))))

(defun init ()
  (let ((conf (read-config)))
    (setf *wiki-directory* (string-to-directory-pathname (get-config-value conf :base :directory))
          *wiki-port* (get-config-value conf :base :port *wiki-port*)
          *wiki-template-directory* (get-config-value conf :base :template-directory *wiki-template-directory*)
          *wiki-page-list* (get-config-value conf :base :page-list *wiki-page-list*)
          *wiki-home-page* (get-config-value conf :base :home-page *wiki-home-page*)
          *behind-proxy-p* (get-config-value conf :base :reverse-proxy *behind-proxy-p*)
          *edit-textarea-size* (get-config-value conf :base :edit-textarea-size *edit-textarea-size*)))

  (read-page-index)

  (setf *attribute-quote-char* #\"
        (html-mode) :sgml
        *escape-char-p* #'(lambda (c) (find c "<>&"))
        ;emb:*locking-function* 'emb-lock-function
        hunchentoot:*default-content-type* "text/html; charset=utf-8")

  (push (hunchentoot:create-prefix-dispatcher "/" 'wiki)
	hunchentoot:*dispatch-table*)

  (push (hunchentoot:create-folder-dispatcher-and-handler
	 "/_static/" (merge-pathnames "_static/"
				      wiki::*wiki-directory*))
	hunchentoot:*dispatch-table*)
  )

;; -------------------------------------------------------------------

(defun start ()
  (init)
  (unless *wiki-directory*
    (error "A directory must be specified. Update your config, please."))
  (setf *wiki-server* (hunchentoot:start (make-instance 'hunchentoot:easy-acceptor 
							:port *wiki-port*))))

(defun stop ()
  (hunchentoot:stop *wiki-server*))
