(require 'cl)
(defun make-index ()
  (let ((data (package-build-archive-alist)))
    (with-temp-file (expand-file-name "index.html"
				      package-build-archive-dir)
      (insert "<html>
<body>
<ul>
")
      (loop for (label . props) in data
	    do
	    (insert
	     (format "<li>%s: %s</li>\n"
		     label
		     (elt props 2)

		     )))
      (insert "
</ul>
</body>
</html>"))))
