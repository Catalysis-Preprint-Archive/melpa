;;; cappa-utils.el --- utilities for CaPPA

;;; Commentary:
;;

(require 'cl)
;;; Code:

(add-to-list 'load-path "~/Dropbox/kitchingroup/jmax/elpa/s-20140910.334")
(add-to-list 'load-path "~/Dropbox/kitchingroup/jmax/elpa/dash-20150717.1321")
(add-to-list 'load-path "/Users/jkitchin/Catalysis-Preprint-Archive/melpa")
(require 's)
(require 'dash)
(require 'package-build)

(defun make-index ()
  (let ((data (package-build-archive-alist)))
    (with-temp-file "index.html"
      (insert "<!DOCTYPE html>
<html lang=\"en-us\">
  <head>
    <meta charset=\"UTF-8\">
    <title>Catalysis-Preprint-Archive</title>
    <meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">
    <link rel=\"stylesheet\" type=\"text/css\" href=\"stylesheets/normalize.css\" media=\"screen\">
    <link href='https://fonts.googleapis.com/css?family=Open+Sans:400,700' rel='stylesheet' type='text/css'>
    <link rel=\"stylesheet\" type=\"text/css\" href=\"stylesheets/stylesheet.css\" media=\"screen\">
    <link rel=\"stylesheet\" type=\"text/css\" href=\"stylesheets/github-light.css\" media=\"screen\">
  </head>
  <body>
    <section class=\"page-header\">
      <h1 class=\"project-name\">Catalysis Preprint Archive</h1>
      <a href=\"https://github.com/Catalysis-Preprint-Archive/cappa\" class=\"btn\">View on GitHub</a>
    </section>

    <section class=\"main-content\">
      Welcome to the Catalysis Preprint Archive.
     </section>
")
      (insert "<table border=\"1\" cellpadding=\"10\">")
      (loop for (label . props) in data
	    do
	    (insert
	     (s-format "<tr>
<td><a href=\"./preprints/$0-$2.$3.html\">$0</a></td>
<td>$1</td>
</tr>"
		       'elt
		       (list
			(symbol-name label)    ;0
			(elt props 2)	      ;1 description
			(format "%s" (nth 0 (elt props 0))) ; 2major version
			(format "%s" (nth 1 (elt props 0)))) ; 3
		       )))
      (insert "</table>")

      (insert "            <script type=\"text/javascript\">
            var gaJsHost = ((\"https:\" == document.location.protocol) ? \"https://ssl.\" : \"http://www.\");
            document.write(unescape(\"%3Cscript src='\" + gaJsHost + \"google-analytics.com/ga.js' type='text/javascript'%3E%3C/script%3E\"));
          </script>
          <script type=\"text/javascript\">
            try {
              var pageTracker = _gat._getTracker(\"UA-73115520-1\");
            pageTracker._trackPageview();
            } catch(err) {}
          </script>

  </body>
</html>"))))


(defun cappa-package-html (key data)
  "Create an html page for KEY and DATA.
KEY is :label
DATA is (plist-get (package-build--archive-alist-for-json) key)."
  ;; first, gather information.
  (let* ((package (substring (symbol-name key) 1))
	 (pkg-el-file (expand-file-name
		       (format "%s/%s.el" package package)
		       package-build-working-dir))
	 (type (plist-get data :type))
	 (ver (plist-get data :ver))
	 (pkg-base (format "%s-%s.%s"
			   package
			   (nth 0 ver)
			   (nth 1 ver)))
	 (pkg-file (format "%s.%s" pkg-base
			   (if (eq 'single type)
			       "el"
			     "tar")))
	 (html (format "%s/%s.html" "html/packages" pkg-base))
	 author doi desc commentary bibtex)

    (with-current-buffer (find-file-noselect pkg-el-file)
      (setq author (lm-header "author")
	    doi (lm-header "doi")
	    bibtex (or (lm-header "bibtex") "No bibtex entry found.")
	    desc (package-desc-summary (package-buffer-info))
	    commentary (or (lm-commentary) "")))

    (with-temp-file html
      (insert
       (s-format
	"<html>
<script type='text/javascript' src='https://d1bxh8uas1mnw7.cloudfront.net/assets/embed.js'></script>
<body>

<h1>${package}</h1>
<a href=\"http://catalysis-preprint-archive.github.io\">Home</a><br>
<img src=\"${package}-badge.svg\">
${altmetric}
${scopus-cite}
<br>
<a href=\"http://dx.doi.org/${doi}\">DOI</a>

<pre>${bibtex}</pre>

<a href=\"${pkg-file}\">${pkg-file}</a>

<pre>${commentary}</pre>

<pre>
Contents:
${contents}
</pre>

</body></html>"

	'aget
	`(("package" . ,package)
	  ("desc" . ,desc)
	  ("author" . ,author)
	  ("pkg-file" . ,pkg-file)
	  ("commentary" . ,commentary)
	  ("bibtex" . ,bibtex)
	  ("doi" . ,(or doi ""))
	  ("altmetric" . ,(if doi
			      (format "<div data-badge-type='medium-donut' class='altmetric-embed' data-badge-details='right' data-doi='%s'></div>" doi)
			    ""))
	  ("scopus-cite" . ,(if doi
				(format "<img src=\"http://api.elsevier.com/content/abstract/citation-count?doi=%s&amp;httpAccept=image/jpeg&amp;apiKey=5cd06d8a7df3de986bf3d0cd9971a47c\">" doi)
			      ""))
	  ("contents" . ,(if (eq type 'single)
			     ;; el-file
			     pkg-file
			   ;; tar-file
			   (with-current-buffer
			       (find-file-noselect
				(format "packages/%s" pkg-file))
			     (buffer-string))))))))))


(defun cappa-generate-package-html ()
  "Generate the package HTML pages."
  (loop for key in (-slice (package-build--archive-alist-for-json) 0 nil 2)
	for data in (-slice (package-build--archive-alist-for-json) 1 nil 2)
	do
	(cappa-package-html key data)))


;; This redefines lm-header in lisp-mnt.el to read multiline headers. This was broken in the provided functions.
(require 'lisp-mnt)

(defun lm-header (header)
  "Return the contents of the header named HEADER.
If the HEADER is multiline, a list of strings is returned."
  (save-excursion
    (goto-char (point-min))
    (let ((case-fold-search t)
	  res)
      (when (and (re-search-forward
		  (lm-get-header-re header)
		  (lm-code-mark) t)
                 ;;   RCS ident likes format "$identifier: data$"
                 (looking-at
                  (if (save-excursion
                        (skip-chars-backward "^$" (match-beginning 0))
                        (= (point) (match-beginning 0)))
                      "[^\n]+" "[^$\n]+")))
	(setq res (list (match-string-no-properties 0)))
	(forward-line 1)
	(while (looking-at "^;+\\(\t\\|[\t\s]\\{2,\\}\\)\\(.+\\)")
	  (push (match-string-no-properties 2) res)
	  (forward-line 1))
	(setq res (nreverse res))
	(if (= 1 (length res))
	    (car res)
	  (mapconcat 'identity res "\n"))))))

(provide 'cappa-utils)

;;; cappa-utils.el ends here
