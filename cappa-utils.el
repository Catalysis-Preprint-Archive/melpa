;;; cappa-utils.el --- utilities for CaPPA

;;; Commentary:
;;

(require 'cl)
;;; Code:

;; This gets loaded in batch, so we have to add a lot of stuff to the path.


(require 'package)
(setq package-user-dir (expand-file-name "elpa"  "~/Dropbox/kitchingroup/jmax"))
(package-initialize)

(add-to-list 'load-path "~/Dropbox/kitchingroup/jmax/org-ref")
(require 'helm-config)
(require 'org-ref)
(load-file "/Users/jkitchin/Catalysis-Preprint-Archive/melpa/private-scopus-key.el")
(require 'org-ref-scopus)

(add-to-list 'load-path "/Users/jkitchin/Catalysis-Preprint-Archive/melpa")
(require 's)
(require 'dash)
(require 'package-build)
(require 'bibtex)

(defun cappa-package-html (key data)
  "Create an html page for the package represented by KEY and DATA.
KEY is :label
DATA is (plist-get (package-build--archive-alist-for-json) key)."
  ;; first, gather information.
  (message "Making %s" key)
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
	 (recipe (format "%s/%s-recipe.html" "html/packages" pkg-base))
	 author doi desc commentary bibtex citation
	 scopus-citing
	 scopus-related-by-keyword
	 scopus-related-by-author
	 scopus-related-by-references
	 wos-citing wos-related)

    (with-current-buffer (find-file-noselect pkg-el-file)
      (setq author (lm-header "author")
	    doi (lm-header "doi")
	    bibtex (or (lm-header "bibtex") "No bibtex entry found.")
	    desc (package-desc-summary (package-buffer-info))
	    commentary (or (lm-commentary) "")))

    (when (and bibtex (not (string= bibtex "No bibtex entry found.")))
      (message "Generating bibtex and RIS for %s: %s" package bibtex)
      (setq bibtex
	    (with-temp-buffer
	      (insert bibtex)
	      (goto-char (point-min))
	      (setq citation (org-ref-bib-citation))
	      (org-ref-sort-bibtex-entry)
	      (buffer-string)))
      (message "Done: %s" bibtex)
      ;; save a bibtex file
      (with-temp-file (format "packages/%s.bib" pkg-base)
        (insert bibtex))

      ;; make RIS file
      (shell-command (format "bib2xml packages/%s.bib > packages/%s.xml" pkg-base pkg-base))
      (shell-command (format "xml2ris packages/%s.xml > packages/%s.ris" pkg-base pkg-base))
      )

    (when doi
      (setq ;; scopus-citing (format
       ;;		   "<a href=\"%s\">citing articles</a>"
       ;;		   (scopus-citing-url doi))
       ;; scopus-related-by-keyword (format
       ;; "<a href=\"%s\">Keyword</a>"
       ;; (scopus-related-by-keyword-url doi)
       ;;)
       ;; scopus-related-by-author (format
       ;; "<a href=\"%s\">Author</a>"
       ;; (scopus-related-by-author-url doi)
       ;;)
       ;; scopus-related-by-references (format
       ;; "<a href=\"%s\">References</a>"
       ;; (scopus-related-by-references-url doi)
       ;;)
       wos-related (format "<a href=\"%s\">related</a>"
			   (concat "http://ws.isiknowledge.com/cps/openurl/service?url_ver=Z39.88-2004&rft_id=info%3Adoi%2F"
				   doi
				   "&svc_val_fmt=info%3Aofi%2Ffmt%3Akev%3Amtx%3Asch_svc&svc.related=yes"))
       wos-citing (format "<a href=\"%s\">citing</a>"
			  (concat
			   "http://ws.isiknowledge.com/cps/openurl/service?url_ver=Z39.88-2004&rft_id=info%3Adoi%2F"
			   doi
			   "&svc_val_fmt=info%3Aofi%2Ffmt%3Akev%3Amtx%3Asch_svc&svc.citing=yes"))))

    (with-temp-file recipe
      (insert
       (format "<html><body><pre>%s<pre></body></html>"
	       (with-temp-buffer
		 (insert-file-contents
		  (format "recipes/%s" package))
		 (buffer-string)))))

    (with-temp-file html
      (insert
       (s-format
	"<html>
<script type='text/javascript' src='https://d1bxh8uas1mnw7.cloudfront.net/assets/embed.js'></script>
<body>

<h1>${package} (<a href=\"./${pkg-base}-recipe.html\">recipe</a>)</h1>
<a href=\"http://catalysis-preprint-archive.github.io\">Home</a><br>
<h2><a href=\"http://dx.doi.org/${doi}\">${citation}</a></h2>
<table>
<tr>
<td>
<img src=\"${package}-badge.svg\">
 ${repo}
${altmetric}
${scopus-cite-badge}<br>
<!-- Scopus (these links require Institutional access)<br> -->
<!-- It seems like these scopus links don't actually work. -->
<!-- Related articles by: K${scopus-related-by-keyword}  A${scopus-related-by-author}  R${scopus-related-by-references} -->
</td>
<td>
<pre>${bibtex}</pre>
<a href=\"${pkg-base}.bib\">bibtex</a> <a href=\"${pkg-base}.ris\">RIS</a>
</td></tr></table>

<br>
<a href=\"https://webofknowledge.com\">
<img alt=\"Web of Science\" title=\"Web of Science\" src=\"/wos_logo.png\"></a>: ${wos-citing} ${wos-related}
<br>
<a href=\"https://twitter.com/share\" class=\"twitter-share-button\">Tweet</a> <script>!function(d,s,id){var js,fjs=d.getElementsByTagName(s)[0],p=/^http:/.test(d.location)?'http':'https';if(!d.getElementById(id)){js=d.createElement(s);js.id=id;js.src=p+'://platform.twitter.com/widgets.js';fjs.parentNode.insertBefore(js,fjs);}}(document, 'script', 'twitter-wjs');</script>
<script type=\"text/javascript\" src=\"//www.reddit.com/buttonlite.js?i=1\"></script>

<div id=\"disqus_thread\"></div>
<script>
 /**
 * RECOMMENDED CONFIGURATION VARIABLES: EDIT AND UNCOMMENT THE SECTION BELOW TO INSERT DYNAMIC VALUES FROM YOUR PLATFORM OR CMS.
 * LEARN WHY DEFINING THESE VARIABLES IS IMPORTANT: https://disqus.com/admin/universalcode/#configuration-variables
 */
 /*
 var disqus_config = function () {
 this.page.url = PAGE_URL; // Replace PAGE_URL with your page's canonical URL variable
 this.page.identifier = PAGE_IDENTIFIER; // Replace PAGE_IDENTIFIER with your page's unique identifier variable
 };
 */
 (function() { // DON'T EDIT BELOW THIS LINE
 var d = document, s = d.createElement('script');

 s.src = '//cappa.disqus.com/embed.js';

 s.setAttribute('data-timestamp', +new Date());
 (d.head || d.body).appendChild(s);
 })();
</script>
<noscript>Please enable JavaScript to view the <a href=\"https://disqus.com/?ref_noscript\" rel=\"nofollow\">comments powered by Disqus.</a></noscript>



<script id=\"dsq-count-scr\" src=\"//cappa.disqus.com/count.js\" async></script>

<a href=\"${pkg-file}\">${pkg-file}</a> (${archive-size})

<pre>${commentary}</pre>

<pre>
Contents:
${contents}
</pre>

<script type=\"text/javascript\">
  var gaJsHost = ((\"https:\" == document.location.protocol) ? \"https://ssl.\" : \"http://www.\");
  document.write(unescape(\"%3Cscript src='\" + gaJsHost + \"google-analytics.com/ga.js' type='text/javascript'%3E%3C/script%3E\"));
</script>
<script type=\"text/javascript\">
  try {
  var pageTracker = _gat._getTracker(\"UA-73115520-1\");
  pageTracker._trackPageview();
  } catch(err) {}
</script>

</body></html>"

	'aget
	`(("package" . ,package)
	  ("pkg-base" . ,pkg-base)
	  ("citation" . ,(or citation ""))
	  ("desc" . ,desc)
	  ("author" . ,author)
	  ("pkg-file" . ,pkg-file)
	  ("repo" . ,(with-current-buffer (find-file-noselect (format "recipes/%s" package))
		       (let ((data (cdr (read (current-buffer)))))
			 (cond
			  ((eq 'github (plist-get data :fetcher))
			   (format "<a href=\"http://github.com/%s\">http://github.com/%s</a>"
				   (plist-get data :repo)
				   (plist-get data :repo)))
			  ((eq 'gitlab (plist-get data :fetcher))
			   (format "<a href=\"http://gitlab.com/%s\">http://gitlab.com/%s</a>"
				   (plist-get data :repo)
				   (plist-get data :repo)))
			  ((eq 'bitbucket (plist-get data :fetcher))
			   (format "<a href=\"http://bitbucket.com/%s\">http://bitbucket.com/%s</a>"
				   (plist-get data :repo)
				   (plist-get data :repo)))
			  ((or (eq 'zenodo (plist-get data :fetcher))
			       (eq 'dropbox (plist-get data :fetcher)))
			   (format "<a href=\"%s\">%s</a>"
				   (plist-get data :url)
				   (plist-get data :url)))
			  ((eq 'gdrive (plist-get data :fetcher))
			   (format "<a href=\"%s\">%s</a>"
				   (format "https://drive.google.com/uc?export=download&id=%s"
					   (plist-get data :id))
				   (format "https://drive.google.com/uc?export=download&id=%s"
					   (plist-get data :id))))
			  (t
			   (format "<a href=\"%s\">" (plist-get data :url)))))))
	  ("archive-size" . ,(shell-command-to-string (format "du -hs packages/%s" pkg-file)))
	  ("commentary" . ,(or commentary ""))
	  ("bibtex" . ,(or bibtex ""))
	  ("doi" . ,(or doi ""))
	  ("altmetric" . ,(if doi
			      (format "<div data-badge-type='medium-donut' class='altmetric-embed' data-badge-details='right' data-doi='%s'></div>" doi)
			    ""))
	  ("scopus-cite-badge" . ,(if doi
				      (format "<object height=\"50\" data=\"http://api.elsevier.com/content/abstract/citation-count?doi=%s&amp;httpAccept=image/jpeg&amp;apiKey=5cd06d8a7df3de986bf3d0cd9971a47c\"></object>"
					      doi)
				    ""))
	  ("contents" . ,(if (eq type 'single)
			     ;; el-file
			     pkg-file
			   ;; tar-file
			   (with-current-buffer
			       (find-file-noselect
				(format "packages/%s" pkg-file))
			     (buffer-string))))
	  ("scopus-related-by-keyword" . ,(or scopus-related-by-keyword ""))
	  ("scopus-related-by-author" . ,(or scopus-related-by-author ""))
	  ("scopus-related-by-references" . ,(or scopus-related-by-references ""))
	  ("wos-citing" . ,(or wos-citing ""))
	  ("wos-related" . ,(or wos-related ""))))))))


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
