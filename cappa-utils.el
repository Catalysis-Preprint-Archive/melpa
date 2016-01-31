(require 'cl)
(load-file "package-build.el")

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
	     (format "<tr>
<td>%s</td>
<td>%s</td>
<td><a href=\"./preprints/%s\">%s</a></td>
</tr>"
		     label
		     (elt props 2)
		     (if (eq 'single (elt props 3))
			 (format "%s-%s.%s.el"
				 label (nth 0 (elt props 0))
				 (nth 1 (elt props 0)))
		       (format "%s-%s.%s.tar"
			       label (nth 0 (elt props 0))
			       (nth 1 (elt props 0))))
		     (if (eq 'single (elt props 3))
			 (format "%s-%s.%s.el"
				 label (nth 0 (elt props 0))
				 (nth 1 (elt props 0)))
		       (format "%s-%s.%s.tar"
			       label (nth 0 (elt props 0))
			       (nth 1 (elt props 0)))))))
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
