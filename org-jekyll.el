;;; org-jekyll.el --- Export jekyll-ready posts form org-mode entries
;;; 
;;; Author: Juan Reyero
;;; Version: 0.1
;;; Home page: http://juanreyero.com/open/org-jekyll/
;;; Repository: http://github.com/juanre/org-jekyll
;;; Public clone: git://github.com/juanre/org-jekyll.git
;;; 
;;; Summary
;;; -------
;;; 
;;; Extract subtrees from your org-publish project files that have
;;; a :blog: keyword and an :on: property with a timestamp, and
;;; export them to a subdirectory _posts of your project's publishing
;;; directory in the year-month-day-title.html format that Jekyll
;;; expects.  Properties are passed over as yaml front-matter in the
;;; exported files.  The title of the subtree is the title of the
;;; entry.  The title of the post is a link to the post's page.  
;;;
;;; Look at http://orgmode.org/worg/org-tutorials/org-jekyll.php for
;;; more info on how to integrate org-mode with Jekyll, and for the
;;; inspiration of the main function down there.

(defvar org-jekyll-new-buffers nil
  "Buffers created to visit org-publish project files looking for blog posts.")

(defvar org-jekyll-use-lang-as-category t
  "If :lang: is set in the post it is used to build the output
   directory as lang/_posts")

(defun org-jekyll-publishing-directory (project lang)
  "Where does the project go."
  (concat (plist-get (cdr (assoc project org-publish-project-alist)) 
                     :publishing-directory)
          (if (and lang org-jekyll-use-lang-as-category)
              (concat lang "/")
            "")
          "_posts/"))

(defun org-get-jekyll-file-buffer (file)
  "Get a buffer visiting FILE.  If the buffer needs to be
created, add it to the list of buffers which might be released
later.  Copied from org-get-agenda-file-buffer, and modified the
list that holds buffers to release."
  (let ((buf (org-find-base-buffer-visiting file)))
    (if buf
        buf
      (setq buf (find-file-noselect file))
      (if buf (push buf org-jekyll-new-buffers))
      buf)))

(defun org-jekyll-export-entry (project)
  (let* ((props (org-entry-properties nil 'standard))
         (time (cdr (assoc "on" props)))
         (lang (cdr (assoc "lang" props)))
         (yaml-front-matter (copy-alist props)))
    (unless (assoc "layout" yaml-front-matter)
      (push '("layout" . "post") yaml-front-matter))
    (when time
      (let* ((heading (org-get-heading t))
             (title (replace-regexp-in-string "[:=\(\)\?]" ""
                                              (replace-regexp-in-string
                                               "[ \t]" "-" heading)))
             (str-time (and (string-match "\\([[:digit:]\-]+\\) " time)
                            (match-string 1 time)))
             (to-file (format "%s-%s.html" str-time title))
             (org-buffer (current-buffer))
             (yaml-front-matter (cons (cons "title" heading) 
                                      yaml-front-matter))
             html)
        (org-narrow-to-subtree)
        (let ((level (- (org-reduced-level (org-outline-level)) 1))
              (contents (buffer-substring (point-min) (point-max))))
          (dotimes (n level nil) (org-promote-subtree))
          (setq html 
                (replace-regexp-in-string 
                 "<h2 id=\"sec-1\">\\(.+\\)</h2>"
                 "<h2 id=\"sec-1\"><a href=\"{{ page.url }}\">\\1</a></h2>"
                 (org-export-as-html nil nil nil 'string t nil)))
          (set-buffer org-buffer)
          (delete-region (point-min) (point-max))
          (insert contents)
          (save-buffer))
        (widen)
        (with-temp-file (expand-file-name 
                         to-file (org-jekyll-publishing-directory project lang))
          (when yaml-front-matter
            (insert "---\n")
            (mapc (lambda (pair) 
                    (insert (format "%s: %s\n" (car pair) (cdr pair))))
                  yaml-front-matter)
            (insert "---\n\n"))
          (insert html))))))

(defun org-jekyll-export-current-entry ()
  (interactive)
  (save-excursion
    (org-publish-initialize-files-alist)
    (let ((project-name (cdr (assoc (expand-file-name (buffer-file-name))
                                    org-publish-files-alist))))
      (org-back-to-heading t)
      (org-jekyll-export-entry project-name))))

(defun org-jekyll-export-blog ()
  "Export all entries in project files that have a :blog: keyword
and an :on: datestamp.  Property drawers are exported as
front-matters, outline entry title is the exported document
title. "
  (interactive)
  (save-excursion
    (org-publish-initialize-files-alist)
    (setq org-jekyll-new-buffers nil)
    (mapc 
     (lambda (jfile-project)
       (let ((jfile (car jfile-project))
             (project (cdr jfile-project)))
         (if (string= (file-name-extension jfile) "org")
             (with-current-buffer (org-get-jekyll-file-buffer jfile)
               (org-map-entries (lambda () (org-jekyll-export-entry project))
                                "blog")))))
     (org-publish-get-files (org-publish-expand-projects
                             (list (org-publish-get-project-from-filename 
                                    (buffer-file-name) 'up)))))
    (org-release-buffers org-jekyll-new-buffers)))

(provide 'org-jekyll)