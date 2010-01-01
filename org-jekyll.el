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
;;; entry.
;;;
;;; Look at http://orgmode.org/worg/org-tutorials/org-jekyll.php for
;;; more info on how to integrate org-mode with Jekyll, and for the
;;; inspiration of the main function down there.

(defvar org-jekyll-new-buffers nil
  "Buffers created to visit org-publish project files looking for blog posts.")

(defun org-jekyll-get-files ()
  "Get a list of files belonging to the current project."
  (org-publish-initialize-files-alist)
  (org-publish-get-base-files (org-publish-get-project-from-filename 
                               (buffer-file-name) 'up)))

(defun org-jekyll-publishing-directory ()
  "Where does the project go. "
  (org-publish-initialize-files-alist)
  (concat (plist-get (cdr (org-publish-get-project-from-filename 
                           (buffer-file-name) 'up)) :publishing-directory)
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

(defun org-jekyll-export-blog ()
  "Export all entries in project files that have a :blog: keyword
and an :on: datestamp.  Property drawers are exported as
front-matters, outline entry title is the exported document
title. "
  (interactive)
  (save-excursion
    (let ((posts-dir (org-jekyll-publishing-directory)))
      (setq org-jekyll-new-buffers nil)
      (mapc 
       (lambda (jfile)
         (with-current-buffer (org-get-jekyll-file-buffer jfile)
           (org-map-entries
            (lambda ()
              (let* ((props (org-entry-properties nil 'standard))
                     (time (cdr (assoc "on" props)))
                     (yaml-front-matter (copy-alist props)))
                (unless (assoc "layout" yaml-front-matter)
                  (push '("layout" . "post") yaml-front-matter))
                (when time
                  (let* ((heading (org-get-heading t))
                         (title (replace-regexp-in-string
                                 "[:=\(\)\?]" ""
                                 (replace-regexp-in-string
                                  "[ \t]" "-" heading)))
                         (str-time (and (string-match "\\([[:digit:]\-]+\\) " 
                                                      time)
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
                      (setq html (org-export-as-html nil nil nil 'string t nil))
                      (set-buffer org-buffer)
                      (delete-region (point-min) (point-max))
                      (insert contents)
                      (save-buffer))
                    (widen)
                    (with-temp-file (expand-file-name to-file posts-dir)
                      (when yaml-front-matter
                        (insert "---\n")
                        (mapc (lambda (pair) (insert (format "%s: %s\n" 
                                                             (car pair) 
                                                             (cdr pair))))
                              yaml-front-matter)
                        (insert "---\n\n"))
                      (insert html))))))
            "blog")))
       (org-jekyll-get-files))
      (org-release-buffers org-jekyll-new-buffers))))

(provide 'org-jekyll)