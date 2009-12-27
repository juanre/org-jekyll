
(add-to-list 'org-publish-project-alist
             `("org-jekyll"
               :base-directory "~/src/prj/org-jekyll/test/"
               :recursive t
               :base-extension "org"
               :publishing-directory "~/src/prj/org-jekyll/test/"
               :publishing-function org-publish-org-to-html
               :section-numbers nil
               :headline-levels 4
               :table-of-contents nil
               :auto-index nil
               :auto-preamble nil
               :body-only t
               ;;:style ,(slurp-file-to-string
               ;;         "~/cjr/jr/style/org/in-header.html")
               ;;:preamble ,(slurp-file-to-string
               ;;            "~/cjr/open/org/preamble.html")
               ;;:postamble ,(slurp-file-to-string
               ;;            "~/cjr/jr/style/org/postamble.html")
               :auto-postamble nil))

