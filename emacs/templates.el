fundamental-mode ;; Available everywhere

(today (format-time-string "%Y-%m-%d"))

org-mode

(elisp "#+begin_src elisp" n> r> n "#+end_src")
(elsp "#+begin_src elisp" n> r> n "#+end_src")
(tempel-org "#+title: " p n n r)

html-mode

(tempel-html "<!doctype html>
<html lang=\"en\">
  <head>
    <meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">
    <title>" p "</title>
  </head>
  <body>
    " r "
  </body>
</html>")
