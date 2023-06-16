fundamental-mode ;; Available everywhere

(today (format-time-string "%Y-%m-%d"))

org-mode
(tempel-org "#+title:" (mapconcat 'capitalize (split-string (file-name-sans-extension (buffer-name)) "-")" ") n n r)
(elisp "#+begin_src elisp" n> r> n "#+end_src")
(elsp "#+begin_src elisp" n> r> n "#+end_src")
(qu "#+begin_quote" n "/" r "/" n "#+end_quote")
(deg "°")

this is a test

js-mode

(fun "function " p "(" p ") {" n> r n "}" >)
(afun "async function " p "(" p ") {" n> r n "}" >)
(for "for (let " (s i) " = 0; " i " < " p "; " i "++) {" n> r n "}" >)
(fori "for (let " (s i) " = " p "; " i " > 0; " i "--) {" n> r n "}" >)
(forin "for (const " p " in " p ") {" n> r n "}" >)
(log "console.log(" r ");")
(oucc "function run(input) {" n> r n "}" n n "process.stdin.on(\"data\", input => {
    console.log(run(input.toString().trim()));
})")

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
(T "<" (s tag) " " p ">" r "</" tag ">")
(t "<" (s tag) ">" r "</" tag ">")

sh-mode
(sb "#!/bin/sh" n n r)
(sh "#!/bin/sh" n n r)
