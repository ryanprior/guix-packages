;;; Copyright © 2020 Ryan Prior <rprior@protonmail.com>

(define-module (testing countdown)
  #:use-module (gnu packages golang)
  #:use-module (gnu packages terminals)
  #:use-module (gnu packages textutils)
  #:use-module (guix build-system go)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages))

(define-public countdown
  (package
    (name "countdown")
    (version "1.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/antonmedv/countdown")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0pdaw1krr0bsl4amhwx03v2b02iznvwvqn7af5zp4fkzjaj14cdw"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/antonmedv/countdown"))
    (native-inputs
     `(("go-github.com-mattn-go-runewidth@0.0.4-1.703b5e6" ,go-github.com-mattn-go-runewidth)
       ("go-github.com-nsf-termbox-go@0.0.0-1.288510b" ,go-github.com-nsf-termbox-go)))
    (home-page "https://github.com/antonmedv/countdown")
    (synopsis "Counts to zero with a text user interface.")
    (description
     "Countdown provides a fancy text display while it counts down to zero from a starting point you provide.  The user can pause and resume the countdown from the text user interface.")
    (license license:expat)))
