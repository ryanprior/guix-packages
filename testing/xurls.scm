;;; Copyright © 2021 Ryan Prior <rprior@protonmail.com>

(define-module (testing xurls)
  #:use-module (gnu packages base)
  #:use-module (gnu packages check)
  #:use-module (gnu packages golang)
  #:use-module (guix build-system go)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages))

(define-public go-github-com-mvdan-xurls
  (package
    (name "xurls")
    (version "2.2.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/mvdan/xurls")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0w7i1yfl5q24wvmsfb3fz1zcqsdh4c6qikjnmswxbjc7wva8rngg"))))
    (build-system go-build-system)
    (arguments
     '(#:install-source? #f
       #:tests? #f
       #:unpack-path "mvdan.cc/xurls/v2"
       #:import-path "mvdan.cc/xurls/v2/cmd/xurls"))
    (inputs
     `(("go-github-com-rogpeppe-go-internal" ,go-github-com-rogpeppe-go-internal)))
    (native-inputs
     `(("gopkg-in-errgo-fmt-errors" ,gopkg-in-errgo-fmt-errors)))
    (home-page "https://github.com/mvdan/xurls")
    (synopsis "Extract urls from text")
    (description
     "This command-line tool extract URLs from text input and writes them as output.")
    (license license:expat)))
