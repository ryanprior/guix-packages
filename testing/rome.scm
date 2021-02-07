;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2020 Ryan Prior <rprior@protonmail.com>

(define-module (testing rome)
  #:use-module (gnu packages node)
  #:use-module (guix build-system node)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages))

;; can I bootstrap this with esbuild instead of the vendored typescript?

(define-public rome
  (package
    (name "rome")
    (version "0.0.26")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/romefrontend/rome.git")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0g8am12dydf1im6wj4kaf6f7q0v28cg5d92n6vkzrcp27rkssc63"))))
    (build-system node-build-system)
    (home-page "https://romefrontend.dev/")
    (synopsis "Build tool for javascript and web technologies.")
    (description "Rome is a linter, compiler, bundler, and more for
JavaScript, TypeScript, HTML, Markdown, and CSS.")
    (license license:expat)))
