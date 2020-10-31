;;; Copyright © 2020 Ryan Prior <rprior@protonmail.com>

(define-module (proposed esbuild)
  #:use-module (gnu packages golang)
  #:use-module (guix build-system go)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages))


(define-public esbuild
  (package
    (name "esbuild")
    (version "0.8.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/evanw/esbuild")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "17qzmadjixjivwbxbj20683j3n6igk7bx7v4k5bs2rqfvigdx2ps"))
       (modules '((guix build utils)))
       (snippet
        '(begin
           ;; Remove prebuilt binaries
           (delete-file-recursively "npm")
           #t))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/evanw/esbuild/cmd/esbuild"
       #:unpack-path "github.com/evanw/esbuild"
       #:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda* (#:key tests? unpack-path #:allow-other-keys)
             (when tests?
               (with-directory-excursion (string-append "src/" unpack-path)
                 (invoke "make" "test-go")))
             #t)))))
    (inputs
     `(("golang.org/x/sys" ,go-golang-org-x-sys)))
    (native-inputs
     `(("github.com/kylelemons/godebug" ,go-github-com-kylelemons-godebug)))
    (home-page "https://github.com/evanw/esbuild")
    (synopsis "Bundler and minifier tool for JavaScript and TypeScript")
    (description
     "The esbuild tool provides a unified bundler, transpiler and
minifier.  It packages up JavaScript and TypeScript code, along with JSON
and other data, for distribution on the web.")
    (license license:expat)))
