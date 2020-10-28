;;; Copyright © 2020 Ryan Prior <rprior@protonmail.com>

(define-module (contributed hugo)
  #:use-module (gnu packages golang)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages web)
  #:use-module (guix build-system go)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages))

(define-public go-github-com-kylelemons-godebug
  (package
    (name "go-github-com-kylelemons-godebug")
    (version "1.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/kylelemons/godebug")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0dkk3friykg8p6wgqryx6745ahhb9z1j740k7px9dac6v5xjp78c"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/kylelemons/godebug/diff"
       #:unpack-path "github.com/kylelemons/godebug"))
    (home-page "https://github.com/kylelemons/godebug")
    (synopsis "Pretty printer for Go values.")
    (description
     "This package will pretty print a compact representation of a Go data
structure.  It can also produce a much more verbose, one-item-per-line
representation suitable for computing diffs.")
    (license license:asl2.0)))

(define-public esbuild
  (package
    (name "esbuild")
    (version "0.7.16")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/evanw/esbuild")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0lmq7yqssnc1cgd63m5zl734ahf8c0q0k1p2zdcn3qm15wfz7sh7"))
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
             (if tests?
               (with-directory-excursion (string-append "src/" unpack-path)
                 (invoke "make" "test-go")))
             #t)))))
    (inputs
     `(("go-golang-org-x-sys" ,go-golang-org-x-sys)))
    (native-inputs
     `(("go-github-com-kylelemons-godebug" ,go-github-com-kylelemons-godebug)))
    (home-page "https://github.com/evanw/esbuild")
    (synopsis "Bundler and minifier tool for JavaScript and TypeScript")
    (description
     "The esbuild tool provides a unified bundler, transpiler and
minifier.  It packages up JavaScript and TypeScript code, along with JSON
and other data, for distribution on the web.")
    (license license:expat)))

(define-public go-golang-org-x-xerrors
  (let ((commit "5ec99f83aff198f5fbd629d6c8d8eb38a04218ca")
        (revision "0"))
    (package
      (name "go-golang-org-x-xerrors")
      (version (git-version "0.0.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://go.googlesource.com/xerrors")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1dbzc3gmf2haazpv7cgmv97rq40g2xzwbglc17vas8dwhgwgwrzb"))))
      (build-system go-build-system)
      (arguments
       '(#:import-path "golang.org/x/xerrors"))
      (synopsis "Go 1.13 error values")
      (description
       "This package holds the transition packages for the new Go 1.13 error values.")
      (home-page "https://godoc.org/golang.org/x/xerrors")
      (license license:bsd-3))))

