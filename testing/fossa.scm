;;; Copyright © 2020 Ryan Prior <rprior@protonmail.com>

(define-module (testing fossa)
  #:use-module (gnu packages base)
  #:use-module (gnu packages check)
  #:use-module (gnu packages golang)
  #:use-module (guix build-system go)
  #:use-module (testing hugo)
  #:use-module (testing summon)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages))

(define-public go-github-com-stretchr-testify-1.6
  (package
    (name "go-github-com-stretchr-testify")
    (version "1.6.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/stretchr/testify")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1yhiqqzjvi63pf01rgzx68gqkkvjx03fvl5wk30br5l6s81s090l"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/stretchr/testify"))
    (propagated-inputs
     `(("github.com/davecgh/go-spew" ,go-github-com-davecgh-go-spew)
       ("github.com/pmezard/go-difflib" ,go-github-com-pmezard-go-difflib)
       ("github.com/stretchr/objx" ,go-github-com-stretchr-objx)
       ("go-gopkg-in-yaml-v3@3" ,go-gopkg-in-yaml-v3)))
    (home-page "https://github.com/stretchr/testify")
    (synopsis "Go helper library for tests and invariant checking")
    (description "This package provide many tools for testifying that your
code will behave as you intend.

Features include:
@itemize
@item Easy assertions
@item Mocking
@item HTTP response trapping
@item Testing suite interfaces and functions.
@end itemize")
    (license license:expat)))

(define-public go-github-com-apex-log
  (package
    (name "go-github-com-apex-log")
    (version "1.9.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/apex/log")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1ibqkncnb8wcwilg2kyfyl5541g69rg551iy6m61q6iwdn5vfhi2"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/apex/log"))
    (home-page "https://github.com/apex/log")
    (propagated-inputs
     `(;; apex/logs
       ;; aphistic/golf
       ;; aphistic/sweet
       ("github.com/aws/aws-sdk-go" ,go-github-com-aws-sdk)
       ;; aybabtme/rgbterm
       ("go-github-com-fatih-color@1.8.0" ,go-github-com-fatih-color)
       ;; go-logfmt/logfmt
       ("github.com/golang/protobuf" ,go-github-com-golang-protobuf)
       ("go-github-com-google-uuid@1.1.1" ,go-github-com-google-uuid)
       ("go-github-com-jpillora-backoff@0.0.0-0.06c7a16" ,go-github-com-jpillora-backoff)
       ("go-github-com-kr-pretty@0.2.0" ,go-github-com-kr-pretty)
       ("go-github-com-mattn-go-colorable@0.0.0-0.efa5899" ,go-github-com-mattn-go-colorable)
       ("go-github-com-pkg-errors@0.9.1" ,go-github-com-pkg-errors)
       ;; rogpeppe/fastuuid
       ;; smartystreets/go-aws-auth
       ("go-github.com-smartystreets-gunit@1.0.0" ,go-github.com-smartystreets-gunit)
       ("go-github-com-stretchr-testify@1.5.1" ,go-github-com-stretchr-testify)
       ;; tj/assert
       ;; tj/go-buffer
       ;; tj/go-elastic
       ;; tj/go-kinesis
       ;; tj/go-spin
       ("go-golang-org-x-net@0.0.0-4.ba9fcec" ,go-golang-org-x-net)
       ("go-golang-org-x-text@0.3.2" ,go-golang-org-x-text)
       ("go-gopkg-in-check-v1@1.0.0-1.788fd78" ,go-gopkg-in-check-v1)
       ("go-gopkg-in-yaml-v2@2.2.2" ,go-gopkg-in-yaml-v2)
       ))
    (synopsis "Structured logging library for golang.")
    (description
     "This library provides structured logging facilities for golang to help
with tracing, debugging and testing.")
    (license license:expat)))

(define-public fossa-cli
  (package
    (name "go-github-com-fossas-fossa-cli")
    (version "1.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/fossas/fossa-cli")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1hwf1r0z2fs246izza89r73q7n2lmy73jpl9whlvl642qybrch5l"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/fossas/fossa-cli"))
    (native-inputs
     `(("go-github-com-burntsushi-toml@0.3.1" ,go-github-com-burntsushi-toml)
       ;; apex/log
       ("go-github-com-blang-semver@0.0.0-0.60ec348" ,go-github-com-blang-semver)
       ("go-github-com-bmatcuk-doublestar@1.3.0" ,go-github-com-bmatcuk-doublestar)
       ;; spinner
       ("go-github-com-cheekybits-genny@1.0.0" ,go-github-com-cheekybits-genny)
       ("go-github-com-fatih-color@1.8.0" ,go-github-com-fatih-color)
       ;; gnewton/jargo
       ("go-github-com-mattn-go-isatty@0.0.11" ,go-github-com-mattn-go-isatty)
       ("go-github-com-mitchellh-mapstructure@1.1.2" ,go-github-com-mitchellh-mapstructure)
       ("go-github-com-pkg-errors@0.9.1" ,go-github-com-pkg-errors)
       ;; rhysd/go-github-selfupdate
       ;; rveen/ogdl
       ("go-github-com-stretchr-testify@1.5.1" ,go-github-com-stretchr-testify)
       ("go-github-com-urfave-cli@1.22.2" ,go-github-com-urfave-cli)
       ;; gopkg.in/src-d/go-git
       ("go-gopkg-in-yaml-v2@2.2.2" ,go-gopkg-in-yaml-v2)
       ("go-golang-org-x-sync@0.0.0-1.6e8e738" ,go-golang-org-x-sync)
       ("go-github-com-olekukonko-tablewriter@0.0.4" ,go-github-com-olekukonko-tablewriter)
       ("go-github-com-masterminds-semver@3.1.0" ,go-github-com-masterminds-semver)))
    (home-page "https://fossa.com/")
    (synopsis "Tool for dependency analysis of source code.")
    (description
     "Fossa analyzes source code to report dependencies, licenses, and
vulnerabilities.  It can process complex code bases that contain multiple
subcomponents, such as monorepos.")
    (license license:mpl2.0)))
