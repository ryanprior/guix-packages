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

(define-public go-github-com-tj-assert
  (package
    (name "go-github-com-tj-assert")
    (version "0.0.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/tj/assert")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1j5swk3fjq1h5fpqkipddz2ccnbidr7qrpm5dpdaflg9q5jnc673"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/tj/assert"))
    (propagated-inputs
     `(("go-github-com-davecgh-go-spew@1.1.1" ,go-github-com-davecgh-go-spew)
       ("go-github-com-stretchr-testify@1.6.1" ,go-github-com-stretchr-testify-1.6)
       ("go-gopkg-in-yaml-v3@3" ,go-gopkg-in-yaml-v3)))
    (home-page "https://github.com/tj/assert")
    (synopsis "Assertion helper library for golang.")
    (description
     "This library provides a facility to make assertions for testing, copied
from the Testify library.")
    (license license:expat)))

(define-public go-github-com-apex-logs
  (package
    (name "go-github-com-apex-logs")
    (version "0.9.23")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/apex/logs")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0dm4bqwnjlb5xgpym3qwmn3gxr05p29fjd6i8vdbc34cj6lyc35h"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/apex/logs"
       ;;; Tests require network.
       #:tests? #f))
    (native-inputs
     `(("go-github-com-stretchr-testify@1.6.1" ,go-github-com-stretchr-testify-1.6)
       ("go-github-com-tj-assert@0.0.3" ,go-github-com-tj-assert)))
    (home-page "https://github.com/apex/logs")
    (synopsis "Client library for the Apex Logs web service.")
    (description
     "This library provides a client interface to the Apex Logs structured log
management product.")
    (license license:expat)))

(define-public go-github-com-nxadm-tail
  (package
    (name "go-github-com-nxadm-tail")
    (version "1.4.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/nxadm/tail")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "034smp88zi8l8pa9h45q64z4x8waf411s46ds3pkh5fpvgpxlyg6"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/nxadm/tail"))
    (propagated-inputs
     `(("go-github-com-fsnotify-fsnotify@1.4.9" ,go-github-com-fsnotify-fsnotify)
       ("go-golang-org-x-sys@0.0.0-7.0598657" ,go-golang-org-x-sys)
       ("go-gopkg.in-tomb.v2@0.0.0-0.d5d1b58" ,go-gopkg.in-tomb.v2)))
    (home-page "https://github.com/nxadm/tail")
    (synopsis "Library for reading from continuously appended log files.")
    (description
     "This library provides facilities for continuously reading and processing
data as it as appended to a log file.")
    (license license:expat)))

(define-public go-github-com-onsi-ginkgo-source
  (package
    (name "go-github-com-onsi-ginkgo")
    (version "1.14.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/onsi/ginkgo")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1pvslbfrpzc8n99x33gyvk9aaz6lvdyyg6cj3axjzkyjxmh6d5kc"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/onsi/ginkgo"
       ;; Source-only package
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
         (delete 'build))))
    (propagated-inputs
     `(("go-github-com-fsnotify-fsnotify@1.4.9" ,go-github-com-fsnotify-fsnotify)
       ("go-github-com-nxadm-tail@1.4.5" ,go-github-com-nxadm-tail)
       ("go-golang-org-x-sys@0.0.0-7.0598657" ,go-golang-org-x-sys)
       ("go-golang-org-x-text@0.3.2" ,go-golang-org-x-text)))
    (home-page "onsi.github.io/ginkgo/")
    (synopsis "Behavior-driven development framework for golang.")
    (description
     "This framework extends golang's testing facilities with behavior-driven
tests written in an expressive style typical of the behavior driven
development (BDD) testing methodology.")
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
