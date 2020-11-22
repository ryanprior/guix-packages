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

(define-public go-github-com-onsi-gomega
  (package
    (name "go-github-com-onsi-gomega")
    (version "1.10.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/onsi/gomega")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1jvp5wlny86ncsqak5rz70qfn61y3ajc64w0hrj4840zgh90zb06"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/onsi/gomega"))
    (propagated-inputs
     `(("go-google-golang-org-protobuf@1.25.0" ,go-google-golang-org-protobuf)
       ("go-github-com-onsi-ginkgo" ,go-github-com-onsi-ginkgo-source)
       ("go-gopkg-in-yaml-v2@2.2.2" ,go-gopkg-in-yaml-v2)
       ("go-golang-org-x-net@0.0.0-4.ba9fcec" ,go-golang-org-x-net)
       ("go-golang-org-x-xerrors@0.0.0-0.5ec99f8" ,go-golang-org-x-xerrors)))
    (home-page "http://onsi.github.io/gomega/")
    (synopsis "Matcher library for golang.")
    (description
     "This library provides functions that match structured data against
various types of patterns.  It is part of the Ginkgo framework for
behavior-driven development.")
    (license license:expat)))

(define-public go-github-com-aphistic-sweet
  (package
    (name "go-github-com-aphistic-sweet")
    (version "0.3.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/aphistic/sweet")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1irr6gdy2dj6ysla84rhk2lhn8i00yzn0gq6q7mqpx3rka5lxrv2"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/aphistic/sweet"))
    (propagated-inputs
     `(("go-github-com-mattn-go-colorable@0.0.0-0.efa5899" ,go-github-com-mattn-go-colorable)
       ("go-github-com-mgutz-ansi@0.0.0-0.9520e82" ,go-github-com-mgutz-ansi)
       ("go-github-com-onsi-gomega@1.10.3" ,go-github-com-onsi-gomega)
       ("go-github-com-mattn-go-isatty@0.0.11" ,go-github-com-mattn-go-isatty)
       ("go-github-com-sergi-go-diff@1.1.0" ,go-github-com-sergi-go-diff)
       ("go-golang-org-x-crypto@0.0.0-5.2aa609c" ,go-golang-org-x-crypto)))
    (home-page "https://github.com/aphistic/sweet")
    (synopsis "Pluggable test runner for golang.")
    (description
     "This package extends the standard golang testing system with plugins.")
    (license license:expat)))

(define-public go-github-com-aphistic-golf
  (let ((commit "02c07f170c5a8a0300a199a10bb04a55c721c017")
        (revision "0"))
    (package
      (name "go-github-com-aphistic-golf")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/aphistic/golf")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "1qixab9bb29wqbr4nc5j3g25hq1j7am93f181rkj7a4qacncx763"))))
      (build-system go-build-system)
      (arguments
       '(#:import-path "github.com/aphistic/golf"
         ;; tests may be broken
         #:tests? #f))
      (native-inputs
       `(("go-github-com-aphistic-sweet@0.3.0" ,go-github-com-aphistic-sweet)))
      (propagated-inputs
       `(("go-github-com-google-uuid@1.1.1" ,go-github-com-google-uuid)))
      (home-page "https://github.com/aphistic/golf")
      (synopsis "Graylog log client for golang.")
      (description
       "This is a client library to send messages in the Graylog Extended Log Format.")
      (license license:expat))))

(define-public go-github-com-aybabtme-rgbterm
  (let ((commit "cc83f3b3ce5911279513a46d6d3316d67bedaa54")
        (revision "0"))
    (package
      (name "go-github-com-aybabtme-rgbterm")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/aybabtme/rgbterm")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "0wvmxvjn64968ikvnxrflb1x8rlcwzpfl53fzbxff2axbx9lq50q"))))
      (build-system go-build-system)
      (arguments
       '(#:import-path "github.com/aybabtme/rgbterm"))
      (home-page "https://godoc.org/github.com/aybabtme/rgbterm")
      (synopsis "Colorized text package for golang.")
      (description
       "This library provides convenience functions to make use of color codes
in terminals that support many colors.")
      (license license:expat))))

(define-public go-github-com-go-logfmt-logfmt
  (package
    (name "go-github-com-go-logfmt-logfmt")
    (version "0.5.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/go-logfmt/logfmt")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0mcvgimnf6a15hhpmmv42kzbsn618f0zi2j3np49xkq1113d7yyj"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/go-logfmt/logfmt"))
    (home-page "https://github.com/go-logfmt/logfmt")
    (synopsis "Structured data library for plaintext logs in golang.")
    (description
     "This package provides marshalling and unmarshalling of data into a log
format that is easy to read and to grep.")
    (license license:expat)))

(define-public go-github-com-rogpeppe-fastuuid
  (package
    (name "go-github-com-rogpeppe-fastuuid")
    (version "1.2.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/rogpeppe/fastuuid")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "028acdg63zkxpjz3l639nlhki2l0canr2v5jglrmwa1wpjqcfff8"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/rogpeppe/fastuuid"))
    (home-page "https://github.com/rogpeppe/fastuuid")
    (synopsis "Fast generator for 192-bit UUIDs.")
    (description
     "This golang library rapidly generates sequential unique identifiers.  It
supports 192-bit UUIDs and 128-bit RFC-4122 V4 UUID strings.")
    (license license:expat)))

;; TODO this package is deprecated upstream, is there a way to mark that in
;; Guix?
(define-public go-github-com-smartystreets-go-aws-auth
  (package
    (name "go-github-com-smartystreets-go-aws-auth")
    (version "1.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/smartystreets/go-aws-auth")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "07lri72f5ywi6h8pd9yf34mv3mqwy2pfdzc78wr14sk8v7i77bfh"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/smartystreets/go-aws-auth"))
    (propagated-inputs
     `(("go-github.com-smartystreets-assertions@1.8.1" ,go-github.com-smartystreets-assertions)
       ("go-github.com-smartystreets-gunit@1.0.0" ,go-github.com-smartystreets-gunit)))
    (home-page "https://github.com/smartystreets/go-aws-auth")
    (synopsis "Tool to sign requests to Amazon Web Services.")
    (description
     "This library provides golang helpers to sign requests to AWS using
Amazon IAM.")
    (license license:expat)))

(define-public go-github-com-tj-go-buffer
  (package
    (name "go-github-com-tj-go-buffer")
    (version "1.2.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/tj/go-buffer")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0xs8dz8m5qy1n80qcpalvfzdjxdr7djmagmhp7mm87rmjkwn05lk"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/tj/go-buffer"))
    (propagated-inputs
     `(("go-github-com-tj-assert@0.0.3" ,go-github-com-tj-assert)))
    (home-page "https://github.com/tj/go-buffer")
    (synopsis "Buffer library for batching I/O.")
    (description
     "This library provides a generic golang buffering facility for batching
expensive I/O, such as writing logs.")
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
     `(("go-github-com-apex-logs@0.9.23" ,go-github-com-apex-logs)
       ("go-github-com-aphistic-golf@0.0.0-0.02c07f1" ,go-github-com-aphistic-golf)
       ("go-github-com-aphistic-sweet@0.3.0" ,go-github-com-aphistic-sweet)
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
