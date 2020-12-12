;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2020 Ryan Prior <rprior@protonmail.com>

(define-module (contributed vlang)
  #:use-module (gnu packages c)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages glib)
  #:use-module (guix build-system gnu)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages))

(define-public cjson
  (package
    (name "cjson")
    (version "1.7.14")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/DaveGamble/cJSON")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32 "1rlnailhjm180zb6pc17jwphjwivw8kfpqgixjfgq4iyryq46sah"))))
    (build-system cmake-build-system)
    (arguments
     `(#:configure-flags '("-DENABLE_CJSON_UTILS=On")))
    (home-page "https://github.com/DaveGamble/cJSON")
    (synopsis "JSON parser written in ANSI C")
    (description "This library provides a portable embeddable JSON parser.")
    (license license:expat)))

(define-public tiny-bignum
  (let ((commit "1d7a1f9b8e77316187a6b3eae8e68d60a6f9a4d4"))
    (package
     (name "tiny-bignum")
     (version (git-version "0" "0" commit))
     (source
      (origin
        (method git-fetch)
        (uri (git-reference
              (url "https://github.com/kokke/tiny-bignum-c")
              (commit commit)))
        (file-name (git-file-name "tiny-bignum" commit))
        (sha256
         (base32 "0vj71qlhlaa7d92bfar1kwqv6582dqrby8x3kdw0yzh82k2023g6"))))
     (build-system gnu-build-system)
     (arguments
      `(#:phases
        (modify-phases %standard-phases
          (delete 'configure)
          (add-after 'unpack 'patch-tests
            (lambda _
              (substitute* "scripts/test_rand.py"
                (("\t") "  ")
                (("\" % (\\w+)" _ symbol) (string-append "\" % int(" symbol ")")))
              #t))
          (replace 'check
            (lambda* (#:key tests? #:allow-other-keys)
              (when tests?
                (invoke "make" "test"))
              #t))
          (replace 'install
            (lambda* (#:key outputs #:allow-other-keys)
              (let ((share (string-append (assoc-ref outputs "out") "/share"))
                    (doc (string-append (assoc-ref outputs "out") "/doc")))
                (mkdir-p share)
                (install-file "bn.c" share)
                (install-file "bn.h" share)
                (mkdir-p doc)
                (install-file "LICENSE" doc)
                (install-file "README.md" doc))
              #t)))))
     (native-inputs
      `(("python" ,python-wrapper)))
     (home-page "https://github.com/kokke/tiny-bignum-c")
     (synopsis "Small portable multiple-precision unsigned integer arithmetic in C")
     (description
      "This library provides portable Arbitrary-precision unsigned integer
arithmetic in C, for calculating with large numbers.  Basic arithmetic (+, -,
*, /, %) and bitwise operations (&, |, ^. <<, >>) plus increments, decrements
and comparisons are supported.")
     (license license:unlicense))))

(define-public v
  (package
   (name "v")
   (version "0.1.27")
   (source
    (origin
     (method git-fetch)
     (uri (git-reference
           (url "https://github.com/vlang/v.git")
           (commit version)))
     (file-name (git-file-name name version))
     (sha256
      (base32 "1d9qhacllvkqif42jaayixhjyhx7pzslh8p1yr5p19447q763fq1"))))
   (build-system gnu-build-system)
   (arguments
    '(#:tests? #f ; tests are broken in v 0.1.27
      #:make-flags
      `("CC=gcc"
        "GITCLEANPULL=true"
        "GITFASTCLONE=mkdir -p"
        "TCCREPO="
        "TMPTCC=tcc"
        ,(string-append "TMPVC=" (assoc-ref %build-inputs "vc"))
        "VCREPO="
        "VERBOSE=1"
        "V_ALWAYS_CLEAN_TMP=false")
      #:phases
      (modify-phases %standard-phases
        (delete 'configure)
        (add-before 'build 'patch-makefile
          (lambda _
            (substitute* "Makefile"
              (("rm -rf") "true")
              (("v self") "v -cc gcc cmd/v"))
            #t))
        ;; A few tests are broken in v 0.1.27. This function should be
        ;; enabled to run tests in the next release.
        ;; (replace 'check
        ;;   (lambda _
        ;;     (let* ((tmpbin "tmp/bin")
        ;;            (gcc (which "gcc")))
        ;;       (mkdir-p tmpbin)
        ;;       (symlink gcc (string-append tmpbin "/cc"))
        ;;       (setenv "PATH" (string-append tmpbin ":" (getenv "PATH")))
        ;;       (invoke "./v" "test-fixed"))
        ;;     #t))
        (replace 'install
          (lambda _
            (let* ((bin (string-append (assoc-ref %outputs "out") "/bin"))
                   (tools (string-append bin "/cmd/tools"))
                   (thirdparty (string-append bin "/thirdparty"))
                   (vlib (string-append bin "/vlib"))
                   (vmod (string-append bin "/v.mod")))
              (mkdir-p bin)
              (copy-file "./v" (string-append bin "/v"))
              ;; v requires as of 0.1.27 that these other components are in the
              ;; same directory. In a future release we may be able to move
              ;; these into other output folders.
              (copy-recursively "cmd/tools" tools)
              (copy-recursively "thirdparty" thirdparty)
              (copy-recursively "vlib" vlib)
              (copy-file "v.mod" vmod))
            #t)))))
   (inputs
    `(("glib" ,glib)
      ("gcc" ,gcc)))
   (native-inputs
    `(("vc"
       ,(let ((vc-version "0884d7092f4c2a4f8ca16da6f1792efa235247be"))
          ;; v bootstraps from generated c source code from a dedicated
          ;; repository. It's readable, as generated source goes, and not at all
          ;; obfuscated, and it's about 15kb. The original source written in
          ;; golang is lost to the forces of entropy; modifying the generated c
          ;; source by hand has been a commonly used technique for iterating on
          ;; the codebase.
          (origin
            (method git-fetch)
            (uri (git-reference
                  (url "https://github.com/vlang/vc.git")
                  (commit vc-version)))
            (file-name (git-file-name "vc" vc-version))
            (sha256
             (base32 "17bs09iwxfd0si70j48n9nd16gfgcj8imd0azypk3xzzbz4wybnz")))))))
   (home-page "https://vlang.io/")
   (synopsis "Compiler for the V programming language")
   (description
    "V is a systems programming language.  It provides memory safety and thread
safety guarantees with minimal abstraction.")
   (license license:expat)))
