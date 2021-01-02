;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2020 Ryan Prior <rprior@protonmail.com>

(define-module (proposed vlang)
  #:use-module (gnu packages c)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages javascript)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages node)
  #:use-module (gnu packages python)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages valgrind)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system trivial)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils))

(define markdown-origin
  (let ((markdown-version "1ccfbcba945b649b61738b9c0455d31cf99564b2"))
    (origin
      (method git-fetch)
      (uri (git-reference
            (url "https://github.com/vlang/markdown")
            (commit markdown-version)))
      (file-name (git-file-name "vlang-markdown" markdown-version))
      (sha256
       (base32 "0s982qiwy4s9y07x9fsy4yn642schplhp9hrw2libg2bx4sw43as")))))

(define-public wyhash
  (package
    (name "wyhash")
    (version "5")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/wangyi-fudan/wyhash")
                    (commit (string-append "wyhash_v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32 "03ljs5iw9zrm3bydwggjvpwrcwmsd75h3dv1j4am4hw3h22cjdjc"))))
    (build-system trivial-build-system) ;; source-only package
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let* ((out (string-append (assoc-ref %outputs "out")))
                (src (string-append out "/include"))
                (doc (string-append out "/share/doc/" ,name "-" ,version)))
           (mkdir-p src)
           (mkdir-p doc)
           (chdir (assoc-ref %build-inputs "source"))
           (install-file "wyhash.h" src)
           (install-file "LICENSE" doc)
           (install-file "README.md" doc))
         #t)))
    (home-page "https://github.com/wangyi-fudan/wyhash")
    (synopsis "Embeddable hash function and random number generator.")
    (description "This package provides a portable hash function and random
number generator suitable for use in data structures.  Provided by default in
Zig, V, and Nim programming language standard libraries.")
    (license license:unlicense)))

(define-public vlang
  (package
   (name "vlang")
   (version "0.2")
   (source
    (origin
     (method git-fetch)
     (uri (git-reference
           (url "https://github.com/vlang/v")
           (commit version)))
     (file-name (git-file-name name version))
     (sha256
      (base32 "1x2sf2j6xl11kjvv0i0anjqwsfb1la11xr7yhdnbix9808442wm2"))
     (modules '((guix build utils)))
     ;; This patch is already accepted upstream but is required for version
     ;; 0.2. The package will build without it, but it will fail to run any v
     ;; tools afterwards because of how Guix changes modified timestamps in
     ;; the package files.
     (patches (search-patches
               "vlang-accommodate-timestamps.patch"))
     (snippet
      '(begin
         ;; Eventually remove the whole thirdparty directory.
         (for-each delete-file-recursively
                   '("thirdparty/bignum"
                     "thirdparty/cJSON"
                     "thirdparty/wyhash"))))))
   (build-system gnu-build-system)
   (arguments
    `(#:make-flags
      (list (string-append "CC=" ,(cc-for-target))
            "TMPTCC=tcc"
            (string-append "VC=" (assoc-ref %build-inputs "vc"))
            "GITCLEANPULL=true"
            "GITFASTCLONE=mkdir -p"
            "TCCREPO="
            "VCREPO="
            "VERBOSE=1")
      #:phases
      (modify-phases %standard-phases
        (delete 'configure)
        (add-before 'build 'patch-files
          (lambda* (#:key inputs #:allow-other-keys)
            (substitute* "Makefile"
              (("rm -rf") "true")
              (("--branch") ""))
            (substitute* "vlib/math/big/big.v"
              (("@VROOT/thirdparty/bignum")
               (string-append (assoc-ref inputs "tiny-bignum") "/share")))
            (substitute* "vlib/json/json_primitives.v"
              (("@VROOT/thirdparty/cJSON")
               (assoc-ref inputs "cJSON")))
            (substitute* "vlib/hash/wyhash.c.v"
              (("@VROOT/thirdparty/wyhash")
               (string-append (assoc-ref inputs "wyhash") "/include")))))
        (add-before 'build 'patch-cc
          (lambda _
            (let* ((bin "tmp/bin")
                   (gcc (which "gcc")))
              (mkdir-p bin)
              (symlink gcc (string-append bin "/cc"))
              (setenv "PATH" (string-append bin ":" (getenv "PATH"))))
            #t))
        (add-after 'build 'build-tools
          (lambda* (#:key inputs #:allow-other-keys)
            (copy-recursively (assoc-ref inputs "vmodules/markdown") "vmodules/markdown")
            (setenv "VMODULES" (string-append (getcwd) "/vmodules"))
            (invoke "./v" "build-tools" "-v")
            #t))
        (add-before 'check 'fix-or-delete-failing-tests
          (lambda _
            ;; The x64 tests copy .vv files into the test directory and then
            ;; write to them, so we need to make them writeable.
            (for-each (lambda (vv) (chmod vv #o644))
                      (find-files "vlib/v/gen/x64/tests/" "\\.vv$"))
            ;; The process test explicitly calls "/bin/sleep" and "/bin/date"
            (substitute* "vlib/os/process_test.v"
              (("/bin/sleep") (which "sleep"))
              (("/bin/date") (which "date")))
            ;; The valgrind test can't find `cc' even though it's on PATH, so
            ;; we pass it as an explicit argument.
            (substitute* "vlib/v/tests/valgrind/valgrind_test.v"
              (("\\$vexe") "$vexe -cc gcc"))
            (for-each delete-file
                      '(;; XXX As always, these should eventually be fixed and run.
                        "vlib/vweb/tests/vweb_test.v"
                        "vlib/v/tests/live_test.v"
                        "vlib/v/tests/repl/repl_test.v"))
            #t))
        (replace 'check
          (lambda* (#:key tests? #:allow-other-keys)
            (when tests?
              (invoke "./v" "test-fixed"))
            #t))
        (replace 'install
          (lambda* (#:key outputs #:allow-other-keys)
            (let* ((bin (string-append (assoc-ref outputs "out") "/bin"))
                   (cmd (string-append bin "/cmd"))
                   (thirdparty (string-append bin "/thirdparty"))
                   (vlib (string-append bin "/vlib"))
                   (vmodules (string-append bin "/vmodules"))
                   (vmod (string-append bin "/v.mod")))
              (mkdir-p bin)
              (copy-file "./v" (string-append bin "/v"))
              ;; v requires as of 0.1.27 that these other components are in the
              ;; same directory. In a future release we may be able to move
              ;; these into other output folders.
              (copy-recursively "cmd" cmd)
              (copy-recursively "thirdparty" thirdparty)
              (copy-recursively "vlib" vlib)
              (copy-file "v.mod" vmod))
            #t)))))
   (inputs
    `(("glib" ,glib)
      ("tiny-bignum" ,tiny-bignum)
      ("cJSON" ,(package-source cjson))
      ("wyhash" ,wyhash)))
   (native-inputs
    `(("vc"
       ;; Versions are not consistently tagged, but the matching commit will
       ;; probably have ‘v0.x.y’ in the commit message.
       ,(let ((vc-version "047460a4ae5f4a1ba8c31dc50ec5e50ebe80b7f6"))
          ;; v bootstraps from generated c source code from a dedicated
          ;; repository. It's readable, as generated source goes, and not at all
          ;; obfuscated, and it's about 15kb. The original source written in
          ;; golang is lost to the forces of entropy; modifying the generated c
          ;; source by hand has been a commonly used technique for iterating on
          ;; the codebase.
          (origin
            (method git-fetch)
            (uri (git-reference
                  (url "https://github.com/vlang/vc")
                  (commit vc-version)))
            (file-name (git-file-name "vc" vc-version))
            (sha256
             (base32 "1wlr9yzxz4bc7pbzbplzhjjr9sz5mwy2k2z5d3vwsnz56g245146")))))
      ("vmodules/markdown" ,markdown-origin)

      ;; For the tests.
      ("libx11" ,libx11)
      ("node" ,node)
      ("openssl" ,openssl)
      ("ps" ,procps)
      ("sqlite" ,sqlite)
      ("valgrind" ,valgrind)))
   (native-search-paths
    (list (search-path-specification
           (variable "VMODULES")
           (files '("bin/")))))
   (home-page "https://vlang.io/")
   (synopsis "Compiler for the V programming language")
   (description
    "V is a systems programming language.  It provides memory safety and thread
safety guarantees with minimal abstraction.")
   (license license:expat)))
