;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2020 Ryan Prior <rprior@protonmail.com>

(define-module (testing v)
  #:use-module (gnu packages c)
  #:use-module (gnu packages glib)
  #:use-module (guix build-system gnu)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages))

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
    '(#:make-flags (list "TMPVC=vc/"
                         "VERBOSE=1")
      #:phases
      (modify-phases %standard-phases
        (replace 'configure
          (lambda* (#:key inputs #:allow-other-keys)
            (let ((vc-in (string-append (assoc-ref inputs "vc") "/v.c"))
                  (vc "vc/v.c"))
              (setenv "CC" (which "tcc"))
              (mkdir-p "vc/")
              (copy-file vc-in vc)
              (substitute* vc
                (("tos_lit\\(\"cc\"\\)") "tos_lit(\"gcc\")"))
            #t)))
        (add-before 'build 'patch-makefile
          (lambda _
            (substitute* "Makefile"
              (("latest_(tcc|vc)") ""))
            #t)))))
   (native-inputs
    `(("tcc" ,tcc)
      ("glib" ,glib)
      ("vc" ,(origin
               (method git-fetch)
               (uri (git-reference
                     (url "https://github.com/vlang/vc.git")
                     (commit "c3c337aa3ab2f9279f2a97ba759f8fa4da8f70c6")))
               (file-name (git-file-name "vc" "c3c337aa3ab2f9279f2a97ba759f8fa4da8f70c6"))
               (sha256
                (base32 "07318idrfkxfxf6zw5qsshx7ljd8gbvfic61mr3zrgsrd7hwwx4n"))))))
   (home-page "https://vlang.io/")
   (synopsis "Compiler for the V programming language")
   (description
    "V is a systems programming language.  It provides memory safety and thread
safety guarantees with minimal abstraction.")
   (license license:expat)))

v
