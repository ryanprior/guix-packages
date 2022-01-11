;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2021 Ryan Prior <rprior@protonmail.com>

(define-module (testing emacs)
  #:use-module (gnu packages base)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages webkit)
  #:use-module (guix build-system gnu)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix utils))


(define-public emacs-edge
  (let ((commit "30d4cfc0806ee1de5a8e5091f0cc5bce22830460")
        (revision "2"))
    (package
      (inherit emacs-next)
      (name "emacs-edge")
      (version (git-version "28.0.90" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://git.savannah.gnu.org/git/emacs.git/")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "16maswj38fk36ajl4gvmjd70m81xy3d6dnsqcdyx2vv25q63l8dn"))))
      (arguments
       (substitute-keyword-arguments (package-arguments emacs-next)
         ((#:configure-flags flags ''())
          `(cons*
            "--with-native-compilation"
            "--with-pgtk"
            "--with-sqlite3"
            "--with-xinput2"
            "--with-xwidgets"
            ,flags))
         ((#:phases phases ''())
          `(modify-phases ,phases
             ;; Required for configure to find libgccjit
             (add-before 'configure 'set-library-path
               (lambda* (#:key inputs #:allow-other-keys)
                 (let ((libgccjit-version ,(package-version libgccjit))
                       (libgccjit-libdir
                        (string-append
                         (assoc-ref inputs "libgccjit") "/lib/gcc/"
                         %host-type "/" libgccjit-version "/")))
                   (setenv "LIBRARY_PATH"
                           (string-append libgccjit-libdir ":"
                                          (getenv "LIBRARY_PATH"))))
                 #t))
             ;; Add runtime library paths for libgccjit.
             (add-after 'unpack 'patch-driver-options
               (lambda* (#:key inputs #:allow-other-keys)
                 (substitute* "lisp/emacs-lisp/comp.el"
                   (("\\(defcustom native-comp-driver-options nil")
                    (format
                     #f "(defcustom native-comp-driver-options '(~s ~s ~s ~s)"
                     (string-append
                      "-B" (assoc-ref inputs "binutils") "/bin/")
                     (string-append
                      "-B" (assoc-ref inputs "glibc") "/lib/")
                     (string-append
                      "-B" (assoc-ref inputs "libgccjit") "/lib/")
                     (string-append
                      "-B" (assoc-ref inputs "libgccjit") "/lib/gcc/"))))
                 #t))))))
      (propagated-inputs
       (list gsettings-desktop-schemas glib-networking))
      (inputs
       `(("glibc" ,glibc)
         ("libgccjit" ,libgccjit)
         ("sqlite" ,sqlite)
         ("webkitgtk" ,webkitgtk-with-libsoup2)
         ,@(package-inputs emacs-next)))
      (home-page "https://github.com/masm11/emacs")
      (synopsis "Emacs text editor with @code{pgtk} and @code{xwidgets} support")
      (description "This is an unofficial Emacs fork build with a pure-GTK
graphical toolkit to work natively on Wayland.  In addition to that, xwidgets
also enabled and works without glitches even on X server."))))
