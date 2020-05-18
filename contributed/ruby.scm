;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014, 2015 Pjotr Prins <pjotr.guix@thebird.nl>
;;; Copyright © 2014, 2015, 2016, 2017 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2014, 2015 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2014, 2015 David Thompson <davet@gnu.org>
;;; Copyright © 2015, 2019 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2015, 2016, 2017 Ben Woodcroft <donttrustben@gmail.com>
;;; Copyright © 2017 ng0 <ng0@n0.is>
;;; Copyright © 2017, 2019 Marius Bakke <mbakke@fastmail.com>
;;; Copyright © 2017, 2018, 2019 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2017, 2018, 2020 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2017 Clément Lassieur <clement@lassieur.org>
;;; Copyright © 2017, 2018, 2019 Christopher Baines <mail@cbaines.net>
;;; Copyright © 2018 Vasile Dumitrascu <va511e@yahoo.com>
;;; Copyright © 2018 Alex Vong <alexvong1995@gmail.com>
;;; Copyright © 2019 Pierre Neidhardt <mail@ambrevar.xyz>
;;; Copyright © 2019 Mikhail Kirillov <w96k.ru@gmail.com>
;;; Copyright © 2019 Jelle Licht <jlicht@fsfe.org>
;;; Copyright © 2019 Brian Leung <bkleung89@gmail.com>
;;; Copyright © 2019 Collin J. Doering <collin@rekahsoft.ca>
;;; Copyright © 2019 Diego N. Barbato <dnbarbato@posteo.de>
;;; Copyright © 2020 Ryan Prior <rprior@protonmail.com>

(define-module (contributed ruby)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages check)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages crypto)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages dbm)
  #:use-module (gnu packages rails)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages java)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages libidn)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages lsof)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages networking)
  #:use-module (gnu packages node)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages ragel)
  #:use-module (gnu packages rsync)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages version-control)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix utils)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages web)
  #:use-module (guix build-system ruby)
  #:use-module ((srfi srfi-1) #:select (alist-delete)))

;; TODO package jaro_winkler-1.5.4
;; TODO package unf_ext-0.0.7.6
;; irb wants these things >:3

(define-public ruby-2.5
  (package
    (name "ruby")
    (version "2.5.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://cache.ruby-lang.org/pub/ruby/"
                           (version-major+minor version)
                           "/ruby-" version ".tar.xz"))
       (sha256
        (base32
         "0vrhrw7kcz9mg0jkqnihkcxqy5k05v8k1j0y2735z8wfk8sx1j8w"))
       (modules '((guix build utils)))
       (snippet `(begin
                   ;; Remove bundled libffi
                   (delete-file-recursively "ext/fiddle/libffi-3.2.1")
                   #t))))
    (build-system gnu-build-system)
    (arguments
     `(#:test-target "test"
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'replace-bin-sh-and-remove-libffi
           (lambda _
             (substitute* '("Makefile.in"
                            "ext/pty/pty.c"
                            "io.c"
                            "lib/mkmf.rb"
                            "process.c"
                            "test/rubygems/test_gem_ext_configure_builder.rb"
                            "test/rdoc/test_rdoc_parser.rb"
                            "test/ruby/test_rubyoptions.rb"
                            "test/ruby/test_process.rb"
                            "test/ruby/test_system.rb"
                            "tool/rbinstall.rb")
               (("/bin/sh") (which "sh")))
             #t)))))
    (inputs
     `(("readline" ,readline)
       ("openssl" ,openssl)
       ("libffi" ,libffi)
       ("gdbm" ,gdbm)
       ("zlib" ,zlib)))
    (native-search-paths
     (list (search-path-specification
            (variable "GEM_PATH")
            (files (list (string-append "lib/ruby/vendor_ruby"))))))
    (synopsis "Programming language interpreter")
    (description "Ruby is a dynamic object-oriented programming language with
a focus on simplicity and productivity.")
    (home-page "https://www.ruby-lang.org")
    (license license:ruby)))

(define-public ruby-2.7
  (package
    (inherit ruby-2.5)
    (version "2.7.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://cache.ruby-lang.org/pub/ruby/"
                           (version-major+minor version)
                           "/ruby-" version ".tar.gz"))
       (sha256
        (base32
         "0674x98f542y02r7n2yv2qhmh97blqhi2mvh2dn5f000vlxlh66l"))
       (modules '((guix build utils)))
       (snippet `(begin
                   ;; Remove bundled libffi
                   (delete-file-recursively "ext/fiddle/libffi-3.2.1")
                   #t))))
    (arguments
     `(#:test-target "test"
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'replace-bin-sh-and-remove-libffi
           (lambda _
             (substitute* '("configure.ac"
                            "template/Makefile.in"
                            "lib/rubygems/installer.rb"
                            "ext/pty/pty.c"
                            "io.c"
                            "lib/mkmf.rb"
                            "process.c"
                            "test/rubygems/test_gem_ext_configure_builder.rb"
                            "test/rdoc/test_rdoc_parser.rb"
                            "test/ruby/test_rubyoptions.rb"
                            "test/ruby/test_process.rb"
                            "test/ruby/test_system.rb"
                            "tool/rbinstall.rb")
               (("/bin/sh") (which "sh")))
             #t)))))
    (native-inputs
     `(("autoconf" ,autoconf)))))

(define-public ruby ruby-2.7)

ruby
