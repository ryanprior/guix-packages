;;; Copyright © 2020 Ryan Prior <rprior@protonmail.com>

(define-module (gnu packages visidata)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages python-science)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages time)
  #:use-module (gnu packages xml)
  #:use-module (guix build-system python)
  #:use-module (guix download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages))

(define-public visidata
  (package
   (name "visidata")
   (version "1.5.2")
   (source (origin
            (method url-fetch)
            (uri (pypi-uri "visidata" version))
            (sha256 (base32 "10adfyn4gkisvciqawgh2lakkhhnjjxiyp7mzbgcwkq1b3sigpf1"))))
   (build-system python-build-system)
   ;; Tests disabled because they are not packaged with the source tarball.
   ;; Upstream suggests tests will be packaged with tarball around 2.0 release.
   (arguments '(#:tests? #f))
   (inputs
    `(("python-dateutil" ,python-dateutil)
      ("python-fonttools" ,python-fonttools)
      ("python-h5py" ,python-h5py)
      ("python-lxml" ,python-lxml)
      ("python-openpyxl" ,python-openpyxl)
      ("python-psycopg2" ,python-psycopg2)
      ("python-pyyaml" ,python-pyyaml)
      ("python-requests" ,python-requests)
      ("python-xlrd" ,python-xlrd)
      ("python-pandas" ,python-pandas)))
   (synopsis "Terminal spreadsheet multitool for discovering and arranging data")
   (description
    "VisiData is an interactive multitool for tabular data.  It combines the
clarity of a spreadsheet, the efficiency of the terminal, and the power of
Python, into a lightweight utility which can handle millions of rows.")
   (home-page "https://www.visidata.org/")
   (license (list license:gpl3
                  license:expat)))) ;; visidata/vdtui.py

visidata
