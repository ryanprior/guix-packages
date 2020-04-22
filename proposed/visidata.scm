(define-module (visidata)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages python-science)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages time)
  #:use-module (gnu packages xml)
  #:use-module (guix build-system python)
  #:use-module (guix download)
  #:use-module (guix licenses)
  #:use-module (guix packages))

(define-public visidata
  (package
   (name "visidata")
   (version "1.5.2")
   (source (origin
            (method url-fetch)
            (uri (string-append "https://github.com/saulpw/visidata/archive/v" version ".tar.gz"))
            (sha256 (base32 "0h7hq6bnc8svkcc9995kkmgcb9n5qgm85rsshzzdicmg9rg3ymhi"))))
   (build-system python-build-system)
   (arguments '(#:tests? #f))
   ;; Tests disabled because they are not packaged with the source tarball.
   ;; View test status here: https://circleci.com/gh/saulpw/visidata/tree/stable
   ;; Upstream suggests tests will be packaged with tarball around 2.0 release.
   (native-inputs
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
   (synopsis "Visidata: A terminal spreadsheet multitool for discovering and arranging data")
   (description
    "VisiData is an interactive multitool for tabular data. It combines the
clarity of a spreadsheet, the efficiency of the terminal, and the power of
Python, into a lightweight utility which can handle millions of rows with ease.")
   (home-page "https://www.visidata.org/")
   (license gpl3)))

visidata
