;;; Copyright © 2021 Ryan Prior <rprior@protonmail.com>

(define-module (testing go)
  #:use-module (gnu packages base)
  #:use-module (gnu packages golang)
  #:use-module (guix build-system go)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages))

(define-public go-git-bytes-zone-brian-similar-sort
  (let ((commit "47a6eea3b0")
        (revision "0"))
    (package
      (name "similar-sort")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://git.bytes.zone/brian/similar-sort")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "1l75wwl3ghsw2fgh833mnb3537h5s02a0rwlf18dsakvrcy23a15"))))
      (build-system go-build-system)
      (arguments
       '(#:import-path "git.bytes.zone/brian/similar-sort"))
      (home-page "https://git.bytes.zone/brian/similar-sort")
      (synopsis "Utility to sort strings according to Levenshtein distance")
      (description
       "This utility takes a reference string and a list of candidates and
sorts them according similarity to the reference. This function is useful in
fuzzy-finding and data scripting.")
      (license license:cc-by-sa4.0))))
