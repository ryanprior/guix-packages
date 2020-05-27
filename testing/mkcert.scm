(define-module (testing mkcert)
  #:use-module (gnu packages)
  #:use-module (gnu packages nss)
  #:use-module (guix build-system go)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages))

(define-public mkcert
  (package
   (name "mkcert")
   (version "1.4.1")
   (source
    (origin
     (method git-fetch)
     (uri (git-reference
           (url "https://github.com/FiloSottile/mkcert.git")
           (commit (string-append "v" version))))
     (file-name (git-file-name name version))
     (sha256
      (base32
       "0w1ji96hbd3anzsz82xjcafsqhgyz7c7n41rsq60yrllwbj5946f"))))
   (build-system go-build-system)
   (arguments
    '(#:import-path "github.com/FiloSottile/mkcert"))
   (native-inputs
    `(("nss" ,nss)
      ;; TODO package more deps:

      ;; "crypto"
      ;; "crypto/x509"
      ;; "flag"
      ;; "fmt"
      ;; "log"
      ;; "net"
      ;; "net/mail"
      ;; "net/url"
      ;; "os"
      ;; "os/exec"
      ;; "os/user"
      ;; "path/filepath"
      ;; "regexp"
      ;; "runtime"
      ;; "runtime/debug"
      ;; "strings"
      ;; "sync"

      ;; "golang.org/x/net/idna"
      ))
   (home-page "https://mkcert.dev/")
   (synopsis "Make locally trusted development certificates")
   (description "Using certificates from real certificate authorities (CAs) for
development can be dangerous or impossible (for hosts like example.test,
localhost or 127.0.0.1), but self-signed certificates cause trust errors.
Managing your own CA is the best solution, and mkcert does this
automatically.")
   (license license:bsd-3)))

mkcert
