(define-module (testing summon)
  #:use-module (gnu packages)
  #:use-module (gnu packages golang)
  #:use-module (guix build-system go)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages))

(define-public go-github-com-codegangsta-cli
  ;; previous name for urfave/cli
  (package
    (inherit go-github-com-urfave-cli)
    (name "go-github.com-codegangsta-cli")
    (arguments
     '(#:import-path "github.com/codegangsta/cli"))))

(define-public go-gopkg-in-yaml-v3
  (package
    (inherit go-gopkg-in-yaml-v2)
    (name "go-gopkg-in-yaml-v3")
    (version "3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gopkg.in/yaml.v3.git")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "06m3kx5mp6vwfcpjl04c39r1dpbakg2la9ql1jw3zvpi6jk7iwbk"))))
    (arguments
     '(#:import-path "gopkg.in/yaml.v3"))
    (home-page "https://gopkg.in/yaml.v3")))
