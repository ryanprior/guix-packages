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

(define-public go-github-com-karrick-golf
  (package
    (name "go-github.com-karrick-golf")
    (version "1.4.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/karrick/golf")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "05gfgajyzjnrqp9c9cy400h8w3hps9kssc5szscp06117z2nb1sp"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/karrick/golf"))
    (home-page "https://github.com/karrick/golf")
    (synopsis "Light-weight long and short command line option parser")
    (description
     "Golf is a modest options parsing library for Go command line interface
programs.  Meant to be small, like flag included in Go's standard library.")
    (license license:asl2.0)))
