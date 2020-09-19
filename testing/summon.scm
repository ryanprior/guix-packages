(define-module (testing summon)
  #:use-module (gnu packages)
  #:use-module (gnu packages golang)
  #:use-module (guix build-system go)
  #:use-module (guix packages))

(define-public go-github-com-codegangsta-cli
  ;; previous name for urfave/cli
  (package
    (inherit go-github-com-urfave-cli)
    (name "go-github.com-codegangsta-cli")
    (arguments
     '(#:import-path "github.com/codegangsta/cli"))))
