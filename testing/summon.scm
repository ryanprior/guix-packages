(define-module (testing summon)
  #:use-module (gnu packages)
  #:use-module (gnu packages check)
  #:use-module (gnu packages golang)
  #:use-module (guix build-system go)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils))

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

(define-public go-github-com-bgentry-go-netrc
  (package
    (name "go-github.com-bgentry-go-netrc")
    (version "master")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/bgentry/go-netrc")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0dn2h8avgavqdzdqnph8bkhj35bx0wssczry1zdczr22xv650g1l"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/bgentry/go-netrc/netrc"
       #:unpack-path "github.com/bgentry/go-netrc"))
    (home-page "https://github.com/bgentry/go-netrc")
    (synopsis "Package for reading and writing netrc files")
    (description
     "This package can parse netrc files, make changes to them, and then
serialize them back to netrc format, while preserving any whitespace that was
present in the source file.")
    (license license:expat)))

(define-public go-github-com-sirupsen-logrus-1.6
  (package
    (inherit go-github-com-sirupsen-logrus)
    (name "go-github-com-sirupsen-logrus")
    (version "1.6.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/sirupsen/logrus")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1zf9is1yxxnna0d1pyag2m9ziy3l27zb2j92p9msm1gx5jjrvzzj"))))))

(define-public go-github-com-cyberark-conjur-api
  (package
    (name "go-github.com-cyberark-conjur-api")
    (version "0.6.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/cyberark/conjur-api-go")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1w3f82miwh280sq5izfbmrr7vnwfjg8vr9lhc5m8b9yvb0cvi83j"))))
    (build-system go-build-system)
    (arguments
     '(#:tests? #f ;; conjur-api-go has tests but it requires a network
                   ;; service to run them.
       #:import-path "github.com/cyberark/conjur-api-go/conjurapi"
       #:unpack-path "github.com/cyberark/conjur-api-go"))
    (inputs
     `(("github.com/smartystreets/goconvey" ,go-github.com-smartystreets-goconvey)))
    (propagated-inputs
     `(("github.com/bgentry/go-netrc" ,go-github-com-bgentry-go-netrc)
       ("github.com/sirupsen/logrus" ,go-github-com-sirupsen-logrus-1.6)))
    (home-page "https://github.com/cyberark/conjur-api-go")
    (synopsis "Go client for the CyberArk Conjur API ")
    (description "This package provides programmatic Golang access to the Conjur API.")
    (license license:asl2.0)))

(define-public go-github-com-cyberark-summon-conjur
  (package
    (name "summon-conjur")
    (version "0.5.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/cyberark/summon-conjur")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "07c0n3pqrh51whwgxcj7r4hgxa3habgwp0l6lirhs8gry9hyzx5a"))))
    (build-system go-build-system)
    (arguments
     '(#:install-source? #f
       #:import-path "github.com/cyberark/summon-conjur/cmd"
       #:unpack-path "github.com/cyberark/summon-conjur"
       #:phases
       (modify-phases %standard-phases
         (add-after 'install 'rename-binary
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (src (string-append out "/bin/cmd"))
                    (dest (string-append out "/bin/summon-conjur")))
               (rename-file src dest)))))))
    (inputs
     `(("github.com/cyberark/conjur-api-go/conjurapi" ,go-github-com-cyberark-conjur-api)
       ("github.com/karrick/golf" ,go-github-com-karrick-golf)))
    (home-page "https://cyberark.github.io/summon/")
    (synopsis "Fetches secrets from a Conjur service")
    (description "The summon-conjur utility fetches a secret from Conjur,
printing it to stdout.")
    (license license:expat)))

(define-public go-github-com-cyberark-summon
  (package
    (name "summon")
    (version "0.8.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/cyberark/summon")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1z4xnrncwvp3rfm97zvc0ivvw2fh1hrjhj3rplvidzxjfyasbvwv"))))
    (build-system go-build-system)
    (arguments
     '(#:install-source? #f
       #:import-path "github.com/cyberark/summon/cmd"
       #:unpack-path "github.com/cyberark/summon"
       #:phases
       (modify-phases %standard-phases
         (add-after 'install 'rename-binary
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (src (string-append out "/bin/cmd"))
                    (dest (string-append out "/bin/summon")))
               (rename-file src dest)))))))
    (inputs
     `(("github.com/codegangsta/cli" ,go-github-com-codegangsta-cli)
       ("gopkg.in/yaml.v3" ,go-gopkg-in-yaml-v3)))
    (home-page "https://cyberark.github.io/summon/")
    (synopsis "Fetches secrets and makes them available to a process")
    (description "Summon fetches secrets using a provider program and a
configuration file, then launches a subprocess with access to those secrets
via its environment or a memory-mapped temporary file.  When the subprocess
exits, it removes the secrets.")
    (license license:expat)))
