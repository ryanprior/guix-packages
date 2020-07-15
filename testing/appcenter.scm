;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2020 Ryan Prior <rprior@protonmail.com>

(define-module (testing appcenter)
  #:use-module (gnu packages calendar)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages pantheon)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages webkit)
  #:use-module (guix build-system meson)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) :prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (testing pantheon))

(define-public appcenter-planner
  (package
    (name "appcenter-planner")
    (version "2.3.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/alainm23/planner.git")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "15d51sqb3k59jjishsw1fq5ib50jmhlk194y3nga329damfv8bmy"))))
    (build-system meson-build-system)
    (arguments
     `(#:glib-or-gtk? #t
       #:phases
       (modify-phases %standard-phases
         (add-before 'install 'set-environment-variables
           (lambda _
             ;; Disable compiling schemas and updating desktop databases
             (setenv "DESTDIR" "/")
             #t))
         (add-after 'install 'install-symlinks
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin/com.github.alainm23.planner"))
                    (link (string-append out "/bin/planner")))
               (symlink bin link)))))))
    (inputs
     `(("evolution-data-server" ,evolution-data-server)
       ("granite" ,granite)
       ("gtk" ,gtk+)
       ("libical" ,libical)
       ;; Planner is designed specifically for Pantheon. It will build and run
       ;; without the GTK and icon themes but it won't render right.
       ("pantheon-gtk-theme" ,pantheon-gtk-theme)
       ("pantheon-icon-theme" ,pantheon-icon-theme)
       ("webkitgtk" ,webkitgtk)))
    (native-inputs
     `(("gettext" ,gettext-minimal)
       ("glib" ,glib)
       ("glib:bin" ,glib "bin") ; for glib-compile-schemas
       ("gnome-settings-daemon" ,gnome-settings-daemon)
       ("intltool" ,intltool)
       ("json-glib" ,json-glib)
       ("libgee" ,libgee)
       ("libsoup" ,libsoup-minimal)
       ("pkg-config" ,pkg-config)
       ("vala" ,vala)))
    (home-page "https://planner-todo.web.app/")
    (synopsis "Desktop planner")
    (description "Planner keeps track of all your tasks, projects, and goals
in one place.")
    (license license:gpl3+)))

(define-public appcenter-ephemeral
    (package
    (name "appcenter-ephemeral")
    (version "6.4.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/cassidyjames/ephemeral.git")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1lzcwaczh601kwbx7fzg32nrzlg67asby7p86qy10qz86xf4g608"))))
    (build-system meson-build-system)
    (arguments
     `(#:glib-or-gtk? #t
       #:phases
       (modify-phases %standard-phases
         (add-before 'install 'set-environment-variables
           (lambda _
             ;; Disable compiling schemas and updating desktop databases
             (setenv "DESTDIR" "/")
             #t))
         (add-after 'install 'install-symlinks
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin/com.github.cassidyjames.ephemeral"))
                    (link (string-append out "/bin/ephemeral")))
               (symlink bin link)))))))
    (inputs
     `(("granite" ,granite)
       ("gtk" ,gtk+)
       ("pantheon-gtk-theme" ,pantheon-gtk-theme)
       ("pantheon-icon-theme" ,pantheon-icon-theme)
       ("libdazzle" ,libdazzle)
       ("webkitgtk" ,webkitgtk)))
    (native-inputs
     `(("gettext" ,gettext-minimal)
       ("glib" ,glib)
       ("glib:bin" ,glib "bin") ; for glib-compile-schemas
       ("gnome-settings-daemon" ,gnome-settings-daemon)
       ("intltool" ,intltool)
       ("json-glib" ,json-glib)
       ("libgee" ,libgee)
       ("libsoup" ,libsoup-minimal)
       ("pkg-config" ,pkg-config)
       ("vala" ,vala)))
    (home-page "https://planner-todo.web.app/")
    (synopsis "Forgetful web browser")
    (description "Ephemeral browses the web without saving any data.")
    (license license:gpl3+)))
