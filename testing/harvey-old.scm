;;; Harvey - the hero that Gotham needs right now
;;; Copyright © 2014 John Darrington <jmd@gnu.org>
;;; Copyright © 2014, 2016 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2016, 2018 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2017 Marius Bakke <mbakke@fastmail.com>
;;; Copyright © 2018 Tobias Geerinckx-Rice <me@tobias.gr>
;;;
;;; This file is not part of GNU Guix.
;;;
;;; GNU Guix is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; GNU Guix is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Guix.  If not, see <http://www.gnu.org/licenses/>.

(define-module (gnu packages harvey)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix utils)
  #:use-module (guix build-system meson)
  #:use-module (gnu packages)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gtk))

(define-public harvey
  (package
    (name "harvey")
    (version "1.0.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/danrabbit/harvey"
                                  "/archive/"
                                  version ".tar.gz"))
              (sha256
               (base32
                "0n57h4541v2vdphwb87qzn3k2p7ppyzkbdnw2fkhbd7awvcfbn2i"))))
    (build-system meson-build-system)
    (inputs
     `(("vala" ,vala)
       ("gtk" ,gtk+-3)))
    (native-inputs
     `(("glib" ,glib "bin")))
    (arguments
     `(#:tests? #f))
    (home-page "https://appcenter.elementary.io/com.github.danrabbit.harvey/")
    (synopsis "Calculate and visualize color contrast")
    (description "The hero that Gotham needs right now.
Harvey calculates and visualizes color contrast.
It checks a given set of colors for WCAG contrast compliance.")
    (license license:gpl2+)))
