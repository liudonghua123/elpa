;;; repology-license.el --- Freedom check for Repology  -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Free Software Foundation, Inc.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This library provides the `repology-free-p' function, which returns
;; a non-nil value when a package or a project can be considered as
;; free.

;; The decision is made by polling a number of "Reference
;; repositories", defined in `repology-license-reference-repositories'.
;; If the ratio of "Free" votes is equal or above
;; `repology-license-poll-threshold', the project is declared as free.

;; In order to see the results of each vote, and possibly debug the
;; process, you can set `repology-license-debug' to a non-nil value.


;;; Constants
(defconst repology-license-reference-repositories
  '(("debian"    "main"     t)
    ("debian"    "contrib"  t)
    ("gnuguix"   nil        t)
    ("hyperbola" nil        t)
    ("parabola"  nil        t)
    ("pureos"    nil        t)
    ("trisquel"  nil        t)
    ("gnu_elpa"  nil        t)
    ("^fedora"   nil        repology--license-check:fedora)
    ("^gentoo"   nil        repology--license-check:gentoo)
    ("^opensuse" "/oss"     repology--license-check:opensuse-oss)
    ("debian"    "non-free" nil)
    ("^opensuse" "/non-oss" nil))
  "List of repositories providing a reliable license information.

This is a list of triplets (REPO SUBREPO PREDICATE) where:

 REPO is a regexp matching the internal name of a repository;
 SUBREPO is a regexp matching a sub-repository or nil;
 PREDICATE is either a boolean or a function called with one string argument.

When PREDICATE is a function, it must return a non-nil value if the argument
is a free license according to the repository.  If PREDICATE is t, we trust
the repository to provide only free software.  Conversely, PREDICATE is nil
when the repository is known to reference only non-free software.

A repository with a PREDICATE function is expected to have the following
properties:
- it audits carefully the licenses it reports;
- it uses a consistent, and documented, license syntax;
- Repology properly fetches the licenses of its packages.
  See URL `https://repology.org/repositories/fields'.")

(defconst repology-license-poll-threshold 0.5
  "Ratio of votes above which a package is declared to be free.")


;;; Tools
(defun repology--license-interpret-vote (free votes)
  "Return freedom vote result as a boolean.
FREE is the number of \"Free\" votes.  VOTES is the total number of votes."
  (and (> votes 0)
       (>= (/ (float free) votes) repology-license-poll-threshold)))


;;; Reference Repository: Fedora
(defun repology--license-check:fedora (license)
  "Return a non-nil value if LICENSE is free, according to Fedora.
See URL \
`https://docs.fedoraproject.org/en-US/packaging-guidelines/LicensingGuidelines/'"
  (let ((case-fold-search t)
        ;; Anything in Fedora is free, unless its license contains the
        ;; following.
        (non-free-license-re
         (rx word-start "Redistributable, no modification permitted" word-end)))
    (not (string-match non-free-license-re license))))


;;; Reference Repository: Gentoo
(defconst repology--license-identifiers:gentoo
  (list
   ;; GPL-COMPATIBLE
   "AGPL-3" "AGPL-3+" "Apache-2.0" "Apache-2.0-with-LLVM-exceptions"
   "Artistic-2" "Boost-1.0" "BSD" "BSD-2" "CC0-1.0" "CeCILL-2"
   "Clarified-Artistic" "Clear-BSD"  "ECL-2.0" "FTL"
   "gcc-runtime-library-exception-3.1" "GPL-1" "GPL-1+" "GPL-2" "GPL-2+" "GPL-3"
   "GPL-3+" "GPL-2-with-classpath-exception" "GPL-2-with-exceptions"
   "GPL-2-with-font-exception" "GPL-2-with-linking-exception"
   "GPL-2-with-MySQL-FLOSS-exception" "GPL-2+-with-openssl-exception"
   "GPL-3+-with-cuda-exception" "GPL-3+-with-cuda-openssl-exception"
   "GPL-3-with-font-exception" "GPL-3+-with-opencl-exception"
   "GPL-3+-with-opencl-openssl-exception" "GPL-3-with-openssl-exception"
   "Transmission-OpenSSL-exception" "UPX-exception" "HPND" "IJG" "ISC" "LGPL-2"
   "LGPL-2+" "LGPL-2.1" "LGPL-2.1+" "LGPL-3" "LGPL-3+"
   "LGPL-2-with-linking-exception" "LGPL-2.1-with-linking-exception"
   "LGPL-3-with-linking-exception" "Nokia-Qt-LGPL-Exception-1.1" "libgcc"
   "libstdc++" "metapackage" "MIT" "MPL-2.0" "OPENLDAP" "PSF-2" "PSF-2.2"
   "PSF-2.3" "PSF-2.4" "public-domain" "PYTHON" "qwt" "Ruby" "Ruby-BSD"
   "SGI-B-2.0" "Sleepycat" "tanuki-community" "unicode" "Unlicense" "UoI-NCSA"
   "vim" "W3C" "WTFPL-2" "wxWinLL-3.1" "ZLIB" "ZPL"
   ;; FSF-APPROVED
   "AFL-2.1" "AFL-3.0" "Apache-1.0" "Apache-1.1" "APSL-2" "BSD-4" "CDDL" "CNRI"
   "CPAL-1.0" "CPL-1.0" "EPL-1.0" "EPL-2.0" "EUPL-1.1" "gnuplot" "IBM"
   "LPPL-1.2" "MPL-1.0" "MPL-1.1" "Ms-PL" "NPL-1.1" "openssl" "OSL-1.1"
   "OSL-2.0" "OSL-2.1" "PHP-3.01" "QPL" "QPL-1.0" "Zend-2.0"
   ;; OSI-APPROVED
   "AFL-3.0" "AGPL-3" "AGPL-3" "Apache-1.1" "Apache-2.0" "APL-1.0" "APSL-2"
   "Artistic" "Artistic-2" "Boost-1.0" "BSD" "BSD-2" "CDDL" "CNRI" "CPAL-1.0"
   "CPL-1.0" "ECL-2.0" "EPL-1.0" "EPL-2.0" "EUPL-1.1" "GPL-1" "GPL-2" "GPL-2"
   "GPL-3" "GPL-3" "HPND" "IBM" "IPAfont" "ISC" "LGPL-2" "LGPL-2.1" "LGPL-2.1"
   "LGPL-3" "LGPL-3" "LPPL-1.3c" "MIT" "MPL-1.0" "MPL-1.1" "MPL-2.0" "Ms-PL"
   "nethack" "NOSA" "OFL-1.1"  "OSL-2.1" "PHP-3" "PHP-3.01" "POSTGRESQL" "PSF-2"
   "QPL" "Sleepycat" "UoI-NCSA" "W3C" "Watcom-1.0" "wxWinLL-3" "ZLIB" "ZPL"
   ;; MISC-FREE
   "Allegro" "alternate" "AMPAS" "bea.ri.jsr173" "BEER-WARE" "boehm-gc" "BSD-1"
   "BSD-with-attribution" "BSD-with-disclosure" "buddy" "bufexplorer.vim"
   "BZIP2" "canfep" "CAOSL" "CDDL-Schily" "CeCILL-C" "CLX" "CMake" "CPL-0.5"
   "CRACKLIB" "Crypt-IDEA" "DES" "docbook" "dom4j" "DUMB-0.9.3"
   "eGenixPublic-1.1" "ElementTree" "Emacs" "ErlPL-1.1" "FastCGI" "feh"
   "File-MMagic" "Flashpix" "FLEX" "flexmock" "FLTK" "freetts" "FVWM" "gd"
   "gsm" "HTML-Tidy" "htmlc" "iASL" "icu" "IDPL" "imagemagick" "Info-ZIP"
   "inner-net" "Interbase-1.0" "ipadic" "ipx-utils" "Ispell" "JasPer2.0" "JDOM"
   "JNIC" "JOVE" "Khronos-CLHPP" "LambdaMOO" "LIBGLOSS" "libmng" "libpng"
   "libpng2" "libtiff" "LLVM-Grant" "LPPL-1.3" "LPPL-1.3b" "lsof"
   "Mail-Sendmail" "mapm-4.9.5" "matplotlib" "Mini-XML" "minpack"
   "MIT-with-advertising" "mm" "mpich2" "NCSA-HDF" "netcat" "NEWLIB" "ngrep"
   "Old-MIT" "openafs-krb5-a" "Openwall" "otter" "PCRE" "perforce" "photopc"
   "PHP-2.02" "pngcrush" "pngnq" "Princeton" "psutils" "qmail-nelson" "rc"
   "rdisc" "regexp-UofT" "repoze" "RSA" "rwpng" "scanlogd" "Sendmail"
   "Sendmail-Open-Source" "shrimp" "SMAIL" "Snd" "SNIA" "SSLeay" "Subversion"
   "SVFL" "symlinks" "tablelist" "tcltk" "tcp_wrappers_license" "TeX"
   "TeX-other-free" "the-Click-license" "Time-Format" "Time-modules" "tm-align"
   "torque-2.5" "totd" "Toyoda" "UCAR-Unidata" "URT" "VTK" "w3m" "x2x" "xbatt"
   "xboing" "XC" "Xdebug" "xtrs" "xvt" "YaTeX" "yuuji" "ZSH"
   ;; FSF-APPROVED-OTHER.
   "Arphic" "CC-BY-2.0" "CC-BY-2.5" "CC-BY-3.0" "CC-BY-4.0" "CC-BY-SA-2.0"
   "CC-BY-SA-2.5" "CC-BY-SA-3.0" "CC-BY-SA-4.0" "FDL-1.1" "FDL-1.1+" "FDL-1.2"
   "FDL-1.2+" "FDL-1.3" "FDL-1.3+" "FreeArt" "GPL-1" "GPL-1+" "GPL-2" "GPL-2+"
   "GPL-3" "GPL-3+" "IPAfont" "OFL" "OFL-1.1" "OPL"
   ;; MISC-FREE-DOCS.
   "BitstreamVera" "CC-PD" "CC-BY-SA-1.0" "CC-SA-1.0" "LDP-1" "LDP-1a"
   "man-pages" "man-pages-posix" "man-pages-posix-2013" "MaxMind2" "mplus-fonts"
   "myspell-en_CA-KevinAtkinson" "quake1-textures" "Texinfo-manual"
   "UbuntuFontLicense-1.0" "Unicode_Fonts_for_Ancient_Scripts" "vlgothic"
   "wxWinFDL-3")
  "List of identifiers considered as free licenses by Gentoo
See URL `https://wiki.gentoo.org/wiki/License_groups'.")

(defun repology--license-gentoo:skip-whitespace ()
  "Skip past the whitespace at point."
  (skip-chars-forward " \t"))

(defun repology--license-gentoo:skip-non-whitespace ()
  "Skip past the whitespace at point."
  (skip-chars-forward "^ \t"))

(defun repology--license-gentoo:advance (&optional n)
  "Advance N characters forward."
  (forward-char n))

(defun repology--license-gentoo:peek (&optional n)
  "Advance N characters forward."
  (following-char))

(defun repology--license-gentoo:and ()
  "Return license freedom for the \"and\" construct at point."
  ;; Skip past "(" token.
  (repology--license-gentoo:advance 1)
  (repology--license-gentoo:skip-whitespace)
  (let ((value t))
    (while (/= (repology--license-gentoo:peek) ?\))
      (repology--license-gentoo:skip-whitespace)
      (unless (repology--license-gentoo:read-next)
        (setq value nil)))
    ;; Skip past ")" character, and to next token.
    (repology--license-gentoo:advance 1)
    (repology--license-gentoo:skip-whitespace)
    value))

(defun repology--license-gentoo:or ()
  "Return license freedom for the \"or\" construct at point."
  ;; Skip past "|| (" token.
  (repology--license-gentoo:advance 4)
  (repology--license-gentoo:skip-whitespace)
  (let ((value nil))
    (while (/= (repology--license-gentoo:peek) ?\))
      (when (repology--license-gentoo:read-next)
        (setq value t)))
    ;; Skip past ")" character, and to next token.
    (repology--license-gentoo:advance 1)
    (repology--license-gentoo:skip-whitespace)
    value))

(defun repology--license-gentoo:identifier ()
  "Return freedom of the license identifier at point."
  (let ((origin (point)))
    (repology--license-gentoo:skip-non-whitespace)
    (let ((value (member-ignore-case (buffer-substring origin (point))
                                     repology--license-identifiers:gentoo)))
      (repology--license-gentoo:skip-whitespace)
      value)))

(defun repology--license-gentoo:parameter ()
  "Assume parameter at point is set, and return license freedom accordingly."
  ;; Skip past parameter.
  (repology--license-gentoo:skip-non-whitespace)
  (repology--license-gentoo:skip-whitespace)
  (repology--license-gentoo:and))

(defun repology--license-gentoo:read-next ()
  "Return license freedom for next token, and move point past it."
  (let ((parameter-re (rx (opt "!") (one-or-more (any alnum "-" "_")) "?")))
    (pcase (repology--license-gentoo:peek)
      (?\| (repology--license-gentoo:or))
      (?\( (repology--license-gentoo:and))
      ((guard (looking-at parameter-re))
       (repology--license-gentoo:parameter))
      (_
       (repology--license-gentoo:identifier)))))

(defun repology--license-check:gentoo (license)
  "Return a non-nil value if LICENSE is free, according to Gentoo."
  (with-temp-buffer
    (insert license)
    (goto-char 1)
    (repology--license-gentoo:skip-whitespace)
    (let ((value (not (eobp))))         ;blank string check
      (while (and value (/= (repology--license-gentoo:peek) 0))
        (unless (repology--license-gentoo:read-next)
          (setq value nil)))
      value)))


;;; Reference Repository: OpenSUSE (OSS)
(defun repology--license-check:opensuse-oss (license)
  "Return a non-nil value if LICENSE is free, according to OpenSUSE (OSS).
See URL `https://en.opensuse.org/openSUSE:Packaging_guidelines#Licensing'."
  (let ((case-fold-search t)
        ;; Anything in Fedora is free, unless its license contains the
        ;; following.
        (non-free-license-re
         (rx word-start "SUSE-Firmware" word-end)))
    (not (string-match non-free-license-re license))))


;;; License Check Debugging
(defvar repology-license-debug nil
  "When non-nil, display explanations when a project declared non-free.
Information is displayed in \"*Repology: License Debug*\" buffer.")

(defun repology--license-debug-line (package free)
  "Format license debug information for PACKAGE.
When FREE is non-nil, declare PACKAGE was reported as free."
  (let ((repo (repology-package-field package 'repo))
        (subrepo (repology-package-field package 'subrepo))
        (name (repology-package-field package 'visiblename)))
    (format "%s (%s%s) => %s\n"
            name
            repo
            (if subrepo (concat "/" subrepo) "")
            (if free "Free" "Non-Free"))))

(defun repology--license-debug-display (project reports free votes)
  "Print license check output for non-free PROJECT.
REPORTS is a list of strings, as returned by `repology--license-debug-line'.
FREE is the number of free packages in PROJECT.  VOTES is the number of packages
from reference repositories in PROJECT."
  (with-current-buffer (get-buffer-create "*Repology: License Debug*")
    (insert (format "=== Project %S: %s (ratio: %.2f) ===\n"
                    (repology-project-name project)
                    (if (repology--license-interpret-vote free votes)
                        "FREE"
                      "NON-FREE")
                    (if (= votes 0)
                        0
                      (/ (float free) votes))))
    (apply #'insert reports)
    (insert "\n")))


;;; Main Function
(defun repology--license-find-reference-repository (package)
  "Return the reference repository containing PACKAGE, or nil.
Return value is a triplet per `repology-license-reference-repositories'."
  (let ((repo (repology-package-field package 'repo))
        (subrepo (repology-package-field package 'subrepo)))
    (seq-find (pcase-lambda (`(,r ,s ,_))
                (and (string-match r repo)
                     (or (not s)
                         (and subrepo (string-match s subrepo)))))
              repology-license-reference-repositories)))

(defun repology--license-free-p (package &optional repository)
  "Return a non-nil value when PACKAGE is free.
A package is free when any reference repository can attest it uses only free
licenses.  When optional argument REPOSITORY is non-nil, use it as a reference."
  (pcase (or repository (repology--license-find-reference-repository package))
    ('nil nil)
    (`(,_ ,_ ,(and (pred functionp) p))
     (seq-every-p p (repology-package-field package 'licenses)))
    (`(,_ ,_ ,boolean) boolean)
    (other (error "Wrong repository definition: %S" other))))

(defun repology-free-p (datum)
  "Return a non-nil value when DATUM is free.

DATUM is a project or a package.

A package is free when any reference repository can attest it uses only free
licenses.  See `repology-license-reference-repositories' for a list of such
repositories.

A project is free if the ratio of free packages among the packages from
reference repositories is above `repology-license-poll-threshold'.
A project without any package from these repositories is declared as non-free.

Of course, it is not a legal statement, merely an indicator."
  (pcase datum
    ((pred repology-package-p) (repology--license-free-p datum))
    ((pred repology-project-p)
     (let ((votes 0)
           (yes 0)
           (reports nil)
           (voters nil))
       (dolist (package (repology-project-packages datum))
         (pcase (repology--license-find-reference-repository package)
           ('nil nil)
           (repository
            (unless (member repository voters)
              (cl-incf votes)
              (push repository voters)  ;a repository votes only once
              (let ((free (repology--license-free-p package repository)))
                (when free (cl-incf yes))
                (when repology-license-debug
                  (push (repology--license-debug-line package free)
                        reports)))))))
       ;; Maybe display vote reports as debugging information.
       (when repology-license-debug
         (repology--license-debug-display datum reports yes votes))
       ;; Return value.
       (repology--license-interpret-vote yes votes)))
    (_ (user-error "Wrong argument type: %S" datum))))

(provide 'repology-license)
;;; repology-license.el ends here
