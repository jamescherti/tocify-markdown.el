(require 'ert)
(require 'el-mock)
(require 'tocify-markdown)
(require 'mocker)

(ert-deftest test-tocify-markdown-version ()
  (setq tocify-markdown-indentation-space 4)
  (let ((tocify-markdown--toc-version "0.1.2"))
    (should (equal "tocify-markdown version: 0.1.2" (tocify-markdown-version)))))

(ert-deftest tocify-markdown--symbol ()
  (setq tocify-markdown-indentation-space 4)
  (should (equal "   "       (tocify-markdown--symbol " " 3)))
  (should (equal "-#--#--#-" (tocify-markdown--symbol "-#-" 3))))

(ert-deftest tocify-markdown--to-link ()
  (setq tocify-markdown-indentation-space 4)
  (should (equal "[some markdown page~title (foo).](#some-markdown-pagetitle-foo-1)"
                 (tocify-markdown--to-link "some markdown page~title (foo)." 1)))
  (should (equal "[some markdown page~title (foo).](#some-markdown-pagetitle-foo)"
                 (tocify-markdown--to-link "some markdown page~title (foo)." 0)))
  (should (equal "[ under_score](#under_score)"
                 (tocify-markdown--to-link " under_score"))))

(ert-deftest tocify-markdown--to-tocify-markdown ()
  (setq tocify-markdown-indentation-space 4)
  (should (equal "- [some markdown page title](#some-markdown-page-title)
- [main title](#main-title)
    - [Sources](#sources)
        - [Marmalade (recommended)](#marmalade-recommended)
        - [Melpa-stable](#melpa-stable)
        - [Melpa (~snapshot)](#melpa-snapshot)
    - [Install](#install)
        - [Load org-trello](#load-org-trello)
        - [Alternative](#alternative)
            - [Git](#git)
            - [Tar](#tar)
- [another title](#another-title)
    - [with](#with)
    - [some](#some)
    - [heading](#heading)"
                 (tocify-markdown--to-tocify-markdown '((0 . "some markdown page title")
                                                  (0 . "main title")
                                                  (1 . "Sources")
                                                  (2 . "Marmalade (recommended)")
                                                  (2 . "Melpa-stable")
                                                  (2 . "Melpa (~snapshot)")
                                                  (1 . "Install")
                                                  (2 . "Load org-trello")
                                                  (2 . "Alternative")
                                                  (3 . "Git")
                                                  (3 . "Tar")
                                                  (0 . "another title")
                                                  (1 . "with")
                                                  (1 . "some")
                                                  (1 . "heading"))))))

(ert-deftest tocify-markdown--compute-toc-structure-from-level ()
  (setq tocify-markdown-indentation-space 4)
  (should (equal '((0 . "Sources") (1 . "Marmalade (recommended)") (1 . "Melpa-stable"))
                 (tocify-markdown--compute-toc-structure-from-level
                  0
                  '("Sources" ("." . 130) ("Marmalade (recommended)" . 311) ("Melpa-stable" . 552)))))

  (should (equal '((0 . "Install") (1 . "Load org-trello") (1 . "Alternative") (2 . "Git") (2 . "Tar"))
                 (tocify-markdown--compute-toc-structure-from-level
                  0
                  '("Install" ("." . 1184) ("Load org-trello" . 1277) ("Alternative" ("." . 1563) ("Git" . 1580) ("Tar" . 1881))))))
  (should (equal '((0 . "some markdown page title"))
                 (tocify-markdown--compute-toc-structure-from-level
                  0
                  '("some markdown page title" . 1)))))

(ert-deftest tocify-markdown--compute-toc-structure ()
  (setq tocify-markdown-indentation-space 4)
  (should (equal
           '((0 . "some markdown page title")
             (0 . "main title")
             (1 . "Sources")
             (2 . "Marmalade (recommended)")
             (2 . "Melpa-stable")
             (2 . "Melpa (~snapshot)")
             (1 . "Install")
             (2 . "Load org-trello")
             (2 . "Alternative")
             (3 . "Git")
             (3 . "Tar")
             (0 . "another title")
             (1 . "with")
             (1 . "some")
             (1 . "heading"))
           (tocify-markdown--compute-toc-structure
            '(("some markdown page title" . 1)
              ("main title"
               (#1="." . 52)
               ("Sources" (#1# . 130) ("Marmalade (recommended)" . 311) ("Melpa-stable" . 552) ("Melpa (~snapshot)" . 792))
               ("Install" (#1# . 1184) ("Load org-trello" . 1277) ("Alternative" (#1# . 1563) ("Git" . 1580) ("Tar" . 1881))))

              ("another title"
               (#1# . 2044)
               ("with" . 2061)
               ("some" . 2070)
               ("heading" . 2079)))))))

(ert-deftest tocify-markdown--compute-full-toc ()
  (setq tocify-markdown-indentation-space 4)
  (should (equal
           "<!-- tocify-markdown start - Don't edit this section. Run M-x tocify-markdown-refresh-toc -->\n**Table of Contents**\n\nsome-toc\n\n<!-- tocify-markdown end -->\n"
           (tocify-markdown--compute-full-toc "some-toc"))))

(defmacro tocify-markdown-with-temp-buffer-and-return-buffer-content (text body-test)
  "A `tocify-markdown' test macro to ease testing.
TEXT is the content of the buffer.
BODY-TEST is the assertion to test on the buffer.
NB-LINES-FORWARD is the number of lines to get back to."
  `(with-temp-buffer
     (markdown-mode)
     (insert ,text)
     (progn
       (goto-char (point-min))
       ,body-test
       (buffer-substring-no-properties (point-min) (point-max)))))

;; Create a new TOC
(ert-deftest tocify-markdown-generate-toc--first-toc ()
  (setq tocify-markdown-indentation-space 4)
  (should (equal "<!-- tocify-markdown start - Don't edit this section. Run M-x tocify-markdown-refresh-toc -->
**Table of Contents**

- [something](#something)
    - [Sources](#sources)
        - [Marmalade (recommended)](#marmalade-recommended)
        - [Melpa-stable](#melpa-stable)
        - [Melpa (~snapshot)](#melpa-snapshot)
        - [[配置 SPF Sender Policy Framework 记录]](#配置-spf-sender-policy-framework-记录)
    - [Install](#install)
        - [Load org-trello](#load-org-trello)
        - [Alternative](#alternative)
            - [Git](#git)
            - [Tar](#tar)

<!-- tocify-markdown end -->
To install **org-trello** in your emacs, you need a few steps.
# something
## Sources
If not already configured, you need to prepare emacs to work with marmalade or melpa.
For this, you need to install a snippet of code in your emacs configuration file.
### Marmalade (recommended)
### Melpa-stable
### Melpa (~snapshot)
### [配置 SPF Sender Policy Framework 记录]
## Install
### Load org-trello
### Alternative
#### Git
#### Tar
"
                 (tocify-markdown-with-temp-buffer-and-return-buffer-content
                  "To install **org-trello** in your emacs, you need a few steps.
# something
## Sources
If not already configured, you need to prepare emacs to work with marmalade or melpa.
For this, you need to install a snippet of code in your emacs configuration file.
### Marmalade (recommended)
### Melpa-stable
### Melpa (~snapshot)
### [配置 SPF Sender Policy Framework 记录]
## Install
### Load org-trello
### Alternative
#### Git
#### Tar
"
                  (tocify-markdown-generate-toc)))))

(ert-deftest tocify-markdown-generate-toc--with-duplicate-titles ()
  (setq tocify-markdown-indentation-space 4)
  (should (equal "<!-- tocify-markdown start - Don't edit this section. Run M-x tocify-markdown-refresh-toc -->
**Table of Contents**

- [something](#something)
- [something](#something-1)
- [something](#something-2)
- [something](#something-3)
    - [Sources](#sources)
        - [Marmalade (recommended)](#marmalade-recommended)
        - [Melpa-stable](#melpa-stable)
        - [Melpa (~snapshot)](#melpa-snapshot)
        - [[配置 SPF Sender Policy Framework 记录]](#配置-spf-sender-policy-framework-记录)
    - [Install](#install)
        - [Load org-trello](#load-org-trello)
        - [Alternative](#alternative)
            - [Git](#git)
            - [Tar](#tar)

<!-- tocify-markdown end -->
To install **org-trello** in your emacs, you need a few steps.
# something
# something
# something
# something
## Sources
If not already configured, you need to prepare emacs to work with marmalade or melpa.
For this, you need to install a snippet of code in your emacs configuration file.
### Marmalade (recommended)
### Melpa-stable
### Melpa (~snapshot)
### [配置 SPF Sender Policy Framework 记录]
## Install
### Load org-trello
### Alternative
#### Git
#### Tar
"
                 (tocify-markdown-with-temp-buffer-and-return-buffer-content
                  "To install **org-trello** in your emacs, you need a few steps.
# something
# something
# something
# something
## Sources
If not already configured, you need to prepare emacs to work with marmalade or melpa.
For this, you need to install a snippet of code in your emacs configuration file.
### Marmalade (recommended)
### Melpa-stable
### Melpa (~snapshot)
### [配置 SPF Sender Policy Framework 记录]
## Install
### Load org-trello
### Alternative
#### Git
#### Tar
"
                  (tocify-markdown-generate-toc)))))

(ert-deftest tocify-markdown-generate-toc--with-customs ()
  (setq tocify-markdown-indentation-space 4)
  (should (equal "<!-- tocify-markdown start -->
** foobar **

- [something](#something)
    - [Sources](#sources)

<!-- tocify-markdown end -->
blahblah.
# something
## Sources
"
                 (let ((tocify-markdown-header-toc-start "<!-- tocify-markdown start -->")
                       (tocify-markdown-header-toc-title "** foobar **")
                       (tocify-markdown-header-toc-end "<!-- tocify-markdown end -->"))
                   (tocify-markdown-with-temp-buffer-and-return-buffer-content
                    "blahblah.
# something
## Sources
"
                    (tocify-markdown-generate-toc))))))

(ert-deftest tocify-markdown-generate-toc--first-toc-with-user-override ()
  (setq tocify-markdown-indentation-space 4)
  (should (equal "<!-- tocify-markdown start - Don't edit this section. Run M-x tocify-markdown-refresh-toc -->
**Table of Contents**

    - [Sources](#sources)
        - [Marmalade (recommended)](#marmalade-recommended)
        - [Melpa-stable](#melpa-stable)
        - [Melpa (~snapshot)](#melpa-snapshot)
    - [Install](#install)
        - [Load org-trello](#load-org-trello)
        - [Alternative](#alternative)
            - [Git](#git)
            - [Tar](#tar)

<!-- tocify-markdown end -->
To install **org-trello** in your emacs, you need a few steps.
# something
## Sources
If not already configured, you need to prepare emacs to work with marmalade or melpa.
For this, you need to install a snippet of code in your emacs configuration file.
### Marmalade (recommended)
### Melpa-stable
### Melpa (~snapshot)
## Install
### Load org-trello
### Alternative
#### Git
#### Tar
"
                 (let ((tocify-markdown-user-toc-structure-manipulation-fn 'cdr))
                   (tocify-markdown-with-temp-buffer-and-return-buffer-content
                    "To install **org-trello** in your emacs, you need a few steps.
# something
## Sources
If not already configured, you need to prepare emacs to work with marmalade or melpa.
For this, you need to install a snippet of code in your emacs configuration file.
### Marmalade (recommended)
### Melpa-stable
### Melpa (~snapshot)
## Install
### Load org-trello
### Alternative
#### Git
#### Tar
"
                    (tocify-markdown-generate-toc))))))

(ert-deftest tocify-markdown-generate-toc--replace-old-toc-if-already-present ()
  (setq tocify-markdown-indentation-space 4)
  (should (equal "<!-- tocify-markdown start - Don't edit this section. Run M-x tocify-markdown-refresh-toc -->
**Table of Contents**

- [Packages](#packages)
    - [Sources](#sources)
        - [Marmalade (recommended)](#marmalade-recommended)
        - [Melpa-stable](#melpa-stable)
        - [Melpa (~snapshot)](#melpa-snapshot)
    - [Install](#install)
        - [Load org-trello](#load-org-trello)
        - [Alternative](#alternative)
            - [Git](#git)
            - [Tar](#tar)

<!-- tocify-markdown end -->
To install **org-trello** in your emacs, you need a few steps.
# Packages
## Sources
If not already configured, you need to prepare emacs to work with marmalade or melpa.
For this, you need to install a snippet of code in your emacs configuration file.
### Marmalade (recommended)
### Melpa-stable
### Melpa (~snapshot)
## Install
### Load org-trello
### Alternative
#### Git
#### Tar
"
                 (tocify-markdown-with-temp-buffer-and-return-buffer-content
                  "<!-- tocify-markdown start - Don't edit this section. Run M-x tocify-markdown-refresh-toc -->
**Table of Contents**

    - [Melpa (~snapshot)](#melpa-snapshot)
    - [Install](#install)
        - [Load org-trello](#load-org-trello)
        - [Alternative](#alternative)
            - [Git](#git)
            - [Tar](#tar)

<!-- tocify-markdown end -->
To install **org-trello** in your emacs, you need a few steps.
# Packages
## Sources
If not already configured, you need to prepare emacs to work with marmalade or melpa.
For this, you need to install a snippet of code in your emacs configuration file.
### Marmalade (recommended)
### Melpa-stable
### Melpa (~snapshot)
## Install
### Load org-trello
### Alternative
#### Git
#### Tar
"
                  (tocify-markdown-generate-toc)))))

(ert-deftest tocify-markdown-generate-toc--replace-old-toc ()
  (setq tocify-markdown-indentation-space 4)
  ;; Update an existing TOC
  (should (equal "some foo bar before

<!-- tocify-markdown start - Don't edit this section. Run M-x tocify-markdown-refresh-toc -->
**Table of Contents**

- [Packages](#packages)
    - [Sources](#sources)
        - [Marmalade (recommended)](#marmalade-recommended)
        - [Melpa-stable](#melpa-stable)
        - [Melpa (~snapshot)](#melpa-snapshot)
    - [Install](#install)
        - [Load org-trello](#load-org-trello)
        - [Alternative](#alternative)
            - [Git](#git)
            - [Tar](#tar)

<!-- tocify-markdown end -->
To install **org-trello** in your emacs, you need a few steps.
# Packages
## Sources
If not already configured, you need to prepare emacs to work with marmalade or melpa.
For this, you need to install a snippet of code in your emacs configuration file.
### Marmalade (recommended)
### Melpa-stable
### Melpa (~snapshot)
## Install
### Load org-trello
### Alternative
#### Git
#### Tar
"
                 (tocify-markdown-with-temp-buffer-and-return-buffer-content
                  "some foo bar before

<!-- tocify-markdown start - Don't edit this section. Run M-x tocify-markdown-refresh-toc -->
**Table of Contents**

    - [Melpa (~snapshot)](#melpa-snapshot)
    - [Install](#install)
        - [Load org-trello](#load-org-trello)
        - [Alternative](#alternative)
            - [Git](#git)
            - [Tar](#tar)

<!-- tocify-markdown end -->
To install **org-trello** in your emacs, you need a few steps.
# Packages
## Sources
If not already configured, you need to prepare emacs to work with marmalade or melpa.
For this, you need to install a snippet of code in your emacs configuration file.
### Marmalade (recommended)
### Melpa-stable
### Melpa (~snapshot)
## Install
### Load org-trello
### Alternative
#### Git
#### Tar
"
                  (tocify-markdown-generate-toc)))))

(ert-deftest test-tocify-markdown-generate-or-refresh-toc--with-existing-toc ()
  (setq tocify-markdown-indentation-space 4)
  ;; Update an existing TOC
  (should (equal "some foo bar before

<!-- tocify-markdown start - Don't edit this section. Run M-x tocify-markdown-refresh-toc -->
**Table of Contents**

- [Packages](#packages)
    - [Sources](#sources)
        - [Marmalade (recommended)](#marmalade-recommended)
        - [Melpa-stable](#melpa-stable)
        - [Melpa (~snapshot)](#melpa-snapshot)
    - [Install](#install)
        - [Load org-trello](#load-org-trello)
        - [Alternative](#alternative)
            - [Git](#git)
            - [Tar](#tar)

<!-- tocify-markdown end -->
To install **org-trello** in your emacs, you need a few steps.
# Packages
## Sources
If not already configured, you need to prepare emacs to work with marmalade or melpa.
For this, you need to install a snippet of code in your emacs configuration file.
### Marmalade (recommended)
### Melpa-stable
### Melpa (~snapshot)
## Install
### Load org-trello
### Alternative
#### Git
#### Tar
"
                 (tocify-markdown-with-temp-buffer-and-return-buffer-content
                  "some foo bar before

<!-- tocify-markdown start - Don't edit this section. Run M-x tocify-markdown-refresh-toc -->
**Table of Contents**

    - [Melpa (~snapshot)](#melpa-snapshot)
    - [Install](#install)
        - [Load org-trello](#load-org-trello)
        - [Alternative](#alternative)
            - [Git](#git)
            - [Tar](#tar)

<!-- tocify-markdown end -->
To install **org-trello** in your emacs, you need a few steps.
# Packages
## Sources
If not already configured, you need to prepare emacs to work with marmalade or melpa.
For this, you need to install a snippet of code in your emacs configuration file.
### Marmalade (recommended)
### Melpa-stable
### Melpa (~snapshot)
## Install
### Load org-trello
### Alternative
#### Git
#### Tar
"
                  (tocify-markdown-generate-or-refresh-toc)))))

(ert-deftest test-tocify-markdown-generate-or-refresh-toc--without-existing-toc ()
  (setq tocify-markdown-indentation-space 4)
  (should (equal "<!-- tocify-markdown start - Don't edit this section. Run M-x tocify-markdown-refresh-toc -->
**Table of Contents**

- [something](#something)
    - [Sources](#sources)
        - [Marmalade (recommended)](#marmalade-recommended)
        - [Melpa-stable](#melpa-stable)
        - [Melpa (~snapshot)](#melpa-snapshot)
        - [[配置 SPF Sender Policy Framework 记录]](#配置-spf-sender-policy-framework-记录)
    - [Install](#install)
        - [Load org-trello](#load-org-trello)
        - [Alternative](#alternative)
            - [Git](#git)
            - [Tar](#tar)

<!-- tocify-markdown end -->
To install **org-trello** in your emacs, you need a few steps.
# something
## Sources
If not already configured, you need to prepare emacs to work with marmalade or melpa.
For this, you need to install a snippet of code in your emacs configuration file.
### Marmalade (recommended)
### Melpa-stable
### Melpa (~snapshot)
### [配置 SPF Sender Policy Framework 记录]
## Install
### Load org-trello
### Alternative
#### Git
#### Tar
"
                 (tocify-markdown-with-temp-buffer-and-return-buffer-content
                  "To install **org-trello** in your emacs, you need a few steps.
# something
## Sources
If not already configured, you need to prepare emacs to work with marmalade or melpa.
For this, you need to install a snippet of code in your emacs configuration file.
### Marmalade (recommended)
### Melpa-stable
### Melpa (~snapshot)
### [配置 SPF Sender Policy Framework 记录]
## Install
### Load org-trello
### Alternative
#### Git
#### Tar
"
                  (tocify-markdown-generate-or-refresh-toc)))))

(ert-deftest test-tocify-markdown--refresh-toc--with-existing-toc ()
  (setq tocify-markdown-indentation-space 4)
  ;; Update an existing TOC
  (should (equal "some foo bar before

<!-- tocify-markdown start - Don't edit this section. Run M-x tocify-markdown-refresh-toc -->
**Table of Contents**

- [Packages](#packages)
    - [Sources](#sources)
        - [Marmalade (recommended)](#marmalade-recommended)
        - [Melpa-stable](#melpa-stable)
        - [Melpa (~snapshot)](#melpa-snapshot)
    - [Install](#install)
        - [Load org-trello](#load-org-trello)
        - [Alternative](#alternative)
            - [Git](#git)
            - [Tar](#tar)

<!-- tocify-markdown end -->
To install **org-trello** in your emacs, you need a few steps.
# Packages
## Sources
If not already configured, you need to prepare emacs to work with marmalade or melpa.
For this, you need to install a snippet of code in your emacs configuration file.
### Marmalade (recommended)
### Melpa-stable
### Melpa (~snapshot)
## Install
### Load org-trello
### Alternative
#### Git
#### Tar
"
                 (tocify-markdown-with-temp-buffer-and-return-buffer-content
                  "some foo bar before

<!-- tocify-markdown start - Don't edit this section. Run M-x tocify-markdown-refresh-toc -->
**Table of Contents**

    - [Melpa (~snapshot)](#melpa-snapshot)
    - [Install](#install)
        - [Load org-trello](#load-org-trello)
        - [Alternative](#alternative)
            - [Git](#git)
            - [Tar](#tar)

<!-- tocify-markdown end -->
To install **org-trello** in your emacs, you need a few steps.
# Packages
## Sources
If not already configured, you need to prepare emacs to work with marmalade or melpa.
For this, you need to install a snippet of code in your emacs configuration file.
### Marmalade (recommended)
### Melpa-stable
### Melpa (~snapshot)
## Install
### Load org-trello
### Alternative
#### Git
#### Tar
"
                  (tocify-markdown-refresh-toc)))))

(ert-deftest test-tocify-markdown-refresh-toc--without-existing-toc ()
  (setq tocify-markdown-indentation-space 4)
  (should (equal "To install **org-trello** in your emacs, you need a few steps.
# something
## Sources
If not already configured, you need to prepare emacs to work with marmalade or melpa.
For this, you need to install a snippet of code in your emacs configuration file.
### Marmalade (recommended)
### Melpa-stable
### Melpa (~snapshot)
### [配置 SPF Sender Policy Framework 记录]
## Install
### Load org-trello
### Alternative
#### Git
#### Tar
"
                 (tocify-markdown-with-temp-buffer-and-return-buffer-content
                  "To install **org-trello** in your emacs, you need a few steps.
# something
## Sources
If not already configured, you need to prepare emacs to work with marmalade or melpa.
For this, you need to install a snippet of code in your emacs configuration file.
### Marmalade (recommended)
### Melpa-stable
### Melpa (~snapshot)
### [配置 SPF Sender Policy Framework 记录]
## Install
### Load org-trello
### Alternative
#### Git
#### Tar
"
                  (tocify-markdown-refresh-toc)))))

(ert-deftest test-tocify-markdown-delete-toc ()
  (setq tocify-markdown-indentation-space 4)
  (should (equal "To install **org-trello** in your emacs, you need a few steps.
# Packages
## Sources
If not already configured, you need to prepare emacs to work with marmalade or melpa.
For this, you need to install a snippet of code in your emacs configuration file.
### Marmalade (recommended)
### Melpa-stable
### Melpa (~snapshot)
## Install
### Load org-trello
### Alternative
#### Git
#### Tar
"
                 (tocify-markdown-with-temp-buffer-and-return-buffer-content
                   "<!-- tocify-markdown start - Don't edit this section. Run M-x tocify-markdown-refresh-toc -->
**Table of Contents**

    - [Melpa (~snapshot)](#melpa-snapshot)
    - [Install](#install)
        - [Load org-trello](#load-org-trello)
        - [Alternative](#alternative)
            - [Git](#git)
            - [Tar](#tar)

<!-- tocify-markdown end -->
To install **org-trello** in your emacs, you need a few steps.
# Packages
## Sources
If not already configured, you need to prepare emacs to work with marmalade or melpa.
For this, you need to install a snippet of code in your emacs configuration file.
### Marmalade (recommended)
### Melpa-stable
### Melpa (~snapshot)
## Install
### Load org-trello
### Alternative
#### Git
#### Tar
"
                  (tocify-markdown-delete-toc)))))

(ert-deftest test-tocify-markdown-log-msg ()
  (setq tocify-markdown-indentation-space 4)
  (should (string= "tocify-markdown - hello dude"
                   (mocker-let ((message (str &rest args)
                                         ((:input '("tocify-markdown - hello %s" "dude") :output "tocify-markdown - hello dude"))))
                     (tocify-markdown-log-msg '("hello %s" "dude"))))))

(ert-deftest test-tocify-markdown--bug-report ()
  (setq tocify-markdown-indentation-space 4)
  (should (string=
           "Please:
- Describe your problem with clarity and conciceness (cf. https://www.gnu.org/software/emacs/manual/html_node/emacs/Understanding-Bug-Reporting.html)
- Explicit your installation choice (melpa, marmalade, el-get, tarball, git clone...).
- Report the following message trace inside your issue.

System information:
- system-type: system-type
- locale-coding-system: locale-coding-system
- emacs-version: emacs-version
- markdown-mode path: /path/to/markdown-mode
- tocify-markdown version: tocify-markdown-version
- tocify-markdown path: /path/to/tocify-markdown"
           (let ((system-type "system-type")
                 (locale-coding-system "locale-coding-system")
                 (tocify-markdown--toc-version "tocify-markdown-version")
                 (request-backend "curl"))
             (mocker-let ((emacs-version ()
                                         ((:input nil :output "emacs-version")))
                          (find-library-name (lib)
                                             ((:input '("markdown-mode") :output "/path/to/markdown-mode")
                                              (:input '("tocify-markdown") :output "/path/to/tocify-markdown"))))
               (tocify-markdown--bug-report))))))

(ert-deftest test-tocify-markdown-bug-report ()
  (setq tocify-markdown-indentation-space 4)
  (should (equal :res
                 (mocker-let ((browse-url (url)
                                          ((:input '("https://github.com/ardumont/tocify-markdown/issues/new")
                                                   :output :opened)))
                              (tocify-markdown--bug-report ()
                                                        ((:input nil :output :message)))
                              (tocify-markdown-log-msg (args)
                                                    ((:input '((:message)) :output :res))))
                   (tocify-markdown-bug-report 'browse))))
  (should (equal :res2
                 (mocker-let ((tocify-markdown--bug-report ()
                                                        ((:input nil :output :message2)))
                              (tocify-markdown-log-msg (args)
                                                    ((:input '((:message2)) :output :res2))))
                   (tocify-markdown-bug-report)))))

(ert-deftest tocify-markdown--read-title-out-of-link ()
  (setq tocify-markdown-indentation-space 4)
  (should (string= "this is the title"
                   (tocify-markdown--read-title-out-of-link "  - [this is the title](#this-is-the-link)   ")))
  (should (string= "another title"
                   (tocify-markdown--read-title-out-of-link "  - [another title](#this-is-the-link)
with multiple line
should not matter "))))

(ert-deftest tocify-markdown--title-level ()
  (setq tocify-markdown-indentation-space 4)
  (should (eq 1
              (tocify-markdown--title-level "- [this is the title](#this-is-the-link)")))
  (should (eq 4
              (let ((tocify-markdown-indentation-space 4))
                (tocify-markdown--title-level "            - [this is the title](#this-is-the-link)"))))
  (should (eq 2
              (let ((tocify-markdown-indentation-space 2))
                (tocify-markdown--title-level "  - [another title](#this-is-the-link)
with multiple line
should not matter "))))
  (should (eq 2
              (let ((tocify-markdown-indentation-space 3))
                (tocify-markdown--title-level "   - [another title](#this-is-the-link)
with multiple line
should not matter "))))
  ;; no - as prefix so considered not a title
  (should-not (tocify-markdown--title-level "[this is the title](#this-is-the-link)"))
  ;; prefixed with a dash but misaligned, title should be indented with a
  ;; multiple of `tocify-markdown-indentation-space` blank spaces
  (should-not (tocify-markdown--title-level " - [title](#this-is-the-link)")))

(ert-deftest tocify-markdown-follow-link-at-point()
  (setq tocify-markdown-indentation-space 4)
  "Follow a correct toc link should follow to the title"
  (should (string= "## Sources"
                   (with-temp-buffer
                     (insert "- [some markdown page title](#some-markdown-page-title)
- [main title](#main-title)
    - [Sources](#sources)
        - [Marmalade (recommended)](#marmalade-recommended)

# main title
## Sources
### marmalade
...
")
                     (search-backward "- [Sources]")
                     (call-interactively 'tocify-markdown-follow-link-at-point)
                     (buffer-substring-no-properties (point-at-bol) (point-at-eol))))))

(ert-deftest tocify-markdown-follow-link-at-point-failures()
  (setq tocify-markdown-indentation-space 4)
  "Follow a misindented toc link should do nothing"
  (should
   ;; not move
   (string= "   - [Sources](#sources)  <- misindented 3 instead of 4 here"
            (with-temp-buffer
              (insert "- [some markdown page title](#some-markdown-page-title)
- [main title](#main-title)
   - [Sources](#sources)  <- misindented 3 instead of 4 here

# main title
## Sources
...
")
              (search-backward "- [Sources]")
              (call-interactively 'tocify-markdown-follow-link-at-point)
              (buffer-substring-no-properties (point-at-bol) (point-at-eol)))))

  (should
   ;; not move as well because
   (string= "not a title"
            (with-temp-buffer
              (insert "- [some markdown page title](#some-markdown-page-title)
- [main title](#main-title)
   - [Sources](#sources)
        - [Marmalade (recommended)](#marmalade-recommended)

# main title
## Sources
### marmalade
not a title
...
")
              (search-backward "not a title")
              (call-interactively 'tocify-markdown-follow-link-at-point)
              (buffer-substring-no-properties (point-at-bol) (point-at-eol))))))

(provide 'tocify-markdown-tests)
;;; tocify-markdown-tests.el ends here
