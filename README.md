tocify-markdown
============

[![Build Status](https://travis-ci.com/ardumont/tocify-markdown.png?branch=master)](https://travis-ci.com/ardumont/tocify-markdown) [![Coverage Status](https://coveralls.io/repos/ardumont/tocify-markdown/badge.svg?branch=master&service=github)](https://coveralls.io/github/ardumont/tocify-markdown?branch=master) [![MELPA Stable](http://stable.melpa.org/packages/tocify-markdown-badge.svg)](http://stable.melpa.org/#/tocify-markdown) [![MELPA](http://melpa.org/packages/tocify-markdown-badge.svg)](http://melpa.org/#/tocify-markdown) [![SWH](https://archive.softwareheritage.org/badge/origin/https://github.com/ardumont/tocify-markdown/)](https://archive.softwareheritage.org/browse/origin/?origin_url=https://github.com/ardumont/tocify-markdown)

<!-- tocify-markdown start - Don't edit this section. Run M-x tocify-markdown-refresh-toc -->
**Table of Contents**

- [tocify-markdown](#tocify-markdown)
- [Use](#use)
    - [Create](#create)
    - [User toc manipulation](#user-toc-manipulation)
    - [Update](#update)
    - [Create elsewhere](#create-elsewhere)
    - [Remove](#remove)
    - [Customize](#customize)
    - [Minor mode](#minor-mode)
- [Install](#install)
    - [emacs package repository](#emacs-package-repository)
        - [Setup](#setup)
            - [melpa stable](#melpa-stable)
            - [melpa](#melpa)
        - [Install](#install-1)
    - [emacs-lisp file](#emacs-lisp-file)
- [Inspiration](#inspiration)

<!-- tocify-markdown end -->

A simple mode to create TOC in a well-formed markdown file.

Note that the TOC is well-formed if the markdown is (cf. #15).

# Use

## Create

Inside a markdown file, the first time, place yourself where you want to insert the TOC:

<kbd>M-x tocify-markdown-generate-toc</kbd>

This will compute the TOC and insert it at current position.

You can also execute: <kbd>M-x tocify-markdown-generate-or-refresh-toc</kbd> to either
gnerate a TOC when none exists or refresh the currently existing one.

Here is one possible output:

```markdown
<!-- tocify-markdown start - Don't edit this section. Run M-x tocify-markdown-refresh-toc -->
**Table of Contents**

- [Use](#use)
    - [Create](#create)
    - [Update](#update)
    - [Create elsewhere](#create-elsewhere)
- [Install](#install)
    - [emacs package repository](#emacs-package-repository)
        - [Setup](#setup)
            - [melpa stable](#melpa-stable)
            - [melpa](#melpa)
            - [marmalade](#marmalade)
        - [Install](#install)
    - [emacs-lisp file](#emacs-lisp-file)
- [Inspiration](#inspiration)
```

## User toc manipulation

If the user would want to enhance the generated toc, (s)he could use
the following function tocify-markdown-user-toc-structure-manipulation-fn:

It expects as argument the toc-structure tocify-markdown uses to generate
the toc. The remaining code expects a similar structure.

Example:

``` lisp
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
```

So for example, as asked in #16, one could drop the first element:

``` lisp
(custom-set-variables '(tocify-markdown-user-toc-structure-manipulation-fn 'cdr))
```

Or drop all h1 titles... or whatever:

``` lisp
(require 'dash)
(custom-set-variables '(tocify-markdown-user-toc-structure-manipulation-fn
  (lambda (toc-structure)
  (-filter (lambda (l) (let ((index (car l)))
                    (<= 1 index)))
           toc-structure)))
```

## Update

To update the existing TOC, simply execute:
<kbd>M-x tocify-markdown-refresh-toc</kbd>

This will update the current TOC.

## Create elsewhere

To create another updated TOC elsewhere, execute <kbd>M-x
tocify-markdown-generate-toc</kbd> again, this will remove the old TOC and
insert the updated one from where you stand.

## Remove

To remove a TOC, execute <kbd>M-x tocify-markdown-delete-toc</kbd>.

## Customize

Currently, you can customize the following:

* `tocify-markdown-header-toc-start`
* `tocify-markdown-header-toc-title`
* `tocify-markdown-header-toc-end`
*  tocify-markdown-indentation-space

Customize them as following format:

``` lisp
(custom-set-variables
 '(tocify-markdown-header-toc-start "<!-- customized start-->")
 '(tocify-markdown-header-toc-title "**customized title**")
 '(tocify-markdown-header-toc-end "<!-- customized end -->")
 '(tocify-markdown-indentation-space 4))
```

## Minor mode

tocify-markdown-mode provides a minor mode with the following default binding:

```
(setq tocify-markdown-mode-map
      (let ((map (make-sparse-keymap)))
        (define-key map (kbd "C-c m .") 'tocify-markdown-follow-link-at-point)
        (define-key map (kbd "C-c m t") 'tocify-markdown-generate-or-refresh-toc)
        (define-key map (kbd "C-c m d") 'tocify-markdown-delete-toc)
        (define-key map (kbd "C-c m v") 'tocify-markdown-version)
        map))
```

To (de)activate this in an org file: /M-x tocify-markdown-mode/

You can also use emacs to setup your own bindings.

# Install

## emacs package repository

You need to add melpa or melpa-stable package repository before installing it.

### Setup

#### melpa stable

``` lisp
(require 'package)
(add-to-list 'package-archives '("melpa-stable" .
                                 "http://melpa-stable.milkbox.net/packages/"))
(package-initialize)
```

Then hit <kbd>M-x eval-buffer</kbd> to evaluate the buffer's contents.

#### melpa

``` lisp
(require 'package)
(add-to-list 'package-archives '("melpa" .
                                 "http://melpa.milkbox.net/packages/"))
(package-initialize)
```

Then hit <kbd>M-x eval-buffer</kbd> to evaluate the buffer's contents.

### Install

<kbd>M-x package-install RET tocify-markdown RET</kbd>

## emacs-lisp file

Retrieve the tocify-markdown.el https://github.com/ardumont/tocify-markdown/releases.

Then hit <kbd>M-x package-install-file RET tocify-markdown.el RET</kbd>

# Inspiration

https://github.com/thlorenz/doctoc

The problem I had with doctoc is the installation process.
I do not want to install the node tools just for this.
