;;
;; CDDL HEADER START
;;
;; The contents of this file are subject to the terms of the Common Development
;; and Distribution License Version 1.0 (the "License").
;;
;; You can obtain a copy of the license at
;; http://www.opensource.org/licenses/CDDL-1.0.  See the License for the
;; specific language governing permissions and limitations under the License.
;;
;; When distributing Covered Code, include this CDDL HEADER in each file and
;; include the License file in a prominent location with the name LICENSE.CDDL.
;; If applicable, add the following below this CDDL HEADER, with the fields
;; enclosed by brackets "[]" replaced with your own identifying information:
;;
;; Portions Copyright (c) [yyyy] [name of copyright owner]. All rights reserved.
;;
;; CDDL HEADER END
;;

;;
;; Copyright (c) 2013--2020, Regents of the University of Minnesota.
;; All rights reserved.
;;
;; Contributors:
;;    Ryan S. Elliott
;;

;;
;; Release: This file is part of the kim-api.git repository.
;;


;;; kim-api-c-style.el --- kim-api's C/C++ style for c-mode

;; Keywords: c, tools

;; This file derives from the google-c-style.el file.


;;; Commentary:

;; Provides the kim-api C/C++ coding style. You may wish to add
;; 'kim-api-set-c-style' to your 'c-mode-common-hook' after requiring this
;; file. For example:
;;
;;    (add-hook 'c-mode-common-hook 'kim-api-set-c-style)
;;
;; If you want the RETURN key to go to the next line and space over
;; to the right place, add this to your .emacs right after the load-file:
;;
;;    (add-hook 'c-mode-common-hook 'kim-api-make-newline-indent)

;;; Code:

;; For some reason 1) c-backward-syntactic-ws is a macro and 2)  under Emacs 22
;; bytecode cannot call (unexpanded) macros at run time:
(eval-when-compile (require 'cc-defs))

(defconst kim-api-c-style
  `((c-recognize-knr-p . nil)
    (c-enable-xemacs-performance-kludge-p . t) ; speed up indentation in XEmacs
    (c-basic-offset . 2)
    (indent-tabs-mode . nil)
    (c-comment-only-line-offset . 0)
    (c-hanging-braces-alist . ((defun-open before after)
                               (defun-close before after)
                               (class-open before after)
                               (class-close before after)
                               (inexpr-class-open before after)
                               (inexpr-class-close before after)
                               (namespace-open before after)
                               (inline-open before after)
                               (inline-close before after)
                               (block-open before after)
                               (block-close . c-snug-do-while)
                               (extern-lang-open before after)
                               (extern-lang-close before after)
                               (statement-case-open before after)
                               (substatement-open before after)))
    (c-hanging-colons-alist . ((case-label)
                               (label after)
                               (access-label after)
                               (member-init-intro before)
                               (inher-intro)))
    (c-hanging-semi&comma-criteria
     . (c-semi&comma-no-newlines-for-oneline-inliners
        c-semi&comma-inside-parenlist
        c-semi&comma-no-newlines-before-nonblanks))
    (c-indent-comments-syntactically-p . t)
    (comment-column . 40)
    (c-indent-comment-alist . ((other . (space . 2))))
    (c-cleanup-list . (empty-defun-braces
                       defun-close-semi
                       list-close-comma
                       scope-operator))
    (c-offsets-alist . (
                        (access-label . *)
                        (arglist-cont . 0)
                        (arglist-intro . ++)
                        (class-close . 0)
                        (class-open . 0)
                        (cpp-define-intro . +)
                        (defun-block-intro . +)
                        (func-decl-cont . 0)
                        (inclass . +)
                        (inline-close . 0)
                        (innamespace . 0)
                        (namespace-close . 0)
                        (namespace-open . 0)
                        (topmost-intro . 0)
                        (topmost-intro-cont . 0)
                        (annotation-top-cont . 0)
                        (annotation-var-cont . +)
                        (arglist-close . c-lineup-close-paren)
                        (arglist-cont-nonempty . (c-lineup-string-cont
                                                  c-lineup-arglist))
                        (block-close . 0)
                        (block-open . 0)
                        (brace-entry-open . 0)
                        (brace-list-close . 0)
                        (brace-list-entry . c-lineup-under-anchor)
                        (brace-list-intro . +)
                        (brace-list-open . 0)
                        (c . c-lineup-C-comments)
                        (case-label . +)
                        (catch-clause . 0)
                        (comment-intro . c-lineup-comment)
                        (composition-close . 0)
                        (composition-open . 0)
                        (cpp-macro . -1000)
                        (cpp-macro-cont . +)
                        (defun-close . 0)
                        (defun-open . 0)
                        (do-while-closure . 0)
                        (else-clause . 0)
                        (extern-lang-close . 0)
                        (extern-lang-open . 0)
                        (friend . 0)
                        (incomposition . +)
                        (inexpr-class . +)
                        (inexpr-statement . +)
                        (inextern-lang . 0)
                        (inher-cont . c-lineup-multi-inher)
                        (inher-intro . ++)
                        (inlambda . c-lineup-inexpr-block)
                        (inline-open . 0)
                        (inmodule . +)
                        (knr-argdecl . 0)
                        (knr-argdecl-intro . +)
                        (label . /)
                        (lambda-intro-cont . +)
                        (member-init-cont . c-lineup-multi-inher)
                        (member-init-intro . ++)
                        (module-close . 0)
                        (module-open . 0)
                        (statement . 0)
                        (statement-block-intro . +)
                        (statement-case-intro . +)
                        (statement-case-open . 0)
                        (statement-cont . (c-lineup-string-cont
                                           c-lineup-cascaded-calls
                                           c-lineup-assignments
                                           ++))
                        (stream-op . c-lineup-streamop)
                        (string . -1000)
                        (substatement . +)
                        (substatement-label . 2)
                        (substatement-open . 0)
                        (template-args-cont c-lineup-template-args +)
                        )
                     )
    )
  "kim-api C/C++ Programming Style.")

(defun kim-api-set-c-style ()
"Set the current buffer's c-style to kim-api C/C++ Programming
  Style. Meant to be added to `c-mode-common-hook'."
(interactive)
(make-local-variable 'c-tab-always-indent)
(setq c-tab-always-indent t)
(c-add-style "kim-api" kim-api-c-style t))

(defun kim-api-make-newline-indent ()
"Sets up preferred newline behavior. Not set by default. Meant
  to be added to `c-mode-common-hook'."
(interactive)
(define-key c-mode-base-map "\C-m" 'newline-and-indent)
(define-key c-mode-base-map [ret] 'newline-and-indent))

(provide 'kim-api-c-style)
;;; kim-api-c-style.el ends here
