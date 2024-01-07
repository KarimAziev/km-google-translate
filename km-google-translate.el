;;; km-google-translate.el --- Configure google-translate -*- lexical-binding: t -*-

;; Copyright (C) 2024 Karim Aziiev <karim.aziiev@gmail.com>

;; Author: Karim Aziiev <karim.aziiev@gmail.com>
;; URL: https://github.com/KarimAziev/km-google-translate
;; Version: 0.1.0
;; Keywords: tools local
;; Package-Requires: ((emacs "25.1") (google-translate "0.12.0"))
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; The `km-google-translate.el' package extends the Emacs interface to Google
;; Translate by introducing customization, automation, and enhanced UI to
;; streamline the translation workflow.

;; Noteworthy features of `km-google-translate' include:
;; - Automatic detection and toggling of translation direction based on
;;   user-configured rules and patterns.
;; - Interactive setup of default source and target languages on initial use.
;; - Enhanced user interface that accommodates both short and long text
;;   translations through popups or echo area output.
;; - Tweaked translation result display to improve readability.
;; - Additional key bindings for an improved results buffer experience.

;; Installation:

;; This package requires an existing Emacs installation (version 25.1 or newer).
;; Add the following to your Emacs configuration and ensure that the `google-translate'
;; package is also installed and loaded:

;; (require 'km-google-translate)

;; Usage:

;; Once installed, call `M-x km-google-translate' to begin translating text.

;; Customize the package's behavior by setting `km-google-translate-auto-switch-config'
;; according to your language switching preferences.


;;; Code:

(require 'google-translate)
(require 'google-translate-smooth-ui)
(require 'google-translate-default-ui)

(defvar km-google-translate-config-lang-options
  (mapcar
   (pcase-lambda (`(,tag . ,lang))
     `(const
       :tag
       ,tag
       :value
       ,lang))
   google-translate-supported-languages-alist))



(defcustom km-google-translate-auto-switch-config '(("en" "ru"
                                                     ("[а-я]"))
                                                    ("ru" "en"
                                                     (not "[а-я]")
                                                     ("[a-z]"))
                                                    ("en" "uk"
                                                     ("[а-я]"))
                                                    ("uk" "en"
                                                     (not "[а-я]")
                                                     ("[a-z]")))
  "Configuration for auto-switching languages during translation.

A configuration for automatically switching languages based on input patterns
when using Google Translate. The configuration is a list of rules, where each
rule is a list containing the source language code, the target language code,
and a list of conditions.

Each rule is applied when the source language matches the current language being
translated from. The conditions are regular expressions that the input text must
match for the rule to trigger a switch to the target language.

The format for each rule in the list is as follows:

- The first element is a string representing the source language code.
- The second element is a string representing the target language code.
- The third element is a list of conditions, where each condition can be:
  - A string containing a regular expression that the input must match.
  - A list starting with the symbol `not' followed by a string containing a
regular expression that the input must not match.

When the input text matches the conditions of a rule, the translation direction
is automatically switched to the target language specified in that rule."
  :group 'km-google-translate
  :type `(alist
          :key-type (choice
                     :tag "When current language "
                     ,@km-google-translate-config-lang-options)
          :value-type
          (group
           (choice
            :tag "Should switch to "
            ,@km-google-translate-config-lang-options)
           (repeat
            :inline t
            (group
             :format "If input %v"
             (set
              :format "%v"
              :inline t
              (const
               :format "%t\n"
               :tag "not " not))
             (regexp :tag "matches"))))))

(defun km-google-translate-sync-directions-alist (value)
  "Synchronize VALUE in `google-translate-translation-directions-alist'.

Argument VALUE is a list where each element is a cons cell (SOURCE . TARGET)
representing a translation direction."
  (dolist (it value)
    (let* ((source (car it))
           (target (cadr it)))
      (add-to-list 'google-translate-translation-directions-alist
                   (cons source
                         target))
      (add-to-list 'google-translate-translation-directions-alist
                   (cons target
                         source)))))

(defun km-google-translate-prompt-custom-variable (var value &optional comment)
  "Prompt to save VAR with VALUE, or set default.

Argument VAR is the custom variable to set.

Argument VALUE is the new value to assign to VAR.

Optional argument COMMENT is a string to describe the change in the custom file."
  (if (and (not noninteractive)
           (yes-or-no-p (format "Save %s with value: %s?" var value)))
      (customize-save-variable var value
                               comment)
    (set-default var value)))


(defun km-google-translate-setup-variables ()
  "Set up Google Translate variables and directions."
  (when (not noninteractive)
    (unless google-translate-default-source-language
      (km-google-translate-prompt-custom-variable
       'google-translate-default-source-language
       (cdr
        (assoc-string
         (google-translate-completing-read
          "Default source language: "
          (google-translate-supported-languages)
          "English")
         google-translate-supported-languages-alist))))
    (unless google-translate-default-target-language
      (km-google-translate-prompt-custom-variable
       'google-translate-default-target-language
       (google-translate-read-target-language)))
    (add-to-list 'google-translate-translation-directions-alist
                 (cons google-translate-default-source-language
                       google-translate-default-target-language))
    (add-to-list 'google-translate-translation-directions-alist
                 (cons google-translate-default-target-language
                       google-translate-default-source-language))))


(defun km-google-translate-auto-switch-p (rules str)
  "Toggle translation based on matching RULES in STR.

Argument RULES is a list of rules, each rule being a list where the first
element can be the symbol `not' and the remaining elements are strings
representing regular expressions.

Argument STR is the string to be checked against the rules."
  (not (catch 'found
         (dolist (ruleset rules)
           (unless
               (when-let ((re (seq-find #'stringp ruleset)))
                 (if (eq 'not (car ruleset))
                     (not (string-match-p re str))
                   (string-match-p re str)))
             (throw 'found t))))))

(defun km-google-translate--detect-lang-from-text (text)
  "Detect language from TEXT based on rules.

Argument TEXT is a string to be checked against translation rules."
  (seq-filter (pcase-lambda (`(,_s ,_target . ,rules))
                (km-google-translate-auto-switch-p
                 rules
                 text))
              km-google-translate-auto-switch-config))



(defun km-google-translate-define-lang (&rest _)
  "Set translation direction based on input text."
  (when (minibufferp)
    (let ((word (minibuffer-contents-no-properties))
          (source-lang
           (google-translate--current-direction-source-language)))
      (when-let* ((rule-set
                   (seq-find
                    (lambda (it)
                      (and
                       (equal source-lang (car it))
                       (assoc (cadr it)
                              google-translate-translation-directions-alist)))
                    (km-google-translate--detect-lang-from-text word)))
                  (target-lang (cadr rule-set))
                  (direction
                   (seq-position google-translate-translation-directions-alist
                                 (cons
                                  target-lang
                                  source-lang))))
        (when direction
          (setq google-translate-current-translation-direction direction)
          (setq google-translate-translation-direction-query
                (minibuffer-contents))
          (setq google-translate-try-other-direction t)
          (exit-minibuffer))))))



(defun km-google-translate-translate (orig-fun source-language target-language
                                               text &optional _output-destination)
  "Translate TEXT, using popup for short or echo area for long inputs.

Argument ORIG-FUN is the original function to be called with the translation
parameters.

Argument SOURCE-LANGUAGE is the language code of the original TEXT to be
translated.

Argument TARGET-LANGUAGE is the language code of the translation target.

Argument TEXT is the string to be translated.

Optional argument _OUTPUT-DESTINATION is ignored and has no effect on the
function's behavior."
  (if (> (length text) 600)
      (funcall orig-fun source-language target-language text 'echo-area)
    (funcall orig-fun source-language target-language text 'popup)))

(defvar km-google-translate-result-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") #'quit-window)
    (define-key map (kbd "C-x 0") #'kill-this-buffer)
    map)
  "Keymap for Google Translate result buffer commands.")

(defun km-google-translate-reverso-results-buffer (buffer content)
  "Display translation results in a buffer.

Argument BUFFER is the name of the buffer to display the results.

Argument CONTENT is the text content to be inserted into the results buffer."
  (let ((buffer (get-buffer-create buffer)))
    (with-current-buffer buffer
      (with-current-buffer-window
          buffer
          (cons (or 'display-buffer-in-direction)
                '((window-height . window-preserve-size)))
          (lambda (window _value)
            (with-selected-window window
              (setq buffer-read-only t)
              (let ((inhibit-read-only t))
                (erase-buffer)
                (progn  (save-excursion
                          (insert content))
                        (use-local-map
                         (let ((map km-google-translate-result-mode-map))
                           (if buffer-read-only
                               (define-key map (kbd "q")
                                           #'kill-this-buffer)
                             (define-key map (kbd "q")
                                         #'self-insert-command))
                           (add-hook
                            'read-only-mode-hook
                            (lambda ()
                              (if buffer-read-only
                                  (define-key map (kbd "q")
                                              #'kill-this-buffer)
                                (define-key map (kbd "q")
                                            #'self-insert-command)))
                            t)
                           (make-composed-keymap map (current-local-map)))))))))
      (when (fboundp 'visual-line-mode)
        (visual-line-mode 1))
      (setq header-line-format (or header-line-format "*Google Translation*"))
      (unless (active-minibuffer-window)
        (select-window (get-buffer-window buffer))))))

(defun km-google-translate-popup-output-translation (gtos)
  "Display translation results in a popup buffer.

Argument GTOS is a Google Translate Output Structure containing the translation
data."
  (let* ((str
          (catch 'suggestion
            (with-temp-buffer
              (google-translate-insert-translation gtos)
              (goto-char (point-min))
              (if
                  (re-search-forward "Did you mean:\s+" nil t 1)
                  (let ((start (point)))
                    (throw
                     'suggestion
                     (list
                      (google-translate--current-direction-source-language)
                      (google-translate--current-direction-target-language)
                      (buffer-substring-no-properties start (point-max)))))
                (while (re-search-forward "[0-9]+\\." nil t 1)
                  (save-excursion
                    (skip-chars-backward "0-9.")
                    (newline-and-indent)))
                (buffer-substring (point-min)
                                  (point-max)))))))
    (cond ((and (listp str)
                (stringp (nth 2 str)))
           (apply #'google-translate-translate str))
          ((and (stringp str)
                (not (string-empty-p str)))
           (km-google-translate-reverso-results-buffer "*google-translate*"
                                                       str)))))



(defun km-google-translate-json-suggestion (json)
  "Retrieve from suggestion from JSON returned from `google-translate-request'.

This function does matter when translating misspelled word. So instead of
translation it is possible to get suggestion."
  (let ((info (aref json 7)))
    (if (and info (> (length info) 0))
        (aref info 1)
      nil)))

(defun km-google-translate--get-b-d1 ()
  "Fix translation attempt is failing with the error:
google-translate--search-tkk: Failed to search TKK"
  ;; https://github.com/atykhonov/google-translate/issues/52#issuecomment-423870290
  ;; TKK='427110.1469889687'
  (list 427110 1469889687))

;;;###autoload
(defun km-google-translate ()
  "Translate text using Google Translate."
  (interactive)
  (km-google-translate-setup-variables)
  (unwind-protect
      (minibuffer-with-setup-hook
          (lambda ()
            (add-hook 'post-command-hook #'km-google-translate-define-lang))
        (google-translate-smooth-translate))
    (remove-hook 'post-command-hook #'km-google-translate-define-lang)))


(advice-add 'google-translate-translate :around
            #'km-google-translate-translate)

(advice-add 'google-translate-popup-output-translation
            :override
            #'km-google-translate-popup-output-translation)

(advice-add 'google-translate-json-suggestion
            :override #'km-google-translate-json-suggestion)

(advice-add 'google-translate--get-b-d1
            :override #'km-google-translate--get-b-d1)

(provide 'km-google-translate)
;;; km-google-translate.el ends here
