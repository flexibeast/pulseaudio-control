;;; pulsectl.el --- Use `pactl' to manage PulseAudio volumes.

;; Copyright (C) 2017  Alexis <flexibeast@gmail.com>

;; Author: Alexis <flexibeast@gmail.com>
;; Maintainer: Alexis <flexibeast@gmail.com>
;; Created: 2017-08-23
;; URL: https://github.com/flexibeast/pulsectl
;; Keywords: multimedia, hardware, sound, PulseAudio
;; Version: 0.1
;; Package-Version: 0.1

;;
;; This file is NOT part of GNU Emacs.
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;

;;; Commentary:

;; `pulsectl' controls PulseAudio volumes from Emacs, via `pactl`.

;; ## Table of Contents

;; - [Installation](#installation)
;; - [Usage](#usage)
;; - [Issues](#issues)
;; - [License](#license)

;; ## Installation

;; Install [pulsectl from MELPA](http://melpa.org/#/pulsectl), or put `pulsectl.el` in your load-path and do a `(require 'pulsectl)'.

;; ## Usage

;; Use C-x / to access the `pulsectl' keymap.  The default keybindings in that keymap are:

;; * + : Increase the volume of the currently-selected sink by `pulsectl-volume-step' (`pulsectl-increase-volume').

;; * - : Decrease the volume of the currently-selected sink by `pulsectl-volume-step' (`pulsectl-decrease-volume').

;; * v : Directly specify the volume of the currently-selected sink (`pulsectl-set-volume').  The value can be:

;;   * a percentage, e.g. '10%';
;;   * in decibels, e.g. '2dB';
;;   * a linear factor, e.g. '0.9' or '1.1'.

;; * m : Toggle muting of the currently-selected sink (`pulsectl-toggle-current-sink-mute').

;; * x : Toggle muting of a sink, specified by index (`pulsectl-toggle-sink-mute-by-index').

;; * e : Toggle muting of a sink, specified by name (`pulsectl-toggle-sink-mute-by-name').

;; * i : Select a sink to be the current sink, specified by index (`pulsectl-select-sink-by-index').

;; * n : Select a sink to be the current sink, specified by name (`pulsectl-select-sink-by-name').

;; Customisation options, including `pulsectl-volume-step', are available via the `pulsectl' customize-group.

;; ## Issues / bugs

;; If you discover an issue or bug in `pulsectl' not already noted:

;; * as a TODO item, or

;; * in [the project's "Issues" section on GitHub](https://github.com/flexibeast/pulsectl/issues),

;; please create a new issue with as much detail as possible, including:

;; * which version of Emacs you're running on which operating system, and

;; * how you installed `pulsectl'.

;; ## License

;; [GNU General Public License version 3](http://www.gnu.org/licenses/gpl.html), or (at your option) any later version.

;;; Code:


;; Customisable variables.

(defgroup pulsectl nil
  "Control PulseAudio volumes via `pactl'."
  :group 'external)

(defcustom pulsectl-default-sink "0"
  "Default Pulse sink index to act on."
  :type 'string
  :group 'pulsectl)

(defcustom pulsectl-pactl-path "/usr/bin/pactl"
  "Absolute path of `pactl' executable."
  :type '(file :must-match t)
  :group 'pulsectl)

(defcustom pulsectl-volume-step "10%"
  "Step to use when increasing or decreasing volume.

The value can be:

* a percentage, e.g. '10%';
* in decibels, e.g. '2dB';
* a linear factor, e.g. '0.9' or '1.1'."
  :type 'string
  :group 'pulsectl)


;; Internal variables.

(defvar pulsectl--current-sink pulsectl-default-sink
  "String containing index of currently-selected Pulse sink.")


;; Internal functions.

(defun pulsectl--get-sinks ()
  "Internal function; get a list of Pulse sinks via `pactl'."
  (let ((bfr (generate-new-buffer " *pactl-output*"))
        (fields-re "^\\(\\S-+\\)\\s-+\\(\\S-+\\)")
        (sinks '()))
    (call-process-shell-command (concat pulsectl-pactl-path " list short sinks") nil bfr)
    (with-current-buffer bfr
      (goto-char (point-min))
      (while (re-search-forward fields-re nil t)
        (let ((number (match-string 1))
              (name (match-string 2)))
          (setq sinks (append sinks (list `(,number . ,name))))))
      sinks)))


;; User-facing functions.

;;;###autoload
(defun pulsectl-decrease-volume ()
  "Decrease volume of currently-selected Pulse sink.

Amount of decrease is specified by `pulsectl-volume-step'."
  (interactive)
  (call-process-shell-command (concat pulsectl-pactl-path
                                      " set-sink-volume "
                                      pulsectl--current-sink
                                      " -"
                                      pulsectl-volume-step)))

;;;###autoload
(defun pulsectl-increase-volume ()
  "Increase volume of currently-selected Pulse sink.

Amount of increase is specified by `pulsectl-volume-step'."
  (interactive)
  (call-process-shell-command (concat pulsectl-pactl-path
                                      " set-sink-volume "
                                      pulsectl--current-sink
                                      " +"
                                      pulsectl-volume-step)))

;;;###autoload
(defun pulsectl-select-sink-by-index (sink)
  "Select which Pulse sink to act on, by numeric index.

Accepts number as prefix argument.

Argument SINK is the number provided by the user."
  (interactive "NSink index: ")
  (let ((sink (number-to-string sink))
        (valid-sinks (mapcar 'car (pulsectl--get-sinks))))
    (if (member sink valid-sinks)
        (progn
          ;;
          ;; NOTE:
          ;;
          ;; The documentation for pactl(1) version 10.0-1+deb9u1
          ;; states:
          ;;
          ;;     set-default-sink SINK
          ;;         Make the specified sink (identified by its symbolic name)
          ;;         the default sink.
          ;;
          ;; However, as at 20170828, it seems to work with
          ;; a numeric index also.
          ;;
          (call-process-shell-command (concat pulsectl-pactl-path
                                              " set-default-sink "
                                              sink))
          (setq pulsectl--current-sink sink))
      (error "Invalid sink index"))))

;;;###autoload
(defun pulsectl-select-sink-by-name ()
  "Select which Pulse sink to act on, by name."
  (interactive)
  (let* ((valid-sinks (mapcar 'cdr (pulsectl--get-sinks)))
         (sink (completing-read "Sink name: " valid-sinks))) 
    (if (member sink valid-sinks)
        (progn
          (call-process-shell-command (concat pulsectl-pactl-path
                                              " set-default-sink "
                                              sink))
          (setq pulsectl--current-sink sink))
      (error "Invalid sink name"))))

;;;###autoload
(defun pulsectl-set-volume (volume)
  "Set volume of currently-selected Pulse sink.

The value can be:

* a percentage, e.g. '10%';
* in decibels, e.g. '2dB';
* a linear factor, e.g. '0.9' or '1.1'.

Argument VOLUME is the volume provided by the user." 
  (interactive "MVolume: ")
  (let ((valid-volumes-re (concat
                           "[[:digit:]]+%"
                           "\\|[[:digit:]]+dB"
                           "\\|[[:digit:]]+\.[[:digit:]]+")))
    (if (string-match valid-volumes-re volume)
        (call-process-shell-command (concat pulsectl-pactl-path
                                            " set-sink-volume "
                                            pulsectl--current-sink
                                            " " volume))
      (error "Invalid volume"))))

;;;###autoload
(defun pulsectl-toggle-current-sink-mute ()
  "Toggle muting of currently-selected Pulse sink."
  (interactive)
  (call-process-shell-command (concat pulsectl-pactl-path
                                      " set-sink-mute "
                                      pulsectl--current-sink
                                      " toggle")))

;;;###autoload
(defun pulsectl-toggle-sink-mute-by-index (sink)
  "Toggle muting of Pulse sink, specified by index.

Argument SINK is the number provided by the user."
  (interactive "NSink index: ")
  (let ((sink (number-to-string sink))
        (valid-sinks (mapcar 'car (pulsectl--get-sinks))))
    (if (member sink valid-sinks)
        (progn
          (call-process-shell-command (concat pulsectl-pactl-path
                                              " set-sink-mute "
                                              sink
                                              " toggle")))
      (error "Invalid sink index"))))

;;;###autoload
(defun pulsectl-toggle-sink-mute-by-name ()
  "Toggle muting of Pulse sink, specified by name."
  (interactive)
  (let* ((valid-sinks (mapcar 'cdr (pulsectl--get-sinks)))
         (sink (completing-read "Sink name: " valid-sinks))) 
    (if (member sink valid-sinks)
        (progn
          (call-process-shell-command (concat pulsectl-pactl-path
                                              " set-sink-mute "
                                              sink
                                              " toggle")))
      (error "Invalid sink name"))))


;; Default keymap.

(define-prefix-command 'pulsectl-map)
(global-set-key (kbd "C-x /") 'pulsectl-map)
(define-key pulsectl-map (kbd "-") 'pulsectl-decrease-volume)
(define-key pulsectl-map (kbd "+") 'pulsectl-increase-volume)
(define-key pulsectl-map (kbd "m") 'pulsectl-toggle-current-sink-mute)
(define-key pulsectl-map (kbd "x") 'pulsectl-toggle-sink-mute-by-index)
(define-key pulsectl-map (kbd "e") 'pulsectl-toggle-sink-mute-by-name)
(define-key pulsectl-map (kbd "i") 'pulsectl-select-sink-by-index)
(define-key pulsectl-map (kbd "n") 'pulsectl-select-sink-by-name)
(define-key pulsectl-map (kbd "v") 'pulsectl-set-volume)


;; --

(provide 'pulsectl)

;;; pulsectl.el ends here
