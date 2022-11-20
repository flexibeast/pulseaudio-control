;;; pulseaudio-control.el --- Use `pactl' to manage PulseAudio volumes.  -*- lexical-binding: t -*-

;; Copyright (C) 2017-2020  Alexis <flexibeast@gmail.com>, Ellington Santos <ellingtonsantos@gmail.com>, Sergey Trofimov <sarg@sarg.org.ru>

;; Author: Alexis <flexibeast@gmail.com>
;;         Ellington Santos <ellingtonsantos@gmail.com>
;;         Sergey Trofimov <sarg@sarg.org.ru>
;; Maintainer: Alexis <flexibeast@gmail.com>
;; Created: 2017-08-23
;; URL: https://github.com/flexibeast/pulseaudio-control
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

;; `pulseaudio-control' controls PulseAudio volumes from Emacs, via `pactl`.

;; ## Table of Contents

;; - [Installation](#installation)
;; - [Usage](#usage)
;; - [Issues](#issues)
;; - [License](#license)

;; ## Installation

;; Install [pulseaudio-control from
;; MELPA](http://melpa.org/#/pulseaudio-control), or put
;; `pulseaudio-control.el` in your load-path and do a `(require
;; 'pulseaudio-control)'.

;; ## Usage

;; Initially, the `pulseaudio-control' keymap is not bound to any
;; prefix. You can call the command
;; `pulseaudio-control-default-keybindings' to use the prefix `C-x /'
;; to access the `pulseaudio-control' keymap globally; if you wish to
;; use this prefix by default, add the line:

;;     (pulseaudio-control-default-keybindings)

;; to your init file.

;; The default keybindings in the `pulseaudio-control' keymap are:

;; * `+` : Increase the volume of the currently-selected sink by
;;   `pulseaudio-control-volume-step'
;;   (`pulseaudio-control-increase-sink-volume').

;; * `=` : Increase the volume of the currently-selected source by
;;   `pulseaudio-control-volume-step'
;;   (`pulseaudio-control-increase-source-volume').

;; * `-` : Decrease the volume of the currently-selected sink by
;;   `pulseaudio-control-volume-step'
;;   (`pulseaudio-control-decrease-sink-volume').

;; * `_` : Decrease the volume of the currently-selected source by
;;   `pulseaudio-control-volume-step'
;;   (`pulseaudio-control-decrease-source-volume').

;; * `v` : Directly specify the volume of the currently-selected sink
;;   (`pulseaudio-control-set-sink-volume').  The value can be:

;;   * a percentage, e.g. '10%';
;;   * in decibels, e.g. '2dB';
;;   * a linear factor, e.g. '0.9' or '1.1'.

;; * `V` : Directly specify the volume of the currently-selected source
;;   (`pulseaudio-control-set-source-volume').  The value can be:

;;   * a percentage, e.g. '10%';
;;   * in decibels, e.g. '2dB';
;;   * a linear factor, e.g. '0.9' or '1.1'.

;; * `m` : Toggle muting of the currently-selected sink
;;   (`pulseaudio-control-toggle-current-sink-mute').

;; * `M` : Toggle muting of the currently-selected source
;;   (`pulseaudio-control-toggle-current-source-mute').

;; * `x` : Toggle muting of a sink, specified by index
;;   (`pulseaudio-control-toggle-sink-mute-by-index').

;; * `X` : Toggle muting of a source, specified by index
;;   (`pulseaudio-control-toggle-source-mute-by-index').

;; * `e` : Toggle muting of a sink, specified by name
;;   (`pulseaudio-control-toggle-sink-mute-by-name').

;; * `E` : Toggle muting of a source, specified by name
;;   (`pulseaudio-control-toggle-source-mute-by-name').

;; * `i` : Select a sink to be the current sink, specified by index
;;   (`pulseaudio-control-select-sink-by-index').

;; * `I` : Select a source to be the current sink, specified by index
;;   (`pulseaudio-control-select-source-by-index').

;; * `n` : Select a sink to be the current sink, specified by name
;;   (`pulseaudio-control-select-sink-by-name').

;; * `N` : Select a source to be the current source, specified by name
;;   (`pulseaudio-control-select-source-by-name').

;; * `d` : Display volume of the currently-selected sink
;;   (`pulseaudio-control-display-volume').

;; * `]` : Toggle use of @DEFAULT_SINK@ for volume operations
;;   (`pulseaudio-control-default-sink-mode').

;; * `[` : Toggle use of @DEFAULT_SOURCE@ for volume operations
;;   (`pulseaudio-control-default-source-mode').

;; Customisation options, including `pulseaudio-control-volume-step',
;; are available via the `pulseaudio-control' customize-group.

;; ## Issues / bugs

;; If you discover an issue or bug in `pulseaudio-control' not already noted:

;; * as a TODO item, or

;; * in [the project's "Issues" section on
;;   GitHub](https://github.com/flexibeast/pulseaudio-control/issues),

;; please create a new issue with as much detail as possible, including:

;; * which version of Emacs you're running on which operating system, and

;; * how you installed `pulseaudio-control'.

;; ## License

;; [GNU General Public License version
;; 3](http://www.gnu.org/licenses/gpl.html), or (at your option) any
;; later version.

;;; Code:

(eval-when-compile
  (require 'cl-macs))

;; Customisable variables.

(defgroup pulseaudio-control nil
  "Control PulseAudio volumes via `pactl'."
  :group 'external)

(defcustom pulseaudio-control-default-sink "0"
  "Default Pulse sink index to act on."
  :type 'string
  :group 'pulseaudio-control)

(defcustom pulseaudio-control-default-source "0"
  "Default Pulse source index to act on."
  :type 'string
  :group 'pulseaudio-control)

(defcustom pulseaudio-control-pactl-path (or (executable-find "pactl")
                                             "/usr/bin/pactl")
  "Absolute path of `pactl' executable."
  :type '(file :must-match t)
  :group 'pulseaudio-control)

(defcustom pulseaudio-control-use-default-sink nil
  "Whether to use @DEFAULT_SINK@ for volume operations."
  :type 'boolean
  :group 'pulseaudio-control)

(defcustom pulseaudio-control-use-default-source nil
   "Whether to use @DEFAULT_SOURCE@ for input volume operations."
   :type 'boolean
   :group 'pulseaudio-control)

(defcustom pulseaudio-control-volume-step "10%"
  "Step to use when increasing or decreasing volume.

The value can be:

* an integer percentage, e.g. '10%';
* an integer in decibels, e.g. '2dB';
* a linear factor, e.g. '0.9' or '1.1'.

Integer percentages and integer decibel values are
required by pactl 10.0."
  :type 'string
  :group 'pulseaudio-control)

(defcustom pulseaudio-control-volume-verbose t
  "Display volume after increase or decrease volume."
  :type 'boolean
  :group 'pulseaudio-control)


;; Internal variables.

(defvar pulseaudio-control--current-sink pulseaudio-control-default-sink
  "String containing index of currently-selected Pulse sink.")

(defvar pulseaudio-control--current-source pulseaudio-control-default-source
  "String containing index of currently-selected Pulse source.")

(defvar pulseaudio-control--volume-maximum '(("percent" . 150)
                                             ("decibels" . 10)
                                             ("raw" . 98000))
  "Alist containing reasonable defaults for maximum volume.

The values for 'decibels' and 'raw' are rough equivalents
of 150%.")

(defvar pulseaudio-control--volume-minimum-db -120
  "Number representing minimum dB value.

pactl represents '0%' volume as '-inf dB', so a non-infinite
number is required for the calculations performed by
`pulseaudio-control-increase-volume'.

'-120' is the rough dB equivalent of 1% volume.")


;; Internal functions.

(defmacro pulseaudio-control--with-thing (thing &rest body)
  "Run BODY in a context where you can invoke a local macro
 `or-source' that takes two expression branches and evaluates
 one of them based on if THING is a source or a sink."
  (let ((expanded-thing (gensym)))
    `(progn
       (let ((,expanded-thing ,thing))
         (cl-macrolet ((or-source
                        (res-source res-sink)
                        `(if (eq ,',expanded-thing 'source)
                             ,res-source
                           ,res-sink)))
           ,@body)))))

(defun pulseaudio-control--call-pactl (command)
  "Call `pactl' with COMMAND as its arguments.

  COMMAND is a single string separated by spaces,
  e.g. 'list short sinks'."
  (let ((locale (getenv "LC_ALL"))
        (args `("" nil
                ,pulseaudio-control-pactl-path
                nil t nil
                ,@(append '("--") (split-string command " ")))))
    (setenv "LC_ALL" "C")
    (apply #'call-process-region args)
    (setenv "LC_ALL" locale)))

(defun pulseaudio-control--get-default-thing (thing)
  "Get index of DEFAULT_THING."
  (pulseaudio-control--with-thing
   thing
   (let (thing-name things-list)
     (with-temp-buffer
       (pulseaudio-control--call-pactl "info")
       (goto-char (point-min))
       (search-forward (format "Default %s: " (or-source "Source" "Sink")))
       (setq beg (point))
       (move-end-of-line nil)
       (setq thing-name (buffer-substring beg (point))))
     (with-temp-buffer
       (pulseaudio-control--call-pactl (format "list short %s" (or-source "sources" "sinks")))
       (goto-char (point-min))
       (while (re-search-forward "\\([[:digit:]]+\\)\\s-+\\(\\S-+\\)" nil t)
         (setq things-list (append things-list `((,(match-string 1) . ,(match-string 2)))))))
     (car (rassoc thing-name things-list)))))

(defun pulseaudio-control--get-default-sink ()
   "Get index of DEFAULT_SINK."
   (pulseaudio-control--get-default-thing 'sink))

(defun pulseaudio-control--get-default-source ()
   "Get index of DEFAULT_SOURCE."
   (pulseaudio-control--get-default-thing 'source))

(defun pulseaudio-control--get-current-volume (thing)
  "Get volume of currently-selected THING."
  (pulseaudio-control--with-thing
   thing
   (let (beg)
     (or-source (pulseaudio-control--maybe-update-current-source)
                (pulseaudio-control--maybe-update-current-sink))
     (with-temp-buffer
       (pulseaudio-control--call-pactl (format "list %s" (or-source "sources" "sinks")))
       (goto-char (point-min))
       (search-forward (concat (or-source "Source" "Sink") " #"
                               (or-source pulseaudio-control--current-source
                                          pulseaudio-control--current-sink)))
       (search-forward "Volume:")
       (backward-word)
       (setq beg (point))
       (move-end-of-line nil)
       (buffer-substring beg (point))))))

(defun pulseaudio-control--get-current-sink-volume ()
   "Get volume of the currently-selected sink."
   (pulseaudio-control--get-current-volume 'sink))

 (defun pulseaudio-control--get-current-source-volume ()
   "Get volume of the currently-selected source."
   (pulseaudio-control--get-current-volume 'source))

(defun pulseaudio-control--get-current-mute (thing)
  "Get mute status of currently-selected THING."
  (pulseaudio-control--with-thing
   thing
   (let (beg)
     (or-source (pulseaudio-control--maybe-update-current-sink)
                (pulseaudio-control--maybe-update-current-source))
     (with-temp-buffer
       (pulseaudio-control--call-pactl
        (format "list %s" (or-source "sources" "sinks")))
       (goto-char (point-min))
       (search-forward (concat (or-source "Source" "Sink") " #"
                               (or-source pulseaudio-control--current-source
                                          pulseaudio-control--current-sink)))
       (search-forward "Mute:")
       (backward-word)
       (setq beg (point))
       (move-end-of-line nil)
       (buffer-substring beg (point))))))

(defun pulseaudio-control--get-current-sink-mute ()
   "Get the mute status of the currently-selected sink."
   (pulseaudio-control--get-current-mute 'sink))

 (defun pulseaudio-control--get-current-source-mute ()
   "Get the mute status of the currently-selected source."
   (pulseaudio-control--get-current-mute 'source))

(defun pulseaudio-control--get-things (thing)
  "Internal function; get a list of Pulse THINGs via `pactl'."
  (pulseaudio-control--with-thing
   thing
   (let ((fields-re "^\\(\\S-+\\)\\s-+\\(\\S-+\\)")
         things)
     (with-temp-buffer
       (pulseaudio-control--call-pactl
        (format "list short %s"
                (or-source "sources" "sinks")))
       (goto-char (point-min))
       (while (re-search-forward fields-re nil t)
         (let ((number (match-string 1))
               (name (match-string 2)))
           (setq things (append things (list `(,number . ,name)))))))
     things)))

(defun pulseaudio-control--get-sinks ()
   "Internal function; get a list of Pulse sinks via `pactl'."
   (pulseaudio-control--get-things 'sink))

(defun pulseaudio-control--get-sources ()
   "Internal function; get a list of Pulse sources via `pactl'."
   (pulseaudio-control--get-things 'source))

(defun pulseaudio-control--maybe-update-current-sink ()
  "If required, update value of `pulseaudio-control--current-sink'."
  (when pulseaudio-control-use-default-sink
    (setq pulseaudio-control--current-sink
          (pulseaudio-control--get-default-sink))))

(defun pulseaudio-control--maybe-update-current-source ()
   "If required, update value of `pulseaudio-control--current-source'."
   (when pulseaudio-control-use-default-source
     (setq pulseaudio-control--current-source
           (pulseaudio-control--get-default-source))))

(defun pulseaudio-control--get-sink-inputs ()
  "Get a list of Pulse sink inputs via `pactl'."
  (with-temp-buffer
    (let ((sink-inputs '())
          input-id props)

      (pulseaudio-control--call-pactl "list sink-inputs")
      (goto-char (point-min))

      (while (re-search-forward "^Sink Input #\\([[:digit:]]+\\)$" nil t)
        (setq input-id (match-string 1))
        (setq props '())

        (while
            (and
             (= (forward-line 1) 0)
             (or (and ; special case \t      balance 0.00
                  (re-search-forward "^\t\s+balance \\(.+\\)$"
                                     (line-end-position) t)
                  (push (cons "balance" (match-string 1)) props))

                 (and ; line format \tKey: value
                  (re-search-forward "^\t\\([^:]+\\): \\(.+\\)$"
                                     (line-end-position) t)
                  (push (cons (match-string 1) (match-string 2)) props)))))

        ;; Now properties in format \t\tdotted.key = "value"
        (re-search-forward "^\tProperties:$")
        (while (and
                (= (forward-line 1) 0)
                (re-search-forward "^\t\t\\([^=]+\\)\s=\s\"\\(.+\\)\"$"
                                   (line-end-position) t)
                (push (cons (match-string 1) (match-string 2)) props)))

        (push (cons input-id props) sink-inputs))
      sink-inputs)))

(defun pulseaudio-control--set-sink-input-mute (id val)
  "Set mute status for sink-input with ID to VAL.
nil or \"0\" - unmute
t or \"1\"   - mute
\"toggle\"   - toggle"
  (pulseaudio-control--call-pactl
   (concat "set-sink-input-mute " id " "
           (if (stringp val) val (if val "1" "0")))))

(defun pulseaudio-control--increase-volume (thing)
  "Increase volume of currently-selected Pulse THING.

Amount of increase is specified by `pulseaudio-control-volume-step'."
  (interactive)
  (pulseaudio-control--with-thing
   thing
   (or-source (pulseaudio-control--maybe-update-current-source)
              (pulseaudio-control--maybe-update-current-sink))
   (let* ((volume-step-unit
           (if (string-match "\\(%\\|dB\\)"
                             pulseaudio-control-volume-step)
               (match-string 1 pulseaudio-control-volume-step)
             nil))
          (volume-step
           (cond
            ((string-equal "%" volume-step-unit)
             (if (string-match "^\\([[:digit:]]+\\)%"
                               pulseaudio-control-volume-step)
                 (string-to-number
                  (match-string 1 pulseaudio-control-volume-step))
               (user-error "Invalid step spec in `pulseaudio-control-volume-step'")))
            ((string-equal "dB" volume-step-unit)
             (if (string-match "^\\([[:digit:]]+\\)dB"
                               pulseaudio-control-volume-step)
                 (string-to-number
                  (match-string 1 pulseaudio-control-volume-step))
               (user-error "Invalid step spec in `pulseaudio-control-volume-step'")))
            ((if (string-match "^\\([[:digit:]]+\\.[[:digit:]]+\\)"
                               pulseaudio-control-volume-step)
                 (string-to-number pulseaudio-control-volume-step)
               (user-error "Invalid step spec in `pulseaudio-control-volume-step'")))))
          (volume-max
           (cond
            ((string-equal "%" volume-step-unit)
             (cdr (assoc "percent" pulseaudio-control--volume-maximum)))
            ((string-equal "dB" volume-step-unit)
             (cdr (assoc "decibels" pulseaudio-control--volume-maximum)))
            (t
             (cdr (assoc "raw" pulseaudio-control--volume-maximum)))))
          (volumes-current (or-source (pulseaudio-control--get-current-source-volume)
                                      (pulseaudio-control--get-current-sink-volume)))
          (volumes-re-component
           (concat
            "\\([[:digit:]]+\\)"
            "\\s-+/\\s-+"
            "\\([[:digit:]]+\\)%"
            "\\s-+/\\s-+"
            "\\(-?\\([[:digit:]]+\\(\\.[[:digit:]]+\\)?\\)\\|-inf\\) dB"))
          (volumes-re (concat volumes-re-component
                              "[^:]+:\\s-+"
                              volumes-re-component))
          (volumes-alist
           (progn
             (string-match volumes-re volumes-current)
             `(("raw-left" . ,(string-to-number
                               (match-string 1 volumes-current)))
               ("percentage-left" . ,(string-to-number
                                      (match-string 2 volumes-current)))
               ("db-left" . ,(if (string=
                                  (match-string 3 volumes-current)
                                  "-inf")
                                 pulseaudio-control--volume-minimum-db
                               (string-to-number
                                (match-string 3 volumes-current))))
               ("raw-right" . ,(string-to-number
                                (match-string 6 volumes-current)))
               ("percentage-right" . ,(string-to-number
                                       (match-string 7 volumes-current)))
               ("db-right" . ,(if (string=
                                   (match-string 8 volumes-current)
                                   "-inf")
                                  pulseaudio-control--volume-minimum-db
                                (string-to-number
                                 (match-string 8 volumes-current))))))))
     (let ((clamp nil)
           (clamp-value nil))
       (cond
        ((string-equal "%" volume-step-unit)
         (if (or (> (+ (cdr (assoc "percentage-left" volumes-alist)) volume-step)
                    volume-max)
                 (> (+ (cdr (assoc "percentage-right" volumes-alist)) volume-step)
                    volume-max))
             (progn
               (setq clamp t)
               (setq clamp-value (concat (number-to-string volume-max) "%")))))
        ((string-equal "dB" volume-step-unit)
         (if (or (> (+ (cdr (assoc "db-left" volumes-alist)) volume-step)
                    volume-max)
                 (> (+ (cdr (assoc "db-right" volumes-alist)) volume-step)
                    volume-max))
             (progn
               (setq clamp t)
               (setq clamp-value (concat (number-to-string volume-max) "dB")))))
        (t
         (if (or (> (+ (cdr (assoc "raw-left" volumes-alist)) volume-step)
                    volume-max)
                 (> (+ (cdr (assoc "raw-right" volumes-alist)) volume-step)
                    volume-max))
             (progn
               (setq clamp t)
               (setq clamp-value (number-to-string volume-max))))))

       (if clamp

           ;; Clamp volume to value of `pulseaudio-control--volume-maximum'.

           (pulseaudio-control--call-pactl
            (or-source (concat "set-source-volume "
                               pulseaudio-control--current-source
                               " "
                               clamp-value)
                       (concat "set-sink-volume "
                               pulseaudio-control--current-sink
                               " "
                               clamp-value)))

         ;; Increase volume by `pulseaudio-control-volume-step'.
         ;;
         ;; Once the PulseAudio volume becomes "0 / 0% / -inf dB", we can't:
         ;; * increase volume by x dB units, because -inf + x = -inf;
         ;; * specify an absolute dB value of -120, because pactl interprets
         ;;   this as "decrease volume by 120dB";
         ;; * scale by a linear factor of x, because 0 * x = 0.
         ;; So in this situation, when the user is using a dB value or
         ;; linear factor to increase volume, we set the volume to an arbitrary
         ;; small non-zero raw value, which subsequent volume increases can
         ;; act upon.

         (if (and (or (not volume-step-unit) ; `volume-step-unit' is nil
                      (string= "dB" volume-step-unit))
                  (or (= 0 (cdr (assoc "raw-left" volumes-alist)))
                      (= 0 (cdr (assoc "raw-right" volumes-alist)))))
             (pulseaudio-control--call-pactl
              (or-source (concat "set-source-volume "
                                 pulseaudio-control--current-source
                                 " 100")
                         (concat "set-sink-volume "
                                 pulseaudio-control--current-sink
                                 " 100")))
           (pulseaudio-control--call-pactl
            (or-source (concat "set-source-volume "
                               pulseaudio-control--current-source
                               " +"
                               pulseaudio-control-volume-step)
                       (concat "set-sink-volume "
                               pulseaudio-control--current-sink
                               " +"
                               pulseaudio-control-volume-step))))))
     (if pulseaudio-control-volume-verbose
         (pulseaudio-control-display-volume)))))

(defun pulseaudio-control--decrease-volume (thing)
  "Decrease volume of currently-selected Pulse THING.

Amount to decrease is specified by `pulseaudio-control-volume-step'."
  (interactive)
  (pulseaudio-control--with-thing
   thing
   (or-source (pulseaudio-control--maybe-update-current-sink)
              (pulseaudio-control--maybe-update-current-source))
   (pulseaudio-control--call-pactl
    (concat (or-source (concat "set-source-volume "
                               pulseaudio-control--current-source)
                       (concat "set-sink-volume "
                               pulseaudio-control--current-sink))
            " -"
            pulseaudio-control-volume-step)))
  (if pulseaudio-control-volume-verbose
      (pulseaudio-control-display-volume)))

(defun pulseaudio-control--select-thing-by-name (thing)
   "Select which Pulse THING to act on, by name."
   (interactive)
   (pulseaudio-control--with-thing
    thing
    (let* ((valid-things (or-source (pulseaudio-control--get-sources)
                                     (pulseaudio-control--get-sinks)))
           (thing (completing-read (format "%s name: "
                                           (or-source "Source" "Sink"))
                                   (mapcar 'cdr valid-things))))
      (if (member thing (mapcar 'cdr valid-things))
          (progn
            (pulseaudio-control--call-pactl
             (concat (format "set-default-%s "
                             (or-source "source" "sink"))
                     thing))
            (or-source (setq pulseaudio-control--current-source
                              (car (rassoc thing valid-things)))
                        (setq pulseaudio-control--current-sink
                              (car (rassoc thing valid-things)))))
        (error (format "Invalid %s name" (or-source "source" "sink")))))))

(defun pulseaudio-control--set-volume (volume thing)
  "Set volume of currently-selected Pulse THING.

The value can be:

* a percentage, e.g. '10%';
* in decibels, e.g. '2dB';
* a linear factor, e.g. '0.9' or '1.1'.

Argument VOLUME is the volume provided by the user."
  (pulseaudio-control--with-thing
   thing
   (or-source (pulseaudio-control--maybe-update-current-source)
              (pulseaudio-control--maybe-update-current-sink))
   (let ((valid-volumes-re (concat
                            "[[:digit:]]+%"
                            "\\|[[:digit:]]+dB"
                            "\\|[[:digit:]]+\\.[[:digit:]]+")))
     (if (string-match valid-volumes-re volume)
         (pulseaudio-control--call-pactl
          (concat (format
                   "set-%s-volume "
                   (or-source "source" "sink"))
                  (or-source
                   pulseaudio-control--current-source
                   pulseaudio-control--current-sink)
                  " "
                  volume))
       (error "Invalid volume")))))

(defun pulseaudio-control--select-thing-by-index (index thing)
  "Select which Pulse THING to act on, by numeric index.

Accepts number as prefix argument.

Argument INDEX is the number provided by the user."
  (interactive)
  (pulseaudio-control--with-thing
   thing
   (let* ((valid-things (or-source (pulseaudio-control--get-sources)
                                   (pulseaudio-control--get-sinks))))
     (if (member index (mapcar 'car valid-things))
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
           ;; 20180701: The man page for pulse-cli-syntax in the same
           ;;           package states, for `set-default-sink':
           ;;
           ;;           "You may specify the sink (resp. source) by its index
           ;;            in the sink (resp. source) list or by its name."
           ;;
           (or-source (setq pulseaudio-control--current-source index)
                      (setq pulseaudio-control--current-sink index))
           (pulseaudio-control--call-pactl
            (format "set-default-%s %s" (or-source "source" "sink") index)))
       (error (format "Invalid %s index" (or-source "source" "sink")))))))

(defun pulseaudio-control--toggle-current-thing-mute (thing)
  "Toggle muting of currently-selected Pulse THING."
  (interactive)
  (pulseaudio-control--with-thing
   thing
   (or-source (pulseaudio-control--maybe-update-current-source)
              (pulseaudio-control--maybe-update-current-sink))
   (pulseaudio-control--call-pactl
    (concat (format "set-%s-mute " (or-source "source" "sink"))
            (or-source pulseaudio-control--current-source
                       pulseaudio-control--current-sink)
            " toggle")))
  (if pulseaudio-control-volume-verbose
      (pulseaudio-control-display-volume)))

(defun pulseaudio-control--toggle-thing-mute-by-index (thing)
  "Toggle muting of Pulse THING, specified by index."
  (pulseaudio-control--with-thing
   thing
   (let* ((thing (completing-read
                  (format "%s index: "
                          (or-source "Source" "Sink"))
                  (or-source (pulseaudio-control--get-sources)
                             (pulseaudio-control--get-sinks))))
          (valid-things (mapcar 'car (or-source
                                      (pulseaudio-control--get-sources)
                                      (pulseaudio-control--get-sinks)))))
     (if (member thing valid-things)
         (progn
           (pulseaudio-control--call-pactl
            (concat (format "set-%s-mute " (or-source "source" "sink"))
                    thing
                    " toggle")))
       (error (format "Invalid %s index" (or-source "source" "sink")))))))

(defun pulseaudio-control--toggle-thing-mute-by-name (thing)
  "Toggle muting of Pulse THING, specified by name."
  (pulseaudio-control--with-thing
   thing
   (let* ((valid-things (mapcar 'cdr (or-source (pulseaudio-control--get-sources)
                                                (pulseaudio-control--get-sinks))))
          (thing (completing-read
                  (format "%s name: " (or-source "Source" "Sink"))
                  valid-things)))
     (if (member thing valid-things)
         (progn
           (pulseaudio-control--call-pactl
            (concat (format "set-%s-mute " (or-source "source" "sink"))
                    thing
                    " toggle")))
       (error (format "Invalid %s name" (or-source "source" "sink")))))))

;; User-facing functions.

;;;###autoload
(defun pulseaudio-control-default-keybindings ()
  "Make `C-x /' the prefix for accessing pulseaudio-control bindings."
  (interactive)
  (global-set-key (kbd "C-x /") 'pulseaudio-control-map))

;;;###autoload
(defun pulseaudio-control-display-volume ()
  "Display volume of currently-selected Pulse sink."
  (interactive)
  (let ((volume (replace-regexp-in-string
                 "%" "%%"
                 (pulseaudio-control--get-current-volume)))
        (mute (pulseaudio-control--get-current-mute)))
    (message (concat volume "   |   " mute))))

;;;###autoload
(defun pulseaudio-control-increase-sink-volume ()
  "Increase the volume of the current Pulse sink."
  (interactive)
  (pulseaudio-control--increase-volume 'sink))

;;;###autoload
(defun pulseaudio-control-increase-source-volume ()
  "Increase the volume of the current Pulse source."
  (interactive)
  (pulseaudio-control--increase-volume 'source))

;;;###autoload
(defun pulseaudio-control-decrease-sink-volume ()
  "Decrease the volume of the current Pulse sink."
  (interactive)
  (pulseaudio-control--decrease-volume 'sink))

;;;###autoload
(defun pulseaudio-control-decrease-source-volume ()
  "Decrease the volume of the current Pulse sink."
  (interactive)
  (pulseaudio-control--decrease-volume 'source))

;;;###autoload
(defun pulseaudio-control-select-sink-by-index (sink)
  "Select which Pulse SINK to act on, by numeric index."
  (interactive
   (list (completing-read "Sink index: "
                          (mapcar 'car (pulseaudio-control--get-sinks)))))
  (pulseaudio-control--select-thing-by-index sink 'sink))

;;;###autoload
(defun pulseaudio-control-select-source-by-index (source)
  "Select which Pulse SOURCE to act on, by numeric index."
   (interactive
    (list (completing-read "Source index: "
                           (mapcar 'car (pulseaudio-control--get-sources)))))
   (pulseaudio-control--select-thing-by-index source 'source))

;;;###autoload
(defun pulseaudio-control-select-sink-by-name ()
  "Select which Pulse sink to act on, by name."
  (interactive)
  (pulseaudio-control--select-thing-by-name 'sink))

;;;###autoload
(defun pulseaudio-control-select-source-by-name ()
  "Select which Pulse source to act on, by name."
   (interactive)
   (pulseaudio-control--select-thing-by-name 'source))

;;;###autoload
(defun pulseaudio-control-set-sink-volume (volume)
  "Set volume of currently-selected Pulse sink.

The value can be:

* a percentage, e.g. '10%';
* in decibels, e.g. '2dB';
* a linear factor, e.g. '0.9' or '1.1'.

Argument VOLUME is the volume provided by the user."
  (interactive "MVolume: ")
  (pulseaudio-control--set-volume volume 'sink))

;;;###autoload
(defun pulseaudio-control-set-source-volume (volume)
  "Set volume of currently-selected Pulse source.

The value can be:

* a percentage, e.g. '10%';
* in decibels, e.g. '2dB';
* a linear factor, e.g. '0.9' or '1.1'.

Argument VOLUME is the volume provided by the user."
  (interactive "MVolume: ")
  (pulseaudio-control--set-volume volume 'source))

;;;###autoload
(defun pulseaudio-control-toggle-current-sink-mute ()
  "Toggle muting of currently-selected Pulse sink."
  (interactive)
  (pulseaudio-control--toggle-current-thing-mute 'sink))

;;;###autoload
(defun pulseaudio-control-toggle-current-source-mute ()
  "Toggle muting of currently-selected Pulse source."
  (interactive)
  (pulseaudio-control--toggle-current-thing-mute 'source))

;;;###autoload
(defun pulseaudio-control-toggle-sink-mute-by-index ()
  "Toggle muting of Pulse sink, specified by index."
  (interactive)
  (pulseaudio-control--toggle-thing-mute-by-index 'sink))

;;;###autoload
(defun pulseaudio-control-toggle-source-mute-by-index ()
  "Toggle muting of Pulse source, specified by index."
  (interactive)
  (pulseaudio-control--toggle-thing-mute-by-index 'source))

;;;###autoload
(defun pulseaudio-control-toggle-sink-mute-by-name ()
  "Toggle muting of Pulse sink, specified by name."
  (interactive)
  (pulseaudio-control--toggle-thing-mute-by-name 'sink))

;;;###autoload
(defun pulseaudio-control-toggle-source-mute-by-name ()
  "Toggle muting of Pulse source, specified by name."
  (interactive)
  (pulseaudio-control--toggle-thing-mute-by-name 'source))

(defun pulseaudio-control-toggle-sink-input-mute-by-index (index)
  "Toggle muting of Pulse sink-input by index."
  (interactive
   (list
    (let* ((valid-sink-inputs (pulseaudio-control--get-sink-inputs))
           (completion-choices
            (mapcar
             (lambda (el)
               (cons (concat
                      (if (string=
                           "yes"
                           (alist-get "Mute" (cdr el) nil nil #'string=)) "🔇" "🔊")
                      " "
                      (alist-get "application.name" (cdr el) nil nil #'string=)
                      " (" (alist-get "application.process.binary" (cdr el) nil nil #'string=)
                      " pid " (alist-get "application.process.id" (cdr el) nil nil #'string=) ")")
                     (car el)))
             (pulseaudio-control--get-sink-inputs)))
           (sink-input (completing-read "Sink input name: " completion-choices)))
      (cdr (assoc sink-input completion-choices)))))

  (pulseaudio-control--set-sink-input-mute index "toggle"))

;;;###autoload
(define-minor-mode pulseaudio-control-default-sink-mode
  "Set the use of @DEFAULT_SINK@ for volume operations."
  :global t :group 'pulseaudio-control
  (if pulseaudio-control-default-sink-mode
      (progn
        (setq pulseaudio-control-use-default-sink t)
        (message "Using @DEFAULT_SINK@ for volume operations"))
    (setq pulseaudio-control-use-default-sink nil)
    (message "No longer using @DEFAULT_SINK@ for volume operations")))

;;;###autoload
(define-minor-mode pulseaudio-control-default-source-mode
  "Set the use of @DEFAULT_SOURCE@ for volume operations."
  :global t :group 'pulseaudio-control
  (if pulseaudio-control-default-source-mode
      (progn
        (setq pulseaudio-control-use-default-source t)
        (message "Using @DEFAULT_SOURCE@ for volume operations"))
    (setq pulseaudio-control-use-default-source nil)
    (message "No longer using @DEFAULT_SOURCE@ for volume operations")))

;; Default keymap.

(defvar pulseaudio-control-map)
(define-prefix-command 'pulseaudio-control-map)
(define-key pulseaudio-control-map (kbd "-")
  'pulseaudio-control-decrease-sink-volume)
(define-key pulseaudio-control-map (kbd "_")
  'pulseaudio-control-decrease-source-volume)
(define-key pulseaudio-control-map (kbd "d")
  'pulseaudio-control-display-volume)
(define-key pulseaudio-control-map (kbd "+")
  'pulseaudio-control-increase-sink-volume)
(define-key pulseaudio-control-map "="
  'pulseaudio-control-increase-source-volume)
(define-key pulseaudio-control-map (kbd "m")
  'pulseaudio-control-toggle-current-sink-mute)
(define-key pulseaudio-control-map "M"
  'pulseaudio-control-toggle-current-source-mute)
(define-key pulseaudio-control-map (kbd "x")
  'pulseaudio-control-toggle-sink-mute-by-index)
(define-key pulseaudio-control-map (kbd "X")
  'pulseaudio-control-toggle-source-mute-by-index)
(define-key pulseaudio-control-map (kbd "e")
  'pulseaudio-control-toggle-sink-mute-by-name)
(define-key pulseaudio-control-map (kbd "E")
  'pulseaudio-control-toggle-source-mute-by-name)
(define-key pulseaudio-control-map (kbd "]")
  'pulseaudio-control-default-sink-mode)
(define-key pulseaudio-control-map (kbd "[")
  'pulseaudio-control-default-source-mode)
(define-key pulseaudio-control-map (kbd "i")
  'pulseaudio-control-select-sink-by-index)
(define-key pulseaudio-control-map (kbd "I")
  'pulseaudio-control-select-source-by-index)
(define-key pulseaudio-control-map (kbd "n")
  'pulseaudio-control-select-sink-by-name)
(define-key pulseaudio-control-map (kbd "N")
  'pulseaudio-control-select-source-by-name)
(define-key pulseaudio-control-map (kbd "v")
  'pulseaudio-control-set-sink-volume)
(define-key pulseaudio-control-map (kbd "V")
  'pulseaudio-control-set-source-volume)

;; --

(provide 'pulseaudio-control)

;;; pulseaudio-control.el ends here
