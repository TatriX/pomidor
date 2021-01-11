;;; pomidor.el --- Simple and cool pomodoro timer

;; Author: TatriX <tatrics@gmail.com>
;; URL: https://github.com/TatriX/pomidor
;; Keywords: tools, time, applications, pomodoro technique
;; Version: 0.6.2
;; Package-Requires: ((emacs "24.3") (alert "1.2") (dash "2.17.0"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; pomidor is a simple and cool
;; [[http://www.pomodorotechnique.com/][pomodoro technique]] timer.

;;; Code:

(require 'cl-lib)
(require 'alert)
(require 'dash)
(require 'json)

;;; Customs
(defgroup pomidor nil
  "Customs for `pomidor'"
  :group 'productivity)

(defcustom pomidor-buffer-name "*pomidor*"
  "Name of the pomidor buffer."
  :type 'string :group 'pomidor)

(defcustom pomidor-history-buffer-name "*pomidor-history*"
  "Name of the pomidor history buffer."
  :type 'string :group 'pomidor)

(defcustom pomidor-seconds (* 25 60)
  "Time length of a Podomoro round."
  :type 'integer :group 'pomidor)

(defcustom pomidor-break-seconds (* 5 60)
  "Time length of a Podomoro break."
  :type 'integer :group 'pomidor)

(defcustom pomidor-update-interval 60
  "Interval in seconds when pomidor should run hooks and play overwork sounds."
  :type 'integer :group 'pomidor)

(defcustom pomidor-confirm-end-break t
  "If t ask for confirmation before ending a break and starting new a pomodoro."
  :type 'boolean :group 'pomidor)

;;; Vars
(defcustom pomidor-time-format "%H:%M:%S"
  "Time format for podomoro clock."
  :type 'string :group 'pomidor)

(defcustom pomidor-history-date-format "%Y-%m-%dT%H:%M:%S"
  "Date format used to save the session ended value."
  :type 'string :group 'pomidor)

(defcustom pomidor-duration-format "%H:%M:%S"
  "Time format for duration intervals."
  :type 'string :group 'pomidor)

(defconst pomidor-dir (file-name-directory (or load-file-name buffer-file-name))
  "Pomidor directory in which sounds store.")

(defcustom pomidor-sound-tick (expand-file-name (concat pomidor-dir "tick.wav"))
  "Tick sound during a pomoro run."
  :type '(file :must-match t)
  :group 'pomidor)

(defcustom pomidor-sound-tack (expand-file-name (concat pomidor-dir "tack.wav"))
  "Tick sound during a pomoro run."
  :type '(file :must-match t)
  :group 'pomidor)

(defcustom pomidor-sound-overwork (expand-file-name (concat pomidor-dir "overwork.wav"))
  "Overwork sound."
  :type '(file :must-match t)
  :group 'pomidor)

(defcustom pomidor-sound-break-over (expand-file-name (concat pomidor-dir "overwork.wav"))
  "Break over sound."
  :type '(file :must-match t)
  :group 'pomidor)

(defcustom pomidor-save-session-file (expand-file-name "pomidor-session.json" user-emacs-directory)
  "Pomidor session store file."
  :type '(file :must-match t)
  :group 'pomidor)

(defcustom pomidor-breaks-before-long 4
  "How many short breaks before the long break."
  :type 'integer :group 'pomidor)

(defcustom pomidor-long-break-seconds (* 20 60)
  "Time length of a Podomoro long break."
  :type 'integer :group 'pomidor)

;; libnotify for some reason can't display svg
(defvar pomidor-icon (concat data-directory "images/icons/hicolor/16x16/apps/emacs.png")
  "Default pomidor icon.")

(defun pomidor-default-alert-message ()
  "Default pomidor alert message if any."
  (cond
   ((and (pomidor-overwork-p) (pomidor-should-long-break-p))
    (format "Take a long break!\nOverwork: [%s]"
            (format-time-string "%H:%M:%S" (pomidor-overwork-duration) t)))
   ((pomidor-overwork-p)
    (format "Take a break!\nOverwork: [%s]"
            (format-time-string "%H:%M:%S" (pomidor-overwork-duration) t)))
   ((pomidor-break-over-notify-p)
    (format "Go back to work!\nBreak: [%s]"
            (format-time-string "%H:%M:%S" (pomidor-break-duration) t)))))

(defun pomidor-default-alert ()
  "Default pomidor alert."
  (let ((message (pomidor-default-alert-message)))
    (when message
      (alert message
             :severity 'normal
             :icon pomidor-icon
             :title "Pomidor"
             :category 'pomidor))))

(defcustom pomidor-alert #'pomidor-default-alert
  "Pomidor alert function.
To disable alerts, set to nil."
  :type '(choice (const nil)
                 function)
  :group 'pomidor)

(defun pomidor-play-sound-file-emacs (file)
  "Play FILE by starting new Emacs process."
  (if (fboundp 'play-sound-internal)
      (start-process "pomidor-play-sound-file-emacs"
                     nil
                     (car command-line-args)
                     "-Q"
                     "--batch"
                     "--eval" (format "(play-sound-file \"%s\")" file))
    (warn "This Emacs binary lacks sound support")))

(defcustom pomidor-play-sound-file #'pomidor-play-sound-file-emacs
  "Function to play sounds (preferably async).
To disable sounds, set to nil."
  :type '(choice (const nil)
                 function)
  :group 'pomidor)

(defvar pomidor-update-hook nil)

;;; Faces
(defface pomidor-time-face
  '(( t (:height 4.0)))
  "pomidor face for time"
  :group 'pomidor)

(defface pomidor-date-face
  '(( t (:height 4.0)))
  "pomidor face for date"
  :group 'pomidor)

(defface pomidor-timer-face
  '(( t (:height 5.0)))
  "pomidor face for timer"
  :group 'pomidor)

(defface pomidor-work-face
  '((t (:inherit 'success)))
  "pomidor face for work"
  :group 'pomidor)

(defface pomidor-overwork-face
  '((t (:inherit 'warning)))
  "pomidor face for overwork"
  :group 'pomidor)

(defface pomidor-break-face
  '((t (:inherit 'font-lock-keyword-face)))
  "pomidor face for break"
  :group 'pomidor)

(defface pomidor-skip-face
  '(( t (:inherit 'font-lock-comment-face)))
  "pomidor face for skip"
  :group 'pomidor)


;;; Vars
(defvar pomidor-timer nil
  "Pomidor timer.")

(defvar pomidor-global-state nil
  "Pomidor global state.")

(defvar pomidor-graph-char ?█
  "Pomidor char for displaying tubes.")

(defvar pomidor-header-separator " — "
  "Pomidor string to separate time and duration in header.")

(defvar pomidor-header-session-name-separator " : "
  "Pomidor string to separate the regular header from the session name in history mode.")

(defvar pomidor--current-history-session nil
  "Hold the current visible pomidor history snapshot.")

(defvar pomidor--count-short-breaks 0
  "Pomidor integer of how many short breaks we have before a long break.")

(defvar pomidor--system-on-hold-p nil
  "Pomidor control of hold in system.")

;;; Private

(defun pomidor--current-state ()
  "Return current state."
  (car (last pomidor-global-state)))

(defun pomidor--reset ()
  "Delete current global state."
  (setq pomidor-global-state (list (pomidor--make-state))))

(defun pomidor--make-state ()
  "Make pomidor state."
  (list :started (current-time)
        :break nil
        :stopped nil
        :snooze nil))

(defun pomidor--started (state)
  "Return started time for STATE."
  (plist-get state :started))

(defun pomidor--break (state)
  "Return break time for STATE."
  (plist-get state :break))

(defun pomidor--stopped (state)
  "Return stopped time for STATE.
It's a time when user started a new timer after this one."
  (plist-get state :stopped))

(defun pomidor--ended (state)
  "Return ended time for STATE.
It's either stopped time or current time."
  (or (pomidor--stopped state) (current-time)))

(defun pomidor--work-duration (state)
  "Return work time for STATE."
  (let* ((started (pomidor--started state))
         (ended (or (pomidor--break state) (pomidor--ended state)))
         (work (time-subtract ended started))
         (max (seconds-to-time pomidor-seconds)))
    (if (time-less-p work max)
        work
      max)))

(defun pomidor--overwork-duration (state)
  "Return overwork time for STATE or nil."
  ;; (cur - started) - (cur - break) - max
  (let* ((started (pomidor--started state))
         (break (or (pomidor--break state) (pomidor--ended state)))
         (work (pomidor--work-duration state))
         (ended (pomidor--ended state))
         (max (seconds-to-time pomidor-seconds))
         (overwork (time-subtract (time-subtract (time-subtract ended started)
                                                 (time-subtract ended break))
                                  max)))
    (when (> (time-to-seconds overwork) 0)
      overwork)))

(defun pomidor-running-p ()
  "Return t if pomidor is running right now."
  (timerp pomidor-timer))

(defun pomidor-overwork-p ()
  "Return t if current state is overwork."
  (let* ((state (pomidor--current-state))
         (overwork (pomidor--overwork-duration state)))
    (and overwork (null (pomidor--break state)))))

(defun pomidor-should-long-break-p ()
  "Return t if current state should take a long break."
  (cl-equalp pomidor--count-short-breaks (1+ pomidor-breaks-before-long)))

(defun pomidor-break-over-notify-p ()
  "Return t if current break is over and user should be notified about it.
To snooze the notification use `pomidor-break'."
  (and (pomidor-break-over-p) (not (pomidor-snooze-p))))

(defun pomidor-break-over-p ()
  "Return t if current break is over."
  (let* ((state (pomidor--current-state))
         (break (pomidor--break-duration state))
         (expected-break-seconds (if (pomidor-should-long-break-p)
                                     pomidor-long-break-seconds
                                   pomidor-break-seconds)))
    (and break (> (time-to-seconds break) expected-break-seconds))))

(defun pomidor-snooze-p ()
  "Return t if user snooze end of break alarm."
  (plist-get (pomidor--current-state) :snooze))

(defun pomidor--total-duration (state)
  "Return total time for STATE."
  (time-subtract (pomidor--ended state) (pomidor--started state)))

(defun pomidor--break-duration (state)
  "Return break time for STATE."
  (let ((break (pomidor--break state)))
    (and break (time-subtract (pomidor--ended state) break))))

(defun pomidor--format-header (time state face)
  "Return formated header for TIME with FACE."
  (let ((freezed-time (plist-get state :session-ended)))
    (concat (pomidor--with-face (concat (pomidor--format-time (or freezed-time (current-time)))
                                        pomidor-header-separator)
                                'pomidor-time-face)
            (propertize (pomidor--format-duration time)
                        'face `(:inherit (,face pomidor-timer-face)))
            (when (eq major-mode 'pomidor-history-mode)
              (pomidor--with-face (concat pomidor-header-session-name-separator
                                          (format-time-string "%Y-%m-%d" freezed-time))
                                  'pomidor-date-face)))))

(defun pomidor--header (state)
  "Return header."
  (let* ((break (pomidor--break-duration state))
         (overwork (pomidor--overwork-duration state))
         (work (pomidor--work-duration state)))
    (cond
     (break (pomidor--format-header break state 'pomidor-break-face))
     (overwork (pomidor--format-header overwork state 'pomidor-overwork-face))
     (work (pomidor--format-header work state 'pomidor-work-face)))))


(defun pomidor--format-time (time)
  "Format TIME as of `pomidor-time-format'."
  (format-time-string pomidor-time-format time))

(defun pomidor--format-duration (time)
  "Format TIME as of `pomidor-duration-format'.
TIME may be nil."
  (format-time-string pomidor-duration-format (or time (seconds-to-time 0)) t))

(defun pomidor--window-width ()
  "Return pomidor buffer width in chars."
  (window-total-width (get-buffer-window (pomidor--get-buffer-create))))

(defun pomidor--with-face (string face)
  "Retrun STRING with FACE."
  (propertize string 'font-lock-face face))

(defun pomidor--format-time-string (time face)
  "Format graph string for TIME with FACE."
  (pomidor--with-face (make-string (round (/ (time-to-seconds time)
                                     (/ (float pomidor-seconds) (/ (pomidor--window-width) 2))))
                                   pomidor-graph-char)
                      face))

(defun pomidor--graph (work overwork break)
  "Format graph based on WORK, OVERWORK and BREAK time."
  (concat
   (pomidor--format-time-string work 'pomidor-work-face)
   (let ((skip (- pomidor-seconds (time-to-seconds work))))
     (when (> skip 0)
       (pomidor--format-time-string (seconds-to-time skip) 'pomidor-skip-face)))
   (and overwork (pomidor--format-time-string overwork 'pomidor-overwork-face))
   (and break (pomidor--format-time-string break 'pomidor-break-face))))

(defun pomidor--play-sound-file (file)
  "Play FILE using `pomidor-play-sound-file' function if any."
  (when (and file (functionp pomidor-play-sound-file))
    (funcall pomidor-play-sound-file file)))

(defun pomidor--tick-tack (ellapsed)
  "Play tick or tack based on ELLAPSED."
  (pomidor--play-sound-file
   (if (zerop (mod ellapsed 2))
       pomidor-sound-tick
     pomidor-sound-tack)))

(defun pomidor--update ()
  "Update pomidor state."
  (let* ((state (pomidor--current-state))
         (total (pomidor--total-duration state))
         (ellapsed (round (time-to-seconds total))))
    (pomidor--tick-tack ellapsed)
    (when (zerop (mod ellapsed pomidor-update-interval))
      (when (functionp pomidor-alert)
        (funcall pomidor-alert))
      (run-hooks 'pomidor-update-hook)
      (cond
       ((pomidor-overwork-p)
        (pomidor--play-sound-file pomidor-sound-overwork))
       ((pomidor-break-over-notify-p)
        (pomidor--play-sound-file pomidor-sound-break-over)))))
  (pomidor--render (pomidor--get-buffer-create) pomidor-global-state))

(defun pomidor--render (buffer states)
  "Render pomidor state."
  (when (get-buffer-window buffer t)
    (with-current-buffer buffer
      (read-only-mode -1)
      (erase-buffer)
      (insert (pomidor--header (car (last states)))
              "\n")
      (cl-loop
       for i from 1
       for state in states

       as work = (pomidor--work-duration state)
       as overwork = (pomidor--overwork-duration state)
       as break = (pomidor--break-duration state)
       as total = (pomidor--total-duration state)

       with sum-work = (seconds-to-time 0)
       with sum-overwork = (seconds-to-time 0)
       with sum-break = (seconds-to-time 0)
       with sum-total = (seconds-to-time 0)

       do (progn
            (setq sum-work (time-add sum-work work)
                  sum-total (time-add sum-total total))
            (when overwork
              (setq sum-overwork (time-add sum-overwork overwork)))
            (when break
              (setq sum-break (time-add sum-break break)))
            (insert
             "\n     "
             (make-string 79 ?-)
             "\n"
             (format "%3d) [%s] | [%s] | [%s] | [%s]\t\t %s → %s"
                     i
                     (pomidor--with-face (pomidor--format-duration work) 'pomidor-work-face)
                     (pomidor--with-face (pomidor--format-duration overwork) 'pomidor-overwork-face)
                     (pomidor--with-face (pomidor--format-duration break) 'pomidor-break-face)
                     (pomidor--format-duration total)
                     (pomidor--format-time (pomidor--started state))
                     (pomidor--format-time (pomidor--ended state)))
             "\n     "
             (pomidor--graph work overwork break)))
       finally
       (insert "\n     "
               (make-string 79 ?-)
               "\n\n"
               (format "     Work\t[%s]\n"
                       (pomidor--with-face (pomidor--format-duration sum-work) 'pomidor-work-face))
               (format "     Overwork\t[%s]\n"
                       (pomidor--with-face (pomidor--format-duration sum-overwork) 'pomidor-overwork-face))
               (format "     Break\t[%s]\n"
                       (pomidor--with-face (pomidor--format-duration sum-break) 'pomidor-break-face))
               (format "     Total\t[%s]\n"
                       (pomidor--format-duration sum-total)))
       )
      (read-only-mode +1))))

(defun pomidor--get-buffer-create ()
  "Return a pomidor buffer."
  (get-buffer-create pomidor-buffer-name))

(defun pomidor--get-history-buffer-create ()
  "Create a history pomidor buffer."
  (get-buffer-create pomidor-history-buffer-name))

(defun pomidor--cancel-timer ()
  "Cancel pomidor timer."
  (when (timerp pomidor-timer)
    (cancel-timer pomidor-timer)
    (setq pomidor-timer nil)))

(defun pomidor--read-session (preserve-timestamp?)
  "Read the saved sessions."
  (let* ((json-object-type 'plist)
         (json-array-type 'list)
         (data (json-read-file pomidor-save-session-file)))
    (if preserve-timestamp?
        data
      (-map (lambda (pomidor)
              (-map (lambda (v) (if (stringp v)
                               (parse-iso8601-time-string v)
                             v))
                    pomidor))
            data))))

(defun pomidor--valid-sessions-dates (session-dates direction)
  "Get valid date of SESSION-DATES from history data to move in correct DIRECTION."
  (let ((fun (lambda (v)
               (if (cl-equalp direction :backward)
                   (time-less-p v pomidor--current-history-session)
                 (time-less-p pomidor--current-history-session v)))))
    (if pomidor--current-history-session
        (-filter (lambda (dt) (funcall fun dt)) session-dates)
      session-dates)))

;;; Public

(defvar pomidor-mode-map
  (let ((map (make-keymap)))
    (define-key map (kbd "q") #'quit-window)
    (define-key map (kbd "Q") #'pomidor-quit)
    (define-key map (kbd "R") #'pomidor-reset)
    (define-key map (kbd "h") #'pomidor-hold)
    (define-key map (kbd "H") #'pomidor-unhold)
    (define-key map (kbd "RET") #'pomidor-stop)
    (define-key map (kbd "SPC") #'pomidor-break)
    (suppress-keymap map)
    map))

(defun pomidor-work-duration ()
  "Return current work duration."
  (pomidor--work-duration (pomidor--current-state)))

(defun pomidor-overwork-duration ()
  "Return current overwork duration."
  (pomidor--overwork-duration (pomidor--current-state)))

(defun pomidor-break-duration ()
  "Return current break duration."
  (pomidor--break-duration (pomidor--current-state)))

(defun pomidor-total-duration ()
  "Return current total duration."
  (pomidor--total-duration (pomidor--current-state)))

(defun pomidor-quit ()
  "Turn off Pomidor."
  (interactive)
  (when (y-or-n-p "Are you sure you want to turn off pomidor? ")
    (kill-buffer (pomidor--get-buffer-create))))

(defun pomidor--reset-long-break-counter ()
  "Reset the pomidor counter after `pomidor-breaks-before-long' short breaks."
  (when (pomidor-should-long-break-p)
    (setq pomidor--count-short-breaks 0)))

(defun pomidor-break ()
  "Break current working pomidor."
  (interactive)
  (let ((state (pomidor--current-state)))
    (if (pomidor--break state)
        (progn
          (plist-put state :snooze t)
          (when (or (not pomidor-confirm-end-break)
                    (yes-or-no-p "Stop break and start new pomidor?"))
            (pomidor-stop))
          (pomidor--reset-long-break-counter))
      (progn
        (plist-put state :break (current-time))
        (setq pomidor--count-short-breaks (1+ pomidor--count-short-breaks))))))

(defun pomidor-reset ()
  "Delete current global state."
  (interactive)
  (when (y-or-n-p "Are you sure you want reset pomidors? ")
    (setq pomidor--count-short-breaks 0)
    (pomidor--reset)))

(defun pomidor-stop ()
  "Stop current working pomidor."
  (interactive)
  (let ((state (pomidor--current-state)))
    (plist-put state :stopped (current-time)))
  (pomidor--reset-long-break-counter)
  (nconc pomidor-global-state (list (pomidor--make-state))))

(defun pomidor-hold ()
  "Stop the current working pomidor and puts the system on hold."
  (interactive)
  (let ((state (pomidor--current-state)))
    (plist-put state :stopped (current-time)))
  (setq pomidor--system-on-hold-p t)
  (pomidor--cancel-timer))

(defun pomidor-unhold ()
  "Unhold and start a new pomidor."
  (interactive)
  (when pomidor--system-on-hold-p
    (nconc pomidor-global-state (list (pomidor--make-state)))
    (setq pomidor--system-on-hold-p nil)
    (setq pomidor-timer (run-at-time nil 1 #'pomidor--update))))

(defun pomidor-save-session ()
  "Save the current session in a file."
  (interactive)
  (let ((time-asked-to-save (current-time)))
    (pomidor-quit)
    (plist-put (pomidor--current-state) :stopped time-asked-to-save)
    (let* ((fmt-time (lambda (time) (when time (format-time-string pomidor-history-date-format time))))
           (history-state (if (file-exists-p pomidor-save-session-file)
                              (pomidor--read-session :preserve-iso-timestamp)
                            (list)))
           (global-state (-map (lambda (pomidor)
                                 (list :started (funcall fmt-time (plist-get pomidor :started))
                                       :break (funcall fmt-time (plist-get pomidor :break))
                                       :stopped (funcall fmt-time (plist-get pomidor :stopped))
                                       :snooze (plist-get pomidor :snooze)
                                       :session-ended (funcall fmt-time time-asked-to-save)))
                               pomidor-global-state))
           (new-history (append history-state global-state))
           (new-history (-filter (lambda (pomidor) (or (plist-get pomidor :stopped)
                                                  (plist-get pomidor :break)
                                                  (plist-get pomidor :snooze)))
                                 new-history)))
      (with-temp-file pomidor-save-session-file
        (insert (json-encode (vconcat new-history))))))
  (message "Pomidor session saved!"))

(defun pomidor-history-previous ()
  "Move backward in your pomidor history."
  (interactive)
  (let* ((session-data (pomidor--read-session nil))
         (session-dates (-map (lambda (pomidor)
                                (plist-get pomidor :session-ended))
                              session-data)))
    (if (= (length session-dates) 0)
        (message "You have no session saved.")
      (let* ((valid-session-dates (pomidor--valid-sessions-dates session-dates :backward)))
        (if valid-session-dates
            (progn
              (setq pomidor--current-history-session (car (last valid-session-dates)))
              (pomidor--render
               (pomidor--get-history-buffer-create)
               (-filter (lambda (pomidor)
                          (equal (car (last valid-session-dates))
                                 (plist-get pomidor :session-ended)))
                        session-data)))
          (message "History is over, go forward."))))))

(defun pomidor-history-next ()
  "Move forward in your pomidor history."
  (interactive)
  (let* ((session-data (pomidor--read-session nil))
         (session-dates (-map (lambda (pomidor)
                                (plist-get pomidor :session-ended))
                              session-data)))
    (if (= (length session-dates) 0)
        (message "You have no sessions saved.")
      (let* ((valid-session-dates (pomidor--valid-sessions-dates session-dates :forward)))
        (if valid-session-dates
            (progn
              (setq pomidor--current-history-session (car valid-session-dates))
              (pomidor--render
               (pomidor--get-history-buffer-create)
               (-filter (lambda (pomidor)
                          (equal (car valid-session-dates)
                                 (plist-get pomidor :session-ended)))
                        session-data)))
          (message "History is over, go backward."))))))


(defun pomidor-history ()
  "A simple pomodoro history feature. Compare your work over time."
  (interactive)
  (if (not (file-exists-p pomidor-save-session-file))
      (message "You should save at least one session first.")
    (switch-to-buffer (pomidor--get-history-buffer-create))
    (unless (eq major-mode 'pomidor-history-mode)
      (pomidor-history-mode))
    (pomidor-history-previous)))

(defvar pomidor-history-mode-map
  (let ((map (make-keymap)))
    (define-key map (kbd "q") #'quit-window)
    (define-key map (kbd "n") #'pomidor-history-next)
    (define-key map (kbd "p") #'pomidor-history-previous)
    (suppress-keymap map)
    map))

(define-derived-mode pomidor-history-mode special-mode "pomidor-history"
  "Major mode for Pomidor History.

\\{pomidor-history-mode-map}"
  (setq pomidor-timer nil)
  (setq pomidor--current-history-session nil))

(define-derived-mode pomidor-mode special-mode "pomidor"
  "Major mode for Pomidor.

\\{pomidor-mode-map}"
  (setq pomidor-timer (run-at-time nil 1 #'pomidor--update))
  (setq-local tab-width 8)
  (add-hook 'kill-buffer-hook #'pomidor--cancel-timer nil t)
  (pomidor--reset))

;;;###autoload
(defun pomidor ()
  "A simple and cool pomodoro technique timer."
  (interactive)
  (switch-to-buffer (pomidor--get-buffer-create))
  (unless (eq major-mode 'pomidor-mode)
    (setq pomidor--count-short-breaks 0)
    (pomidor-mode))
  (pomidor--update))


(provide 'pomidor)

;;; pomidor.el ends here
