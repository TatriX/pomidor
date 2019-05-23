# Pomidor [![MELPA](https://melpa.org/packages/pomidor-badge.svg)](https://melpa.org/#/pomidor)

Pomidor is a simple and cool [pomodoro technique](http://www.pomodorotechnique.com/) timer.

## Installation

It's available on melpa:
```lisp
M-x package-install pomidor

;; or with use-package, global keybinding and disabled sounds:
(use-package pomidor
  :bind (("<f12>" . pomidor))
  :config (setq pomidor-sound-tick nil
                pomidor-sound-tack nil))
```

Or clone the repo:
```sh
cd ~/.emacs.d
git clone https://github.com/TatriX/pomidor
```
and add to your .emacs:
```lisp
(add-to-list 'load-path "~/.emacs.d/pomidor/")
(require 'pomidor)

```

## Usage

Bind it to a key with the following command if you haven't done this with `use-package`:

```lisp
  (global-set-key (kbd "<f12>") #'pomidor)
```
Or run simply `M-x pomidor`

When you start pomidor, you automatically begin your first
pomodoro. There is nothing to do at this point, except to work. You
can, of course, restart the pomodoro if you get distracted, or even
the whole series, but the program takes care of itself until the
25-minute mark is reached. At this point, the overwork period will
start until you press `Space` to start break period.

Then you can press `Space` (ask for confirmation) or `Enter` to start a new period.

After 5 minutes of the break period, pomidor will start telling you
that you should finish your break. To snooze it just press `Space` and
select `n`.

This cycle goes on forever.
[*pomidor* buffer](https://i.imgur.com/33A938J.png)

## Keybindings

| Key   | Description          |
|-------|----------------------|
| Enter | Start new pomodoro.  |
| Space | Start a break.       |
| R     | Resets the timer.    |
| q     | Quit pomidor buffer. |
| Q     | Turns off pomidor.   |

## Customization

You can customize pomidor with `M-x customize-group RET pomidor` or just edit your `.emacs`.

To change timer length:
```lisp
(setq pomidor-seconds (* 25 60)) ; 25 minutes
(setq pomidor-break-seconds (* 5 60)) ; 5 minutes

```

To disable or configure sounds:
```lisp
(setq pomidor-sound-tick nil
      pomidor-sound-tack nil
      pomidor-sound-overwork (expand-file-name (concat pomidor-dir "overwork.wav"))
	  pomidor-sound-break-over (expand-file-name (concat (getenv "HOME") "/Music/overwork.wav")))
```

To change appearance you may you `customize` or set faces via theme or directly:
```lisp
;; for a full list of available faces see `customize' or search for `defface' in the source code
(progn
  (set-face-attribute 'pomidor-break-face nil :foreground "#00ff00")
  (set-face-attribute 'pomidor-overwork-face nil :foreground "#00abff")
  (set-face-attribute 'pomidor-skip-face nil :foreground "#abbac3")
  (set-face-attribute 'pomidor-work-face nil :foreground "#ff0000"))
```

## Sound
If your Emacs can't play sounds you can provide your own function to do it:
```lisp
(setq pomidor-play-sound-file
      (lambda (file)
        (start-process "my-pomidor-play-sound"
                       nil
                       "mplayer"
                       file)))
```

## Notification
By default pomidor will show you an overwork notification once per minute.
See [alert](https://github.com/jwiegley/alert/) documentation to learn how change alert settings.

You can change default notification style globally:
```lisp
(setq alert-default-style 'libnotify)
;; or 'growl (see alert docs)
```



To change notification you can set `pomidor-alert` variable (defaults to `pomidor-default-alert`):
```lisp
(setq pomidor-alert (lambda () (alert "OMG!11")))
```

Also you can set `pomidor-update-hook` to do some work on every update.
```lisp
(defun my-pomidor-update-hook ()
  (alert "Zzz"))

(add-hook 'pomidor-update-hook #'my-pomidor-update-hook)
```

You can adjust update interval by setting `pomidor-update-interval` variable
```lisp
(setq pomidor-update-interval 30) ; seconds
```

## Acknowledgments
Inspired by https://github.com/konr/tomatinho

Sounds from [freesound](http://www.freesound.org/people/InspectorJ/sounds/343130/)
