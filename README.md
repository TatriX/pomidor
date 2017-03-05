Pomidor is a simple and cool [pomodoro technique](http://www.pomodorotechnique.com/) timer.

## Installation

Clone the repo:
```sh
cd ~/.emacs.d
git clone https://github.com/TatriX/pomidor
```
Add to your .emacs:
```lisp
(add-to-list 'load-path "~/.emacs.d/pomidor/")
(require 'pomidor)

```

## Usage

Bind it to a key with the following command:

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

This cycle goes on forever.

## Keybindings

| Key   | Description          |
|-------|----------------------|
| Enter | Start new pomodoro.  |
| Space | Start a break.       |
| R     | Resets the timer.    |
| q     | Quit pomidor buffer. |
| Q     | Turns off pomidor.   |

## Customization

You can customize pomidor with `M-x customize-group RET pomidor`.

To disable sounds add to your .emacs:
```lisp
(setq pomidor-sound-tick nil
      pomidor-sound-tack nil
      pomidor-sound-overwork nil)
```

## Notification
You can set `pomidor-update-hook` to show desired notification.

Example using `notify-send`:

```lisp
(defun my-pomidor-update-hook ()
  "Pomidor update hook."
  (when (pomidor-overwork-p)
    (start-process "notify-send"
                   nil
                   "notify-send"
                   (format "pomidor: make a pause; overwork: [%s]"
                           (format-time-string "%H:%M:%S" (pomidor-overwork-duration) t))
                   "-t" "3000"
                   "-i" "/usr/share/icons/hicolor/16x16/apps/emacs.png")))

(add-hook 'pomidor-update-hook #'my-pomidor-update-hook)

```

## Acknowledgments
Inspired by https://github.com/konr/tomatinho
Sounds from https://www.freesound.org/
