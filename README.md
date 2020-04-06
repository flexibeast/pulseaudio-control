# pulseaudio-control - Use `pactl` to manage PulseAudio volumes

*Author:* Alexis <flexibeast@gmail.com><br>
*Version:* 0.1<br>

`pulseaudio-control` controls PulseAudio volumes from Emacs, via `pactl`.

![Image of selecting a PulseAudio sink in the minibuffer via the Ivy completion UI](screenshot.png)<br>
*Selecting a PulseAudio sink in the minibuffer via the Ivy completion UI (not required).*

## Table of Contents

- [Installation](#installation)
- [Usage](#usage)
- [Issues](#issues)
- [License](#license)

## Installation

Install [pulseaudio-control from
MELPA](http://melpa.org/#/pulseaudio-control), or put
`pulseaudio-control.el` in your load-path and do a `(require
'pulseaudio-control)`.

## Usage

Initially, the `pulseaudio-control` keymap is not bound to any
prefix. You can call the command
`pulseaudio-control-default-keybindings` to use the prefix <kbd>C-x /</kbd>
to access the `pulseaudio-control` keymap globally; if you wish to
use this prefix by default, add the line:

    (pulseaudio-control-default-keybindings)

to your init file.

The default keybindings in the `pulseaudio-control` keymap are:

* <kbd>+</kbd> : Increase the volume of the currently-selected sink by
  `pulseaudio-control-volume-step`
  (`pulseaudio-control-increase-volume`).

* <kbd>-</kbd> : Decrease the volume of the currently-selected sink by
  `pulseaudio-control-volume-step`
  (`pulseaudio-control-decrease-volume`).

* <kbd>v</kbd> : Directly specify the volume of the currently-selected sink
  (`pulseaudio-control-set-volume`).  The value can be:

  * a percentage, e.g. '10%';
  * in decibels, e.g. '2dB';
  * a linear factor, e.g. '0.9' or '1.1'.

* <kbd>m</kbd> : Toggle muting of the currently-selected sink
  (`pulseaudio-control-toggle-current-sink-mute`).

* <kbd>x</kbd> : Toggle muting of a sink, specified by index
  (`pulseaudio-control-toggle-sink-mute-by-index`).

* <kbd>e</kbd> : Toggle muting of a sink, specified by name
  (`pulseaudio-control-toggle-sink-mute-by-name`).

* <kbd>i</kbd> : Select a sink to be the current sink, specified by index
  (`pulseaudio-control-select-sink-by-index`).

* <kbd>n</kbd> : Select a sink to be the current sink, specified by name
  (`pulseaudio-control-select-sink-by-name`).

* <kbd>d</kbd> : Display volume of the currently-selected sink
  (`pulseaudio-control-display-volume`).

* <kbd>]</kbd> : Toggle use of @DEFAULT_SINK@ for volume operations
  (`pulseaudio-control-toggle-use-of-default-sink`).

Customisation options, including `pulseaudio-control-volume-step`,
are available via the `pulseaudio-control` customize-group.

## Issues / bugs

If you discover an issue or bug in `pulseaudio-control` not already noted:

* as a TODO item, or

* in [the project's "Issues" section on
  GitHub](https://github.com/flexibeast/pulseaudio-control/issues),

please create a new issue with as much detail as possible, including:

* which version of Emacs you're running on which operating system, and

* how you installed `pulseaudio-control`.

## License

[GNU General Public License version
3](http://www.gnu.org/licenses/gpl.html), or (at your option) any
later version.


---
Converted from `pulseaudio-control.el` by [*el2markdown*](https://github.com/Lindydancer/el2markdown).
