# pulsectl - Use `pactl` to manage PulseAudio volumes

*Author:* Alexis <flexibeast@gmail.com><br>
*URL:* [https://github.com/flexibeast/pulsectl](https://github.com/flexibeast/pulsectl)<br>

`pulsectl` controls PulseAudio volumes from Emacs, via `pactl`.

## Table of Contents

- [Installation](#installation)
- [Usage](#usage)
- [Issues](#issues)
- [License](#license)

## Installation

Install [pulsectl from MELPA](http://melpa.org/#/pulsectl), or put `pulsectl.el` in your load-path and do a `(require 'pulsectl)`.

## Usage

Use <kbd>C-x /</kbd> to access the `pulsectl` keymap. The default keybindings in that keymap are:

* <kbd>&#0043;</kbd> : Increase the volume of the currently-selected sink by `pulsectl-volume-step` (`pulsectl-increase-volume`).

* <kbd>&#0045;</kbd> : Decrease the volume of the currently-selected sink by `pulsectl-volume-step` (`pulsectl-decrease-volume`).

* <kbd>v</kbd> : Directly specify the volume of the currently-selected sink (`pulsectl-set-volume`). The value can be:

  * a percentage, e.g. '10%';
  * in decibels, e.g. '2dB';
  * a linear factor, e.g. '0.9' or '1.1'.

* <kbd>m</kbd> : Toggle muting of the currently-selected sink (`pulsectl-toggle-current-sink-mute`).

* <kbd>x</kbd> : Toggle muting of a sink, specified by index (`pulsectl-toggle-sink-mute-by-index`).

* <kbd>e</kbd> : Toggle muting of a sink, specified by name (`pulsectl-toggle-sink-mute-by-name`).

* <kbd>i</kbd> : Select a sink to be the current sink, specified by index (`pulsectl-select-sink-by-index`).

* <kbd>n</kbd> : Select a sink to be the current sink, specified by name (`pulsectl-select-sink-by-name`).

Customisation options, including `pulsectl-volume-step`, are available via the `pulsectl` customize-group.

## Issues / bugs

If you discover an issue or bug in `pulsectl` not already noted:

* as a TODO item, or

* in [the project's "Issues" section on GitHub](https://github.com/flexibeast/pulsectl/issues),

please create a new issue with as much detail as possible, including:

* which version of Emacs you're running on which operating system, and

* how you installed `pulsectl`.

## License

[GNU General Public License version 3](http://www.gnu.org/licenses/gpl.html), or (at your option) any later version.


---
Converted from `pulsectl.el` by [*el2markdown*](https://github.com/Lindydancer/el2markdown).
