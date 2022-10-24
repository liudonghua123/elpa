# hiddenquote: A different kind of word puzzle

> hiddenquote is a major mode that lets you play a word puzzle you
might not have played before

## Table of Contents

- [Example](#example)
- [Installation](#installation)
- [Using](#using)
- [License](#license)

---

## Example

`M-x hiddenquote`

Select the source you want to retrieve the puzzle from (you probably
want to select the local source, if it is your first time).
With a prefix argument to hiddenquote, the command will prompt for a
specific ID number.

After the puzzle loads (I recommend using `toggle-frame-maximized`,
which in vanilla Emacs is bound to `M-F10`), read the clues given in the
Definitions buffer, and enter the words where they belong, using the
syllables (and marking them as used) as you go.

---

## Installation

This package is on GNU ELPA, so you can type
`M-x package-install RET hiddenquote`

to install it.

Alternatively, you can follow these steps:

- Download and open up a terminal in hiddenquote dir.
- `make`
- Add the directory where hiddenquote is to your load path:
`(add-to-list 'load-path "/the/hiddenquote/dir")`

Done!

---

## Using

`M-x hiddenquote`

There are three buffers when playing a hiddenquote puzzle, so it's
best to maximize the Emacs frame:
- The grid buffer: Move around each cell with the usual movement
  commands (`C-f`, `C-b`, `C-p`, `C-n`).
  
  Complete each word by typing letters into the cells.  If you make a
  mistake, you can delete the character at point with `C-d`, or the
  character before with `backspace`.
  
  If you make a big mistake, then you can kill the whole word with
  `C-S-backspace`, or kill the rest of the word with `C-k`.
  
  To advance quickly to a far away word, use `M-g M-g`.
  
  To check if you guessed right, type `?`, or click the clue number.
  
  To save your progress, type `C-x C-s`.
  
  To give up, and see the answers, type `C-x !`
  
  To quit, without saving or giving up, type `C-x k`.
  
- The clues buffer: Read the clues to guess what words should go in
  the grid.  The clue where point is at (in the grid buffer) is always
  highlighted.
  
- The syllables buffer: Here you mark the syllables you used for each
  word.  Toggle a syllable state (used/unused) by clicking with the
  mouse or by hitting `RET`.
  
  Once the grid is filled, and the syllables are marked as used, the
  puzzle is complete and the timer (in the grid buffer) will stop, if
  the solution is right.

---

## License

- **[GPL 3](https://www.gnu.org/licenses/gpl-3.0-standalone.html)**
- Copyright 2021-2022 Mauro Aranda
