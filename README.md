# emacs-srs-trainer

`emacs-srs-trainer` is an Emacs-native spaced-repetition trainer for
Emacs documentation keybindings. It asks you to answer by pressing real
Emacs key sequences, not by typing textual answers.

Built-in decks:

- `Emacs Tutorial`: curated from the installed Emacs Tutorial.
- `Info: An Introduction`: curated from the installed Info introduction
  manual, including its advanced Info commands chapter.
- `Org Mode Manual`: curated from the installed Org manual, covering
  important commands across the manual including outline editing, tables,
  TODOs, tags, properties, timestamps, agenda, export, publishing, and
  source blocks.

Built-in decks are validated against keybinding candidates extracted from
the local installed source documents.

## Why Emacs-Native

The tutorial teaches Emacs key behavior: prefixes, multi-key commands,
minibuffer cancellation, `C-g`, and numeric arguments such as
`C-u 8 C-f`. A terminal app cannot reliably observe those as Emacs sees
them. This package runs inside Emacs and captures answers with Emacs input
APIs.

## Requirements

- Emacs 27.1 or newer
- No Anki dependency
- No Doom, Spacemacs, `use-package`, `straight.el`, or package archive
  setup required

## Installation

### Manual Clone

Clone the repository:

```sh
git clone https://github.com/balzacbabylon/emacs-srs-trainer.git
```

Add the clone to your Emacs `load-path`:

```elisp
(add-to-list 'load-path "/path/to/emacs-srs-trainer")
(require 'emacs-srs-trainer)
```

Replace `/path/to/emacs-srs-trainer` with the directory where you cloned
the repository.

### `package-vc-install`

On Emacs versions with `package-vc-install`, you can install directly from
GitHub:

```elisp
(package-vc-install
 '(emacs-srs-trainer
   :url "https://github.com/balzacbabylon/emacs-srs-trainer.git"
   :branch "main"))
```

Then load it:

```elisp
(require 'emacs-srs-trainer)
```

## Usage

Start a due review session:

```text
M-x emacs-srs-trainer-review
```

Practice every card in the default deck, ignoring due times:

```text
M-x emacs-srs-trainer-review-all
```

`review-all` is a drill mode. It grades your answers, but it does not
change stored SRS progress or due times.

Review a specific deck, such as `Info: An Introduction` or
`Org Mode Manual`:

```text
M-x emacs-srs-trainer-review-deck
```

Review one topic:

```text
M-x emacs-srs-trainer-review-topic
```

Other useful commands:

```text
M-x emacs-srs-trainer-stats
M-x emacs-srs-trainer-set-review-card-limit
M-x emacs-srs-trainer-reset
M-x emacs-srs-trainer-validate-deck
M-x emacs-srs-trainer-doctor
M-x emacs-srs-trainer-open-deck
```

Limit the number of due cards in each normal review session:

```text
M-x emacs-srs-trainer-set-review-card-limit
```

Enter `0` for no limit. You can also set a one-time limit with a numeric
prefix, for example `C-u 10 M-x emacs-srs-trainer-review`.

The review buffer is named `*Emacs SRS Trainer*`. A card looks like:

```text
Deck: Emacs Tutorial
State: New: 80    Learning: 4    To Review: 9
Due now: New: 12    Learning: 1    To Review: 3
Due: 16
Card type: New

Q: Move forward 8 characters.

Press the actual Emacs key sequence now.
```

After grading:

```text
RET or SPC: next    q: quit    ?: help
```

Those trainer controls are only active after an answer has already been
captured.

Pressing `q` after a card has been graded leaves study mode and shows a
welcome/dashboard buffer with recently reviewed cards and available decks.
Use `TAB` to move between deck buttons and `RET` to open the selected deck,
or use ordinary Emacs commands such as `C-x 0` to close the window.

When the session ends normally, the buffer shows a completion message. If
all currently due cards were reviewed, it says:

```text
You have reviewed all due cards.
```

At the completion prompt, press `m` to return to the main menu or `q` to
leave the completion screen as-is.

If you used a session limit and more cards remain due, it reports how many
are still due.

## Raw Key Capture

During review, answer capture reads actual Emacs input events and uses
`key-description` and `kbd` to normalize them to canonical Emacs notation.
Captured answers are never dispatched as commands.

Capture is card-aware and stops as soon as the typed prefix can no longer
match the card's canonical answer or accepted alternatives. If the expected
answer is `C-x C-f` and you type `C-x C-s`, the trainer grades it wrong as
soon as `C-s` is pressed. If the expected answer is `C-g C-k` and you type
`C-x`, the trainer grades it wrong immediately.

Prefix arguments are treated the same way. `C-u 8 C-f` is graded as one
answer, but `C-u 9` is wrong as soon as the typed prefix stops matching the
expected numeric-argument variation.

## `C-g` Handling

`C-g` normally signals `keyboard-quit`. During answer capture the trainer
binds `inhibit-quit`, records `C-g` as input, clears temporary quit state,
and restores dynamic input state with `unwind-protect`. This lets `C-g` be
tested without quitting the review session.

## Keyboard Notes

The Emacs Tutorial uses Emacs notation. On many keyboards, especially Mac
keyboards, Emacs `DEL` means the physical Delete/Backspace key. The
trainer displays those answers as `Backspace/Delete (DEL)` and accepts
Emacs `<backspace>` events as well.

Hardware-specific PageUp/PageDown alternatives from the tutorial are
intentionally omitted from the built-in decks.

## Scheduler

Cards are grouped into Anki-style queues:

- `New`: never studied
- `Learning`: seen but not graduated yet, including cards being relearned
  after a wrong answer
- `To Review`: graduated cards due for interval review

New cards are due immediately. The built-in scheduler uses learning steps
similar to Anki's learning-step model while keeping this package's binary
review choices: `Correct` and `Redo`. The default steps are `1 minute`,
`10 minutes`, and `1 day`: an incorrect answer (`Redo`) returns the card
to the first step and makes it due in 1 minute; a correct answer advances
to the next step; after the final learning step is answered correctly, the
card graduates to `To Review` with an initial review interval of 3 days.
Later correct reviews grow by the card's ease factor.

Customize `emacs-srs-trainer-scheduler-learning-steps-seconds` to change
the learning ladder. The default value is:

```elisp
'(60 600 86400)
```

The result screen shows the new state and next due time. While reviewing
due cards, the due queue is refreshed after each answer, so a wrong card
can reappear once that 60-second learning delay has elapsed.

`emacs-srs-trainer-review` updates and saves scheduler state after each
answer. `emacs-srs-trainer-review-all` is practice-only and leaves stored
state unchanged.

Cards are shown by queue bucket: `Learning`, then `To Review`, then `New`.
Within each bucket, card order is shuffled when the due list is built, so
cards that come due together do not reappear in deck order. Customize
`emacs-srs-trainer-shuffle-cards-within-queue` to disable this.

## Storage

Progress is stored as a plain Lisp data file under `user-emacs-directory`:

```text
emacs-srs-trainer-state.el
```

Customize `emacs-srs-trainer-storage-file` to move it:

```elisp
(setq emacs-srs-trainer-storage-file
      (expand-file-name "emacs-srs-trainer-state.el" user-emacs-directory))
```

Reset progress:

```text
M-x emacs-srs-trainer-reset
```

The review card limit is a separate Customize setting named
`emacs-srs-trainer-review-card-limit`; it controls session size, not card
due timestamps.

## Validation

Run:

```text
M-x emacs-srs-trainer-validate-deck
```

Validation checks required fields, unique IDs, parseable key notation,
duplicate question/answer pairs, deterministic generated prefix cards,
source references, and coverage against keybinding candidates extracted
from the local installed tutorial, Info introduction manual, and Org
manual.

Validation also screens for low-value cards: incidental prompt answers,
hardware-specific PageUp/PageDown alternatives, ordinary self-insertion
cards, `DEL` cards without a platform-friendly display answer, and
questions that leak the answer.

The tutorial extractor locates the installed tutorial through
`data-directory`, trying paths such as:

```elisp
(expand-file-name "tutorials/TUTORIAL" data-directory)
```

The Info extractor locates the installed `info.info` file through Emacs's
Info directory list. If the Emacs Tutorial is missing, validation reports
a clear failure. If `info.info` is unavailable, validation still checks
the curated Info cards but reports a warning that source coverage was
skipped for that manual.

The Org extractor locates the installed `org.info` file through Emacs's
Info directory list. If it is unavailable, validation still checks the
curated Org cards but reports a warning that source coverage was skipped
for that manual.

## Adding Decks

Decks are ordinary Emacs Lisp card plists. Register cards with:

```elisp
(emacs-srs-trainer-register-deck "My Deck" my-card-list)
```

Each card should include:

```elisp
(:id "unique-id"
 :deck "My Deck"
 :topic "Topic"
 :command some-command
 :question "What should the user do?"
 :canonical-answer "C-c x"
 :accepted-answers ("<f5>")
 :tags ("custom")
 :source-ref "MANUAL:Section")
```

See `DECK_AUTHORING_RULEBOOK.md` before adding or generating cards. It
defines how to parse tutorials/manuals, what counts as a meaningful
command variation, and how to write questions that require active recall
without leaking the answer.

Future decks can cover Dired, Org Mode, Magit, Help, registers, keyboard
macros, search and replace, window management, buffers, or custom user
material without changing the review UI.

## Development

Run all tests:

```sh
emacs -Q --batch -L . -l emacs-srs-trainer-test.el -f ert-run-tests-batch-and-exit
```

Validate the deck:

```sh
emacs -Q --batch -L . -l emacs-srs-trainer.el -f emacs-srs-trainer-validate-deck
```

Run doctor:

```sh
emacs -Q --batch -L . -l emacs-srs-trainer.el -f emacs-srs-trainer-doctor
```

Byte-compile:

```sh
emacs -Q --batch -L . -f batch-byte-compile \
  emacs-srs-trainer-deck.el \
  emacs-srs-trainer-scheduler.el \
  emacs-srs-trainer-storage.el \
  emacs-srs-trainer-tutorial.el \
  emacs-srs-trainer-info.el \
  emacs-srs-trainer-org.el \
  emacs-srs-trainer-validate.el \
  emacs-srs-trainer.el \
  emacs-srs-trainer-test.el
```

## Known Limitations

- Repeated command behavior such as repeated `C-l` is represented as the
  key command that cycles behavior, not as a macro of repeated keypresses.
- Prompt-only answers, mouse gestures, stand-alone Info reader keys, and
  hardware-specific PageUp/PageDown alternatives are intentionally omitted.
- The source extractors are conservative. They find obvious Emacs key
  notation and validation documents intentional ignores for standalone
  prefix keys or repeated command families.
- Storage currently uses a Lisp data file rather than SQLite.

## License

GPL-3.0-or-later. See `LICENSE`.
