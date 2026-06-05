# Deck Authoring Rulebook

This project trains active recall of Emacs key sequences. A good card asks
for an editing intention or command outcome, and the learner must recall the
keybinding from memory.

## Core Principles

- Ask for the action, not the key. Do not name the expected key, modifier,
  physical key, or key family in the question.
- Prefer concrete user intent: "Move forward one word." is better than
  "Use the Meta word-motion command."
- Keep cards atomic. One card should test one binding or one meaningful
  command variation.
- Include meaningful variants once. Numeric prefix arguments should use one
  representative number unless the tutorial teaches distinct semantics.
- Avoid trivia. Do not train incidental prompt answers such as `y`, `n`, or
  confirmation `SPC`.
- Avoid hardware-specific cards. Do not create cards whose primary answer is
  PageUp/PageDown or another key absent on common keyboards.
- Do not train ordinary self-insertion unless the variation itself is the
  lesson, such as a numeric prefix repeating insertion.
- Use display metadata for platform wording. For example, Emacs `DEL` may be
  displayed as "Backspace/Delete (DEL)", but the question must still avoid
  giving away the physical key.

## Question Wording

Good:

```text
Remove the character just before point.
Kill the word immediately before point.
Undo the most recent change with the tutorial's multi-key alternate undo binding.
Leave a recursive edit, extra-window situation, or minibuffer with the tutorial's all-purpose multi-key quit command.
```

Bad:

```text
Delete the character just before point using the Mac Delete/Backspace key.
Kill the word immediately before point using Meta plus the Mac Delete/Backspace key.
Use the Control-x alternative undo binding.
Use the all-purpose escape command.
```

Bad questions leak the answer. They mention key notation (`C-x`, `M-f`,
`RET`), modifier names (`Control`, `Meta`, `Alt`), or physical key names
(`Backspace`, `Delete`, `Escape`, `Tab`) when those are the thing being
tested.

## Parsing Tutorial Documents

When deriving cards from tutorials or manuals:

1. Extract candidate key notation mechanically.
2. Read the surrounding prose to identify the command concept being taught.
3. Curate a card only if the concept is a durable command, binding, or
   meaningful variation.
4. Record source references such as `TUTORIAL:line-123`,
   `TUTORIAL:Section name`, or `INFO:Node name`.
5. Document ignored candidates when they are prefixes, prose placeholders,
   prompt answers, hardware-specific alternatives, or false positives.
6. Generate combinations only for independent meaningful option groups. Do
   not generate arbitrary numeric variants.

## Validation Expectations

Deck validation should reject:

- missing required fields
- duplicate IDs
- duplicate question/answer pairs
- unparsable key notation
- uncovered source key candidates that are not explicitly ignored
- prompt-only cards
- hardware-specific PageUp/PageDown cards
- ordinary self-insert cards
- `DEL` cards without a platform-friendly display answer
- questions that leak the answer via key notation, modifier names, or physical
  key names
