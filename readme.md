# Persemacs

This repository contains Emacs development by Erwann Rogard.

## Description

- Directory [`./org/`](./org/) contains Org mode meta-code.
- All other directories contain code generated from the Org files.
- Emacs configuration starts with [`./org/init.org`](./org/init.org)
- Extensions are in [`./org/extension`](./org/extension)

## Highlights

### noweb-ref-\<feature\>
This extension hooks into the backend of [Org's noweb reference](https://orgmode.org/manual/Noweb-Reference-Syntax.html) to provide Emacs Lisp functionality for *assembling* source blocks. This allows for more complex transformations of referenced source blocks than Org alone.

#### Assemble JSON blocks
Here are JSON blocks of different types, spanning multiple `noweb-ref`s (not necessarily in a one-to-one mapping). Next, I've wrapped an Emacs-Lisp source block around `erw/noweb-ref-encode`, taking `ref-list` and `ref-keys` (the number of matched blocks must match the number of keys) as input to assemble JSON blocks. Finally, I've [called](https://orgmode.org/manual/Evaluating-Code-Blocks.html) that block for the test case.

![Test Cases](/home/erwann/Pictures/Screenshot/Screenshot_2025-05-25_12-51-19.png)
![Assembler](/home/erwann/Pictures/Screenshot/Screenshot_2025-05-25_13-05-45.png)
![Evaluation](/home/erwann/Pictures/Screenshot/Screenshot_2025-05-25_12-53-23.png)

<!-- TODO generate this file using Org+Export -->

