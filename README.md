# Persemacs

This repository contains Emacs development by Erwann Rogard.

## Description

- Directory [`./org/`](./org/) contains Org mode meta-code.
- All other directories are intended to contain code generated from the Org files.
- Emacs configuration starts with [`./org/init.org`](./org/init.org)
- Extensions are in [`./org/extension`](./org/extension.org)

## Highlights

### noweb-ref
Functions under that heading hook into the backend of [Org's noweb reference](https://orgmode.org/manual/Noweb-Reference-Syntax.html) to provide Emacs Lisp functionality for *assembling* source blocks. This allows for more complex transformations of referenced source blocks than Org alone. Also, it works around the [breaks syntax highlighting](https://emacs.stackexchange.com/questions/63643/noweb-references-in-sh-blocks-breaks-the-syntax-highlighting) problem.

#### Assemble JSON blocks

The test cases are manually defined JSON blocks of various types, each assigned a `noweb-ref` (the mapping is not necessarily one-to-one). Then, a manually defined Emacs Lisp source block is used to pass `ref-list` and `ref-keys`—where each key is matched with a block returned by `ref-list`—as input to `er317/noweb-ref-assemble`. Upon evaluation, a compact JSON object is returned. By omitting `:key-list` and replacing `:encode-fn`'s value with `'json-encode-array`, the same setup returns a JSON array. The complete example in the source code also shows how to post-process the resulting JSON, such as by wrapping around it a `noweb-ref` header and source block. 


<details>
  <summary>Screenshots</summary>

<!-- ##### Test cases-->

![Test cases](aux/Screenshot_2025-05-25_18-15-15.png)

<!-- ##### Assembler-->

![Assembler](./aux/Screenshot_2025-05-25_20-04-56.png)

<!-- ##### Evaluate-->

![Evaluate](./aux/Screenshot_2025-05-25_20-05-15.png)

</details>

<!-- TODO 
- Generate this file using Org+Export 
- Figure out how to emulate github display on local machine
-->

