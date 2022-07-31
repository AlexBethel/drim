<!--

Please keep this document correctly word-wrapped at 70 columns and
with no trailing whitespace or blank lines at the end! Merge requests
with modifications to this document will be not be accepted until it
is formatted correctly! ~~abb

-->

# AlexScript

AlexScript (which is a misnomer; the language will soon be renamed
since it's not actually a scripting language) is a programming
language designed to have a functional feel and very high-level
ergonomics while maintaining speed using strictly-controlled,
deterministic memory management.

## Tools

This repository contains the following current and planned tools:
- `axc`, the AlexScript compiler. This can be used as a binary with a
  fairly standard compiler CLI, or as a library for use in other
  programs.
- `axcd`, the Language Server Protocol (LSP) server for AlexScript
  code support in editors, supporting definition peeking and lookup,
  renaming variables and modules, etc. (Planned; does not exist yet.)
- `axfmt`, the standard formatter for AlexScript code; all AlexScript
  code used in this repository must be formatted with `axfmt`, and its
  use is recommended for other projects. (Planned; does not exist
  yet.)
- `alexscript-mode`, an Emacs mode for editing AlexScript code,
  supporting syntax highlighting, automatic indentation, some basic
  keybindings for common tasks, Emacs-side LSP integration for
  communicating with `acxd`, and a collection of `yasnippet` snippets
  for inserting common AlexScript constructs. (Planned; does not exist
  yet.)
- `alexscript-vsc`, Visual Studio Code plugins and tools for editing
  AlexScript code. (Planned; does not exist yet.)
- `alexscript-vim`, tools and configuration files for optimizing Vim
  and Neovim for editing AlexScript code. (Planned; does not exist
  yet.)
