# Mu 無 [![Build Status](https://travis-ci.org/PelleJuul/Mu.svg?branch=master)](https://travis-ci.org/PelleJuul/Mu)

## Introduction

Mu is a modal, terminal based text editor, which attempts to provide a "fat-free" environment for editing your text documents.

Mu can, of course, be used to edit any text file you want, but it is mostly thought as a code editor. To the ever popular IDEs, which are monolithic giants, packing all of your tools in one single package with tonnes of features, Mu tries to be the exact opposite: Few but powerful features in a small packet, fitting into a larger tool chain. By the old Unix proverb:

>"Do one thing and do it well" - *Douglas Mcllroy*

Mu does text-editing, and nothing else - out of the box at least.

If you enjoy features like autocompletion, syntax highlighting and in-editor debugging, Mu is equipped for great expandability.

## Functional Core

Mu is written entirely in Haskell and spots a fully functional editor core - no side effects! Hopefully, this will provide concise and less buggy code.

Haskell is known for being a hard-to-learn, esoteric, academic language, without real world applications. A side goal of this project was to show that it is entirely possible to write useful software in Haskell.

By keeping the codebase simple, Mu also tries to be a playground for aspiring Haskellers. Even with a limited understanding of Haskell, beginners should be able to write their own expansions.

## Pressing Issues

Mu is still a *very* young project, as thus it has some issues, some of the biggest are:

* The input module is very naïve, with limited support for modifier keys and no mouse support at all.
* The current display module is not very well optimised and shows visual flicker in some terminal emulators.
* Documentation is still nonexistent as of yet. This should be changed by writing a developer guide and a tutorial on writing expansions.
* Nothing can really said to be 'done'.
