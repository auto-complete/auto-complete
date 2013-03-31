% Auto-Complete - Changes

# v1.4 Changes {#changes-v1.4}

## New Options {#new-options_v1.4}

* [`ac-use-dictionary-as-stop-words`](manual.html#ac-use-dictionary-as-stop-words)
* [`ac-non-trigger-commands`](manual.html#ac-non-trigger-commands)

## New Sources {#new-sources_v1.4}

* [`ac-source-ghc-mod`](manual.html#ac-source-ghc-mod)
* [`ac-source-slime`](manual.html#ac-source-slime)

## New Dictionaries {#new-dictionaries_v1.4}

* erlang-mode
* ada-mode

## Fixed Bugs {#fixed-bugs_v1.4}

* Rare completion frequency computation error
* Improve dictionary caching sterategy
* Fixed help-mode error ("help-setup-xref: Symbol's value as variable
  is void: help-xref-following")
* Fixed auto-complete couldn't use pos-tip on Windows
* [Added workaround for linum-mode displaying bug](manual.html#linum-mode-bug)

# v1.3.1 Changes {#changes_v1.3.1}

## Fixed Bugs {#fixed-bugs_v1.3.1}

* Significant bug on css-mode

## Others {#others_v1.3.1}

* Added COPYING files

# v1.3 Changes {#changes_v1.3}

Major changes in v1.3.

## New Options {#new-options_v1.3}

* [`ac-disable-faces`](manual.html#ac-disable-faces)
* [`ac-show-menu-immediately-on-auto-complete`](manual.html#ac-show-menu-immediately-on-auto-complete)
* [`ac-expand-on-auto-complete`](manual.html#ac-expand-on-auto-complete)
* [`ac-use-menu-map`](manual.html#ac-use-menu-map)

## New Sources {#new-sources_v1.3}

* [`ac-source-semantic-raw`](manual.html#ac-source-semantic-raw)
* [`ac-source-css-property`](manual.html#ac-source-css-property)

## New Source Properties {#new-source-properties_v1.3}

* [`summary`](manual.html#summary)
* [`available`](manual.html#available)

## New Dictionaries {#new-dictionaries_v1.3}

* tcl-mode
* scheme-mode

## Changed Behaviors {#changed-behaviors_v1.3}

* Scoring regarding to candidate length (sort by length)

## Fixed Bugs {#fixed-bugs_v1.3}

* Error on Emacs 22.1
* `flyspell-mode` confliction (`M-x flyspell-workaround`)

## Others {#others-v1.3}

* Improved word completion performance (#18)
* Cooperate with [pos-tip.el](manual.html#show-help-beautifully)
* Yasnippet 0.61 support
* Fix many bugs

# v1.2 Changes {#changes_v1.2}

Major changes in v1.2 since v1.0.

## New Features {#new-features_v1.2}

* [Completion by Fuzzy Matching](manual.html#completion-by-fuzzy-matching)
* [Completion by Dictionary](manual.html#completion-by-dictionary)
* [Incremental Filtering](manual.html#filtering-completion-candidates)
* [Intelligent Candidate Suggestion](manual.html#candidate-suggestion)
* [Trigger Key](manual.html#trigger-key)
* [Help](manual.html#Help)

## New Commands {#new-commands_v1.2}

* [`auto-complete`](manual.html#auto-complete-command)

## New Options {#new-options_v1.2}

* [`ac-delay`](manual.html#ac-delay)
* [`ac-auto-show-menu`](manual.html#ac-auto-show-menu)
* [`ac-use-fuzzy`](manual.html#ac-use-fuzzy)
* [`ac-use-comphist`](manual.html#ac-use-comphist)
* [`ac-ignores`](manual.html#ac-ignores)
* [`ac-ignore-case`](manual.html#ac-ignore-case)
* [`ac-mode-map`](manual.html#ac-mode-map)

## New Sources {#new-sources_v1.2}

* [`ac-source-dictionary`](manual.html#ac-source-dictionary)

## Changed Behaviors {#changed-behaviors_v1.2}

* Completion is now delayed to start
  ([`ac-delay`](manual.html#ac-delay))
* Completion menu is now delayed to show
  ([`ac-auto-show-menu`](manual.html#ac-auto-show-menu))

## Others {#others_v1.2}

* Fix many bugs
* Improve performance
