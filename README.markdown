<h1 align="center">
  <br>
  <img width="400" src="https://rawgit.com/passy/android-lint-summary/master/resources/logo.svg" alt="android-lint-summary">
  <br>
  <br>
  <br>
</h1>

> View your Android lint issues with style.

[![Build Status](https://travis-ci.org/passy/android-lint-summary.svg)](https://travis-ci.org/passy/android-lint-summary)

![Screenshot](https://raw.githubusercontent.com/passy/android-lint-summary/master/resources/screenshot.png)

## Features

- Combines multiple projects into one output. You no longer need to check each
  and every of your subprojects' lint results for errors.
- View all your issues nicely formatted in your terminal.
- Default arguments that do what you probably want them to.

## Installation

Grab the latest build for your platform from the [releases page](https://github.com/passy/android-lint-summary/releases).

If your platform isn't there, you can build it yourself with [stack](https://github.com/commercialhaskell/stack):

```
$ git clone https://github.com/passy/android-lint-summary
$ cd android-lint-summary
$ stack setup
$ stack install
```

## Using

```
$ cd my-android-project
$ ./gradle lint
$ android-lint-summary
```

Watch this ASCIICast to see it in action:

[![asciicast](https://asciinema.org/a/22800.png)](https://asciinema.org/a/22800)

## `--help`

```
$ android-lint-summary --help

android-lint-summary - a lint-results.xml pretty printer

Usage: android-lint-summary [FILES] [-g|--glob ARG] [-f|--formatter ARG]
                            [-v|--verbose]
  Format Android Lint XML output nicely

Available options:
  -h,--help                Show this help text
  -g,--glob ARG            Glob pattern to select result
                           files (default: "**/build/outputs/lint-results.xml")
  -f,--formatter ARG       Specify a formatter to use
                           [simple|null] (default: "simple")
  -v,--verbose             Enable verbose mode
  -V,--version             Show version information
```

## Developing

```
$ stack setup
$ stack build
$ stack test
$ stack exec android-lint-summary
```
