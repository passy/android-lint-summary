<h1 align="center">
  <br>
  <img width="400" src="https://rawgit.com/passy/android-lint-summary/master/resources/logo.svg" alt="android-lint-summary">
  <br>
  <br>
  <br>
</h1>

> View your Android lint issues with style.

[![Build Status](https://travis-ci.org/passy/android-lint-summary.svg)](https://travis-ci.org/passy/android-lint-summary)

## Features

- Combines multiple projects into one output. You no longer need to check each
  and every of your subprojects' lint results for errors.
- View all your issues nicely formatted in your terminal.
- Default arguments that do what you probably want them to.

![Screenshot](https://raw.githubusercontent.com/passy/android-lint-summary/master/resources/screenshot.png)

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
# Search in your current working directory
$ android-lint-summary
# Or search for a different pattern
$ android-lint-summary -g '**/lint-summary*.xml'
# Or point directly to a file
$ android-lint-summary app/build/outputs/lint-summary.xml
# Or read from stdin
$ android-lint-summary - < app/build/outputs/lint-summary.xml
```

Watch this ASCIICast to see it in action:

[![asciicast](https://asciinema.org/a/23302.png)](https://asciinema.org/a/23302)

## Gradle Integration

Check out the [LintSummary Sample](https://github.com/passy/Android-LintSummarySample/)
to see how to integrate this with your gradle build.

If you copy this [gradle script](https://github.com/passy/Android-LintSummarySample/blob/c3b30a053053aafccec387aa2c6ef3e1276f0e52/gradle/lint-summary.gradle)
to your project, you can enable it with just one line per sub-project.

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

## Credits

Massive thanks to [Sindre Sorhus](https://twitter.com/sindresorhus) for making
this gorgeous logo.
