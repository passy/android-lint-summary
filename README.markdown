# android-lint-summary [![Build Status](https://travis-ci.org/passy/android-lint-summary.svg)](https://travis-ci.org/passy/android-lint-summary)

A summary view for Android Lint errors.

## `--help`

```bash
$ android-lint-summary --help

android-lint-summary - a lint-results.xml pretty printer

Usage: android-lint-summary [-g|--glob ARG] [-f|--formatter ARG] [-v|--verbose]
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

## Running

```bash
$ stack setup
$ stack build
$ stack exec android-lint-summary
```
