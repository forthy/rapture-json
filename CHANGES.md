# Changes

This document describes the changes included in each release of Rapture JSON.

## Version 1.0.1
 - The `DynamicWorkaround` implicit class has been added to work around a problem accessing members called `self` in Scala 2.10.
 - Extractors now support supression of errors, to support extraction of potentially nonexistent values into `Option` types

## Version 1.0.2
 - Fix for serious infinite recursion issue when accessing a JSON object key, and indexing an array in the same expression (`applyDynamic`).
 - Removed unnecessary `DynamicWorkaround` for Scala 2.11 (where it isn't necessary).

## Version 1.0.3
 - Automatically convert between different JSON backends just by wrapping in `Json`.
 - Fix regression on nested case class extraction, with "double definition" error occurring during macro expansion (#17).

