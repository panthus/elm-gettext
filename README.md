# elm-gettext

An Elm package for internationalization (i18n) using [GNU gettext](https://www.gnu.org/software/gettext/manual/gettext.html) MO/PO files. This library allows you to load compiled binary translation files (`.mo`) and translate text at runtime in your Elm application.

## Features

- Simple `t`, `tn`, `tp`, `tpn` API for translation functions
- Support for string formatting with `{placeholder}` syntax
- Plural form support following gettext plural-form rules
- Context-aware translations to disambiguate identical strings
- Load translations from binary MO files (compiled from PO files)

## Installation

Add this package to your Elm project:

```bash
elm install panthus/elm-gettext
```

## Workflow

A typical i18n workflow looks like this:

1. **Write your Elm code** using `t`, `tn`, `tp`, `tpn` functions for all translatable strings
   
   Note that translatable strings cannot be dynamically passed to the translation functions (via variables, loaded from somewhere etc), because the extraction tool requires static strings to be able to extract them into a `.pot` file.
2. **Extract strings** from your code into a `.pot` template file using [`@panthus/elm-xgettext`](https://www.npmjs.com/package/@panthus/elm-xgettext)
3. **Create/update `.po` files** from the `.pot` file for each target language
4. **Translate** the strings in each `.po` file (using [POEdit](https://poedit.net/) or similar)
5. **Compile** `.po` files to `.mo` binary files using [`@panthus/elm-xgettext`](https://www.npmjs.com/)
6. **Load** the `.mo` files at runtime in your application

## Example

See the [`example/`](example) directory for a complete working example demonstrating:

- Loading translations from MO files
- Switching languages at runtime
- Using all translation functions (`t`, `tn`, `tp`, `tpn`)
