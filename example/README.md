# Elm GetText example

This example demonstrates how to use the [`elm-gettext`](https://github.com/panthus/elm-gettext) package to internationalize (i18n) a Browser application in Elm.

## Getting started

### Installation

Install the required dependencies:

```bash
npm ci
```

### Build

Build does the following:

- Builds the project
- Minifies the build output
- Extracts a pot file containing the strings to be translated from the elm code
- Creates mo files for any po files

```bash
npm run build
```

### Run

Start a local development server:

```bash
npm run serve
```

## How it works

The application starts with default (English) translations. When you select "Dutch" from the language dropdown, it fetches the compiled `nl_NL.mo` file from the `locale/` directory and applies the translations dynamically.

## Adding new translations

1. Run `npm run xgettext` to extract translatable strings into a `.pot` template file
2. Create a new `.po` file from this `.pot` file for your target language (e.g. `locale/de_DE.po`) containing the translations using a tool like [POEdit](https://poedit.net/)
3. Compile the `.po` file to `.mo` using `npm run gen-mo`

   Note [POEdit](https://poedit.net/) also generates the `.mo` file. The `gen-mo` tool is mainly included such that you do not have the commit the `.mo` binary files and can regenerate them during build.

4. Update the `Translation.elm` source to load the new language file
