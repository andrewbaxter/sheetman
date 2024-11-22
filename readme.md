<p align="center" width="100%">
  <img width="33%" src="source/wasm/prestatic/logo.svg" />
</p>

<br/>

An editor for `.jsv` (JSON-separated-values) tables.

JSV is JSON with an array at the root level and where each element is an object which becomes a row in the table. The columns are the superset of fields in each object.

Why JSV? Pretty-printed it works with line-based diff and it's better specified than CSV, plus tooling tends to be better for JSON than CSV in most languages.

# Try it out

Online, at <https://andrewbaxter.github.com/sheetman>

Offline, see the releases for desktop builds (Linux AppImage, Windows)

# How do I calculate things

Use python.

# Usage

- Click a cell once to select for column/row/cell operations

- Click the cell again or press enter to edit it

- Click a different cell or press escape to stop editing

## Keyboard

Generally speaking, all keyboard interaction (hotkeys, typing, navigation) requires a cell to have keyboard focus. When your mouse moves over the table it tries to grab keyboard focus, but clicking buttons or other windows will remove it. (Cell selection and keyboard focus are different - keyboard focus is a browser internal state.)

Some of these may be desktop/website/embedded specific. If there's a button in the menu it should work.

- `Arrow key` - Select a different cell/header

- `Ctrl + arrow key` - Create a new column/row in the specified direction of the current cell

- `Ctrl + o` - Open a file

- `Ctrl + s` - Save the file with the current name

## Cell types

- In the cell menu you can change the "type" of a cell. These correspond to JSON types

- Pressing a button tries to do a dumb, minimal conversion to the specified type even if it results in invalid data (i.e. pressing `Number` on the string `123z` will change the type but highlight it as invalid). Converting to null/missing will erase any data, converting to bool if it's not already `true`/`false` will set it to `false`.

- Values that are invalid will be serialized as strings since strings are never invalid
