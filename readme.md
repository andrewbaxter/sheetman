<p align="center" width="100%">
  <img width="33%" src="source/wasm/prestatic/logo.svg" />
</p>

<br/>

An editor for `.jsv` (JSON-separated-values) tables.

JSV is JSON with an array at the root level and where each element is an object which becomes a row in the table. The columns are the superset of fields in each object.

Why JSV? Pretty-printed it works with line-based diff and it's better specified than CSV, plus tooling tends to be better for JSON than CSV in most languages.

# Try it out

Note: Firefox [doesn't work yet](https://caniuse.com/mdn-html_global_attributes_contenteditable_plaintext-only)

Online, at <https://andrewbaxter.github.io/sheetman/>

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

- `Enter` - Toggle editing/flip value on boolean

- `Esc` - Stop editing cell

- `Arrow key/h/j/k/l` - Select a different cell/header

- `Ctrl + arrow key/h/j/k/l` - Create a new column/row in the specified direction of the current cell

- `Ctrl + o` - Open a file

- `c` - Toggle pin on a column

- `r` - Toggle pin on a row

- `Ctrl + s` - Save the file with the current name

- `Ctrl + z/u` - Undo

- `Ctrl + y` - Redo

- `m` - Move or create mark on focused cell, or unmark if currently marked

- `Shift + m` - Unmark

- `x/Ctrl + x` - Cut marked/focused cells

- `c/Ctrl + c` - Copy marked/focused cells

- `p/Ctrl + v` - Paste with top-left corner over focused cell

- `s` - Sort rows by focused column

- `Shift + s` - Sort rows by focused column (reverse)

- `f` - Fill columns in marked range

## Cell types

- In the cell menu you can change the "type" of a cell. These correspond to JSON types

- Pressing a button tries to do a dumb, minimal conversion to the specified type even if it results in invalid data (i.e. pressing `Number` on the string `123z` will change the type but highlight it as invalid). Converting to null/missing will erase any data, converting to bool if it's not already `true`/`false` will set it to `false`.

- Values that are invalid will be serialized as strings since strings are never invalid
