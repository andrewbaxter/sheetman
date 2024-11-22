<center>

![Sheetman](source/wasm/prestatic/logo.svg)

</center>

<br/>

An editor for `.jsv` (JSON-separated-values) tables.

JSV is JSON with an array at the root level and where each element is an object which becomes a row in the table. The columns are the superset of fields in each object.

Why JSV? It works better with line-based diff and it's better specified than CSV, plus tooling tends to be better for JSON than CSV in most languages.

# Try it out

Online, at <https://andrewbaxter.github.com/sheetman>

Offline, see the releases for desktop builds (Linux AppImage, Windows)
