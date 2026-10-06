# Fonts

Self-hosted so the app makes no request to a third-party font service. `www/styles.css`
declares them with `@font-face`; the Dockerfile copies this folder into the image.

| Family | Files | Source |
|---|---|---|
| Fraunces (variable: weight + optical size) | `fraunces-{latin,latin-ext}-opsz-normal.woff2` | npm `@fontsource-variable/fraunces` 5.3.0 |
| Outfit (variable: weight) | `outfit-{latin,latin-ext}-wght-normal.woff2` | npm `@fontsource-variable/outfit` 5.3.0 |
| IBM Plex Mono 300 / 400 / 500 | `ibm-plex-mono-{latin,latin-ext}-{300,400,500}-normal.woff2` | npm `@fontsource/ibm-plex-mono` 5.3.0 |

All three are licensed under the SIL Open Font License 1.1 (`OFL-*.txt`). To update, take the
same files from a newer Fontsource release and keep the file names, or update the `@font-face`
blocks in `styles.css` to match.
