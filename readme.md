# diggit

    ├─ license
    ├─ readme.md <- you are here
    ├─ claude.md
    ├─ elm.json
    └─ src
       ├─ _redirects
       ├─ index.html
       ├─ style.css
       └─ Main.elm

## Compile

```bash
mkdir -p dist
cp src/* dist
npx elm make src/Main.elm --optimize --output=dist/index.js
```

## Develop

Listen for file changes and automatically rebuild:

```bash
fswatch -o src/ | while read f; do cp src/* dist && npx elm make src/Main.elm --debug --output=dist/index.js; done
```

Serve `dist` locally in "SPA-mode":

```bash
npx serve dist -s -C -S -n
```
