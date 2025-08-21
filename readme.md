```nu
mkdir dist
watch src { try { cp -vu src/* dist ; elm make src/Main.elm --debug --output=dist/index.js } }
deno run -A npm:serve dist -s -C -S -n
```
