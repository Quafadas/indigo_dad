setup-ide:
  scala-cli setup-ide .

dev:
  cs launch io.github.quafadas:live-server-scala-cli-js_3:0.0.12 -- --path-to-index-html {{invocation_directory()}}/static

format:
  scala-cli fmt .