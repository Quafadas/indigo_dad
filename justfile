outDir := justfile_directory() + "/.out"
testDir := justfile_directory() + "/test"


setup-ide:
  scala-cli setup-ide .

dev:
  cs launch io.github.quafadas:live-server-scala-cli-js_3:0.1.1 -- --path-to-index-html {{invocation_directory()}}/static

copyAssets:
  cp -r {{justfile_directory()}}/static/. {{outDir}}

## Builds the front end project
buildJs:
  mkdir -p {{outDir}}
  scala-cli --power package . -o {{outDir}} -f --js-mode release

## JP 20/06/2024 "format" disabled until we can get .scalafmt.conf sorted out
format:
  scala-cli fmt .