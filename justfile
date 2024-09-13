outDir := justfile_directory() + "/.out"
testDir := justfile_directory() + "/test"


setup-ide:
  scala-cli setup-ide .

dev:
  cs launch io.github.quafadas::sjsls:0.2.1 -- --path-to-index-html {{invocation_directory()}}/static

## Builds the front end project
buildJs:
  mkdir -p {{outDir}}
  scala-cli --power package . -o {{outDir}} -f

## JP 23/06/2024 switched to scalafmt during skype call with Simon
format:
  scalafmt ~/GIT/indigoLite

## JP 24/06/2024 added "clean", sometimes help with browser synchronisation with build
clean:
  scala-cli clean .

copyAssets:
  cp -r {{justfile_directory()}}/static /{{outDir}}
  
