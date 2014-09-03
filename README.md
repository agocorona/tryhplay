
tryhplay
========

Compile and run hplayground programs.

[Running here](http://tryplayg.herokuapp.com)

The changes are erased by heroku from time to time.
Run the application full page and download the HTML of your program to
run it locally or to re-distribute it.

https://travis-ci.org/agocorona/tryhplay/builds

heroku config:set PATH=/app/ghc/bin:/app/.cabal/bin:/app/bin:/app/node_modules/.bin:node_modules/.bin:/app/bin:/app/node_modules/.bin:/usr/local/bin:/usr/bin:/bin --app tryplayg
