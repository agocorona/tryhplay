Try HPlay
=========

Basic browser-based interface to the [Haste](http://haste-lang.org/) compiler, including [hplayground](https://github.com/agocorona/hplayground/).

Run locally with Docker
-----------------------

    > docker run -i -t -p 80:80 agocorona/tryhplay  /bin/bash -c 'cd tryhplay ; ./dist/build/tryplayground/tryplayground'

Installation
------------
see the Dockerfile and execute the steps manually in your computer

Deployment
----------

Deploys to [Heroku](http://heroku.com/) in two clicks, using [_Haskell on Heroku_](http://haskellonheroku.com/).

[![Deploy](https://www.herokucdn.com/deploy/button.png)](https://heroku.com/deploy?template=https://github.com/agocorona/tryhaste)
