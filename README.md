
tryhplay
========

Compile and run Haste and hplayground programs.
It is a minimal IDE with edit-compile-run
capabilities. Additionally it can compile and run git project.

ghc, haste and hplayground (and perch) are preinstalled, but it can also run any
Haste project. Since it uses haste-inst, it can install any dependency.

[Running here](http://tryplayg.herokuapp.com)

[The edit/compile/run page](http://tryplayg.herokuapp.com/try/pascaltriangle.hs/edit) with an example


The changes are erased by heroku from time to time.
To save the generated HTML+Javascript,tun the application full page and download the HTML. That
way you can redistribute it, run locally in your phone etc.

To see a typical example of Haste git project that tryhplay can compile and run, see the [todo](https://github.com/agocorona/hplay-todo) project

This is a very early version, so it will have a lot of bugs.

Installation
============
If you want to install it in your computer, install ghc, install Haste and do it as usual in a
haskell project. Run the executable `tryplayground`.

Create a Heroku instance
------------------------

Since my heroku instance has limited resources, create your own and transfer my tryplay slug
This is easy:

signup to heroku.com and download the heroku console software

in a terminal of your PC, create the instance:

       > heroku create yourinstance

install anvil

       > heroku plugins:install https://github.com/ddollar/heroku-anvil

Search for my last compilation slug of tryhplay in [travis] (https://travis-ci.org/agocorona/tryhplay/builds)
.Open the last one, unfold "Deproying application". You will find a line similar to:

       Success, slug is https://api.anvilworks.org/slugs/07af68f6-a4ce-4e9d-9d6f-4e90d31dd8df.tgz

that is the last slug. Install it in your heroku instance:

       > heroku release https://api.anvilworks.org/slugs/07af68f6-a4ce-4e9d-9d6f-4e90d31dd8df.tgz

Finally set the PATH environment variable:

       > heroku config:set PATH=/app/ghc/bin:/app/.cabal/bin:/app/bin:/app/node_modules/.bin:node_modules/.bin:/app/bin:/app/node_modules/.bin:/usr/local/bin:/usr/bin:/bin --app tryplayg

add:

         --app yourinstance


to all heroku commands if you are not in the folder where the instance was created.


Alternatively, after installing anvil, you can build and release everithing from scratch.To do so
in the tryhplay folder perform:

       > heroku build -r -b https://github.com/agocorona/heroku-buildpack-haste.git

But it is quite long.




