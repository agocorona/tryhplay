Try HPlay
=========

Basic browser-based interface to the [Haste](http://haste-lang.org/) compiler, including
[haste-perch](https://github.com/agocorona/haste-perch) and
[hplayground](https://github.com/agocorona/hplayground/).

Run locally with Docker
-----------------------

    > docker run -it -p 80:80 agocorona/tryhplay

it run the last version of tryhplay.

if you have boot2docker do:

    > boot2docker ip

and put the resulting IP as the URL in the web browser, it will execute the examples and you can create new programs.

Alternatively you can invoque boot2docker this way:

boot2docker ssh -L 80:localhost:80

That  sets up port forwarding and starts boot2docker. so you can use localhost as the url of the application. Use other ports if you wish.

Installation
------------
see the Dockerfile and execute the steps manually in your computer

Deployment
----------

Deploys to [Heroku](http://heroku.com/) in two clicks, using [_Haskell on Heroku_](http://haskellonheroku.com/).

[![Deploy](https://www.herokucdn.com/deploy/button.png)](https://heroku.com/deploy?template=https://github.com/agocorona/tryhplay)
