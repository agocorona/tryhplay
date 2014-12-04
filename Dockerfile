FROM biscarch/ghc-7.8.3
RUN apt-get update
RUN apt-get install git
RUN git clone https://github.com/agocorona/tryhplay
RUN apt-get install zlib1g-dev
RUN export LANG="en_US.UTF8"
RUN cabal install cpphs
RUN cd tryhplay
RUN cabal install
