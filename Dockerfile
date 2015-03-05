FROM biscarch/ghc-7.8.3
MAINTAINER Alberto G. Corona "agocorona@gmail.com"
RUN apt-get update
RUN apt-get install -y git
RUN apt-get install -y libbz2-dev zlib1g-dev
RUN cabal update
ENV LANG en_US.UTF8
RUN cabal install cpphs
RUN cabal install
ENV PATH /root/.cabal/bin:/opt/cabal/1.20/bin:/opt/ghc/7.8.3/bin:/opt/happy/1.19.4/bin:/opt/alex/3.1.3/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin
RUN haste-boot
RUN haste-inst install hplayground
CMD ["/bin/bash"]
