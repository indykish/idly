### Not complete yet. Moved to build a sinatra app

Idli - Continious Integration wrapper
=====================================

This a continious integration wrapper for `TravisCI` that we use at [www.megam.io](https://www.megam.io)

This is not a replacement for TravisCI or CircleCI. This was invented to manage completed builds in TravisCI.
Quite commonly upon successful build from TravisCI, we needed an ability to build nightly packages
(trusty, debian, jessie, centos, docker). These were  nightly and had to be automated.

`Idli` the CI wrapper has a UI where your TravisCI can be configured and will watch for build notifications.
The user has a choice to schedule build or build now. Currently we support tasks using `Rake` which can be configured to do so.

Our Rake tasks are [https://github.com/megamsys/packager.git](https://github.com/megamsys/packager.git)

Running
-------

``Linux``

```


```

Building from Source
---------------------

``ArchLinux``

```
yaourt ghc

yaourt cabal

git clone https://github.com/megamsys/idli.git

cabal build


````


Integration with TravisCI
--------------------------

Open the configuration file named idli.conf and provide your Travis CI settings.

```


```
