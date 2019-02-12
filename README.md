# Newstar
## The Netherlands East West Synthesis Telescope Array Reduction

Newstar is the software package used to reduce old data from the WSRT, the Westerbork Synthese Radio Telescope.

The WSRT is operated by the Netherlands Foundation for Research in Astronomy (the NFRA), now ASTRON.

Newstar was mainly written by Wim Brouw. Newstar is not being actively developed or maintained anymore.

Documentation and tutorials are currently hosted at https://lofar-astron.github.io/Newstar

To quickly get started, you can use the [Docker](https://www.docker.com/) distrubution of Newstar, by running
```
docker run -it tammojan/newstar
```

While in the container, some first command could be:
```
> exe nscan
      NSCAN$1 (v6.27) is started at 12-Feb-19 15:58:12
OPTION - Action (QUIT; LOAD,IFLOAD,LEIDEN,LIST,ARC,DUMP; UVFITS,PFITS;
     SHOW; [CVX,NVS,NOPT; REGROUP; FROM_OLD,WERR,AERR,VFIX]) = QUIT:
```
