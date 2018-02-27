# Install the base image through https://github.com/ds82/ubuntu-hardy-8.04-i386-docker
# We use an old ubuntu because g77 is not supported in newer ubuntu's
FROM ubuntu:8.04

# Install dependencies
RUN apt-get update && apt-get install -y build-essential git tcsh g77 wget vim libx11-dev libncursesw5-dev sudo

RUN useradd -ms /bin/tcsh newstar && echo "newstar:newstar" | chpasswd && adduser newstar sudo
RUN echo '%sudo ALL=(ALL) NOPASSWD:ALL' >> /etc/sudoers

USER newstar

# Obtain source code
WORKDIR /home/newstar
# Doesn't work anymore due to certificate issues (old wget uses an old protocol that github doesn't accept anymore)
#RUN wget --no-check-certificate -q https://github.com/tammojan/Newstar/archive/master.tar.gz
ADD master.tar.gz /home/newstar
RUN sudo chown -R newstar:newstar Newstar-master && mv Newstar-master/* . && rmdir Newstar-master

# Set required environment variables for newstar, this replaces the logic in $n_root/src/sys
ENV n_site=docker \
    n_install=li \
    n_hosts=docker-newstar \
    n_ftp=ftp.astron.nl \
    n_root=/home/newstar \
    n_hlp=/home/newstar/hlp

# Initialize paths etc at tcsh login
RUN echo 'source $n_root/src/sys/newstar_env.csh \nsource $n_root/src/sys/newstar_init.csh' >> /home/newstar/.cshrc

# Build newstar
WORKDIR /home/newstar/src
RUN tcsh -c 'nup build -u wntinc'
RUN tcsh -c 'nup build -u -t:exe wntinc'
RUN tcsh -c 'nup build -u all'

WORKDIR /home/newstar
ENTRYPOINT tcsh
