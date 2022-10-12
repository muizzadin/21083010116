#!/bin/bash

# deklarasi array indirect declaration
distroLinuxDesktop[0]=BlankOn
distroLinuxDesktop[1]=Ubuntu
distroLinuxDesktop[2]=Debian
distroLinuxDesktop[3]=ArchLinux
distroLinuxDekstop[4]=LinuxMint

distroLinuxServer[0]=UbuntuServer
distroLinuxServer[1]=CentOS
distroLinuxServer[2]=FedoraServer

# cara mengambil nilai Array
echo ${distroLinuxDesktop[*]}
echo ${distroLinuxServer[*]}

