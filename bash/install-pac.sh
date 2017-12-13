#!/usr/bin/env bash

# install pacmanager for centos 7

curl -q -O https://jaist.dl.sourceforge.net/project/pacmanager/pac-4.0/pac-4.5.5.7-2.x86_64.rpm

sudo yum install -y ftp perl perl-devel vte perl-Crypt-Blowfish perl-Crypt-Rijndael perl-Gtk2-Unique perl-IO-Stty perl-Expect perl-Gnome2-GConf perl-Gtk2-Ex-Simple-List perl-Gtk2-GladeXML perl-Net-ARP perl-Socket6 perl-YAML uuid-perl

sudo rpm -ivh pac-4.5.5.7-2.x86_64.rpm

