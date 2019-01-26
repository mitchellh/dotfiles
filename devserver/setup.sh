#!/bin/bash
#
# This script sets up the image I use for my dev server. Most my dev work
# happens within a Docker container that can run mostly anywhere but this
# base image helps harden the machine a bit more.
set -e

# If run on boot of a cloud-based machine, we need to wait for cloud-init
# to complete or we can run into weird errors later on.
echo "waiting 180 seconds for cloud-init to update /etc/apt/sources.list"
timeout 180 /bin/bash -c \
  'until stat /var/lib/cloud/instance/boot-finished 2>/dev/null; do echo waiting ...; sleep 1; done'

#--------------------------------------------------------------------
# Base Packages

apt-get update -q -y
apt-get install -q -y ntp

#--------------------------------------------------------------------
# Base Firewalls

apt-get install -q -y ufw

ufw default deny incoming
ufw default allow outgoing
ufw allow ssh
ufw allow 60000:61000/udp
ufw --force enable

#--------------------------------------------------------------------
# fail2ban

apt-get install -q -y fail2ban

# We have purposely really aggressive fail2ban settings since we
# don't expect to fail and the dev servers should be ephemeral.
cat <<EOF >/etc/fail2ban/jail.local
[DEFAULT]
bantime = -1
findtime = 3600
maxretry = 5
EOF

#--------------------------------------------------------------------
# ssh

# We want to allow mosh connections for airplanes and such
apt-get install -q -y mosh

# The SSH configuration below is meant to be fairly secure. The expectation
# is that dev servers won't exist that long anyways so we can log verbose and
# so on.

cat <<EOF >/etc/ssh/sshd_config
AllowTcpForwarding no
IgnoreRhosts yes
LoginGraceTime 120
LogLevel VERBOSE
MaxAuthTries 2
MaxStartups 2
Port 22
Protocol 2
PermitEmptyPasswords no
PermitRootLogin yes
PrintLastLog no
PrintMotd no
PasswordAuthentication no
UsePAM yes
X11Forwarding no

# override default of no subsystems
Subsystem	sftp	/usr/lib/openssh/sftp-server
EOF

systemctl reload ssh.service

#--------------------------------------------------------------------
# Docker

apt-get install -q -y docker.io
