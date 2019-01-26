ARG GOLANG_VERSION=1.11.4

#--------------------------------------------------------------------
# Go

FROM golang:$GOLANG_VERSION as golang_stage

#====================================================================
# Actual Image
#====================================================================

#--------------------------------------------------------------------
# Base packages and OS

FROM ubuntu:18.04

# Install all the packages we care about
ENV DEBIAN_FRONTEND=noninteractive
RUN apt-get update -qq && apt-get upgrade -y && apt-get install -qq -y \
    build-essential \
    curl \
    git \
    jq \
	locales \
    openssh-client \
    silversearcher-ag \
    tmux \
	vim-nox \
	--no-install-recommends \
	&& rm -rf /var/lib/apt/lists/*

# Locale defaults to empty, so let's setup our locale
ENV LANG="en_US.UTF-8"
ENV LC_ALL="en_US.UTF-8"
ENV LANGUAGE="en_US.UTF-8"
RUN echo "en_US.UTF-8 UTF-8" > /etc/locale.gen && \
	locale-gen --purge $LANG && \
	dpkg-reconfigure --frontend=noninteractive locales && \
	update-locale LANG=$LANG LC_ALL=$LC_ALL LANGUAGE=$LANGUAGE

#--------------------------------------------------------------------
# Go

COPY --from=golang_stage /usr/local/go /usr/local/go
ENV PATH="/usr/local/go/bin:$PATH"

# Set this so that we can choose to temporarily compile Go versions
# inside our own dev environment if we wanted to.
ENV GOROOT_BOOTSTRAP="/usr/local/go"

#--------------------------------------------------------------------
# Login

# Proper colors
ENV TERM screen-256color

# Make sure when we enter that we start in our home directory
WORKDIR /root

# I use bash, so let's drop right into bash
COPY entrypoint.sh /bin/entrypoint.sh
CMD ["/bin/entrypoint.sh"]
