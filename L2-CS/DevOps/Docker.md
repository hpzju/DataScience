---
id: Docker
title: Docker is a lightweight virtulization technology, drives Cloud-Native DevOps paradigm shifting.
sidebar_label: Docker
---

## Overview

---

## Application

### Features

### Applicable Scenarios

---

## Architecture

### Building Blocks

### Structures

### Domain Tech

---

## Best Practice

### Install and Initialize

#### Install on Ubuntu

- [Ref for installation](https://docs.docker.com/install/linux/docker-ce/ubuntu/)
- [Ref for install docker on WLS](https://medium.com/faun/docker-running-seamlessly-in-windows-subsystem-linux-6ef8412377aa)
- Install Bash Scritp
  - `$ curl -fsSL https://get.docker.com -o get-docker.sh`
  - `$ sudo sh get-docker.sh`

#### Initialize

- `sudo groupadd docker`
- `sudo usermod -aG docker $USER`
- `$ docker run hello-world`

### Docker Deamon Remote Connection via SSH

- Docker Shell Access
  - `eval $(ssh-agent -s)`
  - `ssh-add ID_FILE`
  - `export DOCKER_HOST=ssh://USER@REMOTE_HOST`
  - `docker info`
- One-off Docker CMD Acess
  - `docker -H ssh://USER@REMOTE_HOST info`

---

## Misc

### Online Resources

- [Docker Docs](https://docs.docker.com/)
- [1](https://docs.mongodb.com/)