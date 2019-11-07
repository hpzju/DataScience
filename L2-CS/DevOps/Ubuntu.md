---
id: Ubuntu
title: Ubuntu Linux Distro, a widely used desktop/server environment linux platform.
sidebar_label: Ubuntu
---

## Ubuntu Admin

### Install, Remove, Update, Upgrade

- Commands
  - `sudo apt update`
  - `sudo apt upgrade -y`
  - `sudo do-release-upgrade -d`
  - `sudo apt autoremove -y`
  - `sudo apt install PACKAGE`
  - `sudo dpkg --configure -a`
  - `sudo apt install --reinstall BROKEN_PACKAGE`
- Config Files
  - `/etc/apt/sources.list`

### User and Privilege

#### run sudo command without a password

- `sudo visudo`
- `USER_NAME ALL=(ALL:ALL) NOPASSWD:ALL`
  - The USER can execute from ALL terminals/hosts, acting as ALL (any) users, ALL(any) groups, and run ALL (any) command

---

## Bash Scripting

---

## Linux Technologies

### SSH

#### SSH Client

- Config

  - /etc/ssh/ssh_config
    - `ForwardAgent yes`
    - `ForwardX11 yes`
  - ~/.ssh/
    - `mkdir -p ~/.ssh && chmod 700 ~/.ssh`
  - ~/.ssh/config

    - `touch ~/.ssh/config && chmod 600 ~/.ssh/config`
    - config file template:

      ```bash
      #Server1 Config
      Host SERVER
          HostName SERVER_IP
          User SERVER_USER
          Port Server_PORT
          IdentityFile ID_FILE
      ```

    - [Ref](https://www.cyberciti.biz/faq/create-ssh-config-file-on-linux-unix/)

- ssh-keygen
  - `ssh-keygen -q -t rsa -b 4096 -f testkeygen -N "";`
- ssh-agent and ssh-add
  - ssh-agent is a background program that handles passwords for SSH private keys. The ssh-add command prompts the user for a private key password and adds it to the list maintained by ssh-agent. Once you add a password to ssh-agent, you will not be prompted for it when using SSH or scp to connect to hosts with your public key.
  - `eval $(ssh-agent)`
  - `ssh-add ID_FILE`
  - `kill -9 $SSH_AGENT_PID`
- Private Key
  - `cd ~/.ssh/; cp KEY_FILE .`
  - `chmod 400 KEY_FILE`
- Public Key

#### SSH Server

- Config

---

## Misc

### Online Resources

- [Ubuntu Docs](https://help.ubuntu.com/)
