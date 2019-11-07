---
id: Howto-AWSCLI4Lightsail
title: Howto Install, Config and Run AWS CLI for Lightsail Management
sidebar_label: AWS CLI for Lightsail Mgmt
---

## Backround

- [AWS Command Line Interface](https://docs.aws.amazon.com/cli/latest/userguide/cli-chap-welcome.html)
- [Lightsail Dev APIs](https://docs.aws.amazon.com/cli/latest/reference/lightsail/index.html)
- using AWS cli to manage Lightsail instance is a efficient and convinent way.

---

## Prerequisite and Scenario

- an [IAM user/role](https://docs.aws.amazon.com/IAM/latest/UserGuide/introduction.html) with lightsail full control [policies](https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies.html).
- Obtained [access key and secret access key](https://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_access-keys.html) under the IAM user
- a running [lightsail instance](https://lightsail.aws.amazon.com/ls/webapp/home/instances) management scenario

---

## Procedure

### Step 1 - Install AWS CLI

#### 1.1 Windows

- [for MSI Installer](https://docs.aws.amazon.com/cli/latest/userguide/install-windows.html)
- for Python + pip3
  - `pip install awscli`
  - `pip install --user --upgrade awscli`
- Python + pip3 in virtual Envirment (Anaconda)
  - `conda info --envs`
  - `conda create -n AWSENV python=3.6.0`
  - `conda activate AWSENV`
    - run `activate AWSENV` if conda command complained.
  - `pip install --upgrade awscli`
- Notice: if PyYAML complianed, using:
  - `pip install --upgrade awscli --ignore-installed PyYAML`

#### 1.2 Linux

- for Python + pip3
  - `pip install awscli`
  - `pip install --user --upgrade awscli`
- Python + pip3 in virtual Envirment (Anaconda)
  - `conda info --envs`
  - `conda create -n AWSENV python=3.6.0`
  - `conda activate AWSENV`
  - `pip install --upgrade awscli`
- Notice: if PyYAML complianed, using:
  - `pip install --upgrade awscli --ignore-installed PyYAML`

---

### Step 2 - Config AWS CLI

- check aws version
  - `aws --version`
- upgrade if neccesarry
  - `pip install --user --upgrade awscli`
  - virtual env using:
    - `pip install --upgrade awscli`
- config access key, Secret access key, Region, and Output format
  - `aws configure`
  - Notice: your instance's [region name](https://docs.aws.amazon.com/general/latest/gr/rande.html) correctly, do not include AZ indication characters.

### Step 3 - Run AWS CLI with your lightsail instance

- get lightsail help

  - `aws lightsail help`

- get lightsail bundles info

  - `aws lightsail get-bundles`

- get instance info

  - `aws lightsail get-instance --instance INSTANCENAME`

- get domain info
  - `aws --region us-east-1 lightsail get-domain --domain-name EXAMPLE.COM`

## Result

- Manage Lightsail instances and resources using AWS CLI.

---

## Pitfalls and Cautions

- lightsail domain related APIs works only on region **us-east-1**.
- [PyYAML problem](https://github.com/aws/aws-cli/issues/3828) causes awscli installation fails.

---

## Reference Resources

- [No Ref](www.google.com)
