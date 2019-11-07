---
id: Python4DS
title: Python Data Science DevOps.
sidebar_label: Data Science with Python
---

## Introduction

---

---

## Anaconda

### Package Management

- conda
  - basics
    - `conda --version`
  - virtual env
    - `conda info --envs`
    - `onda create -n MYENV`
    - `conda create -n MYENV --clone OLDENV`
    - `conda create -n MYENV python=3.6.0`
    - `conda activate MYENV`
    - `conda deactivate MYENV`
  - package management
    - `conda list`
    - `conda search PACKAGE`
    - `conda update conda`
    - `conda update anaconda`
    - `conda update --all`
    - `conda update PACKAGE`
    - `conda install PACKAGE`
    - `conda install PACKAGE=M.N.P`
    - `conda remove PACKAGE`
    - `conda build PACKAGE`
  - config sources
    - `conda config --show-source`
    - `conda config --remove channels NOT_WANTED`
  - set conda-forge
    - `conda config --add channels conda-forge`
    - `conda config --set channel_priority strict`

---

---

## Misc

### Learning Resources

- [Anaconda Document](https://docs.anaconda.com/anaconda/)
