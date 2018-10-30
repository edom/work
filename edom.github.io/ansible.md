---
title: Ansible
permalink: /ansible.html
date: 2018-05-15 02:31:00 +0700
---

# Ansible

An [inventory](http://docs.ansible.com/ansible/intro_inventory.html) is a map from host alias to host address.
We use those aliases to select the machines to mutate.

Ansible has two executables: `ansible` and `ansible-playbook`.

The `ansible-playbook` takes a YAML configuration.

The `ansible` executable executes one command.

The `ansible` command is like a "single-task playbook".

See also `man ansible` ("run a task on target hosts") and `man ansible-playbook`.

A machine can have many roles.

An Ansible role should be a noun phrase (`web-server`), not a verb phrase (`install-web-server`).
