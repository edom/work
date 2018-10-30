---
title: Using Python
permalink: /python.html
date: 2018-07-29 03:48 +0700
---

# Using Python

Python virtualenv is relatively forward-compatible.
Don't waste time installing Python from source.
Use the Python packaged with your distro, and use virtualenv.
The Pip that comes with Python 3.7.0 fails because Ubuntu 14.04 OpenSSL is too old (or Python doesn't bother to maintain backward compatibility).

```
sudo apt-get install python-virtualenv
```

Create a virtualenv directory using `virtualenv PATH`
Note: After the directory is created, it can't be renamed.

[How does python find packages?](https://leemendelowitz.github.io/blog/how-does-python-find-packages.html)
