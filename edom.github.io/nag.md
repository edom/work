---
title: Remove nag screens
permalink: /nag.html
date: 2017-07-08 19:55 +0700
featured: yes
---

Copy the respective fragments to Inspector Console.

Don't run codes you don't trust.

## Remove Quora nag screen

Tested on 2017-07-08.

Not only does Quora put up a nag screen, it also disables scrolling.

```javascript
document.querySelectorAll("div[id]").forEach(function (x) {
    if (x.id.indexOf("signup_wall_wrapper") >= 0) { x.remove(); }
});
document.body.classList.remove("signup_wall_prevent_scroll");
```

## Remove Pinterest nag screen

Tested on 2017-07-08.

```javascript
document.querySelector("[data-test-giftwrap]").remove();
```
