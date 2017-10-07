---
title: Remove nag screens
permalink: /nag.html
date: 2017-07-08 19:55 +0700
featured: yes
---

Copy the respective fragments to your browser's JavaScript Console (Ctrl+Shift+J on Chromium).

Don't run codes you don't trust.

## Make [www.webtoon.com](http://www.webtoon.com/) fast

That website has an unacceptably slow scrolling.
This script makes it fast.

### Usage

Open the comic episode you want to read.

Paste this fragment into your browser's JavaScript console.

```javascript
// Retain big images. Discard everything else.
var images = [];
document.querySelectorAll("img").forEach(function (element) {
    images.push(element);
});
document.head.innerHTML = "";
document.body.innerHTML = "";
images.forEach(function (element) {
    const big = 256;
    const url = element.dataset.url;
    if (url && element.width >= big) {
        element.src = url;
        element.style.display = "block";
        document.body.appendChild(element);
    }
});
```

To go the the next episode, increment the `episode_no` parameter in the address bar.

### Notes

I tried `getEventListeners` and `removeEventListener` but they don't work.

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

## Group Gmail mails by sender email

```javascript
(function () {
    var group = {};
    document.querySelectorAll("[email]").forEach(function (elem) {
        var sender = elem.getAttribute("email");
        group[sender] = 1 + (group[sender] || 0);
    });
    var list = [];
    var sender;
    var count;
    for (sender in group) {
        count = group[sender];
        list.push([count, sender]);
    }
    list.sort(function (a, b) {
        return -Math.sign(a[0] - b[0]);
    })
    return list;
})();
```
