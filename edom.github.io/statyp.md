---
title: Argument for static typing
permalink: /statyp.html
date: 2018-07-22 03:53 +0700
---

- There are two camps:
    - DTL (dynamically typed language)
    - STL (statically typed language)
- Every programmer is lazy, but differently.
    - People who use DTLs are too lazy to write the types.
    - People who use STLs are too lazy to do what machines can do, such as
        - detecting typos,
        - avoiding unintentional conversions,
        - tracing the code, 2 weeks later, to find out the type of a variable.
- People who use DTLs are too diligent.
They love to do what machines can do: type checking.
- Static typing enables you to be lazier.
Help the machine help you avoid work.
By investing in a few keystrokes, you will get these in return:
    - The machine will catch more mistakes for you.
    - You can have an IDE that finds references correctly.
    This enables features such as "Jump to definition", "Rename", and even more fancy refactorings.
- Moral of the story:
    - Let the machines do all the boring stuffs.
    - Be future-lazy, not present-lazy.
    Do things now so that you can be lazy later.
    Don't be lazy now only to regret it later.
        - People who organize their things are too lazy to spend mental effort later in a scramble to find things.
        - People who don't organize their things are just too lazy to do it, and would rather just experience regret in the future than experience some hardship now for a better future.
    - The sane solution to "too lazy to write types" is to pick a language with type inference, not to ditch types altogether.
    Don't throw the baby out with the bathwater.
- This argument also applies to functional programming vs procedural programming.
Indeed this argument applies to every technology.
Adopting technology enables us to be lazier.
    - People buy tractors because they are too lazy to till their fields with hoes.
    - People use frameworks because they are too lazy to do the same plumbing again and again.
    - People strive to avoid side-effects in functional programming because they are too lazy to debug synchronization errors.
- The only thing the human race isn't too lazy to do is to think about lazier ways of doing things.
