# The recursivity of category theory

_ Notations:
    - Let C be a category.
    - "A in C" means A is an object of C.
    - C(A,B) means the hom-set (arrow-set) from A to B.
    A is an object of C.
    B is an object of C.
    - f : A -> B is an arrow in C(A,B).
- From category C, we can construct D (an "arrow-only" category):
    - D has only one object, written * ("star").
    - Every object A in C is represented by its identity arrow id_A in C(A,A).
    - Every arrow in C becomes an arrow in D.
    - https://math.stackexchange.com/questions/54601/looking-for-an-arrows-only-intro-to-category-theory
    - https://mathoverflow.net/questions/76083/categories-presented-with-arrows-only-no-objects-partial-monoids
- From category C, we can construct D (the arrow category of C; the category of the arrows of C):
    - https://ncatlab.org/nlab/show/arrow+category
    - Every arrow in C becomes an object in D.
    - What is an arrow in D?
        - https://math.stackexchange.com/questions/1366720/confused-about-the-arrow-category
    - C(X,Y) -> C(Y,Z)
- From category C, we can construct D (the endofunctor category of C):
    - Every endofunctor of C becomes an object in D.
    - What about the arrows?
