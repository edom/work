#lang rackbol/private/prolog-like

;;  See family0.rkt for queries against this database.

(father 'abraham 'isaac)
(father 'abraham 'ishmael)
(mother 'sarah 'isaac)
(mother 'hagar 'ishmael)
(∀ (P C) [(parent_child P C) :- (father P C)])
(∀ (P C) [(parent_child P C) :- (mother P C)])
