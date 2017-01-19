{-# OPTIONS -fno-warn-missing-signatures #-}

{- |
'P.Person' endofunctions and factories of such endofunctions.
'P.Person' builders, for people coming from Java.

You make things of type @'P.Person' -> 'P.Person'@, compose them,
and apply them to a 'P.Person' like 'P.empty', for example.

For example, the following value describes the function
that will transform the empty person into
an Anglo-Saxon Catholic male named Harold Godwinson
who got born on 1 January 1022 and died on 14 October 1066.
(The actual birth date is unknown.)

@
'angloSaxon' . 'catholic' . 'male' . 'named' \"Harold Godwinson\" . 'born' 1022 1 1 . 'died' 1066 10 14
@
-}
module Dynasty.Person.Modify where

import qualified Dynasty.Culture as A
import qualified Dynasty.Date as D
import qualified Dynasty.Level as L
import qualified Dynasty.Person as P
import qualified Dynasty.Religion as R
import qualified Dynasty.Title as T
import qualified Dynasty.Trait as U

-- * Name

named name p = p { P.name = name }

-- * Sex

male p = p { P.sex = L.Male }

female p = p { P.sex = L.Female }

-- * Birth and death date

born y m d p = p { P.born = D.fromYmd y m d }
died y m d p = p { P.died = Just $ D.fromYmd y m d }

-- * Parent-child relationship

type Father = P.Person
type Mother = P.Person
type Child = P.Person

fatheredBy :: Father -> Child -> P.Person
fatheredBy papa child = child { P.fatherId = Just $ P.id papa }

motheredBy :: Mother -> Child -> P.Person
motheredBy mama child = child { P.motherId = Just $ P.id mama }

-- * Culture

angloSaxon p = p { P.culture = A.AngloSaxon }
danish p = p { P.culture = A.Danish }
irish p = p { P.culture = A.Irish }

-- * Religion

catholic p = p { P.religion = R.Catholic }

-- * Title

countOf county p = p { P.titles = T.countOf county : P.titles p }
dukeOf duchy p = p { P.titles = T.dukeOf duchy : P.titles p }
kingOf kingdom p = p { P.titles = T.kingOf kingdom : P.titles p }

-- * Trait

addTrait t p = p { P.traits = t : P.traits p }
kind = addTrait U.Kind
patient = addTrait U.Patient
envious = addTrait U.Envious
wroth = addTrait U.Wroth

addDiplomacy d p = p { P.diplomacy = P.diplomacy p + d }
addStewardship d p = p { P.stewardship = P.stewardship p + d }
