module Dynasty.Main where

import qualified Control.Monad as M
import qualified Foreign as F

import qualified UI.HSCurses.Curses as C

import qualified Dynasty.Culture as A
import qualified Dynasty.Date as D
import qualified Dynasty.Display as E
import qualified Dynasty.Level as L
import qualified Dynasty.Person as P
import qualified Dynasty.Religion as R
import qualified Dynasty.State as S
import qualified Dynasty.Title as T
import qualified Dynasty.Trait as U

dynastyMain :: IO ()
dynastyMain = do
    window <- C.initScr
    if window == F.nullPtr
        then do
            putStrLn "This program does not support your terminal."
        else do
            C.echo False
            let state0 = S.exec initialize S.empty
            mainLoop window state0
            C.endWin

initialize :: S.StateM ()
initialize = do
    M.mapM_ S.newPersonWith $ concat
        [
            map ((irish . catholic) .) [
                named "Murchad" . countOf "Tuadhmhumhain" . dukeOf "Mumu"
            ]
            ,
            map ((angloSaxon . catholic) .)
            [
                named "Edgar" . kingOf "England"
                , named "Harold Godwinson" . countOf "Hereford" . kingOf "England"
            ]
            ,
            [
                envious . wroth . named "Noman" . countOf "Nocounty" . dukeOf "Noduchy" . kingOf "Nomanland"
                , kind . patient . named "Yesman" . countOf "Yescounty" . dukeOf "Yesduchy" . kingOf "Yesland"
                , female . named "Maywoman" . countOf "Maycounty"
            ]
        ]
    where
        irish p = p { P.culture = A.Irish }
        angloSaxon p = p { P.culture = A.AngloSaxon }
        named name p = p { P.name = name }
        countOf county p = p { P.titles = T.countOf county : P.titles p }
        dukeOf duchy p = p { P.titles = T.dukeOf duchy : P.titles p }
        kingOf kingdom p = p { P.titles = T.kingOf kingdom : P.titles p }
        catholic p = p { P.religion = R.Catholic }
        addTrait t p = p { P.traits = t : P.traits p }
        kind = addTrait U.Kind
        patient = addTrait U.Patient
        envious = addTrait U.Envious
        wroth = addTrait U.Wroth
        female p = p { P.sex = L.Female }

mainLoop :: C.Window -> S.State -> IO ()
mainLoop window = theRealMainLoop $ E.curses window

theRealMainLoop :: E.CharIo -> S.State -> IO ()
theRealMainLoop chario =
    loop
    where
        puts = E.puts chario
        getch = E.getch chario
        loop state = do
            let
                today = S.today state
                people = S.people state
                strPeople = unlines $ map (P.formatLong today) people
            C.erase
            puts $ "Dynasty Simulator  Day " ++ D.print today ++ "\n"
            puts "Keyboard:  q Quit  n Next day\n"
            puts $ "People:\n" ++ strPeople
            C.refresh
            key <- getch
            let
                quit = maybe False (E.isChar 'q') key
                nextDay = maybe False (E.isChar 'n') key
                state0 =
                    id
                    . (if nextDay then S.incrementDate else id)
                    $ state
            M.unless quit $ loop state0
