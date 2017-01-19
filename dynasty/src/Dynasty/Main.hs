{-# LANGUAGE NoMonomorphismRestriction #-}

module Dynasty.Main
(
    dynastyMain
)
where

import qualified Control.Monad as M
import qualified Foreign as F

import qualified UI.HSCurses.Curses as C

import qualified Dynasty.Date as D
import qualified Dynasty.Display as E
import qualified Dynasty.Init as I
import qualified Dynasty.Person.Modify as Q
import qualified Dynasty.Random as R
import qualified Dynasty.State as S
import qualified Dynasty.State.Monad as SM

dynastyMain :: IO ()
dynastyMain = do
    window <- C.initScr
    if window == F.nullPtr
        then do
            putStrLn "This program does not support your terminal."
        else do
            C.echo False
            let state0 = SM.exec I.initialize S.empty
            mainLoop window state0
            C.endWin

mainLoop :: C.Window -> S.State -> IO ()
mainLoop window = theRealMainLoop $ E.curses window

theRealMainLoop :: E.CharIo -> S.State -> IO ()
theRealMainLoop chario =
    SM.runStateT_ loop
    where
        puts = E.puts chario
        getch = E.getch chario
        erase = E.erase chario
        refresh = E.refresh chario
        loop = do
            today <- SM.today
            people <- SM.people
            strPeople <- unlines <$> M.mapM SM.formatPersonLong people
            erase
            puts $ "Dynasty Simulator  Day " ++ D.print today ++ "\n"
            puts "Keyboard:  q Quit  n Next day\n"
            R.probM 0.5 $ do
                puts $ "Character 0 gained 1 Diplomacy.\n"
                SM.modifyPerson 0 $ Q.addDiplomacy 1
            R.probM 0.5 $ do
                puts "Character 0 gained 1 Stewardship.\n"
                SM.modifyPerson 0 $ Q.addStewardship 1
            you <- SM.playerChar
            longYou <- M.mapM SM.formatPersonLong you
            puts $ unlines $ "\nYou are playing as:\n" : longYou
            puts $ "People:\n" ++ strPeople
            refresh
            key <- getch
            let
                quit = maybe False (E.isChar 'q') key
                nextDay = maybe False (E.isChar 'n') key
            M.when nextDay SM.incrementDate
            M.unless quit loop
