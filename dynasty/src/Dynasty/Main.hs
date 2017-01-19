{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS -fno-warn-name-shadowing #-}

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
import qualified Dynasty.Person as P
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
            erase
            puts $ "Dynasty Simulator  Today is " ++ D.print today ++ "\n"
            puts "Keyboard:  q Quit  n Next day  p Show all people\n"
            you <- SM.playerChar
            longYou <- M.mapM SM.formatPersonLong you
            puts $ unlines $ "\nYou are playing as:\n" : longYou
            people <- SM.people
            puts "Events at the beginning of today:\n\n"
            M.forM_ people $ \ p -> do
                let id = P.id p
                R.probM 0.5 $ do
                    puts $ "Character " ++ show id ++ " gained 1 Diplomacy.\n"
                    SM.modifyPerson id $ Q.addDiplomacy 1
                R.probM 0.5 $ do
                    puts $ "Character " ++ show id ++ " gained 1 Stewardship.\n"
                    SM.modifyPerson id $ Q.addStewardship 1
            refresh
            key <- getch
            case key >>= E.asChar of
                Just c -> case c of
                    'q' -> return ()
                    'n' -> SM.incrementDate >> loop
                    'p' -> showAllPeople >> loop
                    _ -> loop
                _ -> loop
        showAllPeople = do
            people <- SM.people
            strPeople <- unlines <$> M.mapM SM.formatPersonLong people
            erase
            puts "Keyboard:  q Back\n"
            puts $ "People:\n" ++ strPeople
            key <- getch
            case key >>= E.asChar of
                Just c -> case c of
                    'q' -> return ()
                    _ -> showAllPeople
                _ -> showAllPeople
