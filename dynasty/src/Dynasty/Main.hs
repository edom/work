{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS -fno-warn-name-shadowing #-}

module Dynasty.Main
(
    dynastyMain
)
where

import qualified Control.Monad as M
import qualified Data.Maybe as N
import qualified Foreign as F

import qualified UI.HSCurses.Curses as C

import qualified Dynasty.Date as D
import qualified Dynasty.Display as E
import qualified Dynasty.Event as G
import qualified Dynasty.Init as I
import qualified Dynasty.Person as P
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
    SM.runStateT_ beginDay
    where
        puts = E.puts chario
        getch = E.getch chario
        erase = E.erase chario
        refresh = E.refresh chario
        beginDay = do
            people <- SM.people
            events <- makeEventsFor people
            happenings <- N.catMaybes <$> M.mapM G.roll events
            midDay happenings
        midDay events = do
            today <- SM.today
            erase
            puts $ "Dynasty Simulator  Today is " ++ D.print today ++ "\n"
            puts "Keyboard:  q Quit  n Next day  p Show all people\n"
            you <- SM.playerChar
            longYou <- M.mapM SM.formatPersonLong you
            puts $ unlines $ "\nYou are playing as:\n" : longYou
            puts "Events happened today:\n\n"
            M.forM_ events $ \ event -> do
                puts $ G.message event ++ "\n"
            refresh
            key <- getch
            case key >>= E.asChar of
                Just c -> case c of
                    'q' -> return ()
                    'n' -> SM.incrementDate >> beginDay
                    'p' -> showAllPeople >> midDay events
                    _ -> midDay events
                _ -> midDay events
        makeEventsFor people =
            return $ personalEvents ++ pairEvents
            where
                personalEvents = flip concatMap people $ \ p ->
                    [
                        G.prob (1/4) $ G.addDiplomacy 1 p
                        , G.prob (1/4) $ G.addStewardship 1 p
                    ]
                pairs = [ (p, q) | p <- people, q <- people, P.id p /= P.id q ]
                pairEvents = flip concatMap pairs $ \ (p, q) ->
                    let
                        i = P.id p
                        j = P.id q
                    in
                        [
                            G.prob (1/16)
                            . G.setMessage ("Character " ++ show i ++ "'s opinion of character " ++ show j ++ " improves by 10 until 1066-01-01. (Not implemented.)")
                            $ G.empty
                        ]
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
