module Dynasty.Main where

import qualified Control.Monad as M
import qualified Data.List as L
import qualified Foreign as F

import qualified UI.HSCurses.Curses as C

import qualified Dynasty.Display as E
import qualified Dynasty.Person as P
import qualified Dynasty.State as S
import qualified Dynasty.Title as T

dynastyMain :: IO ()
dynastyMain = do
    window <- C.initScr
    if window == F.nullPtr
        then do
            putStrLn "This program does not support your terminal."
        else do
            C.echo False
            mainLoop window S.initialState
                {
                    S.people =
                    [
                        P.empty { P.name = "Murchad", P.titles = [T.dukeOf "Mumu", T.countOf "Tuadhmhumhain"] }
                        , P.empty { P.name = "Who", P.titles = [T.countOf "Urmhumhain"] }
                        , P.empty { P.name = "Who", P.titles = [T.countOf "Deasmhumhain"] }
                        , P.empty { P.name = "Who", P.titles = [T.countOf "Osraige"] }
                        , P.empty { P.name = "Who", P.titles = [T.countOf "Cill Dara"] }
                        , P.empty { P.name = "Who", P.titles = [T.countOf "Breitne"] }
                        , P.empty { P.name = "Who", P.titles = [T.countOf "Connachta"] }
                        , P.empty { P.name = "Who", P.titles = [T.countOf "Laigin"] }
                        , P.empty { P.name = "Who", P.titles = [T.countOf "Dubhlinn"] }
                        , P.empty { P.name = "Who", P.titles = [T.countOf "Ulster"] }
                        , P.empty { P.name = "Who", P.titles = [T.countOf "Tir Eoghain"] }
                        , P.empty { P.name = "Who", P.titles = [T.countOf "Tir Chonaill"] }
                        , P.empty { P.name = "Domnall" }
                        , P.empty { P.name = "Donnchad" }
                        , P.empty { P.name = "Énna" }
                        , P.empty { P.name = "Diarmaid" }
                        , P.empty { P.name = "Tadg" }
                        , P.empty { P.name = "Conchobar" }
                        , P.empty { P.name = "Eoghan" }
                        , P.empty { P.name = "John" }
                        , P.empty { P.name = "Edward" }
                        , P.empty { P.name = "Henry" }
                    ]
                }
            C.endWin

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
                day = S.day state
                people = S.people state
                strPeople = unlines $ flip map people $ \ p ->
                    P.name p
                    ++ concat (", " : L.intersperse ", " (P.formattedTitlesOf p))
                    ++ ", born at day " ++ show (P.born p)
                    ++ ", " ++ show (day - P.born p) ++ " days old"
            C.erase
            puts "Dynasty Simulator\n"
            puts $ "Day " ++ show day ++ "\n"
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
