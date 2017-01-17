module Dynasty.Main where

import qualified Control.Monad as M
import qualified Data.List as L
import qualified Foreign as F

import qualified UI.HSCurses.Curses as C

import qualified Dynasty.Culture as A
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
            let state0 = flip S.exec S.initialState $ do
                M.mapM_ S.newPersonWith
                    [
                        \ p -> p { P.name = "Murchad", P.culture = A.Irish, P.titles = [T.dukeOf "Mumu", T.countOf "Tuadhmhumhain"] }
                        , \ p -> p { P.name = "Who", P.titles = [T.countOf "Urmhumhain"] }
                        , \ p -> p { P.name = "Who", P.titles = [T.countOf "Deasmhumhain"] }
                        , \ p -> p { P.name = "Who", P.titles = [T.countOf "Osraige"] }
                        , \ p -> p { P.name = "Who", P.titles = [T.countOf "Cill Dara"] }
                        , \ p -> p { P.name = "Who", P.titles = [T.countOf "Breitne"] }
                        , \ p -> p { P.name = "Who", P.titles = [T.countOf "Connachta"] }
                        , \ p -> p { P.name = "Who", P.titles = [T.countOf "Laigin"] }
                        , \ p -> p { P.name = "Who", P.titles = [T.countOf "Dubhlinn"] }
                        , \ p -> p { P.name = "Who", P.titles = [T.countOf "Ulster"] }
                        , \ p -> p { P.name = "Who", P.titles = [T.countOf "Tir Eoghain"] }
                        , \ p -> p { P.name = "Who", P.titles = [T.countOf "Tir Chonaill"] }
                        , \ p -> p { P.name = "Domnall" }
                        , \ p -> p { P.name = "Donnchad" }
                        , \ p -> p { P.name = "Énna" }
                        , \ p -> p { P.name = "Diarmaid" }
                        , \ p -> p { P.name = "Tadg" }
                        , \ p -> p { P.name = "Conchobar" }
                        , \ p -> p { P.name = "Eoghan" }
                        , \ p -> p { P.name = "John" }
                        , \ p -> p { P.name = "Edward" }
                        , \ p -> p { P.name = "Henry" }
                    ]
            mainLoop window state0
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
                    show (P.id p)
                    ++ " " ++ P.name p
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
