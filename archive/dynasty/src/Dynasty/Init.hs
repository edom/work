module Dynasty.Init where

import qualified Control.Monad as M

import Dynasty.Person.Modify

import qualified Dynasty.State.Monad as SM

initialize :: (SM.MonadState m) => m ()
initialize = do
    M.mapM_ SM.newPersonWith $ concat
        [
            map ((irish . catholic) .) [
                named "Murchad mac Donnchad Ua Briain" . born 1024 1 1 . countOf "Tuadhmhumhain" . dukeOf "Mumu"
            ]
            ,
            map ((danish . catholic) .)
            [
                named "Harthacnut" . born 1018 1 1 . died 1042 6 8 . kingOf "England"
            ]
            ,
            map ((angloSaxon . catholic) .)
            [
                named "Edward the Confessor" . born 1003 1 1 . kingOf "England"
                , named "Harold Godwinson" . born 1022 1 1 . countOf "Hereford" . kingOf "England"
            ]
            ,
            [
                kind . patient . named "Yesman" . countOf "Yescounty" . dukeOf "Yesduchy" . kingOf "Yesland"
                , female . named "Maywoman" . countOf "Maycounty"
            ]
        ]
    noman <- SM.newPersonWith $
        envious . wroth . named "Noman" . born 1020 4 21
        . countOf "Nocounty" . dukeOf "Noduchy" . kingOf "Nomanland"
    nowoman <- SM.newPersonWith $
        kind . patient . female . named "Nowoman" . born 1022 9 15
        . kingOf "Nowomanland"
    _ <- SM.marry noman nowoman
    _nomansson <- SM.newPersonWith $
        named "Noman II Nomansson" . born 1045 3 8
        . fatheredBy noman . motheredBy nowoman
        . countOf "Nowomanland"
    return ()
