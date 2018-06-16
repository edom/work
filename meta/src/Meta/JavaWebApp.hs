module Meta.JavaWebApp where

import qualified Meta.Maven as M
import qualified Meta.Relat as R
import qualified Meta.Web as W

type Group_id = M.Group_id

type Artifact_id = M.Artifact_id

type Version = M.Version

type Dep_ver = M.Dep_ver

type Package_name = String

data App
    = MkApp {
        _project :: M.Project
        , aTables :: [R.Table]
        , aPkg :: Package_name
        , _site :: W.Site
    } deriving (Read, Show)

empty :: App
empty = MkApp {
        _project = M.empty
        , aTables = []
        , aPkg = ""
        , _site = W.empty
    }

set_tables :: [R.Table] -> App -> App
set_tables ts a = a { aTables = ts }

set_project :: M.Project -> App -> App
set_project p a = a { _project = p }

set_gav :: Group_id -> Artifact_id -> Version -> App -> App
set_gav g a v app = set_project (M.set_gav g a v $ _project app) app

type Dep = M.Dep

set_deps :: [Dep] -> App -> App
set_deps deps app = set_project (M.set_deps deps $ _project app) app

dep_provided :: Group_id -> Artifact_id -> Dep_ver -> Dep
dep_provided = M.dep_provided

-- * Pages

type Page_url = W.Url

type Page_content = W.Content

add_page :: Page_url -> Page_content -> App -> App
add_page url con app = app { _site = W.add_page url con (_site app) }
