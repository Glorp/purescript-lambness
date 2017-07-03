module Main where

import Lambs.Parse as Parse
import Data.Maybe (Maybe(..))
import Lambs.Define (Definitions, noDefinitions, addDef, removeDef, substDefs, Define(..), Undefine(..))
import Lambs.Eval (Exec(Rename, Reduce, Normal), stepExec)
import Lambs.Toplevel (Toplevel(..))
import Lambs.Unparse (unparse, unparseDefs)
import Prelude ((<>))

noDefs :: Definitions
noDefs = noDefinitions

parse :: String -> Maybe Toplevel
parse s = Parse.parseTop s

updateDefs :: Maybe Toplevel -> Definitions -> Definitions
updateDefs (Just (Undef x)) d = removeDef x d
updateDefs (Just (Def x)) d = addDef x d
updateDefs _ d = d

step :: Maybe Toplevel -> String
step Nothing = ":("
step (Just (Def (Define s _))) = s <> " is defined :)"
step (Just (Undef (Undefine s))) = s <> " is undefined :o"
step (Just (Trm term)) = execStr (Just (stepExec term))

step1 :: Maybe Toplevel -> Maybe Exec
step1 (Just (Trm term)) = Just (stepExec term)
step1 _ = Nothing

stepnhalp :: Exec -> Maybe Exec
stepnhalp (Normal _) = Nothing
stepnhalp t = Just t

stepn :: Maybe Exec -> Maybe Exec
stepn (Just (Reduce _ t)) = stepnhalp (stepExec t)
stepn (Just (Rename _ _ _ t)) = stepnhalp (stepExec t)
stepn _ = Nothing


isJust :: forall a. Maybe a -> Boolean
isJust Nothing = false
isJust _ = true

execStr :: Maybe Exec -> String
execStr (Just (Reduce _ t)) = unparse t
execStr (Just (Rename x _ y t)) = unparse t <> " | [" <> y <> "/" <> x <> "]"
execStr (Just (Normal t)) = unparse t
execStr Nothing = "nope"


renameDefs :: Definitions -> Maybe Toplevel -> String
renameDefs d Nothing = ":("
renameDefs d (Just (Trm t)) = unparse (substDefs d t)
renameDefs d (Just (Def (Define s t))) = s <> " ≜ " <> unparse (substDefs d t)
renameDefs d (Just (Undef (Undefine s))) = s <> " ≜ :("

defsString :: Definitions -> String
defsString = unparseDefs
