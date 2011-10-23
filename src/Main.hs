module Main where

import qualified Data.IntSet as IntSet
import Data.List
import qualified Data.IntMap as IntMap
import Data.IntMap ((!))
import Data.Maybe

data Proc = Proc(Int, Int, String)
getPid (Proc(pid, _, _)) = pid
getPpid (Proc(_, ppid, _)) = ppid
getCmd (Proc(_, _, cmd)) = cmd
instance Show Proc where
  show (Proc(pid, ppid, cmd)) = 
    "Proc(" ++ (show pid) ++ ", " ++ (show ppid) ++ ", " ++ cmd ++ ")"
    
parser :: String -> String -> Proc
parser header = 
  let cols = words header 
      iPid = fromJust $ elemIndex "PID" cols
      iPpid = fromJust $ elemIndex "PPID" cols
      iCmd = fromJust $ findIndex (flip elem ["CMD", "COMMAND"]) cols
  in
    \line -> let ws = words line in
    Proc(read $ ws !! iPid, read $ ws !! iPpid, concat $ drop iCmd ws)

main :: IO()
main = do
  header <- getLine
  contents <- getContents
  let procs = map (parser header) (lines contents)
      -- map from process IDs to processes
      pmap = IntMap.fromList $ map (\p -> (getPid p, p)) procs
      -- map from parent PIDs to their child PIDs
      tmap = IntMap.fromListWith IntSet.union $ 
             map (\p -> (getPpid p, IntSet.singleton $ getPid p)) procs
      -- show all subtrees for a given PID
      showTrees l i = map (showTree l) (IntSet.toList $ tmap ! i)
      -- show a single tree with its subtrees
      showTree l i = (replicate l " ") ++ [show i, ": ", getCmd $ pmap ! i, "\n"] ++ 
                     if IntMap.member i tmap then concat $ showTrees (l + 1) i else []
  putStr $ concat $ concat $ showTrees 0 0
