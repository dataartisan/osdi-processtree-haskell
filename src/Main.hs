import Data.List
import qualified Data.IntMap as IntMap
import Data.IntMap ((!))
import Data.Maybe

data Proc = Proc(Int, Int, String)
getCmd (Proc(_, _, cmd)) = cmd
    
parser :: String -> String -> Proc
parser header = 
  let cols = words header 
      iPid = fromJust $ elemIndex "PID" cols
      iPpid = fromJust $ elemIndex "PPID" cols
      iCmd = fromJust $ findIndex (flip elem ["CMD", "COMMAND"]) cols
  in
    \line -> let ws = words line in
      Proc(read $ ws !! iPid, read $ ws !! iPpid, concat $ drop iCmd ws)

printProcessTree :: [String] -> [String]
printProcessTree (header : lines) = 
  let procs = map (parser header) lines
      pmap = IntMap.fromList $ map (\p @ (Proc(pid, _, _)) -> (pid, p)) procs
      tmap = IntMap.fromListWith (++) $ map (\p @ (Proc(pid, ppid, _)) -> (ppid, [pid])) procs
      showTrees l i = concatMap (showTree l) (tmap ! i)
      showTree l i = ((concat $ replicate l " ") ++ (show i) ++ ": " ++ (getCmd $ pmap ! i)) :
                     if IntMap.member i tmap then showTrees (l + 1) i else []
  in
    showTrees 0 0

main :: IO()
main = interact $ unlines . printProcessTree . lines
