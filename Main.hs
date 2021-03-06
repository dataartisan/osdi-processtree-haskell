import Data.List
import qualified Data.IntMap as IntMap
import Data.IntMap ((!))
import Data.Maybe

type Proc = (Int, Int, String)
    
parser :: String -> String -> (Int, Proc)
parser header = 
  let cols = words header 
      iPid = fromJust $ elemIndex "PID" cols
      iPpid = fromJust $ elemIndex "PPID" cols
      iCmd = fromJust $ findIndex (flip elem ["CMD", "COMMAND"]) cols
  in
    \line -> let ws = words line ; pid = read $ ws !! iPid in
               (pid, (pid, read $ ws !! iPpid, concat $ drop iCmd ws))

printProcessTree :: [String] -> [String]
printProcessTree (header : lines) = 
  let 
    ps = map (parser header) lines
    pmap = IntMap.fromList ps
    tmap = IntMap.fromListWith (++) $ map (\(_, (pid, ppid, _)) -> (ppid, [pid])) ps
    showTrees l i = concatMap (showTree l) (tmap ! i)
    showTree l i = ((concat $ replicate l " ") ++ (show i) ++ ": " ++ (case pmap ! i of (_, _, cmd) -> cmd)) :
                     if IntMap.member i tmap then showTrees (l + 1) i else []
  in
    showTrees 0 0

main :: IO()
main = interact $ unlines . printProcessTree . lines
