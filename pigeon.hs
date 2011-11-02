import Data.List
import Control.Applicative
import System.Environment

p a b = "p" ++ show a ++ "l" ++ show b
nponp i j k = "(~p" ++ show i ++ "l" ++ show k ++ "+~p" ++ show j ++ "l" ++ show k ++ ")"

ci op = (++ ")") . ('(':) . concat . intersperse op

left n = ci "*" $ filter (/=[]) $ map (\i -> ci "+" $ map (p i) [1 .. n]) [1 .. n + 1]
right n = ci "*" $ map (ci "*") $ filter (/=[]) $  map (\i -> map (\j -> ci "*" $ map (nponp i j) [1 .. n]) [i + 1 .. n + 1]) [1 .. n + 1]

pigeon n = left n ++ "  *  " ++ right n

main = do
    n <- read <$> head <$> getArgs
    putStrLn $ pigeon n
