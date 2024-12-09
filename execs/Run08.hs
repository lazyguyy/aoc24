import Day08

import qualified Data.Text.IO as T
import Data.List (intercalate)

main :: IO ()
main = do
    test_input <- T.readFile "inputs/day08-test.in"
    real_input <- T.readFile "inputs/day08.in"
    putStrLn $ "test input\n" <> (intercalate "\n" $ Day08.solve test_input)
    putStrLn $ "real input\n" <> (intercalate "\n" $ Day08.solve real_input)
