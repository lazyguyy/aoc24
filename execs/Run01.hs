import Day01

main :: IO ()
main = do
    test_input <- Day01.readInput "inputs/day01-test.in"
    real_input <- Day01.readInput "inputs/day01.in"
    putStrLn $ "Part 1, test input: " <> (show $ Day01.part1 test_input)
    putStrLn $ "Part 1, real input: " <> (show $ Day01.part1 real_input)
    putStrLn $ "Part 2, test input: " <> (show $ Day01.part2 test_input)
    putStrLn $ "Part 2, real input: " <> (show $ Day01.part2 real_input)
