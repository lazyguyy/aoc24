import Day03

main :: IO ()
main = do
    test_input <- Day03.readInput "inputs/day03-test.in"
    test_input2 <- Day03.readInput "inputs/day03-test2.in"
    real_input <- Day03.readInput "inputs/day03.in"
    putStrLn $ "Part 1, test input: " <> (show $ Day03.part1 test_input)
    putStrLn $ "Part 1, real input: " <> (show $ Day03.part1 real_input)
    putStrLn $ "Part 2, test input: " <> (show $ Day03.part2 test_input2)
    putStrLn $ "Part 2, real input: " <> (show $ Day03.part2 real_input)
