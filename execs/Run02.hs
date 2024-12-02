import Day02

main :: IO ()
main = do
    test_input <- Day02.readInput "inputs/day02-test.in"
    real_input <- Day02.readInput "inputs/day02.in"
    putStrLn $ "Part 1, test input: " <> (show $ Day02.part1 test_input)
    putStrLn $ "Part 1, real input: " <> (show $ Day02.part1 real_input)
    putStrLn $ "Part 2, test input: " <> (show $ Day02.part2 test_input)
    putStrLn $ "Part 2, real input: " <> (show $ Day02.part2 real_input)
