
aoc1 = undefined

main = do
  args <- getArgs
  let selector = args
  let selector = (read $ args !! 0) :: Int
  putStrLn $ show selector
  return ()
