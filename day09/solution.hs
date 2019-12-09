import Common
import IntCode

import Control.Exception
import Data.Time

main = do
    start <- getCurrentTime
    content <- getInputFromCommaList
    let program = parseProgram content
    let solution1 = run (initState [1] program)
    let solution2 = run (initState [2] program)
    print $ solution1
    print $ solution2
    end <- getCurrentTime

    print $ "Time taken: " ++ show (diffUTCTime end start)
    
-- Solution 1: 3546494377
-- Solution 2: 47253