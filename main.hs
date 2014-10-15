import System.Environment
import Apriori

main = do   
    args <- getArgs
    let file = head args
    let st = head $ tail args
    let t = read st :: Float
    ds <- load_file file
    print $ apriori ds t
