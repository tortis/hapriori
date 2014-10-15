import System.IO

genout ents out = out ++ genout (readcust ents)

readcust ents
    | head (head ents)

readcusthlp ents cid out
    | head (head ents) == cid = out ++ 

main = do
    contents <- readFile "h.dat"
    outfile <- openFile "h2.dat" WriteMode
    let l = lines contents

    let ent = map words l
    [ head (tail a) | a <-
    hClose outfile
