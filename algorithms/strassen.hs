addMat :: [[Int]] -> [[Int]] -> [[Int]]
addMat a b = zipWith (zipWith (+)) a b

subMat :: [[Int]] -> [[Int]] -> [[Int]]
subMat a b = zipWith (zipWith (-)) a b

splitMat :: [[Int]] -> ([[Int]], [[Int]], [[Int]], [[Int]])
splitMat mat = (a, b, c, d)
    where n = length mat
          mid = n `div` 2
          a = take mid (map (take mid) mat)
          b = take mid (map (drop mid) mat)
          c = drop mid (map (take mid) mat)
          d = drop mid (map (drop mid) mat)

joinQuadrants :: [[Int]] -> [[Int]] -> [[Int]] -> [[Int]] -> [[Int]]
joinQuadrants a b c d = [aRow ++ bRow | (aRow, bRow) <- zip a b] ++ [cRow ++ dRow | (cRow, dRow) <- zip c d]

strassen :: [[Int]] -> [[Int]] -> [[Int]]
strassen a b
    | n == 1 = [[ (head (head a)) * (head (head b)) ]]
    | otherwise = joinQuadrants c11 c12 c21 c22
    where
        n = length a
        (a11, a12, a21, a22) = splitMat a
        (b11, b12, b21, b22) = splitMat b

        m1 = strassen (addMat a11 a22) (addMat b11 b22)
        m2 = strassen (addMat a21 a22) b11
        m3 = strassen a11 (subMat b12 b22)
        m4 = strassen a22 (subMat b21 b11)
        m5 = strassen (addMat a11 a12) b22
        m6 = strassen (addMat a21 a22) (addMat b11 b12)
        m7 = strassen (subMat a12 a22) (addMat b21 b22)

        c11 = subMat (addMat m1 m4) (subMat m5 m7)
        c12 = addMat m3 m5
        c21 = addMat m2 m4
        c22 = addMat (subMat m1 m3) m6

-- Helper function to print matrices nicely
printMat :: [[Int]] -> IO ()
printMat matrix = mapM_ (putStrLn . unwords . map show) matrix

main :: IO ()
main = do
    -- Define two 2x2 matrices
    let a = [[1, 2],
             [3, 4]]
    let b = [[5, 6],
             [7, 8]]

    -- Compute matrix multiplication using Strassen's algorithm
    let c = strassen a b

    -- Print the result
    putStrLn "Result of matrix multiplication:"
    printMat c