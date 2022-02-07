import Data.List

type Conjunto = [Int]
type Uniao = [Conjunto]

criaConj :: Int -> Int -> Bool -> Bool -> IO Uniao
criaConj a b c d = do
    return [[a+(fromEnum c), a+(fromEnum c)+1 .. b-(fromEnum d)]]

auxiliacontem :: Int -> Conjunto -> Bool
auxiliacontem x y
    |x `elem` y = True
    |otherwise = False
contem :: Int -> Int -> Uniao -> Bool -- Primeiro "Int" é o inicializador e deve sempre ter entrada 0
contem x y z
    |auxiliacontem y (z!!x) == True = True
    |auxiliacontem y (z!!x) == False && x+1 < length z = contem (x+1) (y) (z)
    |otherwise = False

intercepta :: Uniao -> Uniao -> Bool
intercepta x y
    |contem 0 (head (head x)) y == True = True
    |contem 0 (head (head y)) x == True = True
    |contem 0 (last (last x)) y == True = True
    |contem 0 (last (last y)) x == True = True
    |otherwise = False
    
media :: Int -> Int -> Conjunto -> Float
media _ _ [] = 0
media x y z
    |y == length z = (fromIntegral x)/(fromIntegral (length z))
    |otherwise = media (x+(z!!y)) (y+1) (z)
    
produto :: Uniao -> Uniao -> IO Uniao
produto x y = do
    let min = minimum [head (head x) * head (head y), head (head x) * last (last y), last (last x) * head (head y), last (last x) * last (last y)]
    let max = maximum [head (head x) * head (head y), head (head x) * last (last y), last (last x) * head (head y), last (last x) * last (last y)]
    return [[min, min+1 .. max]]
    
uniao :: Uniao -> Uniao -> IO Uniao
uniao x y = do
    let aux = intercepta x y
    let low = minimum [head (head x), head (head y)]
    let high = maximum [last (last x), last (last y)]
    if aux == True then return [[low, low+1 .. high]] else return (sort (x ++ y))

addMe :: Integer -> Integer -> Integer
addMe x y = x + y

main :: IO ()
main =  do

a <- criaConj 0 10 False True
print(a)
b <- criaConj 5 30 True True
print(b)
c <- criaConj 40 50 False False
print(c)
d <- uniao b a
print(d)
e <- uniao c d
print(e)
f <- criaConj 25 45 True False
print(f)
g <- uniao e f
print(g)
