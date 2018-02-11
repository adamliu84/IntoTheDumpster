import Data.List

data Direction = X | Y | Z deriving (Show)
type Coordinate = (Int,Int,Int)
type RotateOnCoordinate = Coordinate -> Int -> [(Coordinate, Direction)]

connectionChildLength :: [Int]
connectionChildLength = [2,1,1,2,1,2,1,1,2,2,1,1,1,2,2,2,2]

-- TODO Refactor upon re-visit
rotateOnX :: Coordinate -> Int -> [(Coordinate, Direction)]
rotateOnX (ex,ey,ez) l = [((ex,ey-l,ez), Y), ((ex,ey+l,ez), Y), ((ex,ey,ez-l), Z), ((ex,ey,ez+l), Z)]
rotateOnY :: Coordinate -> Int -> [(Coordinate, Direction)]
rotateOnY (ex,ey,ez) l = [((ex-l,ey,ez), X), ((ex+l,ey,ez), X), ((ex,ey,ez-l), Z), ((ex,ey,ez+l), Z)]
rotateOnZ :: Coordinate -> Int -> [(Coordinate, Direction)]
rotateOnZ (ex,ey,ez) l = [((ex-l,ey,ez), X), ((ex+l,ey,ez), X), ((ex,ey-l,ez), Y), ((ex,ey+l,ez), Y)]
getRotateOnAxis :: Direction -> RotateOnCoordinate
getRotateOnAxis X = rotateOnX
getRotateOnAxis Y = rotateOnY
getRotateOnAxis Z = rotateOnZ
rotate :: Coordinate -> Direction -> Int -> [(Coordinate, Direction)]
rotate (x,y,z) prevDirection childLength = mmm (x,y,z) childLength
        where mmm = getRotateOnAxis prevDirection

getInterpolate :: Int -> Int -> [Int]
getInterpolate a a' = if a < a' then
                    [a..a']
                   else
                    reverse [a'..a]
getInterpolateOnAxis :: Direction -> Coordinate -> Coordinate -> [Coordinate]
getInterpolateOnAxis X (x,y,z) (x',y',z') = [(a,b,c) | b <- [y], c <- [z], a <- getInterpolate x x']
getInterpolateOnAxis Y (x,y,z) (x',y',z') = [(a,b,c) | b <- getInterpolate y y', c <- [z], a <- [x]]
getInterpolateOnAxis Z (x,y,z) (x',y',z') = [(a,b,c) | b <- [y], c <-getInterpolate z z', a <- [x]]

isWithinRange :: Coordinate -> Bool
isWithinRange (x,y,z) = and $ map (\a -> a `elem` [-1..1]) [x,y,z]

generateNewBank :: [(Coordinate)] -> Coordinate -> Direction -> [(Coordinate)]
generateNewBank [] coord _ = [coord]
generateNewBank bank coord prevDirection =
    bank ++ (tail $ getInterpolateOnAxis prevDirection lastCoord coord)
    where lastCoord = last bank

travelCubes :: [(Coordinate)] -> Coordinate -> Direction -> [Int] -> [[Coordinate]]
travelCubes bank c p [] = [generateNewBank bank c p]
travelCubes bank coord prevDirection (a:as) =
    concatMap (\(c,d) -> travelCubes newBank c d as) possibleCoord
    where newBank = generateNewBank bank coord prevDirection
          possibleCoord = filter (\(c,_) -> not $ c `elem` bank) $
                          filter (\(c,_) -> isWithinRange c) $
                          rotate coord prevDirection a

findCombination :: Coordinate -> Direction -> [Int] -> [[Coordinate]]
findCombination start_coord init_rotation child_length =
    nub $ travelCubes [] start_coord init_rotation child_length

main :: IO ()
main = do
    {-|
    Hacky method to just to confirm the diagram coordinates

    [COORDINATE REPRESENTATION]
    Center of cube is (0,0,0) therefore all 27 cubes require
    to be bound within the range of (+-1) of on XYZ axis

    [STARTING]
    Starting cube starting from (1,1,-1) -> (0,1,-1) -> .....
    Initial rotate only on XY axis (therefore previous rotateion on Z-axis)
    Else require mass mapping across 3 axis

    [ENDING]
    Ending cube end from .... -> (-1,-1,0) -> (-1,-1,1)
    --}

    let init_cube_start_coodinate = (1,1,-1)
        init_cube_rotation_axis = Z
        allPossibleCombination = findCombination init_cube_start_coodinate init_cube_rotation_axis connectionChildLength
        diagram_result = zip [1..] $
                 (
                    filter (\r -> take 2 (reverse r) == [(-1,-1,1), (-1,-1,0)]) $
                    filter (\r -> take 2 r == [(1,1,-1), (0,1,-1)]) $
                    allPossibleCombination
                 )!!0
                 
    -- mapM_ print $ (allPossibleCombination) -- Turn on this to see all combination
    -- Verify all possible combination have 27 blocks
    print "# of possible combination"
    print.length $ allPossibleCombination

    -- Verify all possible combination have 27 blocks
    print "All possible combination have 27 blocks"
    print $ and $ map (\x -> length x == 27) allPossibleCombination

    -- Verify against the diagram coordinates
    print "Diagram coordinate verification"
    mapM_ print diagram_result
