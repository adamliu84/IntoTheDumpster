-- OG: https://www.sciencebuddies.org/science-fair-projects/project-ideas/ArtificialIntelligence_p008/artificial-intelligence/machine-learning-maze?ytid=QUNM-QyM5PA&ytsrc=description
{-# LANGUAGE RecordWildCards #-}
import           Control.Monad      (when)
import           Data.Array.IO      (IOArray, MArray (newArray), readArray,
                                     writeArray)
import           Data.List          (maximumBy)
import           Data.List.NonEmpty as NonEmpty (fromList, head)
import           Data.Ord           (comparing)
import           System.Random      (randomRIO)

type Position = (Int, Int)
type Action = (Int, Int)

{-
Creating the Maze Environment
-}

data Maze = Maze
  { mazeLayout   :: [[Int]]
  , mazeHeight   :: Int
  , mazeWidth    :: Int
  , mazeStartPos :: (Int, Int)
  , mazeGoalPos  :: (Int, Int)
  }

genMaze :: [[Int]] -> Position -> Position -> Maze
genMaze layout start goal =
  Maze { mazeLayout = layout
       , mazeHeight = length layout
       , mazeWidth  = length (NonEmpty.head (NonEmpty.fromList layout))
       , mazeStartPos   = start
       , mazeGoalPos    = goal
       }

showMaze :: Maze -> IO ()
showMaze Maze{..} = mapM_ putStrLn [ [ cellChar (i, j) | j <- [0 .. mazeWidth - 1] ]
                             | i <- [0 .. mazeHeight - 1] ]
            where   cellChar :: Position -> Char
                    cellChar pos
                        | pos == mazeStartPos = 'S'
                        | pos == mazeGoalPos  = 'G'
                        | (mazeLayout !! i !! j) == 1 = '#'
                        | otherwise         = ' '
                        where (i, j) = pos

{-
Implementing the Agent
-}
actions :: [Action]
actions = [ (-1, 0)
          , (1, 0)
          , (0, -1)
          , (0, 1)
          ]

data QLearningAgent = QLearningAgent
  { qTable           :: IOArray (Int, Int, Int) Double
  , learningRate     :: Double
  , discountFactor   :: Double
  , explorationStart :: Double
  , explorationEnd   :: Double
  , numEpisodes      :: Int
  }

genQLearningAgent :: Maze -> Double -> Double -> Double -> Double -> Int -> IO QLearningAgent
genQLearningAgent Maze{..} lr df es ee epi = do
  let h = mazeHeight
      w = mazeWidth
  table <- newArray ((0,0,0), (h-1, w-1, 3)) 0.0
  return QLearningAgent { qTable = table
                        , learningRate = lr
                        , discountFactor = df
                        , explorationStart = es
                        , explorationEnd = ee
                        , numEpisodes = epi
                        }

getExplorationRate :: QLearningAgent -> Int -> Double
getExplorationRate QLearningAgent{..} currentEpisode =
  let episodes = fromIntegral numEpisodes
      current  = fromIntegral currentEpisode
  in explorationStart * ((explorationEnd / explorationStart) ** (current / episodes))

getAction :: QLearningAgent -> (Int, Int) -> Int -> IO Int
getAction agent@QLearningAgent{..} state currentEpisode = do
  let expRate = getExplorationRate agent currentEpisode
  r <- randomRIO (0.0, 1.0) :: IO Double
  if r < expRate
     then randomRIO (0, 3)
     else do
       qs <- mapM (\a -> readArray qTable (fst state, snd state, a)) [0..3]
       let (bestAction, _) = maximumBy (comparing snd) (zip [0..3] qs)
       return bestAction

updateQTable :: QLearningAgent -> (Int, Int) -> Int -> (Int, Int) -> Double -> IO ()
updateQTable agent@QLearningAgent{..} state action nextState reward = do
  currentQ <- readArray qTable (fst state, snd state, action)
  nextQs   <- mapM (\a -> readArray qTable (fst nextState, snd nextState, a)) [0..3]
  let bestNextQ = maximum nextQs
      newQ = currentQ + learningRate * (reward + discountFactor * bestNextQ - currentQ)
  writeArray qTable (fst state, snd state, action) newQ

{-
Defining the Reward System
-}
goalReward :: Double
goalReward  = 100
wallPenalty :: Double
wallPenalty = -10
stepPenalty :: Double
stepPenalty = -1

finishEpisode :: QLearningAgent -> Maze -> Int -> Bool -> IO (Double, Int, [(Int, Int)])
finishEpisode agent Maze{..} currentEpisode train = loop mazeStartPos 0 0 []
  where
    h = mazeHeight
    w = mazeWidth
    layout = mazeLayout
    loop :: (Int, Int) -> Double -> Int -> [(Int, Int)] -> IO (Double, Int, [(Int, Int)])
    loop currentState episodeReward episodeStep path = do
      let (i, j) = currentState
      action <- getAction agent currentState currentEpisode
      let (di, dj) = actions !! action
          nextState@(ni, nj) = (i + di, j + dj)
          outOfBounds = ni < 0 || ni >= h || nj < 0 || nj >= w
          wall = not outOfBounds && (layout !! ni !! nj == 1)
      let (reward, actualNextState, newPath, done) =
            if outOfBounds || wall
              then (wallPenalty, currentState, path, False)
              else if nextState == mazeGoalPos
                     then (goalReward, nextState, path ++ [currentState], True)
                     else (stepPenalty, nextState, path ++ [currentState], False)
      when train $ updateQTable agent currentState action actualNextState reward
      let newEpisodeReward = episodeReward + reward
          newEpisodeStep = episodeStep + 1
      if done
         then return (newEpisodeReward, newEpisodeStep, newPath)
         else loop actualNextState newEpisodeReward newEpisodeStep newPath

{-
Test Agent
-}
testAgent :: QLearningAgent -> Maze -> Int -> IO (Int, Double)
testAgent agent maze numEpisode = do
  (episodeReward, episodeStep, path) <- finishEpisode agent maze numEpisode False
  putStrLn "Learned Path:"
  mapM_ (\(r,c) -> putStr $ "(" ++ show r ++ ", " ++ show c ++ ") -> ") path  --NOTE: Not same as .py. I am referring using row as fst -> col as snd in Haskell version
  putStrLn "Goal!"
  putStrLn $ "Number of steps: " ++ show episodeStep
  putStrLn $ "Total reward: " ++ show episodeReward
  return (episodeStep, episodeReward)

{-
Train Agent
-}
trainAgent :: QLearningAgent -> Maze -> Int -> IO ()
trainAgent agent maze numEps = do
  results <- mapM (\episode -> finishEpisode agent maze episode True) [0 .. numEps - 1]
  let rewards = map (\(r,_,_) -> r) results
      steps   = map (\(_,s,_) -> s) results
      avgReward = sum rewards / fromIntegral numEps
      avgSteps  = fromIntegral (sum steps) / fromIntegral numEps
  putStrLn $ "The average reward is: " ++ show avgReward
  putStrLn $ "The average steps is: " ++ show avgSteps

{-
Main
-}
main :: IO ()
main = do
    let (m,s,g) = simpleMaze -- complexMaze
        maze = genMaze m s g

    -- Show maze
    showMaze maze
    -- Generate agent
    agent <- genQLearningAgent maze 0.1 0.9 1.0 0.01 100
    -- No training
    testAgent agent maze 0
    -- Training
    trainAgent agent maze 100
    -- Test after training
    testAgent agent maze 100

    return ()


{-
Sample maze input
-}

simpleMaze :: ([[Int]], Position, Position)
simpleMaze = let m =    [ [0, 1, 0, 0, 0]
                        , [0, 1, 1, 1, 0]
                        , [0, 0, 0, 1, 0]
                        , [1, 1, 0, 1, 1]
                        , [0, 0, 0, 0, 0]
                        ]
            in (m, (0,0), (4,4))

-- Quick maze reference from https://github.com/oppenheimj/maze-generato
complexMaze :: ([[Int]], Position, Position)
complexMaze = let m =   [ [1, 0, 1, 1, 0, 0, 0, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 0, 1, 0, 1, 1]
                        , [1, 0, 0, 1, 1, 1, 1, 1, 0, 0, 1, 1, 0, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 1, 1, 1, 0]
                        , [1, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 0, 1, 0, 0, 1, 0, 0, 1, 0, 1, 1, 0, 1, 0, 1, 0]
                        , [0, 1, 0, 1, 1, 1, 0, 1, 1, 0, 1, 1, 0, 1, 0, 1, 1, 0, 1, 0, 1, 1, 0, 1, 0, 0, 1, 0, 1, 1]
                        , [1, 1, 0, 0, 0, 1, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 1, 1, 1, 0, 0, 1, 1, 0, 1, 0, 0, 0]
                        , [1, 0, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 0, 1, 0, 0, 1, 0, 0, 0, 1, 1, 1, 0, 1, 0, 1, 1, 1, 0]
                        , [1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 1, 1, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 1]
                        , [1, 1, 1, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 0, 0, 0, 1, 1, 1, 1, 1, 0, 1, 1, 1, 0, 0, 1]
                        , [0, 0, 0, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0, 1, 0, 0, 0, 1, 1, 0, 1]
                        , [1, 1, 0, 1, 0, 1, 0, 1, 0, 1, 1, 1, 1, 1, 0, 0, 0, 1, 0, 1, 1, 1, 1, 0, 1, 0, 0, 1, 0, 1]
                        , [0, 1, 0, 1, 0, 1, 1, 1, 0, 1, 0, 1, 0, 0, 0, 0, 1, 1, 0, 0, 0, 1, 0, 0, 1, 1, 0, 1, 0, 1]
                        , [1, 1, 0, 1, 0, 0, 0, 0, 1, 1, 0, 1, 0, 1, 1, 1, 1, 0, 0, 0, 1, 1, 0, 0, 1, 0, 0, 1, 1, 1]
                        , [1, 0, 0, 1, 0, 0, 1, 1, 1, 0, 0, 1, 1, 1, 0, 0, 0, 0, 1, 1, 1, 0, 1, 1, 1, 0, 0, 0, 1, 0]
                        , [1, 1, 0, 1, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 0, 0, 0, 0, 0, 1, 0, 1, 1, 1, 0]
                        , [0, 1, 0, 1, 0, 1, 0, 1, 1, 1, 1, 1, 1, 1, 0, 1, 1, 0, 1, 1, 0, 1, 1, 1, 1, 0, 1, 0, 0, 1]
                        , [1, 1, 1, 1, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 1, 0, 0, 1]
                        , [1, 0, 1, 0, 0, 1, 1, 1, 0, 1, 1, 1, 0, 1, 1, 1, 1, 1, 0, 1, 0, 1, 1, 1, 0, 1, 1, 0, 0, 1]
                        , [0, 0, 1, 1, 1, 0, 0, 1, 1, 0, 0, 1, 0, 0, 0, 0, 0, 1, 1, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 1]
                        , [1, 1, 1, 0, 1, 0, 0, 0, 1, 1, 0, 1, 1, 0, 1, 1, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0]
                        , [1, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 1, 1, 1, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 1]
                        , [1, 1, 1, 0, 0, 1, 1, 1, 0, 1, 1, 1, 0, 0, 1, 1, 0, 1, 1, 1, 1, 0, 1, 1, 1, 1, 0, 0, 0, 1]
                        , [1, 0, 1, 0, 0, 1, 0, 1, 0, 0, 0, 1, 0, 1, 0, 1, 1, 0, 0, 0, 0, 1, 1, 0, 0, 1, 0, 1, 1, 1]
                        , [1, 0, 1, 0, 1, 1, 0, 1, 1, 1, 0, 1, 0, 1, 0, 0, 1, 1, 0, 1, 1, 1, 0, 0, 0, 1, 0, 1, 0, 0]
                        , [1, 0, 1, 1, 1, 0, 1, 1, 0, 0, 0, 1, 0, 1, 0, 0, 0, 1, 1, 1, 0, 1, 1, 0, 1, 1, 0, 1, 0, 1]
                        , [1, 0, 0, 0, 0, 0, 1, 0, 1, 1, 1, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 1, 1, 1]
                        , [1, 0, 1, 0, 1, 1, 1, 0, 0, 0, 1, 0, 0, 1, 1, 1, 1, 0, 1, 1, 1, 0, 1, 0, 1, 1, 0, 0, 0, 1]
                        , [1, 1, 1, 0, 1, 0, 1, 1, 0, 1, 1, 0, 0, 1, 0, 0, 1, 1, 1, 0, 1, 1, 0, 0, 0, 1, 1, 1, 0, 1]
                        , [0, 1, 0, 0, 1, 0, 0, 0, 0, 1, 0, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 1, 1, 1, 0, 1, 0, 1, 0, 1]
                        , [1, 1, 1, 0, 1, 1, 0, 1, 1, 1, 0, 1, 0, 0, 0, 0, 0, 1, 1, 1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 1]
                        , [1, 0, 1, 0, 0, 1, 1, 1, 0, 1, 1, 1, 0, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1]
                        ]
              in (m, (10,18), (16,18))
