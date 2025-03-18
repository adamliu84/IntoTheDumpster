-- OG: https://www.sciencebuddies.org/science-fair-projects/project-ideas/ArtificialIntelligence_p008/artificial-intelligence/machine-learning-maze?ytid=QUNM-QyM5PA&ytsrc=description
{-# LANGUAGE RecordWildCards #-}
import           Control.Monad       (when)
import           Control.Monad.State (StateT, execStateT, get, liftIO, put,
                                      runStateT)
import           Data.List           (intercalate, maximumBy)
import           Data.List.NonEmpty  as NonEmpty (fromList, head)
import qualified Data.Map            as Map (Map, empty, findWithDefault,
                                             fromList, insert, toList)
import           Data.Ord            (comparing)
import           System.Random       (randomRIO)

type Position = (Int, Int)
type Action = (Int, Int)
type AgentState = (Int, Int)

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

instance Show Maze where
  show Maze{..} = intercalate "\n" $ [[cellChar (i,j) | j <- [0 .. mazeWidth - 1]] | i <- [0..mazeHeight - 1]]
                                     ++ ["Start" ++ show mazeStartPos ++ " Goal" ++ show mazeGoalPos]
              where   cellChar :: Position -> Char
                      cellChar pos
                        | pos == mazeStartPos = 'S'
                        | pos == mazeGoalPos  = 'G'
                        | (mazeLayout !! i !! j) == 1 = '#'
                        | otherwise         = ' '
                        where (i, j) = pos

-- showMaze :: Maze -> IO ()
-- showMaze Maze{..} = mapM_ putStrLn [ [ cellChar (i, j) | j <- [0 .. mazeWidth - 1] ]
--                              | i <- [0 .. mazeHeight - 1] ]
--             where   cellChar :: Position -> Char
--                     cellChar pos
--                         | pos == mazeStartPos = 'S'
--                         | pos == mazeGoalPos  = 'G'
--                         | (mazeLayout !! i !! j) == 1 = '#'
--                         | otherwise         = ' '
--                         where (i, j) = pos

{-
Implementing the Agent
-}
actions :: [Action]
actions = [ (-1, 0)
          , (1, 0)
          , (0, -1)
          , (0, 1)
          ]

type QTable = Map.Map (Int, Int, Int) Double

data QLearningAgent = QLearningAgent
   { learningRate     :: Double
   , discountFactor   :: Double
   , explorationStart :: Double
   , explorationEnd   :: Double
   , numEpisodes      :: Int
   } deriving Show

genQLearningAgent :: Maze -> Double -> Double -> Double -> Double -> Int -> QLearningAgent
genQLearningAgent Maze{..} lr df es ee epi =
  QLearningAgent {    learningRate = lr
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

getAction :: QLearningAgent -> AgentState -> Int -> StateT QTable IO Int
getAction agent@QLearningAgent{..} state currentEpisode = do
  qTable <- get
  r <- randomRIO (0.0, 1.0)
  preGenAr <- randomRIO (0, 3)
  let expRate = getExplorationRate agent currentEpisode
      actionResult =
        if r < expRate then
          preGenAr
        else
          let qs = map (\a -> findQDefaultOnZero state a qTable) [0..3]
              (bestAction, _) = maximumBy (comparing snd) (zip [0..3] qs)
          in bestAction
   in return actionResult

updateQTable :: QLearningAgent -> AgentState -> Int -> AgentState -> Double -> StateT QTable IO ()
updateQTable agent@QLearningAgent{..} state action nextState reward = do
  qTable <- get
  let currentQ = findQDefaultOnZero state action qTable
      nextQs   = map (\a -> findQDefaultOnZero nextState a qTable) [0..3]
      bestNextQ = maximum nextQs
      newQ = currentQ + learningRate * (reward + discountFactor * bestNextQ - currentQ)
      newQTable = Map.insert (fst state, snd state, action) newQ qTable
  put newQTable

findQDefaultOnZero :: AgentState -> Int -> QTable -> Double
findQDefaultOnZero s a = Map.findWithDefault 0 (fst s, snd s, a)

{-
Defining the Reward System
-}
goalReward :: Double
goalReward  = 100
wallPenalty :: Double
wallPenalty = -10
stepPenalty :: Double
stepPenalty = -1

finishEpisode :: QLearningAgent -> Maze -> Int -> Bool -> StateT QTable IO (Double, Int, [(Int, Int)])
finishEpisode agent Maze{..} currentEpisode train = loop mazeStartPos 0 0 []
  where
    h = mazeHeight
    w = mazeWidth
    layout = mazeLayout
    loop :: AgentState -> Double -> Int -> [(Int, Int)] -> StateT QTable IO (Double, Int, [(Int, Int)])
    loop currentState episodeReward episodeStep path = do
      let (i, j) = currentState
      action <- getAction agent currentState currentEpisode
      let (di, dj) = actions !! action
          nextState@(ni, nj) = (i + di, j + dj)
          outOfBounds = ni < 0 || ni >= h || nj < 0 || nj >= w
          wall = not outOfBounds && (layout !! ni !! nj == 1)
      let (reward, actualNextState, newPath, done) = if outOfBounds || wall
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
runTestAgent :: QLearningAgent -> Maze -> Int -> StateT QTable IO (Int, Double)
runTestAgent agent maze numEpisode = do
  (episodeReward, episodeStep, path) <- finishEpisode agent maze numEpisode False
  putStrLn' "Learned Path:"
  mapM_ (\(r,c) -> liftIO $ putStr $ "(" ++ show r ++ ", " ++ show c ++ ") -> ") path  --NOTE: Not same as .py. I am referring using row as fst -> col as snd in Haskell version
  putStrLn' "Goal!"
  putStrLn' $ "Number of steps: " ++ show episodeStep
  putStrLn' $ "Total reward: " ++ show episodeReward
  return (episodeStep, episodeReward)

{-
Train Agent
-}
runTrainAgent :: QLearningAgent -> Maze -> Int -> StateT QTable IO ()
runTrainAgent agent maze numEps = do
  results <- mapM (\episode -> finishEpisode agent maze episode True) [0 .. numEps - 1]
  let rewards = map (\(r,_,_) -> r) results
      steps   = map (\(_,s,_) -> s) results
      avgReward = sum rewards / fromIntegral numEps
      avgSteps  = fromIntegral (sum steps) / fromIntegral numEps
  putStrLn' $ "The average reward is: " ++ show avgReward
  putStrLn' $ "The average steps is: " ++ show avgSteps

putStrLn' :: String -> StateT QTable IO ()
putStrLn' = liftIO . putStrLn

{-
Main
-}
main :: IO ()
main = do
    let (m,s,g) = simpleMaze --complexMaze
        maze = genMaze m s g
        agent = genQLearningAgent maze 0.1 0.9 1.0 0.01 100

    -- Show agent
    print agent
    -- Show maze
    print maze
    -- No training
    runStateT (runTestAgent agent maze 0) Map.empty
    -- Training
    -- (_, knowledge) <- runStateT (runTrainAgent agent maze 100) Map.empty
    knowledge <- execStateT (runTrainAgent agent maze 100) Map.empty
    -- Test after training
    runStateT (runTestAgent agent maze 100) knowledge
    -- Simulate test using stored trainned knowledge
    runStateT (runTestAgent agent maze 100) storedKnowledge

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

{-
Simulate extracted learning from Maze.hs (for simpleMaze)
-}

storedKnowledge :: QTable
storedKnowledge = Map.fromList [((0,0,0),-9.948447054814068),((0,0,1),34.22537209088859),((0,0,2),-10.148098901466653),((0,0,3),-9.26107877068279),((0,1,0),0.0),((0,1,1),0.0),((0,1,2),0.0),((0,1,3),0.0),((0,2,0),0.0),((0,2,1),0.0),((0,2,2),0.0),((0,2,3),0.0),((0,3,0),0.0),((0,3,1),0.0),((0,3,2),0.0),((0,3,3),0.0),((0,4,0),0.0),((0,4,1),0.0),((0,4,2),0.0),((0,4,3),0.0),((1,0,0),-0.5815104382664638),((1,0,1),43.17367970541943),((1,0,2),-8.820690046391794),((1,0,3),-6.206163586711423),((1,1,0),0.0),((1,1,1),0.0),((1,1,2),0.0),((1,1,3),0.0),((1,2,0),0.0),((1,2,1),0.0),((1,2,2),0.0),((1,2,3),0.0),((1,3,0),0.0),((1,3,1),0.0),((1,3,2),0.0),((1,3,3),0.0),((1,4,0),0.0),((1,4,1),0.0),((1,4,2),0.0),((1,4,3),0.0),((2,0,0),-1.9547889094444013),((2,0,1),-7.973213197922975),((2,0,2),-6.670127357284441),((2,0,3),52.073401569447896),((2,1,0),-8.37838851970896),((2,1,1),-8.658729089468496),((2,1,2),3.185558983925559),((2,1,3),60.88356580449275),((2,2,0),1.3066101306347768),((2,2,1),69.67738456281634),((2,2,2),11.971167179386224),((2,2,3),0.4789432238116076),((2,3,0),0.0),((2,3,1),0.0),((2,3,2),0.0),((2,3,3),0.0),((2,4,0),0.0),((2,4,1),0.0),((2,4,2),0.0),((2,4,3),0.0),((3,0,0),0.0),((3,0,1),0.0),((3,0,2),0.0),((3,0,3),0.0),((3,1,0),0.0),((3,1,1),0.0),((3,1,2),0.0),((3,1,3),0.0),((3,2,0),2.0963099543051165),((3,2,1),78.95424329695307),((3,2,2),3.734625491241834),((3,2,3),13.328238057812708),((3,3,0),0.0),((3,3,1),0.0),((3,3,2),0.0),((3,3,3),0.0),((3,4,0),0.0),((3,4,1),0.0),((3,4,2),0.0),((3,4,3),0.0),((4,0,0),0.0),((4,0,1),0.0),((4,0,2),0.0),((4,0,3),0.0),((4,1,0),-0.793462184377376),((4,1,1),-0.793462184377376),((4,1,2),0.0),((4,1,3),13.910397722648323),((4,2,0),15.573601842252241),((4,2,1),7.008380926924714),((4,2,2),1.1207232183181697),((4,2,3),88.97165791138465),((4,3,0),43.10524906660288),((4,3,1),26.038638746041112),((4,3,2),4.901928631778433),((4,3,3),99.99734386011123),((4,4,0),0.0),((4,4,1),0.0),((4,4,2),0.0),((4,4,3),0.0)]
