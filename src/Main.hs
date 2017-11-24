module Main where

import System.IO
import Data.List(elemIndex, foldl')

data Command = Move Direction | Activate | Deactivate deriving Show
data Direction = N | S | E | O deriving Show
data Power = Active | Inactive deriving Show
data Robot = Robot { power :: Power, position :: (Int, Int) } deriving Show
type Field = [String]

main :: IO ()
main = do
    commands <- askCommands
    field    <- askField
    let parser = parse <$> commands;
        start  = field >>= findStartPosition;
        robot  = Just . (Robot Inactive) <$> start
    either (\e -> putStrLn $ "Error: " ++ e) (uncurry showResult) (parser <*> field <*> robot)

parse :: [Command] -> Field -> Maybe Robot -> (Field, Maybe Robot)
parse commands = curry $ foldl' (flip (.)) id (runCommand <$> commands)

runCommand :: Command -> (Field, Maybe Robot) -> (Field, Maybe Robot)
runCommand Activate         = \(f, r) -> (f, (Robot Active).position <$> r)
runCommand Deactivate       = \(f, r) -> (f, (Robot Inactive).position <$> r)
runCommand (Move direction) = uncurry $ move direction

askCommands :: IO (Either String [Command])
askCommands = do
    hSetBuffering stdout NoBuffering
    putStr "Input Command: "
    parseCommands <$> getLine

parseCommands :: String -> Either String [Command]
parseCommands = sequenceA . (fmap parseCommand)

parseCommand :: Char -> Either String Command
parseCommand 'N' = Right $ Move N
parseCommand 'S' = Right $ Move S
parseCommand 'E' = Right $ Move E
parseCommand 'O' = Right $ Move O
parseCommand 'I' = Right $ Activate
parseCommand '-' = Right $ Deactivate
parseCommand  c  = Left $ c:" : unknown Command"

askField :: IO (Either String Field)
askField = do
    hSetBuffering stdout LineBuffering
    c <- putStrLn "Input Field (Press C-d to terminate): " >> getContents
    return $ Right . removeEmptyLines . lines $ c
    where removeEmptyLines = filter (not . ((==) ""))

-- 開始位置'M'を見つける.
-- どこにあってもよい. 複数あった場合, 左上を優先する.
findStartPosition :: Field -> Either String (Int, Int)
findStartPosition f = maybe (Left "Could not find start position M.") Right (foldl' find' Nothing $ zip [0..] f)
    where find' :: Maybe (Int, Int) -> (Int, String) -> Maybe (Int, Int)
          find' Nothing (row, s) = maybe Nothing (\col -> Just (col, row)) (elemIndex 'M' s)
          find' acc _ = acc

peakField :: (Int, Int) -> Field -> Maybe Char
peakField _ [] = Nothing
peakField (x, y) field
    | x < 0 || y < 0             = Nothing
    | y >= length field          = Nothing
    | x >= (length $ field !! y) = Nothing
    | otherwise                  = Just $ (field !! y) !! x

move :: Direction -> Field -> Maybe Robot -> (Field, Maybe Robot)
move _ f Nothing  = (f, Nothing)      -- robot has been exploded; nop.
move d f (Just o) = let n = move' d o
                        p = position n in
                            case (peakField p f) of
                                Nothing  -> (f, Just o)  -- out of bounds; nop.
                                Just '+' -> (f, Just o)  -- wall; nop.
                                Just '*' -> (f, Nothing) -- mine; void the robot.
                                Just 'M' -> (f, Just n)  -- start; move, do not put footprint.
                                Just  _  -> (putFootprint p f, Just n)  -- otherwise; move, put footprint.

move' :: Direction -> Robot -> Robot
move' _ r@(Robot Inactive _) = r
move' d   (Robot _ (x, y)) = case d of
    N -> Robot Active (x, y-1)
    S -> Robot Active (x, y+1)
    E -> Robot Active (x+1, y)
    O -> Robot Active (x-1, y)

putFootprint :: (Int, Int) -> Field -> Field
putFootprint p@(x, y) f = case peakField p f of
    Nothing -> f
    _       -> modify y (modify x '#' (f !! y)) f
    where
        -- 注意: peakField が境界チェックを行うのでmodifyでは行っていない.
        modify :: Int -> a -> [a] -> [a]
        modify 0 x xs = x:(tail xs)
        modify n x xs = let (h, t) = (take n xs, drop (n+1) xs) in h ++ [x] ++ t

showResult :: Field -> Maybe Robot -> IO ()
showResult f Nothing  = putStrLn "LOSE: BOOOOOOM!!" >> (putStrLn $ showField f)
showResult f (Just r)
    | hasWon f r = putStrLn "WIN: Escaped!!"  >> (putStrLn $ showField f)
    | otherwise  = putStrLn "LOSE: You couldn't escape" >> (putStrLn $ showField f)

showField :: Field -> String
showField = unlines

hasWon :: Field -> Robot -> Bool
hasWon _ (Robot Active _) = False
hasWon f (Robot _ p)      = case (peakField p f) of
    Just 'M' -> False -- スタート地点に戻ってきた場合
    _        -> isGoal f p

isGoal :: Field -> (Int, Int) -> Bool
isGoal f (x, y)
    | x == 0 || y == 0 = True
    | y == (length f) - 1 = True
    | x == (length $ f !! y) - 1 = True
    | otherwise = False
