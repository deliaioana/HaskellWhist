module Main where


main :: IO ()
main = do putStr "\n\nHello!\nWelcome to Romanian Haskell Whist!\n\n"
          putStr "Please enter your name: "
          playerName <- getLine
          putStr "\nHello, "
          putStr playerName
          putStr "!\n\n" 
          putStr "Please specify the number of total players for your game.\n" 
          putStr "Choose a number between 3, 4, 5 and 6.\n\n"
          number <- getCorrectNumberOfPlayers --solve loop
          putStr "Your game will start soon!\n\n"
          let game = initGame number playerName 
          let game = playGame game
          return ()


playGame :: Game -> Game
playGame game = playGameRounds game 1

playGameRounds :: Game -> Int -> Game
playGameRounds game round | round ==((getNumberOfRounds game)+1) = game
playGameRounds game round = playGameRounds (playRound game round) (round+1)  

getNumberOfRounds :: Game -> Int
getNumberOfRounds (G number _ _ _ _ _ _ _ _) = (number*3 + 12)

playRound :: Game -> Int -> Game
playRound game round = do 
                        let newGame = initGameForRound game round
                        let newGame2 = deal newGame
                        --let game = startBets game round
                        --let game = startHand game 
                        newGame

deal :: Game -> Game
deal game = dealX game (getHands game)

getHands :: Game -> Int
getHands (G _ _ _ _ _ _ _ h _) = h

setHands :: Game -> Int -> Game
setHands (G p1 p2 p3 p4 p5 p6 p7 p8 p9) x = (G p1 p2 p3 p4 p5 p6 p7 x p9) 

dealX :: Game -> Int -> Game
dealX (G a b c d e f g hands h) 0 = (G a b c d e f g hands h)
dealX (G number players c d e f g hands h) haveToDeal = dealX (dealOneCard (G number players c d e f g hands h) number) (haveToDeal-1)

dealOneCard :: Game -> Int -> Game
dealOneCard game 0 = game
dealOneCard (G number players deck trump nextPlayer scoreboard currentRound hands x) current = do
                                                                                                 let card = getRandomCardFromDeck deck
                                                                                                 let newDeck = (extractCardFromDeck card deck)
                                                                                                 let newPlayers = (addCardToXPlayer current card players)
                                                                                                 let newGame = (G number newPlayers newDeck trump nextPlayer scoreboard currentRound hands x)
                                                                                                 (dealOneCard newGame (current-1))

getCardAsString :: Card -> String
getCardAsString (Val val sign) = "Card: " ++ (show val) ++ [sign]

addCardToXPlayer :: Int -> Card -> Players -> Players
addCardToXPlayer 1 card (P3 p1 p2 p3) = (P3 (p1 ++ [card]) p2 p3)
addCardToXPlayer 2 card (P3 p1 p2 p3) = (P3 p1 (p2 ++ [card]) p3)
addCardToXPlayer 3 card (P3 p1 p2 p3) = (P3 p1 p2 (p3 ++ [card]))

addCardToXPlayer 1 card (P4 p1 p2 p3 p4) = (P4 (p1 ++ [card]) p2 p3 p4)
addCardToXPlayer 2 card (P4 p1 p2 p3 p4) = (P4 p1 (p2 ++ [card]) p3 p4)
addCardToXPlayer 3 card (P4 p1 p2 p3 p4) = (P4 p1 p2 (p3 ++ [card]) p4)
addCardToXPlayer 4 card (P4 p1 p2 p3 p4) = (P4 p1 p2 p3 (p4 ++ [card]))

addCardToXPlayer 1 card (P5 p1 p2 p3 p4 p5) = (P5 (p1 ++ [card]) p2 p3 p4 p5)
addCardToXPlayer 2 card (P5 p1 p2 p3 p4 p5) = (P5 p1 (p2 ++ [card]) p3 p4 p5)
addCardToXPlayer 3 card (P5 p1 p2 p3 p4 p5) = (P5 p1 p2 (p3 ++ [card]) p4 p5)
addCardToXPlayer 4 card (P5 p1 p2 p3 p4 p5) = (P5 p1 p2 p3 (p4 ++ [card]) p5)
addCardToXPlayer 5 card (P5 p1 p2 p3 p4 p5) = (P5 p1 p2 p3 p4 (p5 ++ [card]))

addCardToXPlayer 1 card (P6 p1 p2 p3 p4 p5 p6) = (P6 (p1 ++ [card]) p2 p3 p4 p5 p6)
addCardToXPlayer 2 card (P6 p1 p2 p3 p4 p5 p6) = (P6 p1 (p2 ++ [card]) p3 p4 p5 p6)
addCardToXPlayer 3 card (P6 p1 p2 p3 p4 p5 p6) = (P6 p1 p2 (p3 ++ [card]) p4 p5 p6)
addCardToXPlayer 4 card (P6 p1 p2 p3 p4 p5 p6) = (P6 p1 p2 p3 (p4 ++ [card]) p5 p6)
addCardToXPlayer 5 card (P6 p1 p2 p3 p4 p5 p6) = (P6 p1 p2 p3 p4 (p5 ++ [card]) p6)
addCardToXPlayer 6 card (P6 p1 p2 p3 p4 p5 p6) = (P6 p1 p2 p3 p4 p5 (p6 ++ [card]))

getRandomCardFromDeck :: [Card] -> Card -- de implementat altfel, momentan ia primul card
getRandomCardFromDeck (x:xs) = x

extractCardFromDeck :: Card -> [Card] -> [Card]
extractCardFromDeck _ []                 = []
extractCardFromDeck x (y:ys) | x == y    = extractCardFromDeck x ys
                    | otherwise = y : extractCardFromDeck x ys

initPlayers :: Int -> Players
initPlayers 3 = (P3 [] [] [])
initPlayers 4 = (P4 [] [] [] [])
initPlayers 5 = (P5 [] [] [] [] [])
initPlayers 6 = (P6 [] [] [] [] [] [])

data Players = P3 [Card] [Card] [Card]
                 | P4 [Card] [Card] [Card] [Card]
                 | P5 [Card] [Card] [Card] [Card] [Card]
                 | P6 [Card] [Card] [Card] [Card] [Card] [Card]
                 deriving (Show, Eq)

data Card = Val Int Char deriving (Show, Eq)

initCards :: Int -> [Card]
initCards 3 = getAllCardsFromXToY 14 9
initCards 4 = (initCards 3) ++ (getAllCardsOfValue 8) ++ (getAllCardsOfValue 7)
initCards 5 = (initCards 4) ++ (getAllCardsOfValue 6) ++ (getAllCardsOfValue 5)
initCards 6 = (initCards 5) ++ (getAllCardsOfValue 4) ++ (getAllCardsOfValue 3)

getAllCardsFromXToY :: Int -> Int -> [Card]
getAllCardsFromXToY x y | x == y = (getAllCardsOfValue x)
getAllCardsFromXToY x y = (getAllCardsOfValue x) ++ (getAllCardsFromXToY (x-1) y)

getAllCardsOfValue :: Int -> [Card]
getAllCardsOfValue x = [Val x 'C', Val x 'D', Val x 'H', Val x 'S'] 


getCorrectNumberOfPlayers :: IO Int
getCorrectNumberOfPlayers = do
                              nr <- getLine
                              ;return (getIntFromMaybeInt (parseNumberOfPlayers nr))

parseNumberOfPlayers :: String -> Maybe Int
parseNumberOfPlayers "" = Nothing
parseNumberOfPlayers "\n" = Nothing
parseNumberOfPlayers (x:xs) = if (x <= '6' && x >= '3') then (Just (parseDigit x)) else Nothing

parseDigit :: Char -> Int
parseDigit x | x == '3' = 3
parseDigit x | x == '4' = 4
parseDigit x | x == '5' = 5
parseDigit x | x == '6' = 6
parseDigit _ = 0

wrongNumberOfPlayersMessage :: String
wrongNumberOfPlayersMessage = "The specified number of players is incorrect!\nPlease try again without any additional characters!\n\n"

goodNumberOfPlayersMessage :: String -> String
goodNumberOfPlayersMessage x = "Your game will have " ++ x ++ " players.\n\n"

printThis :: String -> IO() 
printThis x = putStr x

--getCorrectNumberOfPlayersLoop :: IO Int
--getCorrectNumberOfPlayersLoop = do
--                                  xs <- getLine
--                                  let digit = parseNumberOfPlayers xs;
--                                  if (digit == Nothing) 
--                                  then return do
--                                       {putStr (wrongNumberOfPlayersMessage);
--                                       let res = getCorrectNumberOfPlayersLoop
--                                       return res}
--                                  else return (getIntFromMaybeInt digit)

getIntFromMaybeInt :: (Maybe Int) -> Int
getIntFromMaybeInt (Just x) = x
getIntFromMaybeInt Nothing = 0

collectUntil :: (Monad m) => m a -> (a -> Bool) -> m [a]
collectUntil act f = do
  x <- act
  if f x
    then return []
    else (x:) <$> collectUntil act f

readUntil :: (String -> Bool) -> IO [String]
readUntil = collectUntil getLine

initGameForRound :: Game -> Int -> Game
initGameForRound (G number players deck trump nextPlayer scoreboard currentRound hands x) round = 
 (G number (initPlayers number) (initCards number) (Val 0 '0') (computeNextPlayer nextPlayer number) scoreboard round (computeNumberOfHands round number) 0) 

computeNextPlayer :: Int -> Int -> Int
computeNextPlayer current number | current == number = 1
computeNextPlayer current number = (current+1)

computeNumberOfHands :: Int -> Int -> Int
computeNumberOfHands round number | round > number && round <= (number+6) = (round-number+1)
computeNumberOfHands round number | round > (number+6) && round <= (2*number+6) = 8
computeNumberOfHands round number | round > (2*number+6) && round <= (2*number+12) = (round-2*number-6)
computeNumberOfHands _ _ = 1

data Game = G Int Players [Card] Card Int Scoreboard Int Int Int deriving (Show)
-- in order:  	Number of players, Players, Deck, Trump card, Player who is next by index, Scoreboard, 
-- 				Current round number, Current number of cards dealt, Current number of cards played

data Scoreboard = S3 String Int String Int String Int [ScoreLine3]
                | S4 String Int String Int String Int String Int [ScoreLine4]
                | S5 String Int String Int String Int String Int String Int [ScoreLine5]
                | S6 String Int String Int String Int String Int String Int String Int [ScoreLine6] 
                deriving (Show)

data ScoreLine3 = SL3 Int Int Int Int Int Int Int deriving (Show)
data ScoreLine4 = SL4 Int Int Int Int Int Int Int Int Int deriving (Show)
data ScoreLine5 = SL5 Int Int Int Int Int Int Int Int Int Int Int deriving (Show)
data ScoreLine6 = SL6 Int Int Int Int Int Int Int Int Int Int Int Int Int deriving (Show)

computeFinalWinner :: Game -> String
computeFinalWinner (G _ _ _ _ _ scoreboard _ _ _) = computeFinalWinnerFromScoreboard scoreboard

computeFinalWinnerFromScoreboard :: Scoreboard -> String
computeFinalWinnerFromScoreboard (S3 p1 x1 p2 x2 p3 x3 _) | x1>=x2 && x1>=x3 = p1 
computeFinalWinnerFromScoreboard (S3 p1 x1 p2 x2 p3 x3 _) | x2>=x1 && x2>=x3 = p2
computeFinalWinnerFromScoreboard (S3 p1 x1 p2 x2 p3 x3 _) | x3>=x2 && x3>=x1 = p2

computeFinalWinnerFromScoreboard (S4 p1 x1 p2 x2 p3 x3 p4 x4 _) | x1>=x2 && x1>=x3 && x1>=x4 = p1
computeFinalWinnerFromScoreboard (S4 p1 x1 p2 x2 p3 x3 p4 x4 _) | x2>=x3 && x2>=x1 && x2>=x4 = p2
computeFinalWinnerFromScoreboard (S4 p1 x1 p2 x2 p3 x3 p4 x4 _) | x3>=x2 && x3>=x1 && x3>=x4 = p3
computeFinalWinnerFromScoreboard (S4 p1 x1 p2 x2 p3 x3 p4 x4 _) | x4>=x2 && x4>=x3 && x4>=x1 = p4

computeFinalWinnerFromScoreboard (S5 p1 x1 p2 x2 p3 x3 p4 x4 p5 x5 _) | x1>=x2 && x1>=x3 && x1>=x4 && x1>=x5 = p1
computeFinalWinnerFromScoreboard (S5 p1 x1 p2 x2 p3 x3 p4 x4 p5 x5 _) | x2>=x1 && x2>=x3 && x2>=x4 && x2>=x5 = p2
computeFinalWinnerFromScoreboard (S5 p1 x1 p2 x2 p3 x3 p4 x4 p5 x5 _) | x3>=x2 && x3>=x1 && x3>=x4 && x3>=x5 = p3
computeFinalWinnerFromScoreboard (S5 p1 x1 p2 x2 p3 x3 p4 x4 p5 x5 _) | x4>=x2 && x4>=x3 && x4>=x1 && x4>=x5 = p4
computeFinalWinnerFromScoreboard (S5 p1 x1 p2 x2 p3 x3 p4 x4 p5 x5 _) | x5>=x2 && x5>=x3 && x5>=x4 && x5>=x1 = p5

computeFinalWinnerFromScoreboard (S6 p1 x1 p2 x2 p3 x3 p4 x4 p5 x5 p6 x6 _) | x1>=x2 && x1>=x3 && x1>=x4 && x1>=x5 && x1>=x6= p1
computeFinalWinnerFromScoreboard (S6 p1 x1 p2 x2 p3 x3 p4 x4 p5 x5 p6 x6 _) | x2>=x1 && x2>=x3 && x2>=x4 && x2>=x5 && x2>=x6= p2
computeFinalWinnerFromScoreboard (S6 p1 x1 p2 x2 p3 x3 p4 x4 p5 x5 p6 x6 _) | x3>=x2 && x3>=x1 && x3>=x4 && x3>=x5 && x3>=x6= p3
computeFinalWinnerFromScoreboard (S6 p1 x1 p2 x2 p3 x3 p4 x4 p5 x5 p6 x6 _) | x4>=x2 && x4>=x3 && x4>=x1 && x4>=x5 && x4>=x6= p4
computeFinalWinnerFromScoreboard (S6 p1 x1 p2 x2 p3 x3 p4 x4 p5 x5 p6 x6 _) | x5>=x2 && x5>=x3 && x5>=x4 && x5>=x1 && x5>=x6= p5
computeFinalWinnerFromScoreboard (S6 p1 x1 p2 x2 p3 x3 p4 x4 p5 x5 p6 x6 _) | x6>=x2 && x6>=x3 && x6>=x4 && x6>=x5 && x6>=x1= p6

initScores :: Int -> String -> Scoreboard
initScores 3 name = (S3 name 0 "P2" 0 "P3" 0 [])
initScores 4 name = (S4 name 0 "P2" 0 "P3" 0 "P4" 0 [])
initScores 5 name = (S5 name 0 "P2" 0 "P3" 0 "P4" 0 "P5" 0 [])
initScores 6 name = (S6 name 0 "P2" 0 "P3" 0 "P4" 0 "P5" 0 "P6" 0 [])

initGame :: Int -> String -> Game
initGame number playerName = (G number (initPlayers number) (initCards number) (Val 0 '0') 0 (initScores number playerName) 0 0 0)