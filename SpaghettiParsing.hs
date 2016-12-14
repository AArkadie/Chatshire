module Kitchen where

import Data.Char

data Noun = Spaghetti | Plate | Pot | Pan | Stove | Fork | Water | Sink | Cabinet | Sauce | Cheese |
            Table | Me | Myself | I-- | Him | Her | He | She | It
            deriving (Show, Eq)

data Verb = Cook | Throw | Grab | Put | Open | Turn | Attack | Is
            deriving (Show, Eq)

data Preposition = In | On | Around | By | To
                   deriving (Show, Eq)

data Adjective = Delicious | Tasty | Good | Great | Bad | Okay
                 deriving (Show, Eq)
                 
data Article = The | A | An | Some deriving (Show, Eq)

--data NPhrase = Preposition Noun | Adjective Noun deriving (Show, Eq)

data SFrag = SFragVN { v :: Verb, n :: Noun} |
             SFragVNPN { v ::Verb, n1 :: Noun, p :: Preposition, n2 :: Noun} |
             SFragVArtN Verb Article Noun |
             SFragVAdjN Verb Adjective Noun
             deriving (Show, Eq)

data MState = SState {cooked :: Bool, seasoned :: Bool, ruined :: Bool} deriving (Show, Eq)            
             
data WState = WState {sentences :: [SFrag], description :: String, mod :: MState} deriving (Show, Eq)

--type Stepper world = WState -> SFrag -> WState

scrubPunct :: String -> String
scrubPunct sent = [x | x <- sent,  not $ elem x ",.?:;'\"-+=_!@$%&*)("]

lexPrep :: String -> [String]
lexPrep sent = words $ map toLower $ scrubPunct sent

demoLex = lexPrep "What an -unruly- sentence, There are (aLl sOrTs of) puntuation $things going @on; and it _REALLY bothers: me!"
----------------------------------------------------------------------------------------------------

initSpag :: WState
initSpag = WState [] 
                  "You stand in a kitchen.  On the counter there is a bag of spaghetti.  On the stove there is an empty pot.  On the table there is a plate with a fork on it, yearning for some delicious cooked spaghetti."
                  $ SState False False False
--buffer
fragVal :: SFrag -> WState -> WState
fragVal s = error "type check"

wUpdate :: WState -> WState
wUpdate w = error "type check"
--buffer

{-
We want the input to come in, then we want the parser to see if it makes sense.
The parser extracts the relevant info to pass on to a function that finds out what that 
information means to the game and changes the state accordingly.
-}

vnReversal :: SFrag -> String
vnReversal f = "The " ++ nn ++ " has been " ++ nv ++ "ed"
               where nn = map toLower $ show $ n f
                     nv = map toLower $ show $ v f

vnpnReversal :: SFrag -> String
vnpnReversal f = "The " ++ nn1 ++ " is " ++ np ++ " the " ++ nn2
               where nn1 = map toLower $ show $ n1 f
                     nn2 = map toLower $ show $ n2 f
                     np = map toLower $ show $ p f
----------------------------------------------------------------------------------------------------
data Parser a = Parser ([String] -> [(a, [String])])

instance Functor (Parser) where
    fmap f m = do x <- m
                  return (f x)
        
ap :: Monad m => m (a -> b) -> m a -> m b 
ap mf ma = do f <- mf
              a <- ma
              return (f a)
              
instance Applicative (Parser) where
    pure = return
    (<*>) = ap

instance Monad (Parser) where
    return x = Parser (\inp -> [(x, inp)])
    act >>= k = Parser $ \toks -> 
                           [(r', toks'')  | (r, toks') <- runParser act toks,
                                            (r', toks'') <- runParser (k r) toks']

runParser :: Parser a -> [String] -> [(a, [String])]
runParser (Parser p) inp = p inp

Parser p1 ||| Parser p2 = Parser (\toks -> p1 toks ++ p2 toks)

tok :: (String -> Maybe a) -> Parser a
tok f = Parser (\ts -> if null ts then [] else
                          case f (head ts) of
                            Nothing -> []
                            Just v -> [(v, tail ts)])
                            
word :: String -> a -> Parser a
word w v = tok (\s -> if s == w then Just v else Nothing)

end :: Parser ()
end = Parser $ \toks ->
                if toks == [] then [((),[])] else []
----------------------------------------------------------------------------------------------------
parseNoun :: Parser Noun
parseNoun = word "spaghetti" Spaghetti ||| 
            word "plate" Plate |||
            word "pot" Pot |||
            word "pan" Pan |||
            word "stove" Stove |||
            word "fork" Fork |||
            word "water" Water |||
            word "sink" Sink |||
            word "cabinet" Cabinet |||
            word "sauce" Sauce |||
            word "cheese" Cheese |||
            word "table" Table |||
            word "me" Me |||
            word "myself" Myself |||
            word "i" I

parseVerb :: Parser Verb
parseVerb = word "cook" Cook ||| 
            word "throw" Throw |||
            word "grab" Grab |||
            word "put" Put |||
            word "open" Open |||
            word "turn" Turn |||
            word "attack" Attack |||
            word "is" Is           
           
parsePrep :: Parser Preposition
parsePrep = word "in" In ||| 
            word "on" On |||
            word "around" Around |||
            word "by" By |||
            word "to" To          
            
parseAdj :: Parser Adjective
parseAdj = word "delicious" Delicious ||| 
           word "tasty" Tasty |||
           word "good" Good |||
           word "great" Great |||
           word "bad" Bad |||
           word "okay" Okay          
            
parseArt :: Parser Article
parseArt = word "a" A |||
           word "an" An |||
           word "the" The |||
           word "some" Some
----------------------------------------------------------------------------------------------------            
parseVN = do
           v <- parseVerb
           n <- parseNoun
           return $ SFragVN v n
----------------------------------------------------------------------------------------------------
parseCookWin = do word "cook" ()
                  word "spaghetti" ()
                  return "The spaghetti has been cooked!"
parseCookFail = do word "cook" ()
                   n <- parseNoun
                   return "You can't cook that!"
parseThrow = do word "throw" ()
                n <- parseNoun
                word "at" ()
                n2 <- parseNoun
                return "Careful!  Those are important."
parseAttack = do word "attack" ()
                 parseNoun
                 return "This isn't the environment for violence!"
parseTurn = do word "turn" ()
               n <- parseNoun
               return "You get the sneaking suspicion that turning that would be fruitless."
parseOpenWin = do word "open" ()
                  word "cabinet" ()
                  return "Inside of the cabinet is a nice jar of spaghetti sauce.  If only there was cooked spaghetti to put it on."
parseOpenLose = do word "open" ()
                   parseNoun
                   return "You realize that there's no way to open that"
parseGrab1 = do word "grab" ()
                word "spaghetti" ()
                return "You handle the brittle strands of the spaghetti with care."
parseGrab2 = do word "grab" ()
                word "plate" ()
                return "You hold the plate and fondly think about how good some nice cooked spaghetti would look on it."
parseGrab3 = do word "grab" ()
                word "pot" ()
                return "You hold the handle firm and think about how much heavier this pot would be if there was water in it."
parseGrab4 = do word "grab" ()
                word "pan" ()
                return "As you caress the pan, you think about how absurd it would be to cook spaghetti in a pan."
parseGrab5 = do word "grab" ()
                word "fork" ()
                return "You hold the fork in between your fingers and practise your spaghetti twirling technique."
parseGrab6 = do word "grab" ()
                word "sauce" ()
                return "You inspect the sauce, it's your favorite brand; goes well with spaghetti."
parseGrab7 = do word "grab" ()
                word "cheese" ()
                return "The cheese is squishy, as cheese should be."
parseGrab8 = do word "grab" ()
                word "table" ()
                return "The table is nice and sturdy.  It won't budge.  The perfect surface on which to enjoy spaghetti."
parseGrabFail = do word "grab" ()
                   parseNoun
                   return "You can't really grab that..."
parseGrab = parseGrab1 |||
            parseGrab2 |||
            parseGrab3 |||
            parseGrab4 |||
            parseGrab5 |||
            parseGrab6 |||
            parseGrab7 |||
            parseGrab8 |||
            parseGrabFail
parsePut1 = do word "put" ()
               word "water" ()
               word "in" ()
               word "pot" ()
               return "The water looks lonely in the pot.  It yearns for the spaghetti."
parsePut2 = do word "put" ()
               word "spaghetti" ()
               word "in" ()
               word "pot" ()
               return "Now that looks like some spaghetti that's ready to be cooked!"
parsePut3 = do word "put" ()
               word "sauce" ()
               word "on" ()
               word "spaghetti" ()
               return "That's some saucy spaghetti!"
parsePut4 = do word "put" ()
               word "plate" ()
               word "in" ()
               word "sink" ()
               return "Time to clean the dishes already?"
parsePut5 = do word "put" ()
               word "fork" ()
               word "in" ()
               word "spaghetti" ()
               return "Stick a fork in it, because it's done!"
parsePutFail = do word "put" ()
                  o <- parseNoun
                  p <- parsePrep
                  s <- parseNoun
                  return "You don't think that goes there..."
parsePut = parsePut1 |||
           parsePut2 |||
           parsePut3 |||
           parsePut4 |||
           parsePut5 |||
           parsePutFail








parseStringRes = parseCookWin |||
                 parseCookFail |||
                 parseThrow |||
                 parseAttack |||
                 parseTurn |||
                 parseOpenWin |||
                 parseOpenLose |||
                 parseGrab |||
                 parsePut
-- Put
-- Spaghetti | Plate | Pot | Pan | Stove | Fork | Water | Sink | Cabinet | Sauce | Cheese | Table
test :: Parser a -> String -> [a]
test p s = map fst $ runParser p toks
           where toks = lexPrep s
           
demoParse1 = test parsePut "grab cheese"
demoParse2 = test parseGrab "grab cheese"
----------------------------------------------------------------------------------------------------
{-main = do
       putStrLn $ description initSpag
       act <- getLine
       let ans = head $ test parseVN act
       if ans == SFragVN Cook Spaghetti
          then do
               putStrLn "Congratulations, you cooked the spaghetti!"
               putStrLn "Press enter to cook some more"
               ans <- getLine
               main
          else do
               putStrLn "I'm afraid you weren't able to cook the spaghetti"
               putStrLn "Press enter to try again"
               ans <- getLine
               main
               
               -}
main = do
       putStrLn "\nDemo day yay!\n"
       act <- getLine
       let ans = test parseStringRes act
       if ans == [] then do
                         putStrLn "Sorry, I don't understand."
                         main
                    else do
                         let nans = head ans
                         putStrLn nans
                         main