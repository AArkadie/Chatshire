module Kitchen where

import Data.Char

data Noun = Spaghetti | Plate | Pot | Pan | Stove | Fork | Water | Sink | Cabinet | Sauce | Cheese |
            Table
            deriving Show

data Verb = Cook | Throw | Grab | Put | Open | Turn | Attack | Is
            deriving Show

data Preposition = In | On | Around | By | To
                   deriving Show

data Adjective = Delicious | Tasty | Good | Great | Bad | Okay
                 deriving Show

--data NPhrase = Preposition Noun | Adjective Noun deriving Show

data SFrag = SFrag Noun Verb Noun | SFrag Verb Noun Preposition Noun
             deriving Show

data WState = WState {sentences :: [SFrag], description :: String} deriving Show

type Stepper world sent = WState -> SFrag -> WState

scrubPunct :: String -> String
scrubPunct sent = [x | x <- sent,  not $ elem x ",.?:;'\"-+=_!@$%&*)("]

lexPrep :: String -> [String]
lexPrep sent = words $ map toLower $ scrubPunct sent





{-
We want the input to come in, then we want the parser to see if it makes sense.
The parser extracts the relevant info to pass on to a function that finds out what that 
information means to the game and changes the state accordingly.
-}









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
            word "table" Table

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
            
parseNVN = do
           n1 <- parseNoun
           v <- parseVerb
           n2 <- parseNoun
           return $ SFrag n1 v n2
           
test :: Parser a -> String -> [a]
test p s = map fst $ runParser p toks
           where toks = lexPrep s