module Main where

import Data.Char

main = do
       putStrLn "In front of you is some spaghetti that you'd really like to cook.  What will you do?"
       act <- getLine
       let ans = head $ parse $ lexString $ words act
       if ans == Cook "spaghetti"
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

data Lexeme = LOp { operation :: String} | LKeyword {word :: String} | LOther {unknown :: String}
    deriving (Show, Eq)
     
ops = ["cook"]

punct = "(),.-;"

keyword = ["spaghetti"]

lexString :: [String] -> [Lexeme]

lexString [] = []  -- No more left
lexString inp@(c:cs)
   | c `elem` keyword = LKeyword c : lexString cs
   | c `elem` ops = LOp c : lexString cs
   | otherwise = LOther c : lexString cs

   
testLexer = lexString $ words "I want to cook all of the spaghetti in the world!"
   
data Exp =
   Cook String |
   Meaningless String
   deriving (Show, Eq)
   
parse :: [Lexeme] -> [Exp] --work so that we can get the one expression we need, a cook expression with spaghetti in it
parse [] = [Meaningless "I can't do anything with this!"]
parse [_] = [Meaningless "I need a bit more to go off of."]
parse (s:ss)
    | s == LOp "cook" && head ss == LOp "cook" = [Meaningless "Gee Bill, TWO Operations?!"]
    | s == LOp "cook" = [Cook $ word $ head ss]
    | otherwise = [Meaningless "What do?"]

cookParse :: Parser [Lexeme] Exp
cookParse = error "feed me"
   
data Parser inp a = Parser ([inp] -> [(a, [inp])])

instance Functor (Parser inp) where
    fmap f m = do x <- m
                  return (f x)
        
ap :: Monad m => m (a -> b) -> m a -> m b 
ap mf ma = do f <- mf
              a <- ma
              return (f a)
              
instance Applicative (Parser inp) where
    pure = return
    (<*>) = ap

instance Monad (Parser inp) where
    return x = Parser (\inp -> [(x, inp)])
    act >>= k = Parser $ \toks -> 
                           [(r', toks'')  | (r, toks') <- runParser act toks,
                                            (r', toks'') <- runParser (k r) toks']

runParser :: Parser inp a -> [inp] -> [(a, [inp])]
runParser (Parser p) inp = p inp

Parser p1 ||| Parser p2 = Parser (\toks -> p1 toks ++ p2 toks)

tok :: (inp -> Maybe a) -> Parser inp a
tok f = Parser (\ts -> if null ts then [] else
                          case f (head ts) of
                            Nothing -> []
                            Just v -> [(v, tail ts)])
                                      


{-
{-
Grammar:
idk man
-}

data Statement =
     If Exp [Statement] [Statement] |
     While Exp [Statement] |
     Assign Exp Exp |
     Define Exp Statement [Statement] |
     Exp
    deriving Show

try :: Parser inp a -> Parser inp (Maybe a)
try (Parser p) = Parser (\i -> let r = p i in
                           case r of [] -> [(Nothing, i)]
                                     l  -> [(Just res, i') | (res, i') <- l])
                                     
sep :: Parser inp a -> Parser inp b -> Parser inp [a]
sep thing s = do r <- thing
                 nxt <- try s
                 case nxt of
                      Nothing -> return [r]
                      Just _ -> do rst <- sep thing s
                                   return (r:rst)                      
      

parseOp :: String -> a -> Parser Lexeme a
parseOp str v = tok (\t -> case t of LOp s | s == str -> Just v
                                     _ -> Nothing)


parseExp :: Parser Lexeme Exp
parseExp = parseMulExp ||| do e1 <- parseMulExp
                              op <- parseAddOp
                              e2 <- parseExp
                              return $ op e1 e2                              
 


tst p s = runParser p (lexString (words s))

tste s = [r | (r, []) <- tst parseExp s]

p1 :: Parser Lexeme [Exp]
p1 = sep parseVar (parseOp "+" ())

p2 :: Parser Lexeme [Exp]
p2 = (do v <- parseVar
         return [v]) |||
     (do v <- parseVar
         parseOp "+" ()
         r <- p2
         return (v:r))
-}