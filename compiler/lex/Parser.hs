module Parser where

type Location = Int
type Stream = [(Location, Char)]
newtype Parser a = Parser { runParser :: Stream -> [(a, Stream)] }

pEmpty :: a -> Parser a
pEmpty x = Parser (\toks -> [(x, toks)])

pBind :: Parser a -> (a -> Parser b) -> Parser b
px `pBind` f = Parser (\toks -> [ (v, toks'')
                                | (x, toks') <- runParser px toks
                                , (v, toks'') <- runParser (f x) toks'
                                ])

pApply :: (a -> b) -> Parser a -> Parser b
f `pApply` p = Parser (\toks -> [ (f x, toks')
                                | (x, toks') <- runParser p toks
                                ])

pAlt :: Parser a -> Parser a -> Parser a
p `pAlt` q = Parser (\toks -> runParser p toks ++ runParser q toks)

pAltL :: Parser a -> Parser a -> Parser a
p `pAltL` q = Parser (\toks -> runParser p toks <+ runParser q toks)
  where
    [] <+ ys = ys
    xs <+ _  = xs

pAp :: Parser (a -> b) -> Parser a -> Parser b
pf `pAp` px = Parser (\toks -> [ (f v, toks'')
                               | (f, toks') <- runParser pf toks
                               , (v, toks'') <- runParser px toks'
                               ])

pApply2 :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
pApply2 f p q = Parser (\toks -> [ (f x y, toks'')
                                 | (x, toks') <- runParser p toks
                                 , (y, toks'') <- runParser q toks'
                                 ])

pOneOrMore :: Parser a -> Parser [a]
pOneOrMore p = pApply2 (:) p (pZeroOrMore p)

pZeroOrMore :: Parser a -> Parser [a]
pZeroOrMore p = pOneOrMore p `pAlt` pEmpty []

pMunch1 :: Parser a -> Parser [a]
pMunch1 p = pApply2 (:) p (pMunch p)

pMunch :: Parser a -> Parser [a]
pMunch p = pMunch1 p `pAltL` pEmpty []

pOneOrMoreWithSep :: Parser sep -> Parser a -> Parser [a]
pOneOrMoreWithSep sep p = pApply2 (:) p (pZeroOrMore (sep *> p))

pMunch1WithSep :: Parser sep -> Parser a -> Parser [a]
pMunch1WithSep sep p = pApply2 (:) p (pMunch (sep *> p))

pBracket :: Parser open -> Parser close -> Parser a -> Parser a
pBracket open close p = open *> p <* close

instance Functor Parser where
  fmap = pApply

instance Applicative Parser where
  pure = pEmpty
  (<*>) = pAp

instance Monad Parser where
  return = pure
  (>>=) = pBind

toStream :: String -> Stream
toStream = zip [0..]

pSat :: (Char -> Bool)-> Parser Char
pSat p = Parser f
  where
    f [] = []
    f ((_, c):toks)
      | p c = [(c, toks)]
      | otherwise = []

pChar :: Char -> Parser Char
pChar c = pSat (==c)

pLit :: String -> Parser String
pLit []     = Parser (\toks -> [([], toks)])
pLit (c:cs) = pSat (==c) `pBind` \_ -> pLit cs `pBind` \_ -> pEmpty (c:cs)

pEof :: Parser ()
pEof = Parser f
  where
    f [] = [((), [])]
    f _  = []
