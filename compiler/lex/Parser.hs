module Parser where

type Location = Int
type Stream = [(Location, Char)]
newtype Parser a = Parser { runParser :: Stream -> [(a, Stream)] }

{-|
>>> runParser (pChar 'a') $ toStream "abc"
[('a',[(1,'b'),(2,'c')])]
>>> runParser (pChar 'a') $ toStream "bc"
[]
-}
pEmpty :: a -> Parser a
pEmpty x = Parser (\toks -> [(x, toks)])

{-|
>>> runParser (pChar 'a' `pBind` \_ -> pChar 'b') $ toStream "abc"
[('b',[(2,'c')])]
>>> runParser (pChar 'a' `pBind` \_ -> pChar 'b') $ toStream "ac"
[]
-}
pBind :: Parser a -> (a -> Parser b) -> Parser b
px `pBind` f = Parser (\toks -> [ (v, toks'')
                                | (x, toks') <- runParser px toks
                                , (v, toks'') <- runParser (f x) toks'
                                ])

{-|
>>> runParser (Data.Char.toUpper `pApply` pChar 'a') $ toStream "abc"
[('A',[(1,'b'),(2,'c')])]
>>> runParser (Data.Char.toUpper `pApply` pChar 'a') $ toStream "bc"
[]
-}
pApply :: (a -> b) -> Parser a -> Parser b
f `pApply` p = Parser (\toks -> [ (f x, toks')
                                | (x, toks') <- runParser p toks
                                ])

{-|
>>> runParser (pChar 'a' `pAlt` pChar 'b') $ toStream "abc"
[('a',[(1,'b'),(2,'c')])]
>>> runParser (pChar 'a' `pAlt` pChar 'b') $ toStream "bac"
[('b',[(1,'a'),(2,'c')])]
>>> runParser (pChar 'a' `pAlt` pChar 'b') $ toStream "cab"
[]
>>> runParser (pLit "a" `pAlt` pLit "aa")$ toStream "aa"
[("a",[(1,'a')]),("aa",[])]
>>> runParser (pLit "aa" `pAlt` pLit "a")$ toStream "aa"
[("aa",[]),("a",[(1,'a')])]
>>> runParser (pLit "a" `pAlt` pLit "aa")$ toStream "aabc"
[("a",[(1,'a'),(2,'b'),(3,'c')]),("aa",[(2,'b'),(3,'c')])]
>>> runParser (pLit "aa" `pAlt` pLit "a")$ toStream "aabc"
[("aa",[(2,'b'),(3,'c')]),("a",[(1,'a'),(2,'b'),(3,'c')])]
-}
pAlt :: Parser a -> Parser a -> Parser a
p `pAlt` q = Parser (\toks -> runParser p toks ++ runParser q toks)

{-|
>>> runParser (pChar 'a' `pAltL` pChar 'b') $ toStream "abc"
[('a',[(1,'b'),(2,'c')])]
>>> runParser (pChar 'a' `pAltL` pChar 'b') $ toStream "bac"
[('b',[(1,'a'),(2,'c')])]
>>> runParser (pChar 'a' `pAltL` pChar 'b') $ toStream "cab"
[]
>>> runParser (pLit "a" `pAltL` pLit "aa")$ toStream "aa"
[("a",[(1,'a')])]
>>> runParser (pLit "aa" `pAltL` pLit "a")$ toStream "aa"
[("aa",[])]
>>> runParser (pLit "a" `pAltL` pLit "aa")$ toStream "aabc"
[("a",[(1,'a'),(2,'b'),(3,'c')])]
>>> runParser (pLit "aa" `pAltL` pLit "a")$ toStream "aabc"
[("aa",[(2,'b'),(3,'c')])]
-}
pAltL :: Parser a -> Parser a -> Parser a
p `pAltL` q = Parser (\toks -> runParser p toks <+ runParser q toks)
  where
    [] <+ ys = ys
    xs <+ _  = xs

{-|
>>> runParser (pure Data.Char.toUpper `pAp` pChar 'a') $ toStream "abc"
[('A',[(1,'b'),(2,'c')])]
>>> runParser (pure Data.Char.toUpper `pAp` pChar 'a') $ toStream "bc"
[]
-}
pAp :: Parser (a -> b) -> Parser a -> Parser b
pf `pAp` px = Parser (\toks -> [ (f v, toks'')
                               | (f, toks') <- runParser pf toks
                               , (v, toks'') <- runParser px toks'
                               ])

{-|
>>> runParser (pApply2 (,) (pChar 'a') (pChar 'b')) $ toStream "abc"
[(('a','b'),[(2,'c')])]
>>> runParser (pApply2 (,) (pChar 'a') (pChar 'b')) $ toStream "ac"
[]
-}
pApply2 :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
pApply2 f p q = Parser (\toks -> [ (f x y, toks'')
                                 | (x, toks') <- runParser p toks
                                 , (y, toks'') <- runParser q toks'
                                 ])

{-|
>>> runParser (pOneOrMore (pChar 'a')) $ toStream "aaabc"
[("aaa",[(3,'b'),(4,'c')]),("aa",[(2,'a'),(3,'b'),(4,'c')]),("a",[(1,'a'),(2,'a'),(3,'b'),(4,'c')])]
>>> runParser (pOneOrMore (pChar 'a')) $ toStream "abc"
[("a",[(1,'b'),(2,'c')])]
>>> runParser (pOneOrMore (pChar 'a')) $ toStream "bc"
[]
-}
pOneOrMore :: Parser a -> Parser [a]
pOneOrMore p = pApply2 (:) p (pZeroOrMore p)

{-|
>>> runParser (pZeroOrMore (pChar 'a')) $ toStream "aaabc"
[("aaa",[(3,'b'),(4,'c')]),("aa",[(2,'a'),(3,'b'),(4,'c')]),("a",[(1,'a'),(2,'a'),(3,'b'),(4,'c')]),("",[(0,'a'),(1,'a'),(2,'a'),(3,'b'),(4,'c')])]
>>> runParser (pZeroOrMore (pChar 'a')) $ toStream "abc"
[("a",[(1,'b'),(2,'c')]),("",[(0,'a'),(1,'b'),(2,'c')])]
>>> runParser (pZeroOrMore (pChar 'a')) $ toStream "bc"
[("",[(0,'b'),(1,'c')])]
-}
pZeroOrMore :: Parser a -> Parser [a]
pZeroOrMore p = pOneOrMore p `pAlt` pEmpty []

{-|
>>> runParser (pMunch1 (pChar 'a')) $ toStream "aaabc"
[("aaa",[(3,'b'),(4,'c')])]
>>> runParser (pMunch1 (pChar 'a')) $ toStream "abc"
[("a",[(1,'b'),(2,'c')])]
>>> runParser (pMunch1 (pChar 'a')) $ toStream "bc"
[]
-}
pMunch1 :: Parser a -> Parser [a]
pMunch1 p = pApply2 (:) p (pMunch p)

{-|
>>> runParser (pMunch (pChar 'a')) $ toStream "aaabc"
[("aaa",[(3,'b'),(4,'c')])]
>>> runParser (pMunch (pChar 'a')) $ toStream "abc"
[("a",[(1,'b'),(2,'c')])]
>>> runParser (pMunch (pChar 'a')) $ toStream "bc"
[("",[(0,'b'),(1,'c')])]
-}
pMunch :: Parser a -> Parser [a]
pMunch p = pMunch1 p `pAltL` pEmpty []

{-|
>>> runParser (pOneOrMoreWithSep (pChar ',') (pChar 'a')) $ toStream "a,a,a"
[("aaa",[]),("aa",[(3,','),(4,'a')]),("a",[(1,','),(2,'a'),(3,','),(4,'a')])]
>>> runParser (pOneOrMoreWithSep (pChar ',') (pChar 'a')) $ toStream "a,a,b"
[("aa",[(3,','),(4,'b')]),("a",[(1,','),(2,'a'),(3,','),(4,'b')])]
-}
pOneOrMoreWithSep :: Parser sep -> Parser a -> Parser [a]
pOneOrMoreWithSep sep p = pApply2 (:) p (pZeroOrMore (sep *> p))

{-|
>>> runParser (pMunch1WithSep (pChar ',') (pChar 'a')) $ toStream "a,a,a"
[("aaa",[])]
>>> runParser (pMunch1WithSep (pChar ',') (pChar 'a')) $ toStream "a,a,b"
[("aa",[(3,','),(4,'b')])]
-}
pMunch1WithSep :: Parser sep -> Parser a -> Parser [a]
pMunch1WithSep sep p = pApply2 (:) p (pMunch (sep *> p))

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

{-|
>>> runParser (pSat Data.Char.isAlpha) $ toStream "abc"
[('a',[(1,'b'),(2,'c')])]
>>> runParser (pSat Data.Char.isAlpha) $ toStream "123"
[]
-}
pSat :: (Char -> Bool)-> Parser Char
pSat p = Parser f
  where
    f [] = []
    f ((_, c):toks)
      | p c = [(c, toks)]
      | otherwise = []

{-|
>>> runParser (pChar 'a') $ toStream "abc"
[('a',[(1,'b'),(2,'c')])]
>>> runParser (pChar 'a') $ toStream "bc"
[]
-}
pChar :: Char -> Parser Char
pChar c = pSat (==c)

{-|
>>> runParser (pLit "abc") $ toStream "abcdef"
[("abc",[(3,'d'),(4,'e'),(5,'f')])]
>>> runParser (pLit "abc") $ toStream "abdef"
[]
-}
pLit :: String -> Parser String
pLit []     = Parser (\toks -> [([], toks)])
pLit (c:cs) = pSat (==c) `pBind` \_ -> pLit cs `pBind` \_ -> pEmpty (c:cs)

{-|
>>> runParser pEof $ toStream ""
[((),[])]
>>> runParser pEof $ toStream "abc"
[]
-}
pEof :: Parser ()
pEof = Parser f
  where
    f [] = [((), [])]
    f _  = []
