module Token where

import Prelude hiding (Ordering(..))

data Token = EOF
           | ID String
           | STRING String
           | REAL Double
           | INT Int
           | ANDALSO              -- ^ andalso
           | AND                  -- ^ and
           | AS                   -- ^ as
           | CASE                 -- ^ case
           | DO                   -- ^ do
           | END                  -- ^ end
           | EXCEPTION            -- ^ exception
           | FN                   -- ^ fn
           | FUN                  -- ^ fun
           | HANDLE               -- ^ handle
           | IF                   -- ^ if
           | IN                   -- ^ in
           | INFIX                -- ^ infix
           | INFIXR               -- ^ infixr
           | NONFIX               -- ^ nonfix
           | LET                  -- ^ let
           | LOCAL                -- ^ local
           | OF                   -- ^ of
           | OP                   -- ^ op
           | OPEN                 -- ^ open
           | ORELSE               -- ^ orelse
           | RAISE                -- ^ raise
           | REC                  -- ^ rec
           | THEN                 -- ^ then
           | USE                  -- ^ use
           | VAL                  -- ^ val
           | WHILE                -- ^ while
           | COMMA                -- ^ ,
           | PERIOD               -- ^ .
           | TRIPLEDOT            -- ^ ...
           | COLON                -- ^ :
           | SEMICOLON            -- ^ ;
           | EQ                   -- ^ =
           | ARROW                -- ^ =>
           | LBRACE               -- ^ [
           | RBRACE               -- ^ ]
           | UNDERBAR             -- ^ _
           | VERTICALBAR          -- ^ |
           | SPECIAL Char
           deriving (Show, Eq)

toString :: Token -> String
toString = show
