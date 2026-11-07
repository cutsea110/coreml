module Token where

import Prelude hiding (Ordering(..))

-- | Identifier name. Its 'Show' instance prints the raw string without
-- quotes, so that the derived 'Show' for 'Token' renders @ID abc@.
newtype Name = Name String deriving Eq

instance Show Name where
  showsPrec _ (Name s) = showString s

data Token = EOF
           | ID Name
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
           | DOT                  -- ^ .
           | TRIPLEDOTS           -- ^ ...
           | COLON                -- ^ :
           | SEMICOLON            -- ^ ;
           | EQ                   -- ^ =
           | ARROW                -- ^ =>
           | LBRACKET             -- ^ [
           | RBRACKET             -- ^ ]
           | UNDERBAR             -- ^ _
           | VERTICALBAR          -- ^ |
           | SPECIAL Char
           deriving (Show, Eq)
