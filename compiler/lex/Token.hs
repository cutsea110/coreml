module Token where

data Token = EOF
           | UNDERBAR
           | ID String
           | STRING String
           | REAL Double
           | SPECIAL Char
           deriving (Show, Eq)

toString :: Token -> String
toString EOF = "EOF"
toString UNDERBAR = "UNDERBAR"
toString (ID s) = "ID " ++ s
toString (STRING s) = "STRING \"" ++ s ++ "\""
toString (REAL r) = "REAL " ++ show r
toString (SPECIAL c) = "SPECIAL '" ++ [c] ++ "'"
