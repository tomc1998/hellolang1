module Token (Token(Token), tokVal, tokType
             ,TokenType(Punc
                       ,Keyword
                       ,Operator
                       ,Identifier
                       ,Literal)) where

data TokenType = Punc | Keyword | Operator | Identifier | Literal deriving (Show)

data Token = Token {tokVal :: String
                   ,tokType :: TokenType} deriving (Show)


