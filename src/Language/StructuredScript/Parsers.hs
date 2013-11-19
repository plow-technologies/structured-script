{-# LANGUAGE NoImplicitPrelude,OverloadedStrings #-}
module Language.StructuredScript.Parsers where
import ClassyPrelude hiding ((<|>))
import Data.Functor.Identity
import Text.Parsec 
import Text.Parsec.String
import Text.Parsec.Expr
import Text.Parsec.Language
import Text.Parsec.Token

{-| SST Grammer 


Useage examples: 

x := true;

|-}



data Expr = Var String | Con Const | Uno Unop Expr | Duo Duop Expr Expr
     deriving Show


data Const = ConstBool Bool 
           | ConstInteger Integer 
           | ConstString String 
           | ConstChar Char
           | ConstDouble Double
           deriving (Show)
                    

                    
data Unop = Not deriving Show

data Duop = And | Iff deriving Show

data Stmt = Nop | String := Expr | If Expr Stmt Stmt | While Expr Stmt
          | Seq [Stmt]
          deriving Show

opChar = oneOf "=<>@^|&+-*/$%!?~.:"

def :: GenLanguageDef String st Identity
def = emptyDef{ commentStart = "/*"
              , commentEnd = "*/"
              , identStart = letter
              , identLetter = alphaNum
              , nestedComments = False
              , caseSensitive = True                             
              , opStart = opChar
              , opLetter = opChar
              , reservedOpNames = ["**",":=","NOT","~","*","/","MOD","+","-",">=","<=","<",">","=","<>","&","AND","OR","XOR",";"]
              , reservedNames = ["true", "false", "nop",
                                 "if", "then", "else", "fi",
                                 "while", "do", "od"] }


-- | Create specific parsers from the generic parser creator in Parsec
sst_lexer :: GenTokenParser String u Identity
sst_lexer = makeTokenParser def 

sst_parens ::ParsecT String u Identity a -> ParsecT String u Identity a
sst_parens      = parens sst_lexer   

sst_identifier :: ParsecT String u Identity String
sst_identifier  = identifier   sst_lexer 

sst_reservedOp :: String -> ParsecT String u Identity ()
sst_reservedOp  =  reservedOp   sst_lexer 

sst_reserved :: String -> ParsecT String u Identity ()
sst_reserved = reserved     sst_lexer 



sst_semiSep1 :: ParsecT String u Identity a -> ParsecT String u Identity [a]
sst_semiSep1    =       semiSep1     sst_lexer 


-- | Numeric Literals 

sst_natural ::  ParsecT String u Identity Integer
sst_natural     =       natural      sst_lexer 

sst_integer ::  ParsecT String u Identity Integer
sst_integer     =       integer      sst_lexer 

sst_decimal ::  ParsecT String u Identity Integer
sst_decimal     =       decimal      sst_lexer 

sst_octal ::  ParsecT String u Identity Integer
sst_octal       =       octal        sst_lexer 

sst_hexadecimal ::  ParsecT String u Identity Integer
sst_hexadecimal =       hexadecimal  sst_lexer 

sst_double :: ParsecT String u Identity Double 
sst_double = float sst_lexer


-- | String Literal 
sst_stringLiteral :: ParsecT String u Identity String 
sst_stringLiteral = stringLiteral sst_lexer

sst_charLiteral :: ParsecT String u Identity Char
sst_charLiteral = charLiteral sst_lexer

sst_whiteSpace :: ParsecT String u Identity ()
sst_whiteSpace  =  whiteSpace   sst_lexer   

exprparser :: Parser Expr
exprparser = buildExpressionParser table term <?> "expression"

table :: [[Operator String u Identity Expr]]
table = [ [Prefix (sst_reservedOp "~" >> return (Uno Not))]
        , [Infix (sst_reservedOp "&" >> return (Duo And)) AssocLeft]
        , [Infix (sst_reservedOp "=" >> return (Duo Iff)) AssocLeft]
        ]

term :: ParsecT String () Identity Expr        
term = sst_parens exprparser
       <|> fmap Var sst_identifier
       <|> boolTParser
       <|> boolFParser
       <|> doubleParser
       <|> intParser
       <|> stringParser

boolTParser :: ParsecT String t Identity Expr
boolTParser = (sst_reserved "true" >> return (Con (ConstBool True)))
boolFParser :: ParsecT String t Identity Expr
boolFParser = (sst_reserved "false" >> return (Con (ConstBool False)))


-- | Parses any integer type constant to an Integer
intParser :: ParsecT String u Identity Expr
intParser = (sst_natural >>= (\x -> return (Con (ConstInteger x)))) 
            <|> (sst_integer >>= (\x -> return (Con (ConstInteger x)))) 
            <|> (sst_decimal >>= (\x -> return (Con (ConstInteger x))))                        
            <|> (sst_hexadecimal >>= (\x -> return (Con (ConstInteger x))))
            <|> (sst_octal >>= (\x -> return (Con (ConstInteger x))))

doubleParser :: ParsecT String u Identity Expr 
doubleParser = (sst_double >>= (\x -> return (Con (ConstDouble x))))

stringParser :: ParsecT String u Identity Expr
stringParser = (sst_stringLiteral >>= (\x -> return (Con (ConstString x))))

charParser :: ParsecT String u Identity Expr
charParser = (sst_charLiteral >>= (\x -> return ( Con (ConstChar x ))))



mainparser :: Parser Stmt
mainparser = sst_whiteSpace >> stmtparser <* eof
    where
      stmtparser :: Parser Stmt
      stmtparser = fmap Seq (sst_semiSep1 stmt1)
      stmt1 = (sst_reserved "nop" >> return Nop)
              <|> do { v <- sst_identifier
                     ; sst_reservedOp ":="
                     ; e <- exprparser
                     ; return (v := e)
                     }
              <|> do { sst_reserved "if"
                     ; b <- exprparser
                     ; sst_reserved "then"
                     ; p <- stmtparser
                     ; sst_reserved "else"
                     ; q <- stmtparser
                     ; sst_reserved "fi"
                     ; return (If b p q)
                     }
              <|> do { sst_reserved "while"
                     ; b <- exprparser
                     ; sst_reserved "do"
                     ; p <- stmtparser
                     ; sst_reserved "od"
                     ; return (While b p)
                     }








play :: String -> IO ()
play inp = case parse mainparser "" inp of
              { Left err -> print err
              ; Right ans -> print ans
              }
                 
                  
