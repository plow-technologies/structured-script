{-# LANGUAGE NoImplicitPrelude,OverloadedStrings #-}
module Language.StructuredScript.Parsers where
import ClassyPrelude hiding ((<|>))
import Data.Functor.Identity
import Text.Parsec 
import Text.Parsec.String
import Text.Parsec.Expr
import Text.Parsec.Language
import Text.Parsec.Token
import Control.Applicative((<*>),(<$>))
--import Data.Either

{-| SST Grammer 


Useage examples: 

x := true;

|-}


data Expr = Var String | Con Const | Uno Unop Expr | Duo Duop Expr Expr 
    deriving Show

testStmtList = Seq ["x" := Duo Add (Con (ConstInteger 5)) (Con (ConstInteger 5))];
testStmt = "x" := Duo Add (Con (ConstInteger 5)) (Con (ConstInteger 5));
testExpr = Duo Add (Con (ConstInteger 5)) (Con (ConstInteger 5));
testExpr2 = Duo Add (Con (ConstDouble 5.5)) (Con (ConstDouble 4.3));

evalExpr :: Expr -> Either String Const
evalExpr (Con x) = Right x

evalExpr (Duo op e1 e2) = do 
    e1' <-   evalExpr e1
    e2' <-   evalExpr e2
    duopLookUp op  e1' e2'

duopLookUp :: Duop -> Const -> Const -> Either String Const
duopLookUp (Add) (ConstBool _ ) _ = Left "Expected Double or Integer, received Bool First Argument"
duopLookUp (Add) (ConstString _) _ = Left "Expected Double or Integer, received String First Argument"
duopLookUp (Add) (ConstChar _) _ = Left "Expected Double or Integer, received Char First Argument"
duopLookUp (Add) (ConstInteger i) (ConstInteger j) = Right $ ConstInteger $ i + j
duopLookUp (Add) (ConstInteger i) (ConstDouble d) = Right $ ConstDouble $ (fromIntegral i) + d
duopLookUp (Add) (ConstDouble d1) (ConstDouble d2) = Right $ ConstDouble $ d1 + d2
duopLookUp (Add) (ConstDouble d) (ConstInteger i) = Right $ ConstDouble $ d + (fromIntegral i)
duopLookUp (Add) (_) (ConstBool _) = Left "Expected Double or Integer, received Bool Second Argument"
duopLookUp (Add) (_) (ConstString _) = Left "Expected Double or Integer, received String Second Argument"
duopLookUp (Add) (_) (ConstChar _) = Left "Expected Double or Integer, received Char Second Argument"


data Const = ConstBool Bool 
           | ConstInteger Integer 
           | ConstString String 
           | ConstChar Char
           | ConstDouble Double
           deriving (Show, Eq)
                    

                    
data Unop = Not deriving Show

data Duop = And | Or | Xor | Iff 
           | Add | Mul | Div | Sub | Mod 
           | Greater | Less | Equal 
           deriving Show

data Stmt = Nop | External | Global |String := Expr | If Expr Stmt Stmt 
          | Seq [Stmt]
          deriving Show


def :: GenLanguageDef String st Identity
def = emptyDef{ commentStart = "/*"
              , commentEnd = "*/"
              , identStart = letter
              , identLetter = alphaNum
              , nestedComments = False
              , caseSensitive = True                             
              , opStart = oneOf "=<>@^|&+-*/$MOD!?~.:"
              , opLetter = oneOf "=<>@^|&+-*/$MOD!?~.:"
              , reservedOpNames = ["**",":=","NOT","~","*","/","MOD","+","-",">=","<=","<",">","=","<>","&","AND","OR","XOR",";"]
              , reservedNames = ["true", "false", "nop",
                                 "if", "then", "else", "end_if"
                                  ] }


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

sst_naturalOrDouble :: ParsecT String u Identity (Either Integer Double)
sst_naturalOrDouble = naturalOrFloat sst_lexer

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
        , [Infix (sst_reservedOp "|" >> return (Duo Or)) AssocLeft]
        , [Infix (sst_reservedOp "=" >> return (Duo Iff)) AssocLeft]
        , [Infix (sst_reservedOp "+" >> return (Duo Add)) AssocLeft]
        , [Infix (sst_reservedOp "-" >> return (Duo Sub)) AssocLeft]
        , [Infix (sst_reservedOp "*" >> return (Duo Mul)) AssocLeft]
        , [Infix (sst_reservedOp "MOD" >> return (Duo Mod)) AssocLeft]
        , [Infix (sst_reservedOp ">" >> return (Duo Greater)) AssocLeft]
        , [Infix (sst_reservedOp "<" >> return (Duo Less)) AssocLeft]
        ]

term :: ParsecT String () Identity Expr        
term = sst_parens exprparser
       <|> fmap Var sst_identifier
       <|> boolTParser
       <|> boolFParser
       <|>naturalOrDoubleParser
       <|> stringParser
      -- <|> intParser
      -- <|> doubleParser


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

--doubleParser :: ParsecT String u Identity Expr 
--doubleParser = (sst_double >>= (\x -> return (Con (ConstDouble x))))

naturalOrDoubleParser :: ParsecT String  u Identity Expr
naturalOrDoubleParser = (sst_naturalOrDouble >>= (\x ->return $ makeNum x))
                        where makeNum = either makeConstInt makeConstDouble
                              makeConstInt = (Con).(ConstInteger)
                              makeConstDouble = (Con).(ConstDouble)


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
                     ; sst_reserved "end_if"
                     ; return (If b p q)
                     }
              <|> return Nop



testString = "if (true) then y:= 5;else = 6;nop"


play :: String -> IO ()
play inp = case parse mainparser "" inp of
              { Left err -> print err
              ; Right ans -> print ans
              }
                 
