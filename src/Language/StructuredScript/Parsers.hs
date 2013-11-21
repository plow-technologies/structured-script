{-# LANGUAGE NoImplicitPrelude,OverloadedStrings #-}
module Language.StructuredScript.Parsers where
import ClassyPrelude hiding ((<|>), insert, lookup, empty)
import Data.Functor.Identity
import Text.Parsec 
import Text.Parsec.String
import Text.Parsec.Expr
import Text.Parsec.Language
import Text.Parsec.Token
import Control.Applicative((<*>),(<$>))
import Data.HashMap.Lazy hiding (foldl')
import Data.List (foldl')


{-| SST Grammer 


Useage examples: 

x := true;

|-}


data Expr = Var String | Con Const | Uno Unop Expr | Duo Duop Expr Expr 
    deriving Show

type VType = Const 
emptyVTable = VT empty 

newtype VarTable = VT (HashMap Text VType) deriving (Show, Eq) 

testStmtList = Seq ["x" := Duo Add (Con (ConstInteger 5)) (Con (ConstInteger 5))];
testStmt = "x" := Duo Add (Con (ConstInteger 5)) (Con (ConstInteger 5));
testExpr = Duo Add (Con (ConstInteger 5)) (Con (ConstInteger 5));
testExpr2 = Duo Add (Con (ConstDouble 5.5)) (Con (ConstDouble 4.3));

evalExpr :: VarTable -> Expr -> Either String Const
evalExpr _ (Con x) = Right x

evalExpr v (Duo op e1 e2) = do 
    e1' <-   evalExpr v e1
    e2' <-   evalExpr v e2
    duopLookUp op  e1' e2'

evalExpr v@ (VT vt) (Var s) = case lookup (pack s) vt of 
                    Nothing -> Left $ "Does not Exist" ++ s
                    (Just found) ->  Right $ found     

evalStmt ::  VarTable -> Stmt -> Either String VarTable
evalStmt v@ (VT vt) (Seq lst) =  foldl' (\a b -> loop a b) (Right v) lst
                                where loop :: Either String VarTable -> Stmt -> Either String VarTable
                                      loop (Right vtable) s = evalStmt vtable s 
                                      loop error _ = error
                                
evalStmt v st@ (s := e) = insertToLut v st 

evalStmt v@ (VT vt) (If e s1 s2) = case evalExpr v e of 
                                (Right (ConstBool True)) -> evalStmt v s1 
                                (Right (ConstBool False)) -> evalStmt v s2
                                (Right x) -> Left $ (show x) ++ " not a well formed bool"
                                (Left x) -> Left $ x ++ " not a well formed bool"
evalStmt v (Nop) = Right v 

evalStmt _ _ = Left "Not Impremented"

insertToLut :: VarTable -> Stmt -> Either String VarTable 
insertToLut v@ (VT vt) (s := e) =  case evalExpr v e of 
                                Left s -> Left $ s ++ "In insertToLut"
                                Right c -> Right $ VT $ insert (pack s) c vt
insertToLut vt ( _ ) = Left "Received other error in insertToLut"
 
duopLookUp :: Duop -> Const -> Const -> Either String Const
duopLookUp (Add)  (ConstBool _ ) _ = Left "Expected Double or Integer, received Bool First Argument"
duopLookUp (Add) (ConstString _) _ = Left "Expected Double or Integer, received String First Argument"
duopLookUp (Add) (ConstChar _) _ = Left "Expected Double or Integer, received Char First Argument"
duopLookUp (Add) (ConstInteger i) (ConstInteger j) = Right $ ConstInteger $ i + j
duopLookUp (Add) (ConstInteger i) (ConstDouble d) = Right $ ConstDouble $ (fromIntegral i) + d
duopLookUp (Add) (ConstDouble d1) (ConstDouble d2) = Right $ ConstDouble $ d1 + d2
duopLookUp (Add) (ConstDouble d) (ConstInteger i) = Right $ ConstDouble $ d + (fromIntegral i)
duopLookUp (Add) (_) (ConstBool _) = Left "Expected Double or Integer, received Bool Second Argument"
duopLookUp (Add) (_) (ConstString _) = Left "Expected Double or Integer, received String Second Argument"
duopLookUp (Add) (_) (ConstChar _) = Left "Expected Double or Integer, received Char Second Argument"

-- Subtraction
duopLookUp (Sub) (ConstBool _ ) _ = Left "Expected Double or Integer, received Bool First Argument"
duopLookUp (Sub) (ConstString _) _ = Left "Expected Double or Integer, received String First Argument"
duopLookUp (Sub) (ConstChar _) _ = Left "Expected Double or Integer, received Char First Argument"
duopLookUp (Sub) (ConstInteger i) (ConstInteger j) = Right $ ConstInteger $ i - j
duopLookUp (Sub) (ConstInteger i) (ConstDouble d) = Right $ ConstDouble $ (fromIntegral i) - d
duopLookUp (Sub) (ConstDouble d1) (ConstDouble d2) = Right $ ConstDouble $ d1 - d2
duopLookUp (Sub) (ConstDouble d) (ConstInteger i) = Right $ ConstDouble $ d - (fromIntegral i)
duopLookUp (Sub) (_) (ConstBool _) = Left "Expected Double or Integer, received Bool Second Argument"
duopLookUp (Sub) (_) (ConstString _) = Left "Expected Double or Integer, received String Second Argument"
duopLookUp (Sub) (_) (ConstChar _) = Left "Expected Double or Integer, received Char Second Argument"

-- Multiplication
duopLookUp (Mul) (ConstBool _ ) _ = Left "Expected Double or Integer, received Bool First Argument"
duopLookUp (Mul) (ConstString _) _ = Left "Expected Double or Integer, received String First Argument"
duopLookUp (Mul) (ConstChar _) _ = Left "Expected Double or Integer, received Char First Argument"
duopLookUp (Mul) (ConstInteger i) (ConstInteger j) = Right $ ConstInteger $ i * j
duopLookUp (Mul) (ConstInteger i) (ConstDouble d) = Right $ ConstDouble $ (fromIntegral i) * d
duopLookUp (Mul) (ConstDouble d1) (ConstDouble d2) = Right $ ConstDouble $ d1 * d2
duopLookUp (Mul) (ConstDouble d) (ConstInteger i) = Right $ ConstDouble $ d * (fromIntegral i)
duopLookUp (Mul) (_) (ConstBool _) = Left "Expected Double or Integer, received Bool Second Argument"
duopLookUp (Mul) (_) (ConstString _) = Left "Expected Double or Integer, received String Second Argument"
duopLookUp (Mul) (_) (ConstChar _) = Left "Expected Double or Integer, received Char Second Argument"

-- Division
duopLookUp (Div) (ConstBool _ ) _ = Left "Expected Double or Integer, received Bool First Argument"
duopLookUp (Div) (ConstString _) _ = Left "Expected Double or Integer, received String First Argument"
duopLookUp (Div) (ConstChar _) _ = Left "Expected Double or Integer, received Char First Argument"
duopLookUp (Div) (ConstInteger i) (ConstInteger j) = Right $ ConstInteger $ quot i j
duopLookUp (Div) (ConstInteger i) (ConstDouble d) 
			|d /= 0 =  Right $ ConstDouble $ (fromIntegral i) / d
			|otherwise = Left "Divided by Zero Error"
duopLookUp (Div) (ConstDouble d1) (ConstDouble d2) = Right $ ConstDouble $ d1 / d2
duopLookUp (Div) (ConstDouble d) (ConstInteger i) = Right $ ConstDouble $ d / (fromIntegral i)
duopLookUp (Div) (_) (ConstBool _) = Left "Expected Double or Integer, received Bool Second Argument"
duopLookUp (Div) (_) (ConstString _) = Left "Expected Double or Integer, received String Second Argument"
duopLookUp (Div) (_) (ConstChar _) = Left "Expected Double or Integer, received Char Second Argument"


-- testing for equality
duopLookUp (Equal) (ConstBool b1) (ConstBool b2) = Right $ ConstBool $ b1 == b2
duopLookUp (Equal) (ConstString s1) (ConstString s2) = Right $ ConstBool $ s1 == s2
duopLookUp (Equal) (ConstChar c1) (ConstChar c2) = Right $ ConstBool $ c1 == c2
duopLookUp (Equal) (ConstInteger i1) (ConstInteger i2) = Right $ ConstBool $ i1 == i2
duopLookUp (Equal) (ConstInteger i) (ConstDouble d) = Right $ ConstBool $ (fromIntegral i) == d
duopLookUp (Equal) (ConstDouble d) (ConstInteger i) = Right $ ConstBool $ d == (fromIntegral i)
duopLookUp (Equal) (ConstDouble d1) (ConstDouble d2) = Right $ ConstBool $ d1 == d2
duopLookUp (Equal) (_) (_) = Left "The two variables are not comparable"

-- testing for Greater than
duopLookUp (Greater) (ConstBool b1) (ConstBool b2) = Right $ ConstBool $ b1 > b2
duopLookUp (Greater) (ConstString s1) (ConstString s2) = Right $ ConstBool $ s1 > s2
duopLookUp (Greater) (ConstChar c1) (ConstChar c2) = Right $ ConstBool $ c1 > c2
duopLookUp (Greater) (ConstInteger i1) (ConstInteger i2) = Right $ ConstBool $ i1 > i2
duopLookUp (Greater) (ConstInteger i) (ConstDouble d) = Right $ ConstBool $ (fromIntegral i) > d
duopLookUp (Greater) (ConstDouble d) (ConstInteger i) = Right $ ConstBool $ d > (fromIntegral i)
duopLookUp (Greater) (ConstDouble d1) (ConstDouble d2) = Right $ ConstBool $ d1 > d2
duopLookUp (Greater) (_) (_) = Left "The two variables are not comparable"

-- testing for Less than
duopLookUp (Less) (ConstBool b1) (ConstBool b2) = Right $ ConstBool $ b1 < b2
duopLookUp (Less) (ConstString s1) (ConstString s2) = Right $ ConstBool $ s1 < s2
duopLookUp (Less) (ConstChar c1) (ConstChar c2) = Right $ ConstBool $ c1 < c2
duopLookUp (Less) (ConstInteger i1) (ConstInteger i2) = Right $ ConstBool $ i1 < i2
duopLookUp (Less) (ConstInteger i) (ConstDouble d) = Right $ ConstBool $ (fromIntegral i) < d
duopLookUp (Less) (ConstDouble d) (ConstInteger i) = Right $ ConstBool $ d < (fromIntegral i)
duopLookUp (Less) (ConstDouble d1) (ConstDouble d2) = Right $ ConstBool $ d1 < d2
duopLookUp (Less) (_) (_) = Left "The two variables are not comparable"

-- testing for Greater than and Equal
duopLookUp (GreaterEqual) (ConstBool b1) (ConstBool b2) = Right $ ConstBool $ b1 >= b2
duopLookUp (GreaterEqual) (ConstString s1) (ConstString s2) = Right $ ConstBool $ s1 >= s2
duopLookUp (GreaterEqual) (ConstChar c1) (ConstChar c2) = Right $ ConstBool $ c1 >= c2
duopLookUp (GreaterEqual) (ConstInteger i1) (ConstInteger i2) = Right $ ConstBool $ i1 >= i2
duopLookUp (GreaterEqual) (ConstInteger i) (ConstDouble d) = Right $ ConstBool $ (fromIntegral i) >= d
duopLookUp (GreaterEqual) (ConstDouble d) (ConstInteger i) = Right $ ConstBool $ d >= (fromIntegral i)
duopLookUp (GreaterEqual) (ConstDouble d1) (ConstDouble d2) = Right $ ConstBool $ d1 >= d2
duopLookUp (GreaterEqual) (_) (_) = Left "The two variables are not comparable"

-- testing for Less than and Equal
duopLookUp (LessEqual) (ConstBool b1) (ConstBool b2) = Right $ ConstBool $ b1 <= b2
duopLookUp (LessEqual) (ConstString s1) (ConstString s2) = Right $ ConstBool $ s1 <= s2
duopLookUp (LessEqual) (ConstChar c1) (ConstChar c2) = Right $ ConstBool $ c1 <= c2
duopLookUp (LessEqual) (ConstInteger i1) (ConstInteger i2) = Right $ ConstBool $ i1 < i2
duopLookUp (LessEqual) (ConstInteger i) (ConstDouble d) = Right $ ConstBool $ (fromIntegral i) <= d
duopLookUp (LessEqual) (ConstDouble d) (ConstInteger i) = Right $ ConstBool $ d <= (fromIntegral i)
duopLookUp (LessEqual) (ConstDouble d1) (ConstDouble d2) = Right $ ConstBool $ d1 <= d2
duopLookUp (LessEqual) (_) (_) = Left "The two variables are not comparable"

-- testing for Not Equal
duopLookUp (NotEqual) (ConstBool b1) (ConstBool b2) = Right $ ConstBool $ b1 /= b2
duopLookUp (NotEqual) (ConstString s1) (ConstString s2) = Right $ ConstBool $ s1 /= s2
duopLookUp (NotEqual) (ConstChar c1) (ConstChar c2) = Right $ ConstBool $ c1 /= c2
duopLookUp (NotEqual) (ConstInteger i1) (ConstInteger i2) = Right $ ConstBool $ i1 /= i2
duopLookUp (NotEqual) (ConstInteger i) (ConstDouble d) = Right $ ConstBool $ (fromIntegral i) /= d
duopLookUp (NotEqual) (ConstDouble d) (ConstInteger i) = Right $ ConstBool $ d /= (fromIntegral i)
duopLookUp (NotEqual) (ConstDouble d1) (ConstDouble d2) = Right $ ConstBool $ d1 /= d2
duopLookUp (NotEqual) (_) (_) = Left "The two variables are not comparable"




data Const = ConstBool Bool 
           | ConstInteger Integer 
           | ConstString String 
           | ConstChar Char
           | ConstDouble Double
           deriving (Show, Eq, Ord)
                    
                   
data Unop = Not deriving Show

data Duop = And | Or | Xor | Iff 
           | Add | Mul | Div | Sub | Mod 
           | Greater | Less | Equal | GreaterEqual | LessEqual | NotEqual
           deriving Show

data Stmt = Nop | External | Global |String := Expr | If Expr Stmt Stmt 
          | Seq [Stmt]
          deriving Show


def :: GenLanguageDef String st Identity
def = emptyDef{ commentStart = "/*"
              , commentEnd = "*/"
              , identStart = letter
              , identLetter = alphaNum <|> char '_'
              , nestedComments = False
              , caseSensitive = True                             
              , opStart = oneOf "==<>@^|&+-*/$MOD!?~.:>=<="
              , opLetter = oneOf "==<>@^|&+-*/$MOD!?~.:"
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
        , [Infix (sst_reservedOp "==" >> return (Duo Equal)) AssocLeft]
        , [Infix (sst_reservedOp "+" >> return (Duo Add)) AssocLeft]
        , [Infix (sst_reservedOp "-" >> return (Duo Sub)) AssocLeft]
        , [Infix (sst_reservedOp "*" >> return (Duo Mul)) AssocLeft]
        , [Infix (sst_reservedOp "/" >> return (Duo Div)) AssocLeft]
        , [Infix (sst_reservedOp "MOD" >> return (Duo Mod)) AssocLeft]
        , [Infix (sst_reservedOp ">" >> return (Duo Greater)) AssocLeft]
        , [Infix (sst_reservedOp "<" >> return (Duo Less)) AssocLeft]
	, [Infix (sst_reservedOp ">=" >> return (Duo GreaterEqual)) AssocLeft]
        , [Infix (sst_reservedOp "<=" >> return (Duo LessEqual)) AssocLeft]
	, [Infix (sst_reservedOp "<>" >> return (Duo NotEqual)) AssocLeft]
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


testString = "x:=18; y:= 0; if (x > y) then z:= x/y; else z:= y;end_if/*; x:=y */"



play :: String -> IO ()
play inp = case parse mainparser "" inp of
              { Left err -> print err
              ; Right ans -> print ans
              }
                 
