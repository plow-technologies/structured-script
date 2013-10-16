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

expr  ::= var | const | ( expr ) | unop expr | expr duop expr
var   ::= letter { letter | digit }*
const ::= true | false
unop  ::= - | ** | NOT
duop  ::= & | =

def = emptyDef { commentStart = "/*",
                 commentEnd = "*/",
                 identStart = letter,
                 identLetter = alphaNum,
                 nestedComments = False,
                 caseSensitive = True,
                 opStart = "*-<>/=&NAXOM:;",
                 opLetter= "*-<>/=&DRT;",
                 reservedOpNames = ["**",":=",,"NOT","*","/","MOD","+","-",">=","<=","<",">","=","<>","&","AND","OR","XOR",";"],
                 reservedNames = ["VAR","END_VAR","FALSE","TRUE","CONST","END_CONST","IF","THEN",
                                  "BOOL","SINT","INT","DINT","REAL","STRING","CASE","OF","FOR","DO"
                                  ,"WHILE","REPEAT","UNTIL"]
|-}



data Expr = Var String | Con Const | Uno Unop Expr | Duo Duop Expr Expr
     deriving Show


data Const = ConstBool Bool
           deriving (Show)
                    

                    
data Unop = Not deriving Show

data Duop = And | Iff deriving Show

data Stmt = Nop | String := Expr | If Expr Stmt Stmt | While Expr Stmt
          | Seq [Stmt]
          deriving Show


def :: GenLanguageDef String st Identity
def = emptyDef{ commentStart = "/*"
              , commentEnd = "*/"
              , identStart = letter
              , identLetter = alphaNum
              , nestedComments = False
              , caseSensitive = True                             
              , opStart = oneOf "~&=:"
              , opLetter = oneOf "~&=:"
              , reservedOpNames = ["**",":=","NOT","~","*","/","MOD","+","-",">=","<=","<",">","=","<>","&","AND","OR","XOR",";"]
              , reservedNames = ["true", "false", "nop",
                                 "if", "then", "else", "fi",
                                 "while", "do", "od"] }

TokenParser { parens     = m_parens
            , identifier = m_identifier
            , reservedOp = m_reservedOp
            , reserved   = m_reserved
            , semiSep1   = m_semiSep1
            , whiteSpace = m_whiteSpace } = makeTokenParser def

exprparser :: Parser Expr
exprparser = buildExpressionParser table term <?> "expression"

table = [ [Prefix (m_reservedOp "~" >> return (Uno Not))]
        , [Infix (m_reservedOp "&" >> return (Duo And)) AssocLeft]
        , [Infix (m_reservedOp "=" >> return (Duo Iff)) AssocLeft]
        ]
        
term = m_parens exprparser
       <|> fmap Var m_identifier
       <|> (m_reserved "true" >> return (Con (ConstBool True)))
       <|> (m_reserved "false" >> return (Con (ConstBool False)))


mainparser :: Parser Stmt
mainparser = m_whiteSpace >> stmtparser <* eof
    where
      stmtparser :: Parser Stmt
      stmtparser = fmap Seq (m_semiSep1 stmt1)
      stmt1 = (m_reserved "nop" >> return Nop)
              <|> do { v <- m_identifier
                     ; m_reservedOp ":="
                     ; e <- exprparser
                     ; return (v := e)
                     }
              <|> do { m_reserved "if"
                     ; b <- exprparser
                     ; m_reserved "then"
                     ; p <- stmtparser
                     ; m_reserved "else"
                     ; q <- stmtparser
                     ; m_reserved "fi"
                     ; return (If b p q)
                     }
              <|> do { m_reserved "while"
                     ; b <- exprparser
                     ; m_reserved "do"
                     ; p <- stmtparser
                     ; m_reserved "od"
                     ; return (While b p)
                     }








play :: String -> IO ()
play inp = case parse mainparser "" inp of
              { Left err -> print err
              ; Right ans -> print ans
              }
                 
                  
