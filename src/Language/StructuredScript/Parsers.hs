{-# LANGUAGE NoImplicitPrelude,OverloadedStrings #-}
module Language.StructuredScript.Parsers where
import ClassyPrelude
import Data.Functor.Identity
import Text.Parsec 
import Text.Parsec.Char
import Text.Parsec.Text
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
                 reservedOpNames = ["**",":=","NOT","*","/","MOD","+","-",">=","<=","<",">","=","<>","&","AND","OR","XOR",";"],
                 reservedNames = ["VAR","END_VAR","FALSE","TRUE","CONST","END_CONST","IF","THEN",
                                  "BOOL","SINT","INT","DINT","REAL","STRING","CASE","OF","FOR","DO"
                                  ,"WHILE","REPEAT","UNTIL"]
                                  




|-}


newtype SSTbool      = SSTbool    Bool    deriving (Show) 
newtype SSTsint      = SSTsint    Int     deriving (Show) 
newtype SSTint       = SSTint     Int     deriving (Show) 
newtype SSTdint      = SSTdint    Int     deriving (Show) 
newtype SSTreal      = SSTreal    Double  deriving (Show) 
newtype SSTstring    = SSTstring  Text    deriving (Show) 


-- |Symbol definitions 
startList :: String 
startList = "*-<>/=&NAXOM:;"

endList :: String
endList = "*-<>/=&DRT;"



def :: GenLanguageDef Text st Identity
def = emptyDef { commentStart = "/*",
                 commentEnd = "*/",
                 identStart = letter,
                 identLetter = alphaNum,
                 nestedComments = False,
                 caseSensitive = True,
                 opStart = oneOf startList,
                 opLetter= oneOf endList,
                 reservedOpNames = ["**",":=","NOT","*","/","MOD","+","-",">=","<=","<",">","=","<>","&","AND","OR","XOR",";"],
                 reservedNames = ["VAR","END_VAR","FALSE","TRUE","CONST","END_CONST","IF","THEN",
                                  "BOOL","SINT","INT","DINT","REAL","STRING","CASE","OF","FOR","DO"
                                  ,"WHILE","REPEAT","UNTIL"]
               }
                                  
sstTokenParser :: GenTokenParser Text u Identity
sstTokenParser = makeTokenParser def

sstWhitespace  :: ParsecT Text u Identity ()
sstWhitespace  = whiteSpace sstTokenParser

sstLexeme      :: ParsecT Text u Identity () -> ParsecT Text u Identity ()
sstLexeme      = lexeme     sstTokenParser

sstSymbol      :: String -> ParsecT Text u Identity String
sstSymbol      = symbol     sstTokenParser

sstNatural     :: ParsecT Text u Identity Integer
sstNatural     = natural    sstTokenParser

sstParens      :: ParsecT Text u Identity ()-> ParsecT Text u Identity ()
sstParens      = parens     sstTokenParser

sstSemi        :: ParsecT Text u Identity String
sstSemi        = semi       sstTokenParser

sstIdentifier  :: ParsecT Text u Identity String
sstIdentifier  = identifier sstTokenParser

sstReserved    :: String -> ParsecT Text u Identity ()
sstReserved    = reserved   sstTokenParser

sstReservedop  :: String -> ParsecT Text u Identity ()
sstReservedop  = reservedOp sstTokenParser


