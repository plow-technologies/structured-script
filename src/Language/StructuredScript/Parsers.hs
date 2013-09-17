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
unop  ::= ~
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
                                  

sstTokenParser = makeTokenParser def