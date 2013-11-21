# structured-script-lang

## Structured Script is.

+ A subset of [Structured Text](http://en.wikipedia.org/wiki/Structured_text) with some modernizing modifications
+ referentially transparent (no side effects)
+ Interpreted

## Data Types

+ **Double**   -- IEEE DOUBLE 
+ **Integer**  -- Variable Length Integer Type 
+ **String**   -- Packed String Type
+ **Char**     -- UTF8
+ **Bool**     -- Boolean Value Logical operands 


## Operators ##
  (*Operators in Italics are not implemented yet*)
### Logical ###
Boolean operators take Data Types and return whether one value of the data type is greater than another.
*Be careful when trying to compare equality between floating point numbers.*

+ **=** 
+ *AND*
+ *OR*
+ *NOT*


### Arithmetic ###

+ **=**
+ **>** 
+ **<**
+ *>=*
+ *<=*
+ *+*
+ *-*
+ */*
+ *\**


### Bitwise ###
+ *&* Bitwise And
+ *OR* Bitwise Or
+ *XOR* Bitwise exclusive Or
+ *NOT* Bitwise negation


##Functions##
(*Functions in Italics are not implemented yet*)
### Bitwise ###
+ *isSet* checks a bit in position N of an INT, returns true if that bit is set
  * ```isSet 0 4;``` returns **False** ```isSet 3;``` returns **True**
+ *shiftN*



## Constructs
+ *variable_name* **:=** *statement* **;**
  * Use to assign a name to a constant or expression
  * ``` x:=3; ```
+ **IF** *bool_expression* **Then** *statement_list* **Else** *statement_list*
  * Use to branch alternatives
  * ``` IF (3 > 4) THEN x:=3; Else x:=4; END_IF ```





## How to run tests

```
cabal configure --enable-tests && cabal build && cabal test
```

## Contributing

TODO: Write contribution instructions here
