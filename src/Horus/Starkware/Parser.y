{
{-# OPTIONS_GHC -w #-}
module Horus.Starkware.Parser where

import Horus.Starkware.Lexer
import Horus.Starkware.ScopedName
import Horus.Starkware.CairoType

}

%name parseCairo Type
%tokentype { Token }
%error { happyError }

%token
    '.'          {TokenDot}
    ','          {TokenComma}
    ':'          {TokenColon}
    '*'          {TokenStar}
    '**'         {TokenDoubleStar}
    '('          {TokenLParen}
    ')'          {TokenRParen}
    felt         {TokenFelt}
    codeoffset   {TokenCodeoffset}
    identifier   {TokenIdentifier $$}
%%

Identifier : identifier                 { [$1] }
           | Identifier '.' identifier  { $1 ++ [$3] }

NamedType : NonIdentifierType       { (Nothing, Just $1) }
          | Identifier              { (Just (ScopedName $1), Nothing) }
          | Identifier ':' Type     { (Just (ScopedName $1), Just $3) }

CommaTypes : NamedType                {[$1]}
           | CommaTypes ',' NamedType { $1 ++ [$3]}

NonIdentifierType   : felt               { TypeFelt }
                    | codeoffset         { TypeCodeoffset }
                    | Type '*'           { TypePointer $1 }
                    | Type '**'          { TypePointer (TypePointer $1)}
                    | '(' CommaTypes ')' { TypeTuple $2 }

Type : NonIdentifierType    { $1 }
     | Identifier           { TypeStruct (ScopedName $1) }

{

happyError :: [Token] -> a
happyError x = error ("Parse error " <> show x)

}