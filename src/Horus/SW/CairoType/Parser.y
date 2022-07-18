{
{-# OPTIONS_GHC -w #-}
module Horus.SW.CairoType.Parser where

import Data.Text (pack)

import Horus.SW.CairoType.Lexer (Token (..))
import Horus.SW.ScopedName
import Horus.SW.CairoType

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

Identifier : identifier                 { ScopedName [$1] }
           | Identifier '.' identifier  { $1 <> (ScopedName [$3]) }

NamedType : NonIdentifierType       { (Nothing, Just $1) }
          | Identifier              { (Just $1, Nothing) }
          | Identifier ':' Type     { (Just $1, Just $3) }

CommaTypes : NamedType                {[$1]}
           | CommaTypes ',' NamedType { $1 ++ [$3]}

NonIdentifierType   : felt               { TypeFelt }
                    | codeoffset         { TypeCodeoffset }
                    | Type '*'           { TypePointer $1 }
                    | Type '**'          { TypePointer (TypePointer $1)}
                    | '(' CommaTypes ')' { TypeTuple $2 }

Type : NonIdentifierType    { $1 }
     | Identifier           { TypeStruct $1 }

{

happyError :: [Token] -> a
happyError x = error ("Parse error " <> show x)

}
