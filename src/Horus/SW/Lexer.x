{
{-# OPTIONS_GHC -w #-}
module Horus.SW.Lexer where
}

%wrapper "basic"

tokens :-
    $white+                     ;
    \,                          { \s -> TokenComma }
    \*                          { \s -> TokenStar }
    \*\*                        { \s -> TokenDoubleStar }
    felt                        { \s -> TokenFelt }
    codeoffset                  { \s -> TokenCodeoffset }
    [a-zA-Z\_][a-zA-Z\_0-9]*    { \s -> TokenIdentifier s }
    \.                          { \s -> TokenDot }
    ":"                         { \s -> TokenColon }
    \(                          { \s -> TokenLParen }
    \)                          { \s -> TokenRParen }

{ 
data Token 
    = TokenDot
    | TokenComma
    | TokenColon
    | TokenStar
    | TokenDoubleStar
    | TokenLParen
    | TokenRParen
    | TokenFelt
    | TokenCodeoffset
    | TokenIdentifier String
    deriving (Show)
}