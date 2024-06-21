{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Parser (play) where

import Data.Aeson hiding ((<?>))
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Expr
import Text.Parsec.Token
import Text.Parsec.Language

-- Sample grammar
data Expr = Var String | Con Bool | Uno Unop Expr | Duo Duop Expr Expr
    deriving Show
data Unop = Not deriving Show
data Duop = And | Iff deriving Show
data Stmt = Nop | String := Expr | If Expr Stmt Stmt | While Expr Stmt
          | Seq [Stmt]
    deriving Show

-- Define symbols
def = emptyDef{ commentStart = "{-"
              , commentEnd = "-}"
              , identStart = letter
              , identLetter = alphaNum
              , opStart = oneOf "~&=:"
              , opLetter = oneOf "~&=:"
              , reservedOpNames = ["~", "&", "=", ":="]
              , reservedNames = ["true", "false", "nop",
                                 "if", "then", "else", "fi",
                                 "while", "do", "od"]
              }

-- Make token parser
TokenParser{ parens = m_parens
           , identifier = m_identifier
           , reservedOp = m_reservedOp
           , reserved = m_reserved
           , semiSep1 = m_semiSep1
           , whiteSpace = m_whiteSpace } = makeTokenParser def

-- Expression parser
exprparser :: Parser Expr
exprparser = buildExpressionParser table term <?> "expression"
table = [ [Prefix (m_reservedOp "~" >> return (Uno Not))]
        , [Infix (m_reservedOp "&" >> return (Duo And)) AssocLeft]
        , [Infix (m_reservedOp "=" >> return (Duo Iff)) AssocLeft]
        ]
term = m_parens exprparser
       <|> fmap Var m_identifier
       <|> (m_reserved "true" >> return (Con True))
       <|> (m_reserved "false" >> return (Con False))

-- Statement parser
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

instance ToJSON Expr where
    toJSON (Var s) = object ["Var" .= s]
    toJSON (Con b) = object ["Con" .= b]
    toJSON (Uno op expr) = object ["Uno" .= op, "Expr" .= expr]
    toJSON (Duo op expr1 expr2) = object ["Duo" .= op, "Expr1" .= expr1, "Expr2" .= expr2]

instance ToJSON Unop where
    toJSON Not = "Not"

instance ToJSON Duop where
    toJSON And = "And"
    toJSON Iff = "Iff"

instance ToJSON Stmt where
    toJSON Nop = object ["Nop" .= Null]
    toJSON (s := e) = object ["Assign" .= object ["Variable" .= s, "Expression" .= e]]
    toJSON (If cond stmt1 stmt2) = object ["If" .= object ["Condition" .= cond, "Then" .= stmt1, "Else" .= stmt2]]
    toJSON (While cond stmt) = object ["While" .= object ["Condition" .= cond, "Body" .= stmt]]
    toJSON (Seq stmts) = object ["Sequence" .= stmts]

play :: String -> Either ParseError Stmt
play = parse mainparser ""
