module IMP.Parser (parseIMP) where

import IMP.Syntax
import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.Language (emptyDef)
import Text.Parsec.String (Parser)
import qualified Text.Parsec.Token as Tok

-- IMP input parser
parseIMP :: String -> String -> Either ParseError Stm
parseIMP channel input = parse (whitespace *> parseStm <* eof) channel input

lexer :: Tok.TokenParser ()
lexer = Tok.makeTokenParser style
    where
        ops = ["+", "-", "*", ":=", "=", "#", "<", "<=", ">", ">=", "(", ")", ";", "not", "and", "or", "||"]
        names = ["if", "then", "else", "end", "while", "do", "skip", "print", "var", "in", "procedure", "begin"]
        style =
            emptyDef
                { Tok.commentLine = "--"
                , Tok.reservedOpNames = ops
                , Tok.reservedNames = names
                , Tok.identStart = letter
                , Tok.identLetter = alphaNum <|> char '_'
                }

-- lexer helpers
identifier = Tok.identifier lexer
reserved = Tok.reserved lexer
reservedOp = Tok.reservedOp lexer
parens = Tok.parens lexer
integer = Tok.integer lexer
semi = Tok.semi lexer
whitespace = Tok.whiteSpace lexer
symbol = Tok.symbol lexer

parseAexp :: Parser Aexp
parseAexp = buildExpressionParser table term
    where
        -- operator precedence, high to low
        table =
            [ [Infix (reservedOp "*" >> return (Bin Mul)) AssocLeft]
            ,
                [ Infix (reservedOp "+" >> return (Bin Add)) AssocLeft
                , Infix (reservedOp "-" >> return (Bin Sub)) AssocLeft
                ]
            ]
        term =
            parens parseAexp
                <|> (Numeral <$> integer)
                <|> (Variable <$> identifier)

parseBexp :: Parser Bexp
parseBexp = buildExpressionParser table term
    where
        table =
            [ [Prefix (reserved "not" >> return Not)]
            , [Infix (reserved "and" >> return And) AssocLeft]
            , [Infix (reserved "or" >> return Or) AssocLeft]
            ]
        term = parens parseBexp <|> parseRel

parseRel :: Parser Bexp
parseRel = do
    e1 <- parseAexp
    rop <- parseRop
    e2 <- parseAexp
    return $ Rel rop e1 e2

parseRop :: Parser Rop
parseRop =
    (reservedOp "=" >> return Eq)
        <|> (reservedOp "#" >> return Neq)
        <|> (reservedOp "<=" >> return Leq)
        <|> (reservedOp "<" >> return Lt)
        <|> (reservedOp ">=" >> return Geq)
        <|> (reservedOp ">" >> return Gt)

parseStm :: Parser Stm
parseStm = parseSeq

parseSeq :: Parser Stm
parseSeq = do
    l <- sepBy1 parseSingleStm semi
    return $ foldr1 Seq l

parseSingleStm :: Parser Stm
parseSingleStm =
    parseSkip
        <|> parsePrint
        <|> parseAssign
        <|> parseIf
        <|> parseWhile
        <|> parens parseStm

parseSkip :: Parser Stm
parseSkip = reserved "skip" >> return Skip

parsePrint :: Parser Stm
parsePrint = reserved "print" >> Print <$> parseAexp

parseAssign :: Parser Stm
parseAssign = do
    x <- identifier
    reservedOp ":="
    e <- parseAexp
    return $ Assign x e

parseIf :: Parser Stm
parseIf = do
    reserved "if"
    b <- parseBexp
    reserved "then"
    s <- parseStm
    reserved "else"
    s' <- parseStm
    reserved "end"
    return $ If b s s'

parseWhile :: Parser Stm
parseWhile = do
    reserved "while"
    b <- parseBexp
    reserved "do"
    s <- parseStm
    reserved "end"
    return $ While b s
