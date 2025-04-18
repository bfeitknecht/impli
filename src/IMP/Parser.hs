module IMP.Parser where

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
        ops = ["+", "-", "*", ":=", "=", "#", "<", "<=", ">", ">=", "(", ")", ";", "not", "and", "or", "||", ","]
        names =
            [ "if"
            , "then"
            , "else"
            , "end"
            , "while"
            , "do"
            , "skip"
            , "print"
            , "var"
            , "in"
            , "procedure"
            , "begin"
            , "par"
            , "true"
            , "false"
            ]
        style =
            emptyDef
                { Tok.commentLine = "--"
                , Tok.reservedOpNames = ops
                , Tok.reservedNames = names
                , Tok.identStart = letter
                , Tok.identLetter = alphaNum -- <|> char '_'
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
        term =
            parens parseBexp
                <|> parseRel
                <|> (reserved "true" >> return (Boolean True))
                <|> (reserved "false" >> return (Boolean False))

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
parseStm = buildExpressionParser table parseSeq
    where
        table =
            [ [Infix (reservedOp "||" >> return NonDet) AssocLeft]
            , [Infix (reserved "par" >> return Par) AssocLeft]
            ]

parseSeq :: Parser Stm
parseSeq = do
    l <- sepBy1 parseSingle semi
    return $ foldr1 Seq l

parseSingle :: Parser Stm
parseSingle =
    parseSkip
        <|> parsePrint
        <|> parseVarDef
        <|> parseIfElse
        <|> parseWhile
        <|> parseLocal
        <|> parsePar
        <|> parseNonDet
        <|> parseProcDef
        <|> parseProcInvoc
        <|> parens parseStm

parseSkip :: Parser Stm
parseSkip = reserved "skip" >> return Skip

parsePrint :: Parser Stm
parsePrint = reserved "print" >> Print <$> parseAexp

parseVarDef :: Parser Stm
parseVarDef = do
    x <- identifier
    reservedOp ":="
    e <- parseAexp
    return $ Assign x e

parseIfElse :: Parser Stm
parseIfElse = do
    reserved "if"
    b <- parseBexp
    reserved "then"
    s <- parseStm
    s' <- option Skip (reserved "else" >> parseStm)
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

parseLocal :: Parser Stm
parseLocal = do
    reserved "var"
    x <- identifier
    reservedOp ":="
    e <- parseAexp
    reserved "in"
    s <- parseStm
    reserved "end"
    return $ Local x e s

parsePar :: Parser Stm
parsePar = do
    s1 <- parseStm
    reserved "par"
    s2 <- parseStm
    return $ Par s1 s2

parseNonDet :: Parser Stm
parseNonDet = do
    s1 <- parseStm
    reservedOp "||"
    s2 <- parseStm
    return $ NonDet s1 s2

parseProcDef :: Parser Stm
parseProcDef = do
    reserved "procedure"
    p <- identifier
    (params, rets) <- parseParamsRets
    reserved "begin"
    s <- parseStm
    reserved "end"
    return $ ProcDef p params rets s

parseProcInvoc :: Parser Stm
parseProcInvoc = do
    p <- identifier
    (args, rets) <- parseArgsRets
    return $ ProcInvoc p args rets

parseParamsRets :: Parser ([Var], [Var])
parseParamsRets = parens $ do
    params <- sepBy identifier (symbol ",")
    reservedOp ";"
    rets <- sepBy identifier (symbol ",")
    return (params, rets)

parseArgsRets :: Parser ([Aexp], [Var])
parseArgsRets = parens $ do
    args <- sepBy parseAexp (symbol ",")
    reservedOp ";"
    rets <- sepBy identifier (symbol ",")
    return (args, rets)
