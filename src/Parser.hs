module Parser where

import Text.Parsec
import Text.Parsec.String (Parser)

import Protolude hiding (many, (<|>), try, for)
import Prelude(String)

import qualified Text.Parsec.Expr as Ex
import qualified Text.Parsec.Token as Tok

import Lexer 
import Syntax 

binary s f = Ex.Infix (reservedOp s >> pure (BinOp f)) 

table = [[binary "*" Times Ex.AssocLeft
        , binary "/" Divide Ex.AssocLeft]
        ,[binary "+" Plus Ex.AssocLeft
        , binary "-" Minus Ex.AssocLeft]]

int :: Parser Expr
int = do
    Float . fromInteger <$> integer

floating :: Parser Expr
floating = do
    Float <$> float

expr :: Parser Expr
expr = Ex.buildExpressionParser table factor

variable :: Parser Expr
variable = do
    Var <$> identifier

function :: Parser Expr
function = do
    reserved "def"
    name <- identifier 
    args <- parens $ many variable
    Function name args <$> expr

extern :: Parser Expr
extern = do
    reserved "extern"
    name <- identifier 
    args <- parens $ many variable
    pure $ Extern name args

call :: Parser Expr
call = do
    name <- identifier
    args <- parens $ commaSep expr
    pure $ Call name args

factor :: Parser Expr
factor = try floating
     <|> try int
     <|> try extern
     <|> try function
     <|> try call
     <|> variable
     <|> parens expr

defn :: Parser Expr
defn = try extern
   <|> try function
   <|> expr

contents :: Parser a -> Parser a
contents p = do
    Tok.whiteSpace lexer
    r <- p
    eof
    return r

toplevel :: Parser [Expr]
toplevel = many $ do
    def <- defn
    reservedOp ";"
    return def 

parseExpr :: String -> Either ParseError Expr
parseExpr = parse (contents expr) "<stdin>"

parseTopLevel :: String -> Either ParseError [Expr]
parseTopLevel = parse (contents toplevel) "<stdin>"
