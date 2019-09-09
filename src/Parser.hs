{-# LANGUAGE BlockArguments #-}
module Parser where 

import Data.Char (toLower)

import Text.ParserCombinators.Parsec 
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Tok 

-- | Parses a case insensitive string 
--   and returns its name, if it exists, 
--   and the associated function code 
parseString :: String -> Either ParseError Program 
parseString = parse file "" . map toLower 
  where file = do whitespace
                  f <- program  

                  return f 

type Program = [Statement]
data Statement = Increment String
               | Assign String Expression
               | Do Expression Program deriving Show 

data Expression = Variable String 
                | Constant Integer deriving Show 


program :: Parser Program 
program = many1 statement 

statement :: Parser Statement 
statement = assign <|> increment <|> doBlock 
  where increment, assign, doBlock :: Parser Statement 
        increment = Increment <$> identifier <* reservedOp "++" <* reservedOp ";"
        assign = do 
          name <- try $ identifier <* reservedOp "="
          exp <- expression 
        
          reservedOp ";"
        
          return $ Assign name exp 
        
        doBlock = do 
          reserved "do"
          numTimes <- expression 
          reservedOp ":"
        
          body <- program 
        
          reserved "od"
        
          return $ Do numTimes body 

expression =   Variable <$> identifier 
           <|> Constant <$> integer 

lexer = Tok.makeTokenParser (emptyDef {
  Tok.reservedNames = ["do", "od"],
  Tok.identStart = letter,
  Tok.identLetter = alphaNum <|> char '\'' <|> char '_',
  Tok.commentLine = "//"
})

whitespace = Tok.whiteSpace lexer 
reserved = Tok.reserved lexer 
reservedOp = Tok.reservedOp lexer 

identifier = Tok.identifier lexer 
integer = Tok.natural lexer 