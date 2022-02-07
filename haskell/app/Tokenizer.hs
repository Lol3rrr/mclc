module Tokenizer where
  data Keyword = Entity | InPorts | OutPorts | Behaviour deriving Show;
  data Token
    = OpenParen | CloseParen
    | OpenCurly | CloseCurly
    | Colon | Semicolon | Comma
    | PortAssign | VarAssign
    | Keyword Keyword
    | Literal String deriving Show;

  tokenize :: String -> [Token]
  tokenize [] = []
  tokenize (' ':rest) = tokenize rest
  tokenize ('\n':rest) = tokenize rest
  tokenize ('(':rest) = OpenParen : (tokenize rest)
  tokenize (')':rest) = CloseParen : (tokenize rest)
  tokenize ('{':rest) = OpenCurly : (tokenize rest)
  tokenize ('}':rest) = CloseCurly : (tokenize rest)
  tokenize (';':rest) = Semicolon : (tokenize rest)
  tokenize (':':rest) = Colon : (tokenize rest)
  tokenize (',':rest) = Comma : (tokenize rest)
  tokenize ('<':'=':rest) = PortAssign : (tokenize rest)
  tokenize ('=':rest) = VarAssign : (tokenize rest)
  tokenize ('e':'n':'t':'i':'t':'y':rest) = (Keyword Entity) : (tokenize rest)
  tokenize ('i':'n':'_':'p':'o':'r':'t':'s':rest) = (Keyword InPorts) : (tokenize rest)
  tokenize ('o':'u':'t':'_':'p':'o':'r':'t':'s':rest) = (Keyword OutPorts) : (tokenize rest)
  tokenize ('b':'e':'h':'a':'v':'i':'o':'u':'r':rest) = (Keyword Behaviour) : (tokenize rest)
  tokenize rest = ((Literal lit) : (tokenize other)) where (lit, other) = tokenizeLiteral rest

  tokenizeLiteral :: String -> (String, String)
  tokenizeLiteral [] = ([], [])
  tokenizeLiteral (tmp:[]) = ([tmp], [])
  tokenizeLiteral (tmp:' ':rest) = ([tmp], rest)
  tokenizeLiteral (tmp:';':rest) = ([tmp], ';':rest)
  tokenizeLiteral (tmp:',':rest) = ([tmp], ',':rest)
  tokenizeLiteral (tmp:'(':rest) = ([tmp], '(':rest)
  tokenizeLiteral (tmp:')':rest) = ([tmp], ')':rest)
  tokenizeLiteral (tmp:rest) = (tmp:rest_literal, rest_str) where (rest_literal, rest_str) = tokenizeLiteral rest