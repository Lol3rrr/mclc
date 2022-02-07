module Parser where
  import qualified Tokenizer

  data ParseError = UnexpectedToken | EntityError String | Other String deriving (Show);
  data ParseResult a = Ok a | Error ParseError [Tokenizer.Token] deriving (Show);

  instance Functor ParseResult where
    fmap f (Ok x) = Ok (f x)

  instance Applicative ParseResult where
    pure = Ok
    Ok f <*> Ok x = Ok (f x)

  instance Monad ParseResult where
    (Error e_str e_toks) >>= f = Error e_str e_toks
    (Ok x) >>= f = f x
    return = Ok

  unwrap :: ParseResult a -> a
  unwrap (Ok v) = v
  unwrap (Error err e_toks) = error ((show err) ++ " " ++ (show e_toks))

  data DType = Bit deriving Show;
  type Port = (String, DType);
  data Value = Literal String  | Operation String [String] deriving Show;
  type AssignTarget = [String];
  data BehaviourStatement = PortAssign AssignTarget Value | VariableAssign AssignTarget Value deriving Show;
  type Entity = (String, [Port], [Port], [BehaviourStatement]);
  data TopLevel = ParsedEntity Entity deriving Show;

  parse :: [Tokenizer.Token] -> ParseResult [TopLevel]
  parse ((Tokenizer.Keyword Tokenizer.Entity):(Tokenizer.Literal name):rest)
    = do  let (inner, (_:other)) = getInnerScope rest
          entity <- parseEntity name inner
          other_tops <- parse other
          return ((ParsedEntity entity):other_tops)
  parse [] = return ([])
  parse other = Error UnexpectedToken other

  parseEntity :: String -> [Tokenizer.Token] -> ParseResult Entity
  parseEntity name ((Tokenizer.Keyword Tokenizer.InPorts):rest)
    = do  let (inner, (_:outer)) = getInnerScope rest
          let in_ports = parsePorts inner
          (_, o_in_ports, out_ports, behaviour) <- parseEntity name outer
          return (name, (in_ports ++ o_in_ports), out_ports, behaviour)
  parseEntity name ((Tokenizer.Keyword Tokenizer.OutPorts):rest)
    = do  let (inner, (_:outer)) = getInnerScope rest
          let out_ports = parsePorts inner
          (_, in_ports, o_out_ports, behaviour) <- parseEntity name outer
          return (name, in_ports, (out_ports ++ o_out_ports), behaviour)
  parseEntity name ((Tokenizer.Keyword Tokenizer.Behaviour):rest)
    = do  let (inner, (_:outer)) = getInnerScope rest
          behaviour <- parseBehaviour inner
          (_, in_ports, out_ports, o_behaviour) <- parseEntity name outer
          return (name, in_ports, out_ports, (behaviour ++ o_behaviour))
  parseEntity name [] = return (name, [], [], [])
  parseEntity name ((Tokenizer.CloseCurly):rest) = Error (Other "Pending Close Curly Brace") (Tokenizer.CloseCurly:rest)
  parseEntity name others = Error (Other "Parsing Entity") others

  parsePorts :: [Tokenizer.Token] -> [Port]
  parsePorts [] = []
  parsePorts ((Tokenizer.Literal name):(Tokenizer.Colon):(Tokenizer.Literal type_name):(Tokenizer.Semicolon):rest)
    = (name, ty) : (parsePorts rest)
    where ty = parseDType type_name

  parseDType :: String -> DType
  parseDType "bit" = Bit

  parseBehaviour :: [Tokenizer.Token] -> ParseResult [BehaviourStatement]
  parseBehaviour ((Tokenizer.OpenParen):other)
    = do
      let (in_parens, (_:rest)) = getBetweenParens (Tokenizer.OpenParen:other)
      let targets = parseAssignTarget in_parens
      (assign_stmnt, rest_toks) <- parseAssignStatement targets rest
      other_stmnts <- parseBehaviour rest_toks
      return (assign_stmnt : other_stmnts)
  parseBehaviour [] = return ([])
  parseBehaviour rest = Error (Other "Parsing Behaviour") rest

  parseAssignment :: AssignTarget -> [Tokenizer.Token] -> ParseResult (BehaviourStatement, [Tokenizer.Token])
  parseAssignment targets (Tokenizer.PortAssign:rest)
    = do
      let (value, v_rest) = parseValue rest;
      return ((PortAssign targets value), v_rest)
  parseAssignment targets (Tokenizer.VarAssign:rest)
    = do
      let (value, v_rest) = parseValue rest;
      return ((VariableAssign targets value), v_rest)
  parseAssignment targets other = Error (Other "Unknown Assignment") other

  parseAssignStatement :: AssignTarget -> [Tokenizer.Token] -> ParseResult (BehaviourStatement, [Tokenizer.Token])
  parseAssignStatement targets tokens = parseStatement tokens (parseAssignment targets)

  parseStatement :: [Tokenizer.Token] -> ([Tokenizer.Token] -> ParseResult (BehaviourStatement, [Tokenizer.Token])) -> ParseResult (BehaviourStatement, [Tokenizer.Token])
  parseStatement tokens f
    = do
      (stmnt, rest) <- (f tokens);
      case rest of
        (Tokenizer.Semicolon:rest) -> do {
          return (stmnt, rest);
        }
        other -> Error (Other "Missing Semicolon after Statement") other

  parseAssignTarget :: [Tokenizer.Token] -> AssignTarget
  parseAssignTarget [] = []
  parseAssignTarget ((Tokenizer.Literal name):rest) = name : (parseAssignTarget rest)
  parseAssignTarget ((Tokenizer.Comma):rest) = parseAssignTarget rest

  parseLiteralArguments :: [Tokenizer.Token] -> ([String], [Tokenizer.Token])
  parseLiteralArguments ((Tokenizer.Literal val):rest) = (val:other_values, after_tokens) where (other_values, after_tokens) = parseLiteralArguments rest
  parseLiteralArguments ((Tokenizer.Comma):rest) = parseLiteralArguments rest
  parseLiteralArguments ((Tokenizer.CloseParen):rest) = ([], rest)

  parseValue :: [Tokenizer.Token] -> (Value, [Tokenizer.Token])
  parseValue ((Tokenizer.Literal name):(Tokenizer.OpenParen):rest) = ((Operation name args), after) where (args, after) = parseLiteralArguments rest
  parseValue ((Tokenizer.Literal name):rest) = (Literal name, rest)

  -- As long as the Count is not zero, it will continue to iterate over the Tokens
  getWithCount :: Int -> (Tokenizer.Token -> Int) -> [Tokenizer.Token] -> ([Tokenizer.Token], [Tokenizer.Token])
  getWithCount 0 _ rest = ([], rest)
  getWithCount n f (tmp:rest)
    | (n + delta) > 0 = (tmp:inner_toks, outer_toks)
    | otherwise = (inner_toks, tmp:outer_toks)
    where delta = f tmp
          (inner_toks, outer_toks) = getWithCount (n + delta) f rest

  -- Returns a List of Tokens between the first Pair of Curly Braces and the Rest afterwards
  getInnerScope :: [Tokenizer.Token] -> ([Tokenizer.Token], [Tokenizer.Token])
  getInnerScope (Tokenizer.OpenCurly:rest)
    = getWithCount 1 (\t -> case t of
      Tokenizer.OpenCurly -> 1
      Tokenizer.CloseCurly -> -1
      _ -> 0) rest

  getBetweenParens :: [Tokenizer.Token] -> ([Tokenizer.Token], [Tokenizer.Token])
  getBetweenParens (Tokenizer.OpenParen:rest)
    = getWithCount 1 (\t -> case t of
      Tokenizer.OpenParen -> 1
      Tokenizer.CloseParen -> -1
      _ -> 0) rest