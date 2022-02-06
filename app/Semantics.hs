module Semantics where
  import qualified Parser
  import Data.Maybe
  import Data.List
  import Control.Monad

  data SemanticError = MismatchedType {expected :: DType, received :: DType} | UnknownOperand String | UnknownPort String | Other String deriving (Show);
  data SemanticResult a = Ok a | Error SemanticError deriving (Show);

  instance Functor SemanticResult where
    fmap f (Ok x) = Ok (f x)

  instance Applicative SemanticResult where
    pure = Ok
    Ok f <*> Ok x = Ok (f x)

  instance Monad SemanticResult where
    (Error err) >>= f = Error err
    (Ok x) >>= f = f x
    return = Ok

  unwrap :: SemanticResult a -> a
  unwrap (Ok v) = v
  unwrap (Error err) = error (show err)

  expect :: SemanticResult a -> String -> a
  expect (Ok v) _ = v
  expect (Error err) msg = error (msg ++ " " ++ (show err))

  data DType = Bit deriving (Show, Eq);
  
  type PortName = String;
  type Port = (PortName, DType);
  type Variable = (String, DType);

  data LiteralValue = BitValue Bool deriving (Show, Eq);
  data Operand = Port PortName | Variable String | LiteralOperand LiteralValue deriving (Show, Eq);
  data OperationType = BuiltInOp String [Operand] | EntityOp String [Operand] deriving (Show, Eq);
  data Value = Literal LiteralValue | Operation OperationType | VarValue String deriving (Show, Eq);

  data Behaviour = PortAssign [Port] Value | VarAssign [Variable] Value deriving Show;
  newtype BehaviourStatement = BehaviourStatement (Int, Behaviour) deriving Show;

  data EntityHeader = Header {name :: String, inputs :: [Port], outputs :: [Port]} deriving Show;
  newtype Entity = Entity (EntityHeader, [BehaviourStatement]) deriving Show;

  data HeaderContext = HContext { current :: EntityHeader, all_h :: [EntityHeader] };

  parseEntities :: [Parser.Entity] -> SemanticResult [Entity]
  parseEntities [] = return []
  parseEntities entities
    = do
      headers <- parseHeaders entities;
      parseEntityList headers entities

  parseHeaders :: [Parser.Entity] -> SemanticResult [EntityHeader]
  parseHeaders entities = parseList (\e -> parseEntityHeader e) entities

  parseEntityHeader :: Parser.Entity -> SemanticResult EntityHeader
  parseEntityHeader (name, r_ins, r_outs, r_behav) =
    do
      inputs <- parsePorts r_ins
      outputs <- parsePorts r_outs
      return (Header name inputs outputs)

  parsePorts :: [Parser.Port] -> SemanticResult [Port]
  parsePorts [] = return []
  parsePorts ((p_name, p_type):rest) =
    do
      ty <- parseDType p_type
      rest_ports <- parsePorts rest
      return ((p_name, ty) : rest_ports)
  
  parseDType :: Parser.DType -> SemanticResult DType
  parseDType Parser.Bit = return Bit

  parseEntityList :: [EntityHeader] -> [Parser.Entity] -> SemanticResult [Entity]
  parseEntityList headers entities = parseList (\e -> parseEntity headers e) entities

  parseEntity :: [EntityHeader] -> Parser.Entity -> SemanticResult Entity
  parseEntity headers (e_name, r_ins, r_outs, r_behav) =
    do
      let current_header = fromJust (find (\h -> ((name h) == e_name)) headers)
      behaviour <- parseBehaviours [] 0 current_header headers r_behav
      return (Entity (current_header, behaviour))

  parseBehaviours :: [Variable] -> Int -> EntityHeader -> [EntityHeader] -> [Parser.BehaviourStatement] -> SemanticResult [BehaviourStatement]
  parseBehaviours vars _ c_header headers [] = return []
  parseBehaviours vars c_id c_header headers (stmnt:stmnts) =
    do
      (c_behav, n_vars) <- parseBehaviour (HContext c_header headers) vars stmnt c_id;
      other_behaviours <- parseBehaviours (vars ++ n_vars) (c_id + 1) c_header headers stmnts;
      return (c_behav : other_behaviours)

  parseBehaviour :: HeaderContext -> [Variable] -> Parser.BehaviourStatement -> Int -> SemanticResult (BehaviourStatement, [Variable])
  parseBehaviour headers_ctx vars (Parser.PortAssign target (Parser.Operation op_name args)) id =
    do
      (oper_value, oper_result_types) <- parseOperation (current headers_ctx) (all_h headers_ctx) vars op_name args;
      targets <- parsePortTargets (outputs (current headers_ctx)) target oper_result_types;
      return (BehaviourStatement (id, PortAssign targets oper_value), [])
  parseBehaviour headers_ctx vars (Parser.PortAssign target (Parser.Literal name)) id =
    do
      let (_, var_type) = fromJust (find (\(v_name, _) -> v_name == name) vars);
      targets <- parsePortTargets (outputs (current headers_ctx)) target [var_type];
      return (BehaviourStatement (id, PortAssign targets (VarValue name)), [])
  parseBehaviour headers_ctx vars (Parser.VariableAssign target (Parser.Operation op_name args)) id =
    do
      (oper_value, oper_result_type) <- parseOperation (current headers_ctx) (all_h headers_ctx) vars op_name args;
      let (targets, n_vars) = parseVariableTargets vars target oper_result_type;
      return (BehaviourStatement (id, VarAssign targets oper_value), n_vars)
  parseBehaviour _ _ stmnt _ = error ("Parse Behaviour: " ++ (show stmnt))

  parseOperation :: EntityHeader -> [EntityHeader] -> [Variable] -> String -> [String] -> SemanticResult (Value, [DType])
  parseOperation c_header all_headers vars op_name raw_operands
    | (isBuiltinOperation op_name) = parseBuiltinOperation c_header vars op_name raw_operands
    | otherwise = parseEntityOperation c_header all_headers vars op_name raw_operands

  parseBuiltinOperation :: EntityHeader -> [Variable] -> String -> [String] -> SemanticResult (Value, [DType])
  parseBuiltinOperation c_header vars op_name raw_operands =
    do
      let (op_input_types, op_output_types) = builtinOperationTypes op_name;
      when ((length op_input_types) /= (length raw_operands)) $ error "Mismatched Operand Counts";
      operands <- parseOperands (inputs c_header) vars (zip raw_operands op_input_types);
      return (Operation (BuiltInOp op_name operands), op_output_types)

  isBuiltinOperation :: String -> Bool
  isBuiltinOperation "and" = True
  isBuiltinOperation "not" = True
  isBuiltinOperation "xor" = True
  isBuiltinOperation "or" = True
  isBuiltinOperation _ = False

  builtinOperationTypes :: String -> ([DType], [DType])
  builtinOperationTypes "and" = ([Bit, Bit], [Bit])
  builtinOperationTypes "xor" = ([Bit, Bit], [Bit])
  builtinOperationTypes "or" = ([Bit, Bit], [Bit])
  builtinOperationTypes "not" = ([Bit], [Bit])
  builtinOperationTypes other = error ("Unknown Builtin Operation: " ++ other)

  parseEntityOperation :: EntityHeader -> [EntityHeader] -> [Variable] -> String -> [String] -> SemanticResult (Value, [DType])
  parseEntityOperation c_header headers vars op_name raw_operands =
    do
      op_entity <- (\e_name -> case (find (\tmp_h -> (name tmp_h) == e_name) headers) of
        Just h -> return h
        Nothing -> error "Unknown entity used"
        ) op_name;
      let op_input_types = map (\(name, ty) -> ty) (inputs op_entity);
      let op_output_types = map (\(name, ty) -> ty) (outputs op_entity);
      when ((length op_input_types) /= (length raw_operands)) $ error "Mismatched Operand Counts";
      operands <- parseOperands (inputs c_header) vars (zip raw_operands op_input_types);
      return (Operation (EntityOp op_name operands), op_output_types)

  parseOperands :: [Port] -> [Variable] -> [(String, DType)] -> SemanticResult [Operand]
  parseOperands ports vars inputs = parseList (\(op_name, ty) -> (parseOperand ports vars op_name ty)) inputs 

  parseOperand :: [Port] -> [Variable] -> String -> DType -> SemanticResult Operand
  parseOperand in_ports vars op_name expected_ty = case (find (\(p_name, _) -> p_name == op_name) in_ports) of
    Just (name, ty) -> if (ty == expected_ty) then return (Port name) else error ("Expected Type " ++ (show expected_ty) ++ " got " ++ (show ty))
    Nothing -> case (find (\(v_name, _) -> v_name == op_name) vars) of
      Just (name, ty) -> if (ty == expected_ty) then return (Variable name) else error ("Expected Type " ++ (show expected_ty) ++ " got " ++ (show ty))
      Nothing -> Error (UnknownOperand op_name)

  parsePortTargets :: [Port] -> Parser.AssignTarget -> [DType] -> SemanticResult [Port]
  parsePortTargets _ [] _ = return []
  parsePortTargets ports (target:rest) (target_type:rest_types) =
    do
      let port_res = find (\(p_name, _) -> p_name == target) ports;
      case port_res of
        Just (port_name, port_type) -> if (port_type == target_type) then do
          other_targets <- parsePortTargets ports rest rest_types
          return ((port_name, port_type) : other_targets)
          else error "Mismatched Types"
        Nothing -> Error (UnknownPort target)

  parseVariableTargets :: [(String, DType)] -> Parser.AssignTarget -> [DType] -> ([(String, DType)], [(String, DType)])
  parseVariableTargets _ [] [] = ([], [])
  parseVariableTargets vars (target:rest_targets) (target_type:rest_types)
    = case raw_found_var of
      Just tmp -> error "Using existing Variable"
      Nothing -> let (other_target, other_n_vars) = (parseVariableTargets vars rest_targets rest_types) in ((target, target_type):other_target, (target, target_type):other_n_vars)
    where raw_found_var = find (\(v_name, _) -> v_name == target) vars

  parseList :: (a -> SemanticResult b) -> [a] -> SemanticResult [b]
  parseList _ [] = return []
  parseList f (tmp:rest) =
    do
      current <- f tmp
      others <- parseList f rest
      return (current : others)