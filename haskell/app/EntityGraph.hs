module EntityGraph where
  import qualified Semantics
  import Data.Maybe
  import Data.List

  data GraphNodeID = InputID String | OutputID String | VariableID String Int | EntityID Int deriving (Show, Eq, Ord);
  data GraphNodeValue = ValueNode Semantics.Value | VariableNode String | InputNode String | OutputNode String deriving (Show, Eq);
  newtype GraphNode = GraphNode (GraphNodeID, GraphNodeValue) deriving (Show, Eq);
  data GraphEdge
    = InputEntityEdge { input :: String, dest :: Int, dest_port_number :: Int }
    | VariableEntityEdge { input :: String, input_id :: Int, dest :: Int, dest_port_number :: Int }
    | VariableOutputEdge { input :: String, input_id :: Int, dest_output :: String }
    | EntityOutputEdge { src_id :: Int, src_port :: Int, dest_output :: String }
    | EntityVariableEdge { src_id :: Int, src_port :: Int, dest_var :: String, dest_id :: Int }
    deriving (Show, Eq);
  type Graph = ([GraphNode], [GraphEdge]);

  build :: Semantics.Entity -> Graph
  build (Semantics.Entity (header, statements))
    = (nodes, edges)
    where entity_nodes = getEntityNodes statements
          input_nodes = map (\(i_name, _) -> GraphNode (InputID i_name, InputNode i_name)) (Semantics.inputs header)
          output_nodes = map (\(i_name, _) -> GraphNode (OutputID i_name, OutputNode i_name)) (Semantics.outputs header)
          var_nodes = getVariables statements
          nodes = input_nodes ++ entity_nodes ++ var_nodes ++ output_nodes
          edges = getEntityEdges nodes statements

  getVariables :: [Semantics.BehaviourStatement] -> [GraphNode]
  getVariables [] = []
  getVariables ((Semantics.BehaviourStatement (id, (Semantics.VarAssign vars _))):rest)
    = var_nodes ++ (getVariables rest)
    where var_nodes = map (\(v_name, _) -> GraphNode (VariableID v_name id, VariableNode v_name)) vars
  getVariables (x:rest) = getVariables rest

  getEntityNodes :: [Semantics.BehaviourStatement] -> [GraphNode]
  getEntityNodes [] = []
  getEntityNodes ((Semantics.BehaviourStatement (id, Semantics.PortAssign _ value)):rest_stmnts)
    = (GraphNode (EntityID id, ValueNode value)) : (getEntityNodes rest_stmnts)
  getEntityNodes ((Semantics.BehaviourStatement (id, Semantics.VarAssign _ value)):rest_stmnts)
    = (GraphNode (EntityID id, ValueNode value)) : (getEntityNodes rest_stmnts)

  getEntityEdges :: [GraphNode] -> [Semantics.BehaviourStatement] -> [GraphEdge]
  getEntityEdges _ [] = []
  getEntityEdges nodes ((Semantics.BehaviourStatement (id, Semantics.PortAssign targets (Semantics.VarValue var_name))):rest_stmnts)
    = (VariableOutputEdge v_name v_id target) : rest_edges
    where GraphNode (VariableID v_name v_id, _) = case (find (\n -> case n of
            GraphNode (VariableID name _, _) -> name == var_name
            _ -> False) nodes) of
              Just v -> v
              Nothing -> error ("No VariableID with name " ++ (show var_name) ++ " in: \n" ++ (show nodes))
          (target, _) = head targets
          rest_edges = getEntityEdges nodes rest_stmnts
  getEntityEdges nodes ((Semantics.BehaviourStatement (id, Semantics.PortAssign targets value)):rest_stmnts)
    = input_edges ++ target_edges ++ rest_edges
    where input_edges = getInputEntityEdges nodes value id
          target_edges = getEntityPortEdges id 0 targets
          rest_edges = getEntityEdges nodes rest_stmnts
  getEntityEdges nodes ((Semantics.BehaviourStatement (id, Semantics.VarAssign targets value)):rest_stmnts)
    = input_edges ++ target_edges ++ rest_edges
    where input_edges = getInputEntityEdges nodes value id
          target_edges = getEntityVarEdges id 0 targets
          rest_edges = getEntityEdges nodes rest_stmnts

  getInputEntityEdges :: [GraphNode] -> Semantics.Value -> Int -> [GraphEdge]
  getInputEntityEdges nodes (Semantics.Operation (Semantics.BuiltInOp _ operands)) id
    = inputEntityEdges nodes id operands 0
  getInputEntityEdges nodes (Semantics.Operation (Semantics.EntityOp _ operands)) id
    = inputEntityEdges nodes id operands 0
  getInputEntityEdges nodes value id = error ("Unknown: " ++ (show value))

  inputEntityEdges :: [GraphNode] -> Int -> [Semantics.Operand] -> Int -> [GraphEdge]
  inputEntityEdges _ _ [] _ = []
  inputEntityEdges nodes e_id ((Semantics.Port in_name):rest_ops) p_index
    = (InputEntityEdge in_name e_id p_index) : (inputEntityEdges nodes e_id rest_ops (p_index + 1))
  inputEntityEdges nodes e_id ((Semantics.Variable var_name):rest_ops) p_index
    = (VariableEntityEdge var_name var_id e_id p_index) : (inputEntityEdges nodes e_id rest_ops (p_index + 1))
    where var_node = case (find (\n -> case n of
            GraphNode (VariableID name _, _) -> name == var_name
            _ -> False) nodes) of
              Just v -> v
              Nothing -> error ("No VariableID with name " ++ (show var_name) ++ " in: \n" ++ (show nodes))
          var_id = case var_node of
            GraphNode (VariableID _ i, VariableNode _) -> i
            other -> error (show other)

  getEntityPortEdges :: Int -> Int -> [Semantics.Port] -> [GraphEdge]
  getEntityPortEdges _ _ [] = []
  getEntityPortEdges id c_port ((name, _) : rest) = (EntityOutputEdge id c_port name) : (getEntityPortEdges id (c_port + 1) rest)

  getEntityVarEdges :: Int -> Int -> [Semantics.Variable] -> [GraphEdge]
  getEntityVarEdges _ _ [] = []
  getEntityVarEdges id c_port ((name, _) : rest) = (EntityVariableEdge id c_port name id) : (getEntityVarEdges id (c_port + 1) rest)

  getEntityPredecessors :: Graph -> [(GraphNodeID, [GraphNodeID])]
  getEntityPredecessors ([], _) = []
  getEntityPredecessors (( GraphNode (EntityID n_id, _):rest), edges)
    = (EntityID n_id, predecessors) : (getEntityPredecessors (rest, edges))
    where predecessors = map (\e -> case e of
            VariableEntityEdge var_name _ _ _ -> (head (map mapVarSrc (filter filterVarSrc edges)))
            _ -> error "") variable_edges
          variable_edges = filter (\e -> case e of
            VariableEntityEdge _ _ dest _ -> dest == n_id
            _ -> False) edges
          filterVarSrc = (\edge -> case edge of
            EntityVariableEdge src _ _ _ -> True
            _ -> False)
          mapVarSrc = (\edge -> case edge of
            EntityVariableEdge src _ _ _ -> EntityID src
            _ -> error "")
  getEntityPredecessors ((n:rest), edges) = getEntityPredecessors (rest, edges)

  connectedEdges :: Graph -> GraphNode -> [GraphEdge]
  connectedEdges (nodes, edges) (GraphNode (InputID i_name, _))
    = filter (\e -> case e of
        (InputEntityEdge src _ _) -> src == i_name
        (VariableEntityEdge _ _ _ _) -> False
        (VariableOutputEdge _ _ _) -> False
        (EntityOutputEdge _ _ _) -> False
        (EntityVariableEdge _ _ _ _) -> False) edges
  connectedEdges (nodes, edges) (GraphNode (OutputID out_name, _))
    = filter (\e -> case e of
        (InputEntityEdge src _ _) -> False
        (VariableEntityEdge _ _ _ _) -> False
        (VariableOutputEdge _ _ o_name) -> o_name == out_name
        (EntityOutputEdge _ _ o_name) -> o_name == out_name
        (EntityVariableEdge _ _ _ _) -> False) edges
  connectedEdges (nodes, edges) (GraphNode (EntityID e_id, _))
    = filter (\e -> case e of
        (InputEntityEdge _ d_id _) -> d_id == e_id
        (VariableEntityEdge _ _ d_id _) -> d_id == e_id
        (VariableOutputEdge _ _ _) -> False
        (EntityOutputEdge s_id _ _) -> s_id == e_id
        (EntityVariableEdge s_id _ _ _) -> s_id == e_id) edges
  connectedEdges (nodes, edges) unknown_node = error ("Unknown: " ++ (show unknown_node))

  highestID :: Graph -> Int
  highestID ([], _) = 0
  highestID (nodes, _)
    = maximum (map (\(GraphNode (id, _)) -> case id of
      EntityID id -> id
      VariableID _ id -> id
      _ -> 0) nodes)