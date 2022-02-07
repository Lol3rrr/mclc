module NormalizedGraph where
  import qualified Semantics
  import qualified BuiltinGraph

  import Debug.Trace

  import Data.List
  import Data.Maybe

  data GraphNodeID = InputID String | OutputID String | VariableID String Int | EntityID Int | SplitterID Int deriving (Show, Eq, Ord);
  data GraphNodeValue = OperationNode String | InputNode String | OutputNode String | VariableNode String | SplitterNode Int deriving (Show, Eq);
  newtype GraphNode = GraphNode (GraphNodeID, GraphNodeValue) deriving (Show, Eq);
  data GraphEdge
    = InputToNode { input :: GraphNodeID, dest :: GraphNodeID, port :: Int }
    | NodeToNode { src :: GraphNodeID, src_port :: Int, dest :: GraphNodeID, dest_port :: Int }
    | NodeToOutput { src :: GraphNodeID, src_port :: Int, dest :: GraphNodeID }
    deriving Show;
  newtype Graph = Graph ([GraphNode], [GraphEdge]) deriving Show;

  data ConnectionSource = Input String | VariableSrc String Int | EntitySrc Int Int | SplitterSrc Int Int deriving (Show, Eq);
  data ConnectionDestination = VariableDest String Int | EntityDest Int Int | SplitterDest Int | Output String deriving (Show, Eq);
  newtype Connection = Connection (ConnectionSource, ConnectionDestination) deriving (Show);

  fromBuiltin :: BuiltinGraph.Graph -> Graph
  fromBuiltin (BuiltinGraph.Graph (nodes, edges))
    = Graph (n_nodes, n_edges)
    where connections = map buildConnection edges
          grouped_connections = groupConnection connections
          unique_connections
            = map
                (\(src, (dest:[])) -> Connection (src, dest))
                (filter (\(_, dests) -> (length dests) == 1) grouped_connections)
          max_id = maximum (map builtinNodeID nodes)
          non_unique_connections = filter (\(_, dests) -> (length dests) > 1) grouped_connections
          (splitter_nodes, splitter_connections) = generateSplitters (max_id + 1) non_unique_connections
          base_nodes = map convertNode nodes
          n_nodes = base_nodes ++ splitter_nodes
          n_edges = map convertConnection (unique_connections ++ splitter_connections)

  buildConnection :: BuiltinGraph.GraphEdge -> Connection
  buildConnection (BuiltinGraph.InputToNode (BuiltinGraph.InputID in_name) (BuiltinGraph.VariableID var_name var_id) port)
    = Connection (Input in_name, VariableDest var_name var_id)
  buildConnection (BuiltinGraph.InputToNode (BuiltinGraph.InputID in_name) (BuiltinGraph.EntityID e_id) port)
    = Connection (Input in_name, EntityDest e_id port)
  buildConnection (BuiltinGraph.NodeToNode (BuiltinGraph.EntityID src_id) src_port (BuiltinGraph.VariableID var_name var_id) _)
    = Connection (EntitySrc src_id src_port, VariableDest var_name var_id)
  buildConnection (BuiltinGraph.NodeToNode (BuiltinGraph.EntityID src_id) src_port (BuiltinGraph.EntityID dest_id) dest_port)
    = Connection (EntitySrc src_id src_port, EntityDest dest_id dest_port)
  buildConnection (BuiltinGraph.NodeToNode (BuiltinGraph.VariableID src_name src_id) _ (BuiltinGraph.EntityID dest_id) dest_port)
    = Connection (VariableSrc src_name src_id, EntityDest dest_id dest_port)
  buildConnection (BuiltinGraph.NodeToOutput (BuiltinGraph.EntityID src_id) src_port (BuiltinGraph.OutputID out_name))
    = Connection (EntitySrc src_id src_port, Output out_name)
  buildConnection (BuiltinGraph.NodeToOutput (BuiltinGraph.VariableID src_name src_id) src_port (BuiltinGraph.OutputID out_name))
    = Connection (VariableSrc src_name src_id, Output out_name)
  buildConnection other = error (show other)

  groupConnection :: [Connection] -> [(ConnectionSource, [ConnectionDestination])]
  groupConnection [] = []
  groupConnection ((Connection (src, dest)):connections)
    = ((src, dest : other_dests)) : (groupConnection rest_cons)
    where other_dests = map (\(Connection (_, o_dest)) -> o_dest) (filter (\(Connection (o_src, _)) -> src == o_src) connections)
          rest_cons = filter (\(Connection (o_src, _)) -> src /= o_src) connections
  
  builtinNodeID :: BuiltinGraph.GraphNode -> Int
  builtinNodeID (BuiltinGraph.GraphNode (BuiltinGraph.VariableID _ id, _)) = id
  builtinNodeID (BuiltinGraph.GraphNode (BuiltinGraph.EntityID id, _)) = id
  builtinNodeID _ = 0

  generateSplitters :: Int -> [(ConnectionSource, [ConnectionDestination])] -> ([GraphNode], [Connection])
  generateSplitters _ [] = ([], [])
  generateSplitters max_id ((src, dests) : other_cons)
    = (splitter_node : rest_nodes, cons ++ rest_cons)
    where splitter_id = SplitterID max_id
          splitter_val = SplitterNode (length dests)
          splitter_node = GraphNode (splitter_id, splitter_val)
          to_splitter_con = Connection (src, SplitterDest max_id)
          splitter_rest_cons = map (\(dest, port) -> Connection (SplitterSrc max_id port, dest)) (zip dests [0..])
          cons = to_splitter_con : splitter_rest_cons
          (rest_nodes, rest_cons) = generateSplitters (max_id + 1) other_cons
  
  convertNode :: BuiltinGraph.GraphNode -> GraphNode
  convertNode (BuiltinGraph.GraphNode (id, val)) = GraphNode (convertID id, convertValue val)

  convertID :: BuiltinGraph.GraphNodeID -> GraphNodeID
  convertID (BuiltinGraph.InputID name) = InputID name
  convertID (BuiltinGraph.EntityID id) = EntityID id
  convertID (BuiltinGraph.VariableID name id) = VariableID name id
  convertID (BuiltinGraph.OutputID name) = OutputID name

  convertValue :: BuiltinGraph.GraphNodeValue -> GraphNodeValue
  convertValue (BuiltinGraph.InputNode name) = InputNode name
  convertValue (BuiltinGraph.OperationNode op) = OperationNode op
  convertValue (BuiltinGraph.VariableNode name) = VariableNode name
  convertValue (BuiltinGraph.OutputNode name) = OutputNode name

  convertConnection :: Connection -> GraphEdge
  convertConnection (Connection (Input in_name, SplitterDest dest_id))
    = InputToNode (InputID in_name) (SplitterID dest_id) 0
  convertConnection (Connection (Input in_name, EntityDest dest_id dest_port))
    = InputToNode (InputID in_name) (EntityID dest_id) dest_port
  convertConnection (Connection (VariableSrc src_name src_id, SplitterDest dest_id))
    = NodeToNode (VariableID src_name src_id) 0 (SplitterID dest_id) 0
  convertConnection (Connection (EntitySrc src_id src_port, VariableDest dest_name dest_id))
    = NodeToNode (EntityID src_id) src_port (VariableID dest_name dest_id) 0
  convertConnection (Connection (EntitySrc src_id src_port, SplitterDest dest_id))
    = NodeToNode (EntityID src_id) src_port(SplitterID dest_id) 0
  convertConnection (Connection (VariableSrc src_name src_id, EntityDest dest_id dest_port))
    = NodeToNode (VariableID src_name src_id) 0 (EntityID dest_id) dest_port
  convertConnection (Connection (SplitterSrc src_id src_port, EntityDest dest_id dest_port))
    = NodeToNode (SplitterID src_id) src_port (EntityID dest_id) dest_port
  convertConnection (Connection (SplitterSrc src_id src_port, VariableDest dest_name dest_id))
    = NodeToNode (SplitterID src_id) src_port (VariableID dest_name dest_id) 0
  convertConnection (Connection (EntitySrc src_id src_port, Output out_name))
    = NodeToOutput (EntityID src_id) src_port (OutputID out_name)
  convertConnection (Connection (VariableSrc src_name src_id, Output out_name))
    = NodeToOutput (VariableID src_name src_id) 0 (OutputID out_name)
  convertConnection (Connection (SplitterSrc src_id src_port, Output out_name))
    = NodeToOutput (SplitterID src_id) src_port (OutputID out_name)
  convertConnection unknown = error ("Unknown Connection: " ++ (show unknown))

  nodePredecessors :: Graph -> GraphNode -> [GraphNode]
  nodePredecessors (Graph (nodes, edges)) (GraphNode (id, _))
    = map (\t -> fromJust (find (\(GraphNode (t_id, _)) -> t_id == t) nodes)) target_src
    where target_edges = filter (\e -> case e of
                          (InputToNode _ dest _) -> dest == id
                          (NodeToNode _ _ dest _) -> dest == id
                          (NodeToOutput _ _ dest) -> dest == id) edges
          target_src = map (\e -> case e of
                          (InputToNode src _ _) -> src
                          (NodeToNode src _ _ _) -> src
                          (NodeToOutput src _ _) -> src) target_edges

  toString :: Graph -> String
  toString (Graph (nodes, edges)) = foldl1 (++) strs
    where str_nodes = map nodeToString nodes
          str_edges = map edgeToString edges
          strs = str_nodes ++ str_edges
          

  nodeToString :: GraphNode -> String
  nodeToString (GraphNode (id, value))
    = "Node (" ++ id_str ++ "," ++ value_str ++ ");"
    where id_str = idToString id
          value_str = case value of
            (InputNode name) -> "Input " ++ name
            (OperationNode op) -> "Operation " ++ op
            (VariableNode var) -> "Variable " ++ var
            (SplitterNode count) -> "Splitter " ++ (show count)
            (OutputNode name) -> "Output " ++ name
            other -> error (show other)

  edgeToString :: GraphEdge -> String
  edgeToString (InputToNode src_id dest_id dest_port)
    = "InputToNode (" ++ src_id_str ++ "," ++ dest_id_str ++ "," ++ (show dest_port) ++ ");"
    where src_id_str = idToString src_id
          dest_id_str = idToString dest_id
  edgeToString (NodeToNode src_id src_port dest_id dest_port)
    = "NodeToNode (" ++ src_id_str ++ "," ++ (show src_port) ++ "," ++ dest_id_str ++ "," ++ (show dest_port) ++ ");"
    where src_id_str = idToString src_id
          dest_id_str = idToString dest_id
  edgeToString (NodeToOutput src_id src_port dest_id)
    = "NodeToOutput (" ++ src_id_str ++ "," ++ (show src_port) ++ "," ++ dest_id_str ++ ");"
    where src_id_str = idToString src_id
          dest_id_str = idToString dest_id

  idToString :: GraphNodeID -> String
  idToString (InputID name) = "Input " ++ name
  idToString (EntityID id) = "Entity " ++ (show id)
  idToString (VariableID var_name var_id) = "Variable " ++ var_name ++ " " ++ (show var_id)
  idToString (SplitterID id) = "Splitter " ++ (show id)
  idToString (OutputID name) = "Output " ++ name