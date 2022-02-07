module BuiltinGraph where
  import qualified Semantics
  import qualified EntityGraph

  import Debug.Trace

  import Data.List
  import Data.Maybe

  data GraphNodeID = InputID String | OutputID String | VariableID String Int | EntityID Int deriving (Show, Eq, Ord);
  data GraphNodeValue = OperationNode String | InputNode String | OutputNode String | VariableNode String deriving (Show, Eq);
  newtype GraphNode = GraphNode (GraphNodeID, GraphNodeValue) deriving (Show, Eq);
  data GraphEdge
    = InputToNode { input :: GraphNodeID, dest :: GraphNodeID, port :: Int }
    | NodeToNode { src :: GraphNodeID, src_port :: Int, dest :: GraphNodeID, dest_port :: Int }
    | NodeToOutput { src :: GraphNodeID, src_port :: Int, dest :: GraphNodeID }
    deriving Show;
  newtype Graph = Graph ([GraphNode], [GraphEdge]) deriving Show;

  fromEntityGraph :: [Semantics.Entity] -> EntityGraph.Graph -> Graph
  fromEntityGraph entities (nodes, edges)
    | (null e_nodes) = Graph (map convertNode nodes, map convertEdge edges)
    | otherwise = fromEntityGraph entities (replaceEntity entities (nodes, edges) (head e_nodes))
    where e_nodes = filter (\(EntityGraph.GraphNode (_, n_type)) -> case n_type of
                      EntityGraph.ValueNode (Semantics.Operation (Semantics.EntityOp _ _)) -> True
                      _ -> False) nodes

  convertNode :: EntityGraph.GraphNode -> GraphNode
  convertNode (EntityGraph.GraphNode (EntityGraph.InputID input_name, _)) = GraphNode (InputID input_name, InputNode input_name)
  convertNode (EntityGraph.GraphNode (EntityGraph.EntityID e_id, EntityGraph.ValueNode (Semantics.Operation (Semantics.BuiltInOp op_name _))))
    = GraphNode (EntityID e_id, OperationNode op_name)
  convertNode (EntityGraph.GraphNode (EntityGraph.EntityID e_id, EntityGraph.ValueNode (Semantics.VarValue var_name)))
    = GraphNode (EntityID e_id, VariableNode var_name)
  convertNode (EntityGraph.GraphNode (EntityGraph.VariableID name v_id, _)) = GraphNode (VariableID name v_id, VariableNode name)
  convertNode (EntityGraph.GraphNode (EntityGraph.OutputID name, _)) = GraphNode (OutputID name, OutputNode name)
  convertNode other = error ("Convert: " ++ (show other))

  convertEdge :: EntityGraph.GraphEdge -> GraphEdge
  convertEdge (EntityGraph.InputEntityEdge input dest port) = InputToNode (InputID input) (EntityID dest) port
  convertEdge (EntityGraph.VariableEntityEdge input input_id dest port) = NodeToNode (VariableID input input_id) 0 (EntityID dest) port
  convertEdge (EntityGraph.VariableOutputEdge input input_id output) = NodeToOutput (VariableID input input_id) 0 (OutputID output)
  convertEdge (EntityGraph.EntityOutputEdge src port output) = NodeToOutput (EntityID src) port (OutputID output)
  convertEdge (EntityGraph.EntityVariableEdge src port var var_id) = NodeToNode (EntityID src) port (VariableID var var_id) 0

  replaceEntity :: [Semantics.Entity] -> EntityGraph.Graph -> EntityGraph.GraphNode -> EntityGraph.Graph
  replaceEntity entities (nodes, edges) (EntityGraph.GraphNode (id, value))
    = mergeGraphEntity (nodes, edges) (replace_entity, (EntityGraph.GraphNode (id, value)), offset_replacement)
    where entity_name = case value of
            EntityGraph.ValueNode (Semantics.Operation (Semantics.EntityOp e_name _)) -> e_name
            other -> error (show other)
          replace_entity = fromJust (find (\(Semantics.Entity (h, _)) -> (Semantics.name h) == entity_name) entities)
          replace_entitygraph = EntityGraph.build replace_entity
          max_og_id = EntityGraph.highestID (nodes, edges)
          offset_replacement = offsetIDs (max_og_id + 1) replace_entitygraph

  offsetIDs :: Int -> EntityGraph.Graph -> EntityGraph.Graph
  offsetIDs offset (nodes, edges)
    = (n_nodes, n_edges)
    where n_nodes = map (\(EntityGraph.GraphNode (id, value)) -> EntityGraph.GraphNode (offsetGraphNodeID offset id, value)) nodes
          n_edges = map (offsetGraphEdgeID offset) edges

  offsetGraphNodeID :: Int -> EntityGraph.GraphNodeID -> EntityGraph.GraphNodeID
  offsetGraphNodeID _ (EntityGraph.InputID name) = EntityGraph.InputID name
  offsetGraphNodeID _ (EntityGraph.OutputID name) = EntityGraph.OutputID name
  offsetGraphNodeID offset (EntityGraph.EntityID id) = EntityGraph.EntityID (id + offset)
  offsetGraphNodeID offset (EntityGraph.VariableID name id) = EntityGraph.VariableID name (id + offset)

  offsetGraphEdgeID :: Int -> EntityGraph.GraphEdge -> EntityGraph.GraphEdge
  offsetGraphEdgeID offset (EntityGraph.InputEntityEdge input dest port)
    = EntityGraph.InputEntityEdge input (dest + offset) port
  offsetGraphEdgeID offset (EntityGraph.VariableEntityEdge v v_id dest port)
    = EntityGraph.VariableEntityEdge v (v_id + offset) (dest + offset) port
  offsetGraphEdgeID offset (EntityGraph.EntityOutputEdge src port dest)
    = EntityGraph.EntityOutputEdge (src + offset) port dest
  offsetGraphEdgeID offset (EntityGraph.EntityVariableEdge src s_port dest dest_id)
    = EntityGraph.EntityVariableEdge (src + offset) s_port dest (dest_id + offset)

  mergeGraphEntity :: EntityGraph.Graph -> (Semantics.Entity, EntityGraph.GraphNode, EntityGraph.Graph) -> EntityGraph.Graph
  mergeGraphEntity (base_nodes, base_edges) (Semantics.Entity (r_header, _), (EntityGraph.GraphNode (r_id, r_value)), (r_nodes, r_edges))
    = (n_nodes, n_edges)
    where connected_edges = EntityGraph.connectedEdges (base_nodes, base_edges) (EntityGraph.GraphNode (r_id, r_value))
          (input_edges, output_edges) = partition (\e -> case e of
            (EntityGraph.InputEntityEdge _ _ _) -> True
            (EntityGraph.VariableEntityEdge _ _ _ _) -> True
            _ -> False) connected_edges
          r_input_nodes = filter (\(EntityGraph.GraphNode (n_id, _)) -> case n_id of
            EntityGraph.InputID _ -> True
            _ -> False) r_nodes
          r_output_nodes = filter (\(EntityGraph.GraphNode (n_id, _)) -> case n_id of
            EntityGraph.OutputID _ -> True
            _ -> False) r_nodes
          r_input_use_edges = map
            (\(node, index) -> (index, EntityGraph.connectedEdges (r_nodes, r_edges) node))
            (map
              (\(name, node) -> (node, fromJust (findIndex (\(p_name, _) -> p_name == name) (Semantics.inputs r_header))))
              (map
                (\(EntityGraph.GraphNode (id, val)) -> case id of
                  EntityGraph.InputID name -> (name, EntityGraph.GraphNode (id, val))
                  other -> error ("Unexpected ID: " ++ (show other)))
                r_input_nodes))
          r_output_write_edges = concat (map (\n -> EntityGraph.connectedEdges (r_nodes, r_edges) n) r_output_nodes)
          remapped_input_edges = remapInputs input_edges r_input_use_edges
          remapped_output_edges = remapOutputs r_header r_output_write_edges output_edges
          
          filtered_base_nodes = filter (\n -> n /= (EntityGraph.GraphNode (r_id, r_value))) base_nodes
          filtered_r_nodes = filter
            (\n -> isNothing (find (\r_n -> n == r_n) r_input_nodes))
            (filter (\n -> isNothing (find (\r_n -> n == r_n) r_output_nodes)) r_nodes)
          n_nodes = filtered_base_nodes ++ filtered_r_nodes

          filtered_base_edges = filter
            (\e -> isNothing (find (\b_e -> e == b_e) input_edges))
            (filter (\e -> isNothing (find (\b_e -> e == b_e) output_edges)) base_edges)
          filtered_r_edges = filter
            (\e -> isNothing (find (\r_e -> e == r_e) (concat (map (\(_, e) -> e) r_input_use_edges))))
            (filter
              (\e -> isNothing (find (\r_e -> e == r_e) r_output_write_edges))
              r_edges)
          n_edges = filtered_base_edges ++ filtered_r_edges ++ remapped_input_edges ++ remapped_output_edges

  remapInputs :: [EntityGraph.GraphEdge] -> [(Int, [EntityGraph.GraphEdge])] -> [EntityGraph.GraphEdge]
  remapInputs [] _ = []
  remapInputs ((EntityGraph.InputEntityEdge input dest port_number):srcs) uses
    = (remapEdge (EntityGraph.InputEntityEdge input dest port_number) port_uses) ++ (remapInputs srcs uses)
    where (_, port_uses) = fromJust (find (\(port, _) -> port == port_number) uses)
  remapInputs ((EntityGraph.VariableEntityEdge input input_id dest port_number):srcs) uses
    = (remapEdge (EntityGraph.VariableEntityEdge input input_id dest port_number) port_uses) ++ (remapInputs srcs uses)
    where (_, port_uses) = fromJust (find (\(port, _) -> port == port_number) uses)
  remapInputs (other:srcs) uses = error ("Unknown Remap-Input: " ++ (show other))

  remapOutputs :: Semantics.EntityHeader -> [EntityGraph.GraphEdge] -> [EntityGraph.GraphEdge] -> [EntityGraph.GraphEdge]
  remapOutputs _ [] _ = []
  remapOutputs header ((EntityGraph.EntityOutputEdge s_id s_port output_target):srcs) uses
    = (remapEdge (EntityGraph.EntityOutputEdge s_id s_port output_target) filtered_uses) ++ (remapOutputs header srcs uses)
    where port_number = fromJust (findIndex (\(p_name, _) -> p_name == output_target) (Semantics.outputs header))
          filtered_uses = filter (\e -> case e of
            (EntityGraph.EntityVariableEdge _ _ _ _) -> True
            (EntityGraph.EntityOutputEdge _ _ _) -> True
            other -> error (show other)) uses

  remapEdge :: EntityGraph.GraphEdge -> [EntityGraph.GraphEdge] -> [EntityGraph.GraphEdge]
  remapEdge (EntityGraph.InputEntityEdge input _ _) dests
    = map (\e -> case e of
        (EntityGraph.InputEntityEdge _ dest port) -> (EntityGraph.InputEntityEdge input dest port)
        other -> error (show other)) dests
  remapEdge (EntityGraph.VariableEntityEdge input input_id _ _) dests
    = map (\e -> case e of
        (EntityGraph.InputEntityEdge _ dest port) -> (EntityGraph.VariableEntityEdge input input_id dest port)
        --(EntityGraph.EntityOutputEdge _ _ output) -> (EntityGraph.VariableOutputEdge input input_id output)
        other -> error (show other)) dests
  remapEdge (EntityGraph.EntityOutputEdge s_id s_port _) dests
    = map (\e -> case e of
        (EntityGraph.EntityVariableEdge _ _ dest_var dest_id) -> (EntityGraph.EntityVariableEdge s_id s_port dest_var dest_id)
        (EntityGraph.EntityOutputEdge _ _ output) -> (EntityGraph.EntityOutputEdge s_id s_port output)
        other -> error (show other)) dests
  remapEdge src dests = error ((show src) ++ " -> " ++ (show dests))

  inputNodes :: Graph -> [GraphNode]
  inputNodes (Graph (nodes, _)) = filter (\(GraphNode (_, val)) -> case val of
                                            VariableNode _ -> True
                                            _ -> False) nodes
  
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