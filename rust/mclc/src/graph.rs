#[derive(Debug)]
pub struct NormalizedGraph(pub Vec<Node>, pub Vec<Edge>);

#[derive(Debug, PartialEq, Clone)]
pub enum Node {
    Input { name: String },
    Entity { id: u32, value: EntityValue },
    Splitter { id: u32, ports: u32 },
    Variable { id: u32, name: String },
    Output { name: String },
}

#[derive(Debug, Clone)]
pub enum Edge {
    InputToNode {
        input: String,
        dest_id: ID,
        dest_port: u32,
    },
    NodeToNode {
        src_id: ID,
        src_port: u32,
        dest_id: ID,
        dest_port: u32,
    },
    NodeToOutput {
        src_id: ID,
        src_port: u32,
        output: String,
    },
}

#[derive(Debug, PartialEq, Clone)]
pub enum EntityValue {
    Operation { op: Operation },
    Variable { name: String },
}

#[derive(Debug, PartialEq, Clone)]
pub enum Operation {
    Not,
    Or,
    Xor,
    And,
}

#[derive(Debug)]
enum ListEntry {
    Node(Node),
    Edge(Edge),
}

fn parse_line(line: &str) -> ListEntry {
    let (prefix, rest) = line.split_once('(').unwrap();
    let prefix = prefix.trim();
    let content = rest.strip_suffix(')').unwrap().trim();

    match prefix {
        "Node" => {
            let mut parts = content.split(',').map(|e| e.trim());

            let raw_id = parts.next().expect("Node needs ID");
            let raw_value = parts.next().expect("Node needs Value");

            let id = ID::parse(raw_id).unwrap();
            let value = Value::parse(raw_value).unwrap();

            match id {
                ID::Input { name } => ListEntry::Node(Node::Input { name }),
                ID::Entity { id } => match value {
                    Value::Operation { op } => {
                        let op = match op.as_str() {
                            "or" => Operation::Or,
                            "xor" => Operation::Xor,
                            "and" => Operation::And,
                            _ => panic!("Unknown OP: {:?}", op),
                        };
                        ListEntry::Node(Node::Entity {
                            id,
                            value: EntityValue::Operation { op: op },
                        })
                    }
                    Value::Variable { name } => ListEntry::Node(Node::Entity {
                        id,
                        value: EntityValue::Variable { name },
                    }),
                    other => panic!("Unexpected: {:?}", other),
                },
                ID::Splitter { id } => {
                    let port_count = match value {
                        Value::Splitter { port_count } => port_count,
                        other => {
                            dbg!(&other);
                            todo!()
                        }
                    };

                    ListEntry::Node(Node::Splitter {
                        id,
                        ports: port_count,
                    })
                }
                ID::Variable { id, name } => ListEntry::Node(Node::Variable { id, name }),
                ID::Output { name } => ListEntry::Node(Node::Output { name }),
                other => {
                    dbg!(other);
                    todo!("")
                }
            }
        }
        "NodeToNode" => {
            let mut parts = content.split(',');

            let raw_src_id = parts.next().expect("");
            let raw_src_port = parts.next().expect("");
            let raw_dest_id = parts.next().expect("");
            let raw_dest_port = parts.next().expect("");

            let src_id = ID::parse(raw_src_id).unwrap();
            let dest_id = ID::parse(raw_dest_id).unwrap();

            let src_port: u32 = raw_src_port.parse().unwrap();
            let dest_port: u32 = raw_dest_port.parse().unwrap();

            ListEntry::Edge(Edge::NodeToNode {
                src_id,
                src_port,
                dest_id,
                dest_port,
            })
        }
        "NodeToOutput" => {
            let mut parts = content.split(',');

            let raw_src_id = parts.next().expect("");
            let raw_src_port = parts.next().expect("");
            let raw_dest = parts.next().expect("");

            let src_id = ID::parse(raw_src_id).unwrap();
            let src_port: u32 = raw_src_port.parse().unwrap();
            let dest_id = ID::parse(raw_dest).unwrap();

            let dest_name = match dest_id {
                ID::Output { name } => name,
                other => panic!("{:?}", other),
            };

            ListEntry::Edge(Edge::NodeToOutput {
                src_id,
                src_port,
                output: dest_name,
            })
        }
        "InputToNode" => {
            let mut parts = content.split(',');

            let raw_src_id = parts.next().expect("");
            let raw_dest = parts.next().expect("");
            let raw_dest_port = parts.next().expect("");

            let src_id = ID::parse(raw_src_id).unwrap();
            let dest_id = ID::parse(raw_dest).unwrap();
            let dest_port: u32 = raw_dest_port.parse().unwrap();

            let input = match src_id {
                ID::Input { name } => name,
                other => {
                    panic!("{:?}", other)
                }
            };

            ListEntry::Edge(Edge::InputToNode {
                input,
                dest_id,
                dest_port,
            })
        }
        other => {
            println!("Unknown: {}", other);
            todo!()
        }
    }
}

#[derive(Debug, Clone)]
pub enum ID {
    Input { name: String },
    Entity { id: u32 },
    Splitter { id: u32 },
    Variable { name: String, id: u32 },
    Output { name: String },
}

impl ID {
    pub fn parse(content: &str) -> Result<Self, ()> {
        let (prefix, rest) = match content.split_once(' ') {
            Some(v) => v,
            None => {
                panic!("ID: {:?}", content);
            }
        };

        match prefix {
            "Input" => Ok(Self::Input {
                name: rest.to_string(),
            }),
            "Entity" => {
                let id: u32 = rest.parse().unwrap();
                Ok(Self::Entity { id })
            }
            "Splitter" => {
                let id: u32 = rest.parse().unwrap();
                Ok(Self::Splitter { id })
            }
            "Variable" => {
                let mut rest_parts = rest.split(' ');

                let name = rest_parts.next().expect("");
                let id: u32 = rest_parts.next().expect("").parse().unwrap();

                Ok(Self::Variable {
                    name: name.to_string(),
                    id,
                })
            }
            "Output" => Ok(Self::Output {
                name: rest.to_string(),
            }),
            other => {
                dbg!(other, rest);
                todo!()
            }
        }
    }
}

impl PartialEq<Node> for ID {
    fn eq(&self, other: &Node) -> bool {
        match (self, other) {
            (Self::Input { name: sname }, Node::Input { name: nname }) => sname == nname,
            (Self::Splitter { id: sid }, Node::Splitter { id: nid, .. }) => sid == nid,
            (
                Self::Variable {
                    name: sname,
                    id: sid,
                },
                Node::Variable {
                    id: nid,
                    name: nname,
                },
            ) => sid == nid && sname == nname,
            (Self::Entity { id: sid }, Node::Entity { id: nid, .. }) => sid == nid,
            (Self::Output { name: sname }, Node::Output { name: nname }) => sname == nname,
            _ => false,
        }
    }
}

#[derive(Debug)]
pub enum Value {
    Input { name: String },
    Operation { op: String },
    Splitter { port_count: u32 },
    Variable { name: String },
    Output { name: String },
}

impl Value {
    pub fn parse(content: &str) -> Result<Self, ()> {
        let (prefix, rest) = content.split_once(' ').unwrap();

        match prefix {
            "Input" => Ok(Self::Input {
                name: rest.to_string(),
            }),
            "Operation" => Ok(Self::Operation {
                op: rest.to_string(),
            }),
            "Splitter" => {
                let count: u32 = rest.parse().unwrap();
                Ok(Self::Splitter { port_count: count })
            }
            "Variable" => Ok(Self::Variable {
                name: rest.to_string(),
            }),
            "Output" => Ok(Self::Output {
                name: rest.to_string(),
            }),
            other => {
                dbg!(other, rest);
                todo!()
            }
        }
    }
}

impl NormalizedGraph {
    pub fn parse(content: &str) -> Result<Self, ()> {
        let lines = content.split(';').filter(|l| !l.trim().is_empty());
        let raw = lines.map(parse_line);
        let raw_entries: Vec<_> = raw.collect();

        let edge_iter = raw_entries.iter().filter_map(|raw_entry| match raw_entry {
            ListEntry::Edge(e) => Some(e.clone()),
            _ => None,
        });

        let node_iter = raw_entries.iter().filter_map(|raw_entry| match raw_entry {
            ListEntry::Node(n) => Some(n.clone()),
            _ => None,
        });

        Ok(Self(node_iter.collect(), edge_iter.collect()))
    }

    pub fn nodes_with_predecessors(&self) -> Vec<(Node, Vec<Node>)> {
        let mut result: Vec<(Node, Vec<Node>)> =
            self.0.iter().map(|n| (n.clone(), Vec::new())).collect();

        for edge in self.1.iter() {
            match edge {
                Edge::InputToNode {
                    input,
                    dest_id,
                    dest_port,
                } => {
                    let (dest_index, _) = result
                        .iter()
                        .enumerate()
                        .find(|(_, (node, _))| dest_id.eq(node))
                        .unwrap();
                    dbg!(dest_index);

                    let (src_index, _) = self
                        .0
                        .iter()
                        .enumerate()
                        .find(|(_, node)| match node {
                            Node::Input { name } => name == input,
                            _ => false,
                        })
                        .unwrap();
                    dbg!(src_index);

                    let src_node = self.0.get(src_index).unwrap().clone();

                    let (_, preds) = result.get_mut(dest_index).unwrap();
                    if !preds.contains(&src_node) {
                        preds.push(src_node);
                    }
                }
                Edge::NodeToNode {
                    src_id,
                    src_port,
                    dest_id,
                    dest_port,
                } => {
                    let (dest_index, _) = result
                        .iter()
                        .enumerate()
                        .find(|(_, (node, _))| dest_id.eq(node))
                        .unwrap();

                    let (src_index, _) = self
                        .0
                        .iter()
                        .enumerate()
                        .find(|(_, node)| src_id.eq(node))
                        .unwrap();

                    let src_node = self.0.get(src_index).unwrap().clone();

                    let (_, preds) = result.get_mut(dest_index).unwrap();
                    if !preds.contains(&src_node) {
                        preds.push(src_node);
                    }
                }
                Edge::NodeToOutput {
                    src_id,
                    src_port,
                    output,
                } => {
                    let (dest_index, _) = result
                        .iter()
                        .enumerate()
                        .find(|(_, (node, _))| match node {
                            Node::Output { name } => name == output,
                            _ => false,
                        })
                        .unwrap();

                    let (src_index, _) = self
                        .0
                        .iter()
                        .enumerate()
                        .find(|(_, node)| src_id.eq(node))
                        .unwrap();

                    let src_node = self.0.get(src_index).unwrap().clone();

                    let (_, preds) = result.get_mut(dest_index).unwrap();
                    if !preds.contains(&src_node) {
                        preds.push(src_node);
                    }
                }
            };
        }

        result
    }
}
