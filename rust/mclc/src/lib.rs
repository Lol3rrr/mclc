//! https://www.minecraftforum.net/forums/minecraft-java-edition/redstone-discussion-and/commands-command-blocks-and/2980388-1-14-1-17-multiple-commands-in-one-command-block

use std::fmt::Display;

use space::Space;

pub mod astar;
pub mod commands;
pub mod connect_nodes;
pub mod graph;
pub mod placement;
pub mod space;
pub mod visualize;

#[derive(Debug, PartialEq, Clone)]
pub enum SpaceCell {
    Empty,
    Reserved,
    Used(SpaceBlock),
}

impl Default for SpaceCell {
    fn default() -> Self {
        Self::Empty
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum SpaceBlock {
    SolidBlock,
    Redstone,
    Comparator {
        direction: Orientation,
        activated: bool,
    },
    Repeater {
        direction: Orientation,
    },
    TorchOnBlock {
        direction: Orientation,
    },
}

#[derive(Debug, PartialEq, Clone)]
pub enum Orientation {
    North,
    East,
    South,
    West,
}

impl Display for Orientation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::North => write!(f, "north"),
            Self::East => write!(f, "east"),
            Self::South => write!(f, "south"),
            Self::West => write!(f, "west"),
        }
    }
}

#[derive(Debug)]
pub enum PlacedNodeData {
    Input {
        name: String,
    },
    Output {
        name: String,
    },
    Variable {
        name: String,
        id: u32,
    },
    Splitter {
        id: u32,
        input: (usize, usize, usize),
        ports: Vec<(usize, usize, usize)>,
    },
    Entity {
        id: u32,
        in_ports: Vec<(usize, usize, usize)>,
        out_ports: Vec<(usize, usize, usize)>,
    },
}

#[derive(Debug)]
pub struct PlacedNode((usize, usize, usize), PlacedNodeData);

const RESERVE_SPACE: usize = 3;
const COLUMN_SPACING: usize = 10;

pub struct Layout {
    space: Space<SpaceCell>,
}

pub fn generate_layout(graph_str: &str) -> Layout {
    let graph = graph::NormalizedGraph::parse(graph_str).unwrap();

    let mut space: Space<SpaceCell> = space::Space::new();

    let mut placed_nodes = Vec::new();
    let mut nodes_to_place = graph.nodes_with_predecessors();

    let mut x_offset = 1;
    let z = 8;
    while !nodes_to_place.is_empty() {
        let placeable: Vec<graph::Node> = nodes_to_place
            .iter()
            .filter(|(_, preds)| preds.is_empty())
            .map(|(n, _)| n.clone())
            .collect();

        let mut y_offset = 1;
        let mut max_width = 0;
        for to_place in placeable {
            let (node_index, _) = nodes_to_place
                .iter()
                .enumerate()
                .find(|(_, (n, _))| n == &to_place)
                .unwrap();
            nodes_to_place.remove(node_index);

            nodes_to_place.iter_mut().for_each(|(_, preds)| {
                match preds.iter().enumerate().find(|(_, n)| n == &&to_place) {
                    Some((pred_index, _)) => {
                        preds.remove(pred_index);
                    }
                    None => {}
                };
            });

            let ((width, depth, _), placed_data) =
                placement::place_node(&mut space, to_place, x_offset, y_offset, z);
            placed_nodes.push(PlacedNode((x_offset, y_offset, z), placed_data));

            y_offset += depth + 5;
            max_width = std::cmp::max(max_width, width);
        }

        x_offset += max_width + COLUMN_SPACING + 2 * RESERVE_SPACE;
    }

    connect_nodes::connect_nodes(&mut space, &graph, &placed_nodes);

    Layout { space }
}

#[derive(Debug)]
pub enum BlockData {
    Stone,
    Redstone,
    TorchOnBlock {
        orient: Orientation,
    },
    Repeater {
        orient: Orientation,
    },
    Comparator {
        orient: Orientation,
        activated: bool,
    },
}

#[derive(Debug)]
pub struct MinecraftBlock {
    position: (usize, usize, usize),
    data: BlockData,
}

pub struct BlockLayout {
    blocks: Vec<MinecraftBlock>,
}

impl Layout {
    pub fn generate_svg(&self, path: &str) {
        visualize::visualize(&self.space, path);
    }

    pub fn placement(&self) -> BlockLayout {
        let block_iter = self
            .space
            .iter()
            .filter_map(|(pos, cell)| match cell {
                SpaceCell::Used(inner) => Some((pos, inner)),
                _ => None,
            })
            .map(|(pos, content)| {
                let data = match content {
                    SpaceBlock::Redstone => BlockData::Redstone,
                    SpaceBlock::SolidBlock => BlockData::Stone,
                    SpaceBlock::Repeater { direction } => BlockData::Repeater { orient: direction },
                    SpaceBlock::Comparator {
                        direction,
                        activated,
                    } => BlockData::Comparator {
                        orient: direction,
                        activated,
                    },
                    SpaceBlock::TorchOnBlock { direction } => {
                        BlockData::TorchOnBlock { orient: direction }
                    }
                };

                MinecraftBlock {
                    position: pos,
                    data,
                }
            });

        BlockLayout {
            blocks: block_iter.collect(),
        }
    }
}

impl MinecraftBlock {
    pub fn place_cmd(&self) -> String {
        let (x, y, z) = self.position;

        let block_str = match &self.data {
            BlockData::Stone => "stone".to_string(),
            BlockData::Redstone => "redstone_wire".to_string(),
            BlockData::TorchOnBlock { orient } => format!("redstone_wall_torch[facing={}]", orient),
            BlockData::Repeater { orient } => format!("repeater[facing={}]", orient),
            BlockData::Comparator { orient, activated } if *activated => {
                format!("comparator[facing={},mode=subtract]", orient)
            }
            other => {
                dbg!(&other);
                todo!()
            }
        };

        format!(
            "setblock ~{} ~{} ~{} {}",
            -(x as i64),
            -(z as i64),
            -(y as i64),
            block_str
        )
    }
}

impl BlockLayout {
    pub fn place_commands(&self) -> Vec<String> {
        let (stones, rest): (Vec<_>, Vec<_>) =
            self.blocks.iter().partition(|block| match &block.data {
                BlockData::Stone => true,
                _ => false,
            });

        let cmds: Vec<_> = stones
            .into_iter()
            .chain(rest.into_iter())
            .map(|b| b.place_cmd())
            .collect();

        cmds.chunks(400)
            .map(|cmds| {
                let bundled_cmds = cmds
                    .into_iter()
                    .map(|raw_cmd| format!("{{id:command_block_minecart,Command:'{}'}},", raw_cmd))
                    .fold("".to_string(), |mut acc, cmd| {
                        acc.push_str(&cmd);
                        acc
                    });
                format!("summon falling_block ~ ~1 ~ {{Time:1,BlockState:{{Name:redstone_block}},Passengers:[\
                {{id:falling_block,Passengers:[\
                {{id:falling_block,Time:1,BlockState:{{Name:activator_rail}},Passengers:[\
                {{id:command_block_minecart,Command:'gamerule commandBlockOutput false'}},{}\
                {{id:command_block_minecart,Command:'setblock ~ ~1 ~ command_block{{auto:1,Command:\"fill ~ ~ ~ ~ ~-3 ~ air\"}}'}},\
                {{id:command_block_minecart,Command:'kill @e[type=command_block_minecart,distance=..1]'}}]}}]}}]}}", bundled_cmds)
            })
            .collect()
    }
}
