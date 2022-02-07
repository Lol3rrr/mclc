use std::vec;

use crate::{
    astar, graph, space::Space, PlacedNode, PlacedNodeData, SpaceBlock, SpaceCell, RESERVE_SPACE,
};

pub fn connect_nodes(
    space: &mut Space<SpaceCell>,
    graph: &graph::NormalizedGraph,
    placed_nodes: &[PlacedNode],
) {
    let edges = &graph.1;

    for edge in edges {
        place_edge(space, edge, placed_nodes);
    }
}

fn place_edge(space: &mut Space<SpaceCell>, edge: &graph::Edge, nodes: &[PlacedNode]) {
    let (src_pos, dest_pos): ((usize, usize, usize), (usize, usize, usize)) = match edge {
        graph::Edge::InputToNode {
            input,
            dest_id,
            dest_port,
        } => {
            let src_pos = nodes
                .iter()
                .find_map(|PlacedNode(pos, n)| match n {
                    PlacedNodeData::Input { name } if input == name => Some(pos),
                    _ => None,
                })
                .unwrap();

            let (dest_ports, _) = get_node_ports(dest_id, nodes);
            let dest_pos = dest_ports.get(*dest_port as usize).unwrap();

            (src_pos.clone(), dest_pos.clone())
        }
        graph::Edge::NodeToNode {
            src_id,
            src_port,
            dest_id,
            dest_port,
        } => {
            let (_, src_ports) = get_node_ports(src_id, nodes);
            let src_pos = src_ports.get(*src_port as usize).unwrap();

            let (dest_ports, _) = get_node_ports(dest_id, nodes);
            let dest_pos = dest_ports.get(*dest_port as usize).unwrap();

            (src_pos.clone(), dest_pos.clone())
        }
        graph::Edge::NodeToOutput {
            src_id,
            src_port,
            output,
        } => {
            let (_, src_ports) = get_node_ports(src_id, nodes);
            let src_pos = src_ports.get(*src_port as usize).unwrap();

            let dest_pos = nodes
                .iter()
                .find_map(|PlacedNode(pos, n)| match n {
                    PlacedNodeData::Output { name } if output == name => Some(pos),
                    _ => None,
                })
                .unwrap();

            (src_pos.clone(), dest_pos.clone())
        }
    };

    let unreserve_src_pos_x = (src_pos.0..(src_pos.0 + RESERVE_SPACE + 1));
    let unreserve_dest_pos_x = ((dest_pos.0.saturating_sub(RESERVE_SPACE + 1))..dest_pos.0);

    let unreserve_src_pos = unreserve_src_pos_x.map(|x| (x, src_pos.1, src_pos.2));
    let unreserve_dest_pos = unreserve_dest_pos_x
        .flat_map(|x| [(x, dest_pos.1, dest_pos.2), (x, dest_pos.1, dest_pos.2 + 1)]);

    let unreserve_pos = unreserve_src_pos.chain(unreserve_dest_pos);

    for pos in unreserve_pos {
        space.set(pos, |_| SpaceCell::Empty);
    }

    let search_s_pos = (src_pos.0 + 1, src_pos.1, src_pos.2);
    let search_d_pos = (dest_pos.0 - 1, dest_pos.1, dest_pos.2);

    let mut path = astar::path(
        space,
        search_s_pos,
        search_d_pos,
        |(src_x, src_y, src_z), (dest_x, dest_y, dest_z)| {
            let result = (dest_x as i64 - src_x as i64).abs()
                + (dest_y as i64 - src_y as i64).abs()
                + (dest_z as i64 - src_z as i64).abs();

            result as i64
        },
        |s, pos| neighbours(s, pos, search_d_pos),
    );

    path.push(src_pos);
    path.push(dest_pos);

    place_path(space, path);
}

fn get_node_ports(
    id: &graph::ID,
    nodes: &[PlacedNode],
) -> (Vec<(usize, usize, usize)>, Vec<(usize, usize, usize)>) {
    match id {
        graph::ID::Entity { id: s_id } => nodes
            .iter()
            .find_map(|PlacedNode(_, n)| match n {
                PlacedNodeData::Entity {
                    id,
                    in_ports,
                    out_ports,
                    ..
                } if id == s_id => Some((in_ports.clone(), out_ports.clone())),
                _ => None,
            })
            .unwrap(),
        graph::ID::Variable {
            name: sname,
            id: sid,
        } => nodes
            .iter()
            .find_map(|PlacedNode(pos, n)| match n {
                PlacedNodeData::Variable { id, name } if id == sid && name == sname => {
                    Some((vec![pos.clone()], vec![pos.clone()]))
                }
                _ => None,
            })
            .unwrap(),
        graph::ID::Splitter { id: s_id } => nodes
            .iter()
            .find_map(|PlacedNode(pos, n)| match n {
                PlacedNodeData::Splitter { id, ports, input } if id == s_id => {
                    Some((vec![input.clone()], ports.clone()))
                }
                _ => None,
            })
            .unwrap(),
        other => todo!("Unknown ID:  {:?}", other),
    }
}

fn neighbours(
    s: &Space<SpaceCell>,
    pos: (usize, usize, usize),
    dest: (usize, usize, usize),
) -> Vec<((usize, usize, usize), i64)> {
    let base = base_neighbours(s, pos);

    if let Some(v) = base.iter().find(|(p, _)| p == &dest) {
        return vec![v.clone()];
    }

    base.into_iter()
        .filter(|(pos, _)| match s.get(pos.clone()) {
            SpaceCell::Empty => true,
            _ => false,
        })
        .filter(|((x, y, z), _)| match s.get((*x, *y, z + 1)) {
            SpaceCell::Empty => true,
            _ => false,
        })
        .filter(|((x, y, z), _)| match s.get((*x, *y, z - 1)) {
            SpaceCell::Empty => true,
            SpaceCell::Reserved => true,
            SpaceCell::Used(SpaceBlock::SolidBlock) => true,
            _ => false,
        })
        .collect()
}

fn base_neighbours(
    s: &Space<SpaceCell>,
    pos: (usize, usize, usize),
) -> Vec<((usize, usize, usize), i64)> {
    let base_cords = [
        (pos.0 + 1, pos.1, pos.2),
        (pos.0 - 1, pos.1, pos.2),
        (pos.0, pos.1 + 1, pos.2),
        (pos.0, pos.1 - 1, pos.2),
    ];

    let lower_cords = base_cords
        .clone()
        .into_iter()
        .map(|(x, y, z)| (x, y, z + 1));

    let top_free = if pos.2 > 0 {
        match s.get((pos.0, pos.1, pos.2 - 1)) {
            SpaceCell::Empty => true,
            _ => false,
        }
    } else {
        false
    };

    let upper_cords = base_cords
        .clone()
        .into_iter()
        .filter(|_| top_free)
        .map(|(x, y, z)| (x, y, z - 1));

    base_cords
        .into_iter()
        .map(|p| (p, 1))
        .chain(lower_cords.map(|p| (p, 2)))
        .chain(upper_cords.map(|p| (p, 2)))
        .collect()
}

fn place_path(s: &mut Space<SpaceCell>, path: Vec<(usize, usize, usize)>) {
    for pos in path {
        s.set(pos, |_| SpaceCell::Used(SpaceBlock::Redstone));
        s.set((pos.0, pos.1, pos.2 + 1), |_| {
            SpaceCell::Used(SpaceBlock::SolidBlock)
        });

        let sourinding = [
            (pos.0 + 1, pos.1, pos.2),
            (pos.0 - 1, pos.1, pos.2),
            (pos.0, pos.1 + 1, pos.2),
            (pos.0, pos.1 - 1, pos.2),
            (pos.0, pos.1, pos.2 + 1),
            (pos.0 + 1, pos.1, pos.2 - 1),
            (pos.0 - 1, pos.1, pos.2 - 1),
            (pos.0, pos.1 + 1, pos.2 - 1),
            (pos.0, pos.1 - 1, pos.2 - 1),
            (pos.0 + 1, pos.1, pos.2 + 1),
            (pos.0 - 1, pos.1, pos.2 + 1),
            (pos.0, pos.1 + 1, pos.2 + 1),
            (pos.0, pos.1 - 1, pos.2 + 1),
        ];
        for s_pos in sourinding {
            s.set(s_pos, |prev| match prev {
                SpaceCell::Empty => SpaceCell::Reserved,
                SpaceCell::Reserved => SpaceCell::Reserved,
                other => other.clone(),
            });
        }
    }
}
