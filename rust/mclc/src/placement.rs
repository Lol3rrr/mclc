use crate::{graph, space, Orientation, PlacedNodeData, SpaceBlock, SpaceCell, RESERVE_SPACE};

pub fn place_node(
    space: &mut space::Space<SpaceCell>,
    to_place: graph::Node,
    x_offset: usize,
    y_offset: usize,
    z_pos: usize,
) -> ((usize, usize, usize), PlacedNodeData) {
    match to_place {
        graph::Node::Input { name } => {
            space.set((x_offset, y_offset, z_pos), |_| {
                SpaceCell::Used(SpaceBlock::Redstone)
            });
            space.set((x_offset, y_offset, z_pos + 1), |_| {
                SpaceCell::Used(SpaceBlock::SolidBlock)
            });
            reserve_around(space, (x_offset, y_offset, z_pos), (1, 1, 1), RESERVE_SPACE);
            ((1, 1, 1), PlacedNodeData::Input { name })
        }
        graph::Node::Output { name } => {
            space.set((x_offset, y_offset, z_pos), |_| {
                SpaceCell::Used(SpaceBlock::Redstone)
            });
            space.set((x_offset, y_offset, z_pos + 1), |_| {
                SpaceCell::Used(SpaceBlock::SolidBlock)
            });
            reserve_around(space, (x_offset, y_offset, z_pos), (1, 1, 1), RESERVE_SPACE);
            ((1, 1, 1), PlacedNodeData::Output { name })
        }
        graph::Node::Variable { id, name } => {
            space.set((x_offset, y_offset, z_pos), |_| {
                SpaceCell::Used(SpaceBlock::Redstone)
            });
            space.set((x_offset, y_offset, z_pos + 1), |_| {
                SpaceCell::Used(SpaceBlock::SolidBlock)
            });
            reserve_around(space, (x_offset, y_offset, z_pos), (1, 1, 1), RESERVE_SPACE);
            ((1, 1, 1), PlacedNodeData::Variable { id, name })
        }
        graph::Node::Splitter { id, ports } => {
            let height = 1 + 2 * ((ports as usize) - 1);
            let input_height = (height - 1) / 2;
            let port_yoff = (0..(ports as usize)).map(|p| p * 2);

            let port_pos = port_yoff.map(|y_off| (x_offset + 2, y_offset + y_off, z_pos));
            let connecting_pos = (0..height).map(|y_off| (x_offset + 1, y_offset + y_off, z_pos));

            let place_pos = std::iter::once((x_offset, y_offset + input_height, z_pos))
                .chain(port_pos.clone())
                .chain(connecting_pos);

            place_pos.clone().for_each(|pos| {
                space.set(pos, |_| SpaceCell::Used(SpaceBlock::Redstone));
                space.set((pos.0, pos.1, pos.2 + 1), |_| {
                    SpaceCell::Used(SpaceBlock::SolidBlock)
                });
            });

            reserve_around(
                space,
                (x_offset, y_offset, z_pos),
                (3, height, 1),
                RESERVE_SPACE,
            );
            (
                (3, height, 1),
                PlacedNodeData::Splitter {
                    id,
                    input: (x_offset, y_offset + input_height, z_pos),
                    ports: port_pos.collect(),
                },
            )
        }
        graph::Node::Entity { id, value } => match value {
            graph::EntityValue::Operation {
                op: graph::Operation::Xor,
            } => {
                let redstone_pos = [
                    (x_offset, y_offset, z_pos),
                    (x_offset, y_offset + 3, z_pos),
                    (x_offset + 3, y_offset, z_pos),
                    (x_offset + 3, y_offset + 3, z_pos),
                    (x_offset + 4, y_offset, z_pos),
                    (x_offset + 4, y_offset + 1, z_pos),
                    (x_offset + 4, y_offset + 2, z_pos),
                    (x_offset + 4, y_offset + 3, z_pos),
                    (x_offset + 5, y_offset + 1, z_pos),
                    (x_offset + 6, y_offset + 1, z_pos),
                ];
                let solid_pos = [
                    (x_offset + 2, y_offset, z_pos),
                    (x_offset + 2, y_offset + 1, z_pos),
                    (x_offset + 2, y_offset + 2, z_pos),
                    (x_offset + 2, y_offset + 3, z_pos),
                ];
                let repeater_pos = [
                    (x_offset + 1, y_offset, z_pos),
                    (x_offset + 1, y_offset + 3, z_pos),
                ];
                let comparator_pos = [
                    (x_offset + 3, y_offset + 1, z_pos),
                    (x_offset + 3, y_offset + 2, z_pos),
                ];

                for pos in redstone_pos {
                    space.set(pos, |_| SpaceCell::Used(SpaceBlock::Redstone));
                    space.set((pos.0, pos.1, pos.2 + 1), |_| {
                        SpaceCell::Used(SpaceBlock::SolidBlock)
                    });
                }
                for pos in solid_pos {
                    space.set(pos, |_| SpaceCell::Used(SpaceBlock::SolidBlock));
                }
                for pos in repeater_pos {
                    space.set(pos, |_| {
                        SpaceCell::Used(SpaceBlock::Repeater {
                            direction: Orientation::East,
                        })
                    });
                    space.set((pos.0, pos.1, pos.2 + 1), |_| {
                        SpaceCell::Used(SpaceBlock::SolidBlock)
                    });
                }
                for pos in comparator_pos {
                    space.set(pos, |_| {
                        SpaceCell::Used(SpaceBlock::Comparator {
                            direction: Orientation::East,
                            activated: true,
                        })
                    });
                    space.set((pos.0, pos.1, pos.2 + 1), |_| {
                        SpaceCell::Used(SpaceBlock::SolidBlock)
                    });
                }

                reserve_around(space, (x_offset, y_offset, z_pos), (7, 4, 1), RESERVE_SPACE);

                (
                    (7, 4, 1),
                    PlacedNodeData::Entity {
                        id,
                        in_ports: [(x_offset, y_offset, z_pos), (x_offset, y_offset + 3, z_pos)]
                            .to_vec(),
                        out_ports: [(x_offset + 6, y_offset + 1, z_pos)].to_vec(),
                    },
                )
            }
            graph::EntityValue::Operation {
                op: graph::Operation::And,
            } => {
                let redstone_pos = [
                    (x_offset, y_offset, z_pos),
                    (x_offset, y_offset + 2, z_pos),
                    (x_offset + 3, y_offset + 1, z_pos),
                    (x_offset + 4, y_offset + 1, z_pos),
                ];
                let repeater_pos = [
                    (x_offset + 1, y_offset, z_pos),
                    (x_offset + 1, y_offset + 2, z_pos),
                ];
                let torch_pos = [
                    (x_offset + 3, y_offset, z_pos),
                    (x_offset + 3, y_offset + 2, z_pos),
                ];

                let solid_pos = [
                    (x_offset + 2, y_offset, z_pos),
                    (x_offset + 2, y_offset + 1, z_pos),
                    (x_offset + 2, y_offset + 2, z_pos),
                ];

                for pos in redstone_pos {
                    space.set(pos, |_| SpaceCell::Used(SpaceBlock::Redstone));
                    space.set((pos.0, pos.1, pos.2 + 1), |_| {
                        SpaceCell::Used(SpaceBlock::SolidBlock)
                    });
                }
                for pos in repeater_pos {
                    space.set(pos, |_| {
                        SpaceCell::Used(SpaceBlock::Repeater {
                            direction: Orientation::East,
                        })
                    });
                    space.set((pos.0, pos.1, pos.2 + 1), |_| {
                        SpaceCell::Used(SpaceBlock::SolidBlock)
                    });
                }
                for pos in torch_pos {
                    space.set(pos, |_| {
                        SpaceCell::Used(SpaceBlock::TorchOnBlock {
                            direction: Orientation::West,
                        })
                    });
                }
                for pos in solid_pos {
                    space.set(pos, |_| SpaceCell::Used(SpaceBlock::SolidBlock));
                }

                reserve_around(space, (x_offset, y_offset, z_pos), (5, 3, 1), RESERVE_SPACE);

                (
                    (5, 3, 1),
                    PlacedNodeData::Entity {
                        id,
                        in_ports: [(x_offset, y_offset, z_pos), (x_offset, y_offset + 2, z_pos)]
                            .to_vec(),
                        out_ports: [(x_offset + 4, y_offset + 1, z_pos)].to_vec(),
                    },
                )
            }
            graph::EntityValue::Operation {
                op: graph::Operation::Or,
            } => {
                let redstone_pos = [
                    (x_offset, y_offset, z_pos),
                    (x_offset, y_offset + 2, z_pos),
                    (x_offset + 1, y_offset, z_pos),
                    (x_offset + 1, y_offset + 1, z_pos),
                    (x_offset + 1, y_offset + 2, z_pos),
                    (x_offset + 2, y_offset + 1, z_pos),
                ];

                for pos in redstone_pos {
                    space.set(pos, |_| SpaceCell::Used(SpaceBlock::Redstone));
                    space.set((pos.0, pos.1, pos.2 + 1), |_| {
                        SpaceCell::Used(SpaceBlock::SolidBlock)
                    });
                }

                reserve_around(space, (x_offset, y_offset, z_pos), (3, 3, 1), RESERVE_SPACE);

                (
                    (3, 3, 1),
                    PlacedNodeData::Entity {
                        id,
                        in_ports: [(x_offset, y_offset, z_pos), (x_offset, y_offset + 2, z_pos)]
                            .to_vec(),
                        out_ports: [(x_offset + 2, y_offset + 1, z_pos)].to_vec(),
                    },
                )
            }
            graph::EntityValue::Variable { name } => {
                space.set((x_offset, y_offset, z_pos), |_| {
                    SpaceCell::Used(SpaceBlock::Redstone)
                });
                space.set((x_offset, y_offset, z_pos + 1), |_| {
                    SpaceCell::Used(SpaceBlock::SolidBlock)
                });
                reserve_around(space, (x_offset, y_offset, z_pos), (1, 1, 1), RESERVE_SPACE);
                (
                    (1, 1, 1),
                    PlacedNodeData::Entity {
                        id,
                        in_ports: vec![(x_offset, y_offset, z_pos)],
                        out_ports: vec![(x_offset, y_offset, z_pos)],
                    },
                )
            }
            other => todo!("Handle: {:?}", other),
        },
        other => {
            dbg!(&other);
            todo!("Place Node")
        }
    }
}

fn reserve_around(
    space: &mut space::Space<SpaceCell>,
    pos: (usize, usize, usize),
    size: (usize, usize, usize),
    reserve_size: usize,
) {
    let (x, y, z) = pos;
    let (width, depth, height) = size;

    let row = ((x.saturating_sub(reserve_size))..(x.saturating_add(width + reserve_size)));
    let top_row = row.clone().map(|x_pos| (x_pos, y - 1, z));
    let bottom_row = row.clone().map(|x_pos| (x_pos, y + depth, z));

    let column = ((y.saturating_sub(2))..(y.saturating_add(depth + 1)));
    let left_column = column
        .clone()
        .map(|y_pos| (x.saturating_sub(reserve_size), y_pos, z));
    let right_column = column
        .clone()
        .map(|y_pos| (x.saturating_add(width + reserve_size), y_pos, z));

    let raw_sides = left_column.chain(right_column);

    let sides = raw_sides
        .clone()
        .chain(raw_sides.map(|(x, y, z)| (x, y, z + 1)));

    let to_reserve = top_row.chain(bottom_row).chain(sides);

    to_reserve.for_each(|pos| {
        space.set(pos, |_| SpaceCell::Reserved);
    });
}
