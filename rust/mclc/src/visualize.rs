use crate::{space::Space, Orientation, SpaceBlock, SpaceCell};

const SCALE: usize = 10;

pub fn visualize(space: &Space<SpaceCell>, path: &str) {
    let (max_x, max_y, max_z) = space.size();
    let layer_height = (max_y + 1) * SCALE;

    let svg_width = (max_x + 1) * SCALE;
    let svg_height = layer_height * max_z;

    let grid_group = grid(space);
    let cell_group = cells(space);

    let doc_builder = svg::Document::new()
        .set("width", svg_width)
        .set("height", svg_height)
        .set("version", "1.11.1")
        .add(grid_group)
        .add(cell_group);

    svg::save(path, &doc_builder).unwrap();
}

fn grid(space: &Space<SpaceCell>) -> svg::node::element::Group {
    let (max_x, max_y, max_z) = space.size();
    let layer_height = (max_y + 1) * SCALE;

    let mut result = svg::node::element::Group::new();
    for layer_c in 0..max_z {
        let y_offset = layer_c * layer_height;

        for x in (0..max_x).map(|x| x * SCALE) {
            let path_data = svg::node::element::path::Data::new()
                .move_to((x, y_offset))
                .line_by((0, max_y * SCALE));

            let path = svg::node::element::Path::new()
                .set("fill", "none")
                .set("stroke", "black")
                .set("stroke-width", 1)
                .set("d", path_data);

            result = result.add(path);
        }

        for y in (1..max_y).map(|y| y * SCALE) {
            let path_data = svg::node::element::path::Data::new()
                .move_to((0, y + y_offset))
                .line_by((max_x * SCALE, 0));

            let path = svg::node::element::Path::new()
                .set("fill", "none")
                .set("stroke", "black")
                .set("stroke-width", 1)
                .set("d", path_data);

            result = result.add(path);
        }
    }

    result
}

fn cells(space: &Space<SpaceCell>) -> svg::node::element::Group {
    let mut result = svg::node::element::Group::new();

    let cell_iter = space.iter().filter_map(|(pos, c)| match c {
        SpaceCell::Used(inner) => Some((pos, inner)),
        _ => None,
    });

    let (_, max_y, _) = space.size();
    let layer_height = (max_y + 1) * SCALE;

    for (pos, item) in cell_iter {
        let x = pos.0 * SCALE;
        let y = pos.1 * SCALE + pos.2 * layer_height;

        match item {
            SpaceBlock::SolidBlock => {
                result = result.add(
                    svg::node::element::Rectangle::new()
                        .set("fill", "gray")
                        .set("x", x)
                        .set("y", y)
                        .set("width", SCALE)
                        .set("height", SCALE),
                );
            }
            SpaceBlock::Redstone => {
                result = result.add(
                    svg::node::element::Rectangle::new()
                        .set("fill", "#FF0000")
                        .set("x", x)
                        .set("y", y)
                        .set("width", SCALE)
                        .set("height", SCALE),
                );
            }
            SpaceBlock::Repeater { direction } => {
                let mut repeater = svg::node::element::Group::new();
                repeater = repeater.add(
                    svg::node::element::Rectangle::new()
                        .set("fill", "lightgrey")
                        .set("x", x)
                        .set("y", y)
                        .set("width", SCALE)
                        .set("height", SCALE),
                );

                match direction {
                    Orientation::East => {
                        let torch_size = SCALE / 5;
                        let torch_y = y + SCALE / 2 - torch_size / 2;
                        repeater = repeater
                            .add(
                                svg::node::element::Rectangle::new()
                                    .set("fill", "#FF0000")
                                    .set("x", x + SCALE / 5 * 4)
                                    .set("y", torch_y)
                                    .set("width", torch_size)
                                    .set("height", torch_size),
                            )
                            .add(
                                svg::node::element::Rectangle::new()
                                    .set("fill", "#FF0000")
                                    .set("x", x + SCALE / 5 * 2)
                                    .set("y", torch_y)
                                    .set("width", torch_size)
                                    .set("height", torch_size),
                            );
                    }
                    other => {
                        dbg!(other);
                        todo!()
                    }
                };

                result = result.add(repeater);
            }
            SpaceBlock::Comparator {
                direction,
                activated,
            } => {
                let mut comparator = svg::node::element::Group::new();

                comparator = comparator.add(
                    svg::node::element::Rectangle::new()
                        .set("fill", "lightgrey")
                        .set("x", x)
                        .set("y", y)
                        .set("width", SCALE)
                        .set("height", SCALE),
                );

                let third_torch_color = if activated { "#FF0000" } else { "#AA3333" };
                match direction {
                    Orientation::East => {
                        let torch_size = SCALE / 5;
                        let torch_x = x + SCALE / 5 * 1;

                        comparator = comparator
                            .add(
                                svg::node::element::Rectangle::new()
                                    .set("fill", "#FF0000")
                                    .set("x", torch_x)
                                    .set("y", y + SCALE / 5 * 1)
                                    .set("width", torch_size)
                                    .set("height", torch_size),
                            )
                            .add(
                                svg::node::element::Rectangle::new()
                                    .set("fill", "#FF0000")
                                    .set("x", torch_x)
                                    .set("y", y + SCALE / 5 * 3)
                                    .set("width", torch_size)
                                    .set("height", torch_size),
                            )
                            .add(
                                svg::node::element::Rectangle::new()
                                    .set("fill", third_torch_color)
                                    .set("x", x + SCALE / 5 * 3)
                                    .set("y", y + SCALE / 2 - torch_size / 2)
                                    .set("width", torch_size)
                                    .set("height", torch_size),
                            );
                    }
                    other => {
                        dbg!(other);
                        todo!()
                    }
                };

                result = result.add(comparator);
            }
            SpaceBlock::TorchOnBlock { direction } => {
                let torch_size = SCALE / 5;
                let (x, y) = match direction {
                    Orientation::West => (x + 0, y + SCALE / 2 - torch_size / 2),
                    other => {
                        dbg!(other);
                        todo!()
                    }
                };

                result = result.add(
                    svg::node::element::Rectangle::new()
                        .set("fill", "#FF0000")
                        .set("x", x)
                        .set("y", y)
                        .set("width", torch_size)
                        .set("height", torch_size),
                );
            }
        };
    }

    result
}
