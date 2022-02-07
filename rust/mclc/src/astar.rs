use std::{collections::HashMap, fmt::Debug, hash::Hash};

pub trait Container {
    type Index;
}

fn reconstruct_path<I>(came_from: &HashMap<I, I>, mut current: I) -> Vec<I>
where
    I: Eq + Hash + Clone,
{
    let mut path = vec![current.clone()];

    while came_from.contains_key(&current) {
        current = came_from.get(&current).unwrap().clone();
        path.insert(0, current.clone());
    }

    path
}

pub fn path<C, DF, NF>(
    container: &mut C,
    start: C::Index,
    dest: C::Index,
    mut dist: DF,
    mut neigh: NF,
) -> Vec<C::Index>
where
    C: Container,
    C::Index: Eq + Hash + Clone + Debug,
    DF: FnMut(C::Index, C::Index) -> i64,
    NF: FnMut(&C, C::Index) -> Vec<(C::Index, i64)>,
{
    let mut open_set: Vec<C::Index> = vec![start.clone()];
    let mut came_from: HashMap<C::Index, C::Index> = HashMap::new();
    let mut gscores: HashMap<C::Index, i64> = HashMap::new();
    let mut fscores: HashMap<C::Index, i64> = HashMap::new();

    gscores.insert(start.clone(), 0);
    fscores.insert(start.clone(), dist(start.clone(), dest.clone()));

    while !open_set.is_empty() {
        let ((index, current), _): ((usize, C::Index), i64) = open_set
            .iter()
            .enumerate()
            .map(|(i, c)| {
                let f_score_val = fscores.get(c).cloned().unwrap();
                ((i, c), f_score_val)
            })
            .min_by(|(_, x), (_, y)| x.cmp(y))
            .map(|((i, c), v)| ((i, c.clone()), v))
            .unwrap();

        //dbg!(&current);

        if current == dest {
            return reconstruct_path(&came_from, current);
        }

        open_set.remove(index);

        let current_gscore = gscores.get(&current).cloned().unwrap();
        let neighbours = neigh(&container, current.clone());
        for (neighbour, neighbour_cost) in neighbours {
            let tentative_gscore = current_gscore + neighbour_cost;
            let prev_neighbour_gscore = gscores.get(&neighbour).cloned().unwrap_or(i64::MAX);

            if tentative_gscore < prev_neighbour_gscore {
                came_from.insert(neighbour.clone(), current.clone());
                gscores.insert(neighbour.clone(), tentative_gscore);
                fscores.insert(
                    neighbour.clone(),
                    tentative_gscore + dist(neighbour.clone(), dest.clone()),
                );

                if !open_set.contains(&neighbour) {
                    open_set.push(neighbour);
                }
            }
        }
    }

    todo!("Run A*")
}
