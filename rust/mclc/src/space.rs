use crate::astar;

pub struct Space<T> {
    content: Vec<Vec<Vec<T>>>,
}

impl<T> astar::Container for Space<T> {
    type Index = (usize, usize, usize);
}

impl<T> Space<T>
where
    T: Clone + Default,
{
    pub fn new() -> Self {
        Self {
            content: Vec::new(),
        }
    }

    pub fn get(&self, pos: (usize, usize, usize)) -> T {
        let (x, y, z) = pos;

        let layer = match self.content.get(z) {
            Some(l) => l,
            None => {
                return T::default();
            }
        };

        let row = match layer.get(y) {
            Some(r) => r,
            None => {
                return T::default();
            }
        };

        match row.get(x) {
            Some(c) => c.clone(),
            None => T::default(),
        }
    }

    pub fn set<F>(&mut self, pos: (usize, usize, usize), value: F)
    where
        F: Fn(&T) -> T,
    {
        let (x, y, z) = pos;

        let layer = match self.content.get_mut(z) {
            Some(l) => l,
            None => {
                let current_layer_count = self.content.len();
                let to_add = z - current_layer_count + 1;
                self.content
                    .extend(std::iter::repeat(Vec::new()).take(to_add + 1));

                self.content.get_mut(z).unwrap()
            }
        };

        let row = match layer.get_mut(y) {
            Some(r) => r,
            None => {
                let current_row_count = layer.len();
                let to_add = y - current_row_count + 1;
                layer.extend(std::iter::repeat(Vec::new()).take(to_add));

                layer.get_mut(y).unwrap()
            }
        };

        let cell = match row.get_mut(x) {
            Some(c) => c,
            None => {
                let current_cell_count = row.len();
                let to_add = x - current_cell_count + 1;
                row.extend(std::iter::repeat(T::default()).take(to_add));

                row.get_mut(x).unwrap()
            }
        };

        let n_value = value(&cell);

        *cell = n_value;
    }

    pub fn iter(&self) -> impl Iterator<Item = ((usize, usize, usize), T)> {
        self.content
            .clone()
            .into_iter()
            .enumerate()
            .map(|(z, layer)| {
                layer
                    .into_iter()
                    .enumerate()
                    .map(move |(y, row)| {
                        row.into_iter()
                            .enumerate()
                            .map(move |(x, c)| ((x, y, z), c))
                    })
                    .flatten()
            })
            .flatten()
    }

    pub fn size(&self) -> (usize, usize, usize) {
        let height = self.content.len();

        let depth = self.content.iter().map(|layer| layer.len()).max().unwrap();

        let width = self
            .content
            .iter()
            .map(|layer| layer.iter().map(|row| row.len()).max().unwrap_or(0))
            .max()
            .unwrap();

        (width, depth, height)
    }
}
