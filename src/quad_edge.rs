pub struct QuadEdge<T = ()> {
    edges: Vec<EdgeRecord<T>>,
}

#[derive(Debug, Copy, Clone)]
pub struct EdgeRecord<T> {
    next: EdgeRef,
    data: T,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct EdgeRef(usize);

#[derive(Debug, Copy, Clone, Default)]
pub struct Vertices<T> {
    pub origin: T,
    pub right: T,
    pub target: T,
    pub left: T,
}

impl<T> QuadEdge<T> {
    pub fn with_capacity(capacity: usize) -> Self {
        QuadEdge {
            edges: Vec::with_capacity(4 * capacity),
        }
    }

    pub fn make_edge(&mut self, values: Vertices<T>) -> EdgeRef {
        let base_index = self.edges.len();

        let new_edge = |next, data| EdgeRecord {
            next: EdgeRef(next),
            data,
        };

        self.edges.reserve(4);
        self.edges.push(new_edge(base_index + 0, values.origin));
        self.edges.push(new_edge(base_index + 3, values.right));
        self.edges.push(new_edge(base_index + 2, values.target));
        self.edges.push(new_edge(base_index + 1, values.left));

        EdgeRef(base_index)
    }

    /// Depending on the two origin vertices of the two edges, there are two cases:
    ///
    /// 1. if the two vertices are distinct, `splice` will combine them into one.
    /// 2. if the two vertices are the same, `splice` will break it in two separate pieces.
    pub fn splice(&mut self, a: EdgeRef, b: EdgeRef) {
        debug_assert!(
            a.0 % 2 == b.0 % 2,
            "splice is not defined for a primal and dual edge"
        );

        let alpha = a.onext(self).rot();
        let beta = b.onext(self).rot();

        let a_next = self[a].next;
        let b_next = self[b].next;
        let alpha_next = self[alpha].next;
        let beta_next = self[beta].next;

        // interchange: a <-> b
        self[a].next = b_next;
        self[b].next = a_next;

        // interchange: alpha <-> beta
        self[alpha].next = beta_next;
        self[beta].next = alpha_next;
    }

    pub fn iter_primitive(&self) -> impl Iterator<Item = EdgeRef> + ExactSizeIterator {
        let len = self.edges.len();
        (0..len).step_by(2).map(|index| EdgeRef(index))
    }

    pub fn iter_dual(&self) -> impl Iterator<Item = EdgeRef> + ExactSizeIterator {
        let len = self.edges.len();
        (1..len).step_by(2).map(|index| EdgeRef(index))
    }
}

impl EdgeRef {
    pub fn dual_index(self) -> usize {
        (self.0 - 1) / 2
    }

    // x00 -> x01 -> x10 -> x11 -> x00
    pub fn rot(mut self) -> Self {
        self.0 ^= (self.0 & 1) << 1;
        self.0 ^= 1;
        self
    }

    // x00 <-> x10
    // x01 <-> x11
    pub fn sym(mut self) -> Self {
        self.0 ^= 2;
        self
    }

    // x00 -> x11 -> x10 -> x01 -> x00
    pub fn rot_inv(mut self) -> Self {
        self.0 ^= 1;
        self.0 ^= (self.0 & 1) << 1;
        self
    }

    pub fn onext<T>(self, quad: &QuadEdge<T>) -> EdgeRef {
        quad[self].next
    }

    pub fn oprev<T>(self, quad: &QuadEdge<T>) -> EdgeRef {
        self.rot().onext(quad).rot()
    }

    pub fn lnext<T>(self, quad: &QuadEdge<T>) -> EdgeRef {
        self.rot_inv().onext(quad).rot()
    }

    pub fn rnext<T>(self, quad: &QuadEdge<T>) -> EdgeRef {
        self.rot().onext(quad).rot_inv()
    }

    pub fn dnext<T>(self, quad: &QuadEdge<T>) -> EdgeRef {
        self.sym().onext(quad).sym()
    }

    pub fn lprev<T>(self, quad: &QuadEdge<T>) -> EdgeRef {
        self.onext(quad).sym()
    }

    pub fn rprev<T>(self, quad: &QuadEdge<T>) -> EdgeRef {
        self.sym().onext(quad)
    }

    pub fn dprev<T>(self, quad: &QuadEdge<T>) -> EdgeRef {
        self.rot_inv().onext(quad).rot_inv()
    }
}

impl<T> std::ops::Index<EdgeRef> for QuadEdge<T> {
    type Output = EdgeRecord<T>;

    fn index(&self, edge: EdgeRef) -> &Self::Output {
        &self.edges[edge.0]
    }
}

impl<T> std::ops::IndexMut<EdgeRef> for QuadEdge<T> {
    fn index_mut(&mut self, edge: EdgeRef) -> &mut Self::Output {
        &mut self.edges[edge.0]
    }
}

impl<T> std::ops::Deref for EdgeRecord<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.data
    }
}

impl<T> std::ops::DerefMut for EdgeRecord<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.data
    }
}

impl<T: std::fmt::Debug> std::fmt::Debug for QuadEdge<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "QuadEdge ")?;
        let mut list = f.debug_list();

        for (i, edge) in self.edges.iter().enumerate() {
            list.entry(&format!("{}: {:?} -> {}", i, edge.data, edge.next.0));
        }

        list.finish()
    }
}
