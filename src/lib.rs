mod quad_edge;

use quad_edge::{EdgeRef, QuadEdge, Vertices};

#[derive(Copy, Clone, Default, PartialEq)]
pub struct Point {
    pub x: f32,
    pub y: f32,
}

// For each edge, we store the position of its starting vertex.
#[derive(Debug, Copy, Clone, Default)]
struct Vertex {
    x: f32,
    y: f32,
    original: usize,
}

impl PartialEq for Vertex {
    fn eq(&self, other: &Self) -> bool {
        self.original == other.original
    }
}

/// Triangulates the points by returning a list of triangles. Duplicate points will not be
/// considered.
pub fn triangulate(points: &[Point]) -> Vec<[usize; 3]> {
    fn ordered_cmp(a: f32, b: f32) -> std::cmp::Ordering {
        a.partial_cmp(&b).unwrap_or(std::cmp::Ordering::Equal)
    }

    let mut vertices = points
        .iter()
        .enumerate()
        .map(|(index, point)| Vertex {
            x: point.x,
            y: point.y,
            original: index,
        })
        .collect::<Vec<_>>();

    vertices.sort_unstable_by(|a, b| ordered_cmp(a.x, b.x).then(ordered_cmp(a.y, b.y)));

    let unique = partition_unique(&mut vertices);

    let mut triangles = Triangulation {
        quad: QuadEdge::with_capacity(5 * unique.len()),
    };

    if unique.len() >= 2 {
        triangulate_recursive(&mut triangles, unique);
    }

    collect_triangles(&triangles, unique)
}

fn collect_triangles(triangles: &Triangulation, vertices: &[Vertex]) -> Vec<[usize; 3]> {
    let dual_edges = triangles.quad.iter_dual();
    let mut visited = vec![false; dbg!(dual_edges.len())];

    let mut indices = Vec::with_capacity(2 * vertices.len());

    for edge in dual_edges {
        if visited[edge.dual_index()] {
            continue;
        }

        let mut corners = [0; 3];
        let mut probe = edge;
        for i in 0..3 {
            visited[probe.dual_index()] = true;
            corners[i] = triangles.origin(probe.rot()).original;
            probe = probe.onext(&triangles.quad);
        }

        // make sure that this is an actual triangle
        if edge == probe {
            indices.push(corners);
        }
    }

    indices
}

/// Triangulate the given vertices, and return the leftmost counter-clockwise and rightmost
/// clockwise edge on the convex hull, respectively.
fn triangulate_recursive(triangles: &mut Triangulation, vertices: &[Vertex]) -> (EdgeRef, EdgeRef) {
    match vertices {
        [] | [_] => panic!("cannot add edge between {} vertices", vertices.len()),
        &[a, b] => {
            let edge = triangles.make_edge(a, b);
            (edge, edge.sym())
        }
        &[a, b, c] => {
            let ab = triangles.make_edge(a, b);
            let bc = triangles.make_edge(b, c);
            triangles.quad.splice(ab.sym(), bc);

            if ccw([a, b, c]) {
                triangles.connect(bc, ab);
                (ab, bc.sym())
            } else if ccw([a, c, b]) {
                let ca = triangles.connect(bc, ab);
                (ca.sym(), ca)
            } else {
                // colinear
                (ab, bc.sym())
            }
        }
        vertices @ [_, _, _, _, ..] => {
            let (left, right) = vertices.split_at(vertices.len() / 2);
            let (mut left_outside, mut left_inside) = triangulate_recursive(triangles, left);
            let (mut right_inside, mut right_outside) = triangulate_recursive(triangles, right);

            // compute lower common tangent
            loop {
                if triangles.is_left_of(triangles.origin(right_inside), left_inside) {
                    left_inside = left_inside.lnext(&triangles.quad);
                } else if triangles.is_right_of(triangles.origin(left_inside), right_inside) {
                    right_inside = right_inside.rprev(&triangles.quad);
                } else {
                    break;
                }
            }

            let base = triangles.connect(right_inside.sym(), left_inside);
            if triangles.origin(left_inside) == triangles.origin(left_outside) {
                left_outside = base.sym();
            }
            if triangles.origin(right_inside) == triangles.origin(right_outside) {
                right_outside = base;
            }

            merge_triangles(triangles, base);

            (left_outside, right_outside)
        }
    }
}

fn merge_triangles(triangles: &mut Triangulation, mut base: EdgeRef) {
    loop {
        let left_init = base.sym().onext(&triangles.quad);
        let left_candidate = next_candidate(triangles, base, left_init, EdgeRef::onext);

        let right_init = base.oprev(&triangles.quad);
        let right_candidate = next_candidate(triangles, base, right_init, EdgeRef::oprev);

        base = match (left_candidate, right_candidate) {
            (None, None) => break,
            (Some(left), None) => triangles.connect(base.sym(), left.sym()),
            (None, Some(right)) => triangles.connect(right, base.sym()),
            (Some(left), Some(right)) => {
                let base_left = triangles.target(base);
                let base_right = triangles.origin(base);
                let left_cand = triangles.target(left);
                let right_cand = triangles.target(right);

                if inside_circumference([base_left, base_right, left_cand], right_cand) {
                    triangles.connect(right, base.sym())
                } else {
                    triangles.connect(base.sym(), left.sym())
                }
            }
        };
    }
}

fn next_candidate(
    triangles: &mut Triangulation,
    base: EdgeRef,
    mut candidate: EdgeRef,
    next: impl Fn(EdgeRef, &QuadEdge<Vertex>) -> EdgeRef,
) -> Option<EdgeRef> {
    let base_left = triangles.target(base);
    let base_right = triangles.origin(base);

    let above_line = |candidate| ccw([base_left, base_right, candidate]);
    if above_line(triangles.target(candidate)) {
        let mut cand_target = triangles.target(candidate);
        loop {
            let next_candidate = next(candidate, &triangles.quad);
            let next_target = triangles.target(next_candidate);
            if !inside_circumference([base_left, base_right, cand_target], next_target) {
                break;
            }

            triangles.delete_edge(candidate);

            cand_target = next_target;
            candidate = next_candidate;
        }

        if above_line(triangles.target(candidate)) {
            Some(candidate)
        } else {
            None
        }
    } else {
        None
    }
}

/// Is the triangle winded counter clockwise.
fn ccw(triangle: [Vertex; 3]) -> bool {
    let [a, b, c] = triangle;
    let ab = Point::new(b.x - a.x, b.y - a.y);
    let ac = Point::new(c.x - a.x, c.y - a.y);
    ab.x * ac.y - ab.y * ac.x > 0.0
}

fn partition_unique(values: &mut [Vertex]) -> &mut [Vertex] {
    if values.len() <= 1 {
        return values;
    }

    let mut write = 1;
    for read in 1..values.len() {
        let next = &values[read];
        let prev = &values[write - 1];

        if next.x != prev.x || next.y != prev.y {
            values.swap(write, read);
            write += 1;
        }
    }

    &mut values[..write]
}

struct Triangulation {
    quad: QuadEdge<Vertex>,
}

impl Triangulation {
    fn make_edge(&mut self, origin: Vertex, target: Vertex) -> EdgeRef {
        self.quad.make_edge(Vertices {
            origin,
            target,
            ..Default::default()
        })
    }

    fn origin(&self, edge: EdgeRef) -> Vertex {
        *self.quad[edge]
    }

    fn target(&self, edge: EdgeRef) -> Vertex {
        *self.quad[edge.sym()]
    }

    fn is_left_of(&self, point: Vertex, edge: EdgeRef) -> bool {
        ccw([point, self.origin(edge), self.target(edge)])
    }

    fn is_right_of(&self, point: Vertex, edge: EdgeRef) -> bool {
        ccw([point, self.target(edge), self.origin(edge)])
    }

    /// Create a new edge that connects the end point of `a` and the origin of `b` so that
    /// they form a triangle.
    fn connect(&mut self, a: EdgeRef, b: EdgeRef) -> EdgeRef {
        let vertices = Vertices {
            origin: *self.quad[a.sym()],
            target: *self.quad[b],
            ..Default::default()
        };

        let e = self.quad.make_edge(vertices);
        self.quad.splice(e, a.lnext(&self.quad));
        self.quad.splice(e.sym(), b);
        e
    }

    /// Removes an edge from the triangle mesh.
    fn delete_edge(&mut self, edge: EdgeRef) {
        self.quad.splice(edge, edge.oprev(&self.quad));
        self.quad.splice(edge.sym(), edge.sym().oprev(&self.quad));
    }
}

fn inside_circumference(triangle: [Vertex; 3], point: Vertex) -> bool {
    let [a, b, c] = triangle;

    let ap = Point::new(a.x - point.x, a.y - point.y);
    let bp = Point::new(b.x - point.x, b.y - point.y);
    let cp = Point::new(c.x - point.x, c.y - point.y);

    let mag2 = |v: Point| v.x * v.x + v.y * v.y;

    let det = determinant_3x3([
        [ap.x, ap.y, mag2(ap)],
        [bp.x, bp.y, mag2(bp)],
        [cp.x, cp.y, mag2(cp)],
    ]);

    det > 0.0
}

fn determinant_3x3(matrix: [[f32; 3]; 3]) -> f32 {
    #[rustfmt::skip]
    let [
        [a, b, c],
        [d, e, f], 
        [g, h, i],
    ] = matrix;
    a * (e * i - f * h) - b * (d * i - f * g) + c * (d * h - e * g)
}

impl Point {
    pub fn new(x: f32, y: f32) -> Point {
        Point { x, y }
    }
}

impl std::fmt::Debug for Point {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Point [{}, {}]", self.x, self.y)
    }
}

impl std::fmt::Display for Point {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "[{}, {}]", self.x, self.y)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn simple_4_points() {
        let mut vertices = [
            Point::new(1.0, 1.0),
            Point::new(2.0, 4.0),
            Point::new(5.0, 1.0),
            Point::new(5.0, 4.0),
        ];

        let triangles = triangulate(&mut vertices);
        assert_eq!(triangles, [[1, 0, 2], [2, 3, 1]]);
    }
}
