
use rand::Rng;
use delaunay::Point;

fn main() {
    let mut rng = rand::thread_rng();
    let count = 100_000;

    let mut points = Vec::with_capacity(count);
    for _ in 0..count {
        let x = rng.gen_range(-1.0, 1.0);
        let y = rng.gen_range(-1.0, 1.0);

        points.push(Point::new(x, y));
    }

    let before = std::time::Instant::now();
    let tris = delaunay::triangulate(&points);
    let duration = before.elapsed();

    println!("done in {} ms", duration.as_millis());
    println!("points: {}    tris: {}", points.len(), tris.len());
}

