use ggez::{
    conf::{NumSamples, WindowSetup},
    event::{self, EventHandler, KeyCode, MouseButton},
    graphics::{self, DrawMode, DrawParam},
    Context, ContextBuilder, GameResult,
};

use std::time::Instant;

use delaunay::Point;

struct Gui {
    points: Vec<Point>,
    triangles: graphics::Mesh,
    counter_start: Instant,
    frames: f32,
    fps: f32,
}

fn main() -> GameResult {
    let (mut ctx, mut events) = ContextBuilder::new("", "")
        .window_setup(WindowSetup::default().samples(NumSamples::Four))
        .build()?;
    let mut gui = Gui::new(&mut ctx)?;
    event::run(&mut ctx, &mut events, &mut gui)
}

impl Gui {
    pub fn new(ctx: &mut Context) -> GameResult<Self> {
        Ok(Gui {
            points: Vec::new(),
            triangles: graphics::Mesh::new_circle(
                ctx,
                DrawMode::fill(),
                [-10.0, 0.0],
                1.0,
                1.0,
                (0, 0, 0).into(),
            )?,
            counter_start: Instant::now(),
            frames: 0.0,
            fps: 0.0,
        })
    }
}

impl EventHandler for Gui {
    fn update(&mut self, _ctx: &mut Context) -> GameResult {
        Ok(())
    }

    fn draw(&mut self, ctx: &mut Context) -> GameResult {
        graphics::clear(ctx, graphics::Color::from_rgb(40, 40, 40));

        graphics::draw(ctx, &self.triangles, DrawParam::default())?;

        for &point in self.points.iter() {
            let circle = graphics::Mesh::new_circle(
                ctx,
                DrawMode::fill(),
                [point.x, point.y],
                3.0,
                0.1,
                (255, 0, 0).into(),
            )?;

            graphics::draw(ctx, &circle, DrawParam::default())?;
        }

        self.frames += 1.0;
        let elapsed = self.counter_start.elapsed().as_secs_f32();
        if elapsed > 0.5 {
            self.counter_start = Instant::now();
            self.fps = self.frames / elapsed;
            self.frames = 0.0;
        }

        let fps = graphics::Text::new(format!("fps: {}", self.fps.round()));
        graphics::draw(ctx, &fps, DrawParam::default())?;

        graphics::present(ctx)
    }

    fn mouse_button_down_event(
        &mut self,
        ctx: &mut Context,
        button: ggez::event::MouseButton,
        x: f32,
        y: f32,
    ) {
        if matches!(button, MouseButton::Left) {
            self.points.push(Point::new(x, y));
            self.update_triangles(ctx);
        }
    }

    fn key_down_event(
        &mut self,
        ctx: &mut Context,
        keycode: event::KeyCode,
        _keymods: event::KeyMods,
        _repeat: bool,
    ) {
        use rand::Rng;

        match keycode {
            KeyCode::Escape => event::quit(ctx),
            KeyCode::R => {
                let mut rng = rand::thread_rng();
                let count = 100_000;

                let (width, height) = graphics::drawable_size(ctx);

                self.points.clear();
                for _ in 0..count {
                    let x = rng.gen_range(0.0, width);
                    let y = rng.gen_range(0.0, height);

                    self.points.push(Point::new(x, y));
                }

                self.update_triangles(ctx);
            }
            _ => {}
        }
    }
}

impl Gui {
    pub fn update_triangles(&mut self, ctx: &mut Context) {
        let before = Instant::now();
        let indices = delaunay::triangulate(&self.points);
        let duration = before.elapsed();
        eprintln!("triangulation: {:.3} ms", duration.as_secs_f64() * 1000.0);

        if !indices.is_empty() {
            let before = Instant::now();

            let mut mesh = graphics::MeshBuilder::new();

            for [a, b, c] in indices {
                let vertex = |index: usize| [self.points[index].x, self.points[index].y];
                let vertices = [vertex(a), vertex(b), vertex(c)];
                mesh.line(&[vertex(a), vertex(b)], 1.0, (255, 255, 255).into()).unwrap();
                mesh.line(&[vertex(b), vertex(c)], 1.0, (255, 255, 255).into()).unwrap();
                mesh.line(&[vertex(c), vertex(a)], 1.0, (255, 255, 255).into()).unwrap();
            }

            self.triangles = mesh.build(ctx).unwrap();

            let duration = before.elapsed();
            eprintln!("build mesh: {:.3} ms", duration.as_secs_f64() * 1000.0);
        }
    }
}
