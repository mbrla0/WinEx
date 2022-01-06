fn main() {
	let event_loop = winit::event_loop::EventLoop::new();
	let window = winit::window::WindowBuilder::new()
		.with_resizable(true)
		.with_decorations(true)
		.with_inner_size(winit::dpi::PhysicalSize::new(800, 600))
		.build(&event_loop).unwrap();

	let blur = winex::Blur::Quality;
	winex::set_blur(&window, blur).unwrap();

	event_loop.run(move |event, _, flow| {
		*flow = winit::event_loop::ControlFlow::Poll;
		match event {
			winit::event::Event::WindowEvent { window_id, event }
			if window_id == window.id() => match event {

				winit::event::WindowEvent::CloseRequested =>
					*flow = winit::event_loop::ControlFlow::Exit,
				_ => {}
			},
			_ => {}
		}
	})
}