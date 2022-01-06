# WinEx 
The library for those who need a little extra from their windows. ™

WinEx - Short for Window Extended - is a library whose goal is to implement 
support for the sort of eye-candy and extra support functionality, such as the 
management of background blur or fine-grained control over window border 
behavior, that is outside the scope of most of the windowing libraries already 
available in Rust. 

Another goal of the library is to be as unobtrusive and, thus, as easy to 
integrate into existing project workflows, as possible. In fact, most effects 
can be configured and enabled or disabled in just a few lines right next to 
your window bring-up code. But don't take my word for it! Here's all the code
you need to enable a default blur effect for a window:

```rust
winex::set_blur(&window, winex::Blur::Quality).unwrap();
```

The current feature set is, admittedly, still rather small, but the project is
open to contributions, and I would love to work with anyone who can help, from 
people who work routinely with these sorts of things and that I could 
undoubtedly learn a lot from, to people who just fancy dropping a quick fix
or reporting a bug. All are welcome.

## Features and Support
The main features of this library and the platforms on which they're supported
are, currently:
 - [ ] Blurring the background of windows (`winex::set_blur`)
   - [X] Windows 7+
   - [ ] macOS
   - [ ] X11
   - [ ] Wayland
 - [ ] Managing control regions in borderless windows. (Planned)

## Non-goals

This library may handle all sorts of disparate use cases, but what it's not, 
however, is a library for window bring up or event loop management. the folks 
over at [winit](https://crates.io/crates/winit) do a much better job at that 
than I could ever hope to given my schedule, and for this reason WinEx is only 
meant as an addon to it and other such libraries.

WinEx seeks to implement and support as many platforms as possible, to a 
reasonable degree. Where to a reasonable degree means only enabling features 
on platforms that offer at least partial or unofficial support for them.
 
