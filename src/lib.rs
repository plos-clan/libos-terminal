#![no_std]
#![no_main]
#![feature(linkage)]
#![feature(alloc_error_handler)]
#![allow(static_mut_refs)]
#![allow(unsafe_op_in_unsafe_fn)]
#![allow(clippy::not_unsafe_ptr_arg_deref)]

extern crate alloc;

use alloc::boxed::Box;
use alloc::string::String;
use core::alloc::{GlobalAlloc, Layout};
use core::ffi::{CStr, c_char, c_void};
use core::fmt;
use core::fmt::Write;
use core::panic::PanicInfo;
use core::slice::from_raw_parts_mut;
use os_terminal::font::TrueTypeFont;
use os_terminal::{ClipboardHandler, DrawTarget};
use os_terminal::{MouseInput, Palette, Rgb, Terminal};

#[panic_handler]
unsafe fn panic(info: &PanicInfo) -> ! {
    println!("panicked: {}", info.message());
    terminal_destroy();
    loop {}
}

#[global_allocator]
static ALLOCATOR: Allocator = Allocator;

#[alloc_error_handler]
fn alloc_error_handler(layout: Layout) -> ! {
    panic!("allocation error: {:?}", layout);
}

struct Allocator;

unsafe impl GlobalAlloc for Allocator {
    unsafe fn alloc(&self, layout: Layout) -> *mut u8 {
        unsafe { MALLOC.unwrap()(layout.size()) as *mut u8 }
    }

    unsafe fn dealloc(&self, ptr: *mut u8, _layout: Layout) {
        unsafe { FREE.unwrap()(ptr as *mut c_void) };
    }
}

static mut MALLOC: Option<extern "C" fn(usize) -> *mut c_void> = None;
static mut FREE: Option<extern "C" fn(*mut c_void)> = None;
static mut SERIAL_PRINT: Option<extern "C" fn(*const c_char)> = None;

struct Print;

impl Write for Print {
    fn write_str(&mut self, s: &str) -> fmt::Result {
        if let Some(serial_print) = unsafe { SERIAL_PRINT } {
            let mut buffer = [0u8; 1024];
            buffer[..s.len()].copy_from_slice(s.as_bytes());
            buffer[s.len()] = 0;
            let c_str = CStr::from_bytes_until_nul(&buffer).unwrap();
            serial_print(c_str.as_ptr());
        }
        Ok(())
    }
}

#[inline]
pub fn _print(args: fmt::Arguments) {
    Print.write_fmt(format_args!("{}", args)).unwrap();
}

#[macro_export]
macro_rules! print {
    ($($arg:tt)*) => (
        $crate::_print(format_args!($($arg)*))
    )
}

#[macro_export]
macro_rules! println {
    () => ($crate::print!("\n"));
    ($($arg:tt)*) => ($crate::print!("{}\n", format_args!($($arg)*)))
}

#[unsafe(no_mangle)]
// #[linkage = "weak"]
extern "C" fn fmaxf(x: f32, y: f32) -> f32 {
    (if x.is_nan() || x < y { y } else { x }) * 1.0
}

#[unsafe(no_mangle)]
// #[linkage = "weak"]
extern "C" fn fminf(x: f32, y: f32) -> f32 {
    (if y.is_nan() || x < y { x } else { y }) * 1.0
}

static mut TERMINAL: Option<Terminal<Display>> = None;

pub struct Display {
    width: usize,
    height: usize,
    buffer: &'static mut [u32],
}

#[repr(C)]
pub struct TerminalDisplay {
    width: usize,
    height: usize,
    address: *mut u32,
}

impl From<&TerminalDisplay> for Display {
    fn from(info: &TerminalDisplay) -> Self {
        Self {
            width: info.width,
            height: info.height,
            buffer: unsafe { from_raw_parts_mut(info.address, info.width * info.height) },
        }
    }
}

impl DrawTarget for Display {
    fn size(&self) -> (usize, usize) {
        (self.width, self.height)
    }

    #[inline(always)]
    fn draw_pixel(&mut self, x: usize, y: usize, color: Rgb) {
        let value = (color.0 as u32) << 16 | (color.1 as u32) << 8 | color.2 as u32;
        self.buffer[y * self.width + x] = value;
    }
}

#[repr(C)]
pub struct TerminalClipboard {
    get: extern "C" fn() -> *const c_char,
    set: extern "C" fn(*const c_char),
}

impl ClipboardHandler for TerminalClipboard {
    fn get_text(&mut self) -> Option<String> {
        let s = unsafe { CStr::from_ptr((self.get)()) };
        Some(s.to_string_lossy().into_owned())
    }

    fn set_text(&mut self, text: String) {
        let c_str = CStr::from_bytes_with_nul(text.as_bytes());
        (self.set)(c_str.unwrap().as_ptr());
    }
}

#[repr(C)]
pub struct TerminalPalette {
    foreground: u32,
    background: u32,
    ansi_colors: [u32; 16],
}

impl From<&TerminalPalette> for Palette {
    fn from(palette: &TerminalPalette) -> Self {
        let u32_to_rgb = |u32: u32| ((u32 >> 16) as u8, (u32 >> 8) as u8, u32 as u8);

        Self {
            foreground: u32_to_rgb(palette.foreground),
            background: u32_to_rgb(palette.background),
            ansi_colors: palette.ansi_colors.map(u32_to_rgb),
        }
    }
}

#[repr(C)]
pub enum TerminalMouseInput {
    Moved(i16, i16),
    Scroll(f32),
}

#[repr(C)]
pub enum TerminalInitResult {
    Success,
    MallocIsNull,
    FreeIsNull,
    FontBufferIsNull,
}

#[unsafe(no_mangle)]
#[cfg(feature = "embedded-font")]
pub extern "C" fn terminal_init(
    display: *const TerminalDisplay,
    font_size: f32,
    malloc: extern "C" fn(usize) -> *mut c_void,
    free: extern "C" fn(*mut c_void),
    serial_print: extern "C" fn(*const c_char),
) -> TerminalInitResult {
    let font_buffer = include_bytes!(env!("FONT_PATH"));
    terminal_init_internal(
        display,
        font_buffer.as_ptr(),
        font_buffer.len(),
        font_size,
        malloc,
        free,
        serial_print,
    )
}

#[unsafe(no_mangle)]
#[cfg(not(feature = "embedded-font"))]
pub extern "C" fn terminal_init(
    display: *const TerminalDisplay,
    font_buffer: *const u8,
    font_buffer_size: usize,
    font_size: f32,
    malloc: extern "C" fn(usize) -> *mut c_void,
    free: extern "C" fn(*mut c_void),
    serial_print: extern "C" fn(*const c_char),
) -> TerminalInitResult {
    terminal_init_internal(
        display,
        font_buffer,
        font_buffer_size,
        font_size,
        malloc,
        free,
        serial_print,
    )
}

fn terminal_init_internal(
    display: *const TerminalDisplay,
    font_buffer: *const u8,
    font_buffer_size: usize,
    font_size: f32,
    malloc: extern "C" fn(usize) -> *mut c_void,
    free: extern "C" fn(*mut c_void),
    serial_print: extern "C" fn(*const c_char),
) -> TerminalInitResult {
    match (malloc as usize, free as usize, font_buffer.is_null()) {
        (0, _, _) => return TerminalInitResult::MallocIsNull,
        (_, 0, _) => return TerminalInitResult::FreeIsNull,
        (_, _, true) => return TerminalInitResult::FontBufferIsNull,
        _ => {}
    }

    unsafe {
        MALLOC = Some(malloc);
        FREE = Some(free);
        SERIAL_PRINT = Some(serial_print);

        let mut terminal: Terminal<Display> = Terminal::new((&*display).into());

        let font_buffer = core::slice::from_raw_parts(font_buffer, font_buffer_size);
        terminal.set_font_manager(Box::new(TrueTypeFont::new(font_size, font_buffer)));

        if serial_print as usize != 0 {
            println!("Terminal: serial print is set!");
            terminal.set_logger(|args| println!("Terminal: {:?}", args));
        }
        TERMINAL.replace(terminal);
    }

    TerminalInitResult::Success
}

#[unsafe(no_mangle)]
pub extern "C" fn terminal_destroy() {
    unsafe { TERMINAL.take() };
}

#[unsafe(no_mangle)]
pub extern "C" fn terminal_flush() {
    if let Some(terminal) = unsafe { TERMINAL.as_mut() } {
        terminal.flush();
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn terminal_process(s: *const c_char) {
    if let Ok(s) = unsafe { CStr::from_ptr(s).to_str() } {
        if let Some(terminal) = unsafe { TERMINAL.as_mut() } {
            terminal.process(s.as_bytes());
        }
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn terminal_process_char(c: c_char) {
    if let Some(terminal) = unsafe { TERMINAL.as_mut() } {
        terminal.process(&[c as u8]);
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn terminal_set_history_size(size: usize) {
    if let Some(terminal) = unsafe { TERMINAL.as_mut() } {
        terminal.set_history_size(size);
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn terminal_set_scroll_speed(speed: usize) {
    if let Some(terminal) = unsafe { TERMINAL.as_mut() } {
        terminal.set_scroll_speed(speed);
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn terminal_set_auto_flush(auto_flush: bool) {
    if let Some(terminal) = unsafe { TERMINAL.as_mut() } {
        terminal.set_auto_flush(auto_flush);
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn terminal_set_auto_crnl(auto_crnl: bool) {
    if let Some(terminal) = unsafe { TERMINAL.as_mut() } {
        terminal.set_auto_crnl(auto_crnl);
    }
}

#[unsafe(no_mangle)]
#[allow(improper_ctypes_definitions)]
pub extern "C" fn terminal_set_bell_handler(handler: fn()) {
    if let Some(terminal) = unsafe { TERMINAL.as_mut() } {
        terminal.set_bell_handler(handler);
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn terminal_set_color_scheme(palette_index: usize) {
    if let Some(terminal) = unsafe { TERMINAL.as_mut() } {
        terminal.set_color_scheme(palette_index);
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn terminal_set_custom_color_scheme(palette: *const TerminalPalette) {
    if let Some(terminal) = unsafe { TERMINAL.as_mut() } {
        let palette = unsafe { &*palette };
        terminal.set_custom_color_scheme(&palette.into());
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn terminal_set_clipboard(clipboard: TerminalClipboard) {
    if let Some(terminal) = unsafe { TERMINAL.as_mut() } {
        terminal.set_clipboard(Box::new(clipboard));
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn terminal_set_pty_writer(writer: extern "C" fn(*const u8)) {
    if let Some(terminal) = unsafe { TERMINAL.as_mut() } {
        terminal.set_pty_writer(Box::new(move |s| writer(s.as_ptr())));
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn terminal_handle_keyboard(scancode: u8) {
    if let Some(terminal) = unsafe { TERMINAL.as_mut() } {
        terminal.handle_keyboard(scancode);
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn terminal_handle_mouse_scroll(delta: isize) {
    if let Some(terminal) = unsafe { TERMINAL.as_mut() } {
        terminal.handle_mouse(MouseInput::Scroll(delta));
    }
}
