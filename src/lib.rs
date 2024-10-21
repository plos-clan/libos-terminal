#![no_std]
#![no_main]
#![feature(linkage)]
#![feature(alloc_error_handler)]

extern crate alloc;

use alloc::boxed::Box;
use alloc::ffi::CString;
use core::alloc::{GlobalAlloc, Layout};
use core::ffi::{c_char, c_void, CStr};
use core::fmt;
use core::fmt::Write;
use core::panic::PanicInfo;
use core::slice::from_raw_parts_mut;
use os_terminal::font::TrueTypeFont;
use os_terminal::{DrawTarget, Palette, Rgb888, Terminal};
use spin::Mutex;

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

static mut MALLOC: Option<extern "C" fn(usize) -> *mut c_void> = None;
static mut FREE: Option<extern "C" fn(*mut c_void)> = None;
static mut SERIAL_PRINT: Option<extern "C" fn(*const c_char)> = None;

unsafe impl GlobalAlloc for Allocator {
    unsafe fn alloc(&self, layout: Layout) -> *mut u8 {
        MALLOC.unwrap()(layout.size()) as *mut u8
    }

    unsafe fn dealloc(&self, ptr: *mut u8, _layout: Layout) {
        FREE.unwrap()(ptr as *mut c_void);
    }
}

struct Print;

impl Write for Print {
    fn write_str(&mut self, s: &str) -> fmt::Result {
        if let Some(serial_print) = unsafe { SERIAL_PRINT } {
            serial_print(s.as_ptr() as *const c_char);
        }
        Ok(())
    }
}

#[inline]
pub fn _print(args: fmt::Arguments) {
    Print.write_fmt(format_args!("{}\0", args)).unwrap();
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

#[no_mangle]
// #[linkage = "weak"]
extern "C" fn fmaxf(x: f32, y: f32) -> f32 {
    (if x.is_nan() || x < y { y } else { x }) * 1.0
}

#[no_mangle]
// #[linkage = "weak"]
extern "C" fn fminf(x: f32, y: f32) -> f32 {
    (if y.is_nan() || x < y { x } else { y }) * 1.0
}

static TERMINAL: Mutex<Option<Terminal<Display>>> = Mutex::new(None);

struct Display {
    width: usize,
    height: usize,
    buffer: &'static mut [u32],
}

impl Display {
    fn new(width: usize, height: usize, buffer: &'static mut [u32]) -> Self {
        Display {
            width,
            height,
            buffer,
        }
    }
}

impl DrawTarget for Display {
    fn size(&self) -> (usize, usize) {
        (self.width, self.height)
    }

    #[inline(always)]
    fn draw_pixel(&mut self, x: usize, y: usize, color: Rgb888) {
        let value = (color.0 as u32) << 16 | (color.1 as u32) << 8 | color.2 as u32;
        self.buffer[y * self.width + x] = value;
    }
}

#[repr(C)]
pub struct TerminalPalette {
    foreground: u32,
    background: u32,
    ansi_colors: [u32; 16],
}

impl From<TerminalPalette> for Palette {
    fn from(palette: TerminalPalette) -> Self {
        let u32_to_rgb888 = |u32: u32| {
            ((u32 >> 16) as u8, (u32 >> 8) as u8, u32 as u8)
        };

        let mut ansi_colors = [Rgb888::default(); 16];
        for (i, &color) in palette.ansi_colors.iter().enumerate() {
            ansi_colors[i] = u32_to_rgb888(color);
        }

        Self {
            foreground: u32_to_rgb888(palette.foreground),
            background: u32_to_rgb888(palette.background),
            ansi_colors,
        }
    }
}

#[no_mangle]
#[allow(static_mut_refs)]
#[cfg(feature = "embedded-font")]
pub unsafe extern "C" fn terminal_init(
    width: usize,
    height: usize,
    screen: *mut u32,
    font_size: f32,
    malloc: Option<extern "C" fn(usize) -> *mut c_void>,
    free: Option<extern "C" fn(*mut c_void)>,
    serial_print: Option<extern "C" fn(*const c_char)>,
) -> bool {
    let buffer = from_raw_parts_mut(screen, width * height);

    // serial_print can be null
    if malloc.is_none() || free.is_none() {
        return false;
    }

    MALLOC = malloc;
    FREE = free;
    SERIAL_PRINT = serial_print;

    let display = Display::new(width, height, buffer);
    let mut terminal = Terminal::new(display);

    let font_buffer = include_bytes!("../fonts/SourceHanMonoSC-Min3500.ttf");
    terminal.set_font_manager(Box::new(TrueTypeFont::new(font_size, font_buffer)));

    if serial_print.is_some() {
        println!("Terminal: serial print is set!");
        terminal.set_logger(Some(|args| println!("Terminal: {:?}", args)));
    }

    TERMINAL.lock().replace(terminal);

    true
}

#[no_mangle]
#[allow(static_mut_refs)]
#[cfg(not(feature = "embedded-font"))]
pub unsafe extern "C" fn terminal_init(
    width: usize,
    height: usize,
    screen: *mut u32,
    font_buffer: *const u8,
    font_buffer_size: usize,
    font_size: f32,
    malloc: Option<extern "C" fn(usize) -> *mut c_void>,
    free: Option<extern "C" fn(*mut c_void)>,
    serial_print: Option<extern "C" fn(*const c_char)>,
) -> bool {
    let buffer = from_raw_parts_mut(screen, width * height);

    // serial_print can be null
    if malloc.is_none() || free.is_none() || font_buffer.is_null() {
        return false;
    }

    MALLOC = malloc;
    FREE = free;
    SERIAL_PRINT = serial_print;

    let display = Display::new(width, height, buffer);
    let mut terminal = Terminal::new(display);

    let font_buffer = core::slice::from_raw_parts(font_buffer, font_buffer_size);
    terminal.set_font_manager(Box::new(TrueTypeFont::new(font_size, font_buffer)));

    if serial_print.is_some() {
        println!("Terminal: serial print is set!");
        terminal.set_logger(Some(|args| println!("Terminal: {:?}", args)));
    }

    TERMINAL.lock().replace(terminal);

    true
}

#[no_mangle]
pub extern "C" fn terminal_destroy() {
    TERMINAL.lock().take();
}

#[no_mangle]
pub extern "C" fn terminal_flush() {
    if let Some(terminal) = TERMINAL.lock().as_mut() {
        terminal.flush();
    }
}

#[no_mangle]
pub extern "C" fn terminal_set_auto_flush(auto_flush: usize) {
    if let Some(terminal) = TERMINAL.lock().as_mut() {
        terminal.set_auto_flush(auto_flush != 0);
    }
}

#[no_mangle]
pub extern "C" fn terminal_advance_state(s: *const c_char) {
    let s = unsafe { CStr::from_ptr(s) };
    let s = s.to_str().unwrap();
    if let Some(terminal) = TERMINAL.lock().as_mut() {
        terminal.advance_state(s.as_bytes());
    }
}

#[no_mangle]
pub extern "C" fn terminal_advance_state_single(c: c_char) {
    if let Some(terminal) = TERMINAL.lock().as_mut() {
        terminal.advance_state(&[c as u8]);
    }
}

#[no_mangle]
pub extern "C" fn terminal_handle_keyboard(scancode: u8) -> *const c_char {
    if let Some(terminal) = TERMINAL.lock().as_mut() {
        if let Some(s) = terminal.handle_keyboard(scancode) {
            return CString::new(s).unwrap().into_raw();
        }
    }
    core::ptr::null()
}

#[no_mangle]
pub extern "C" fn terminal_set_color_scheme(palette_index: usize) {
    if let Some(terminal) = TERMINAL.lock().as_mut() {
        terminal.set_color_scheme(palette_index);
    }
}

#[no_mangle]
pub extern "C" fn terminal_set_custom_color_scheme(palette: TerminalPalette) {
    if let Some(terminal) = TERMINAL.lock().as_mut() {
        terminal.set_custom_color_scheme(palette.into());
    }
}
