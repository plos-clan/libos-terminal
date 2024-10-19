#![no_std]
#![no_main]
#![feature(linkage)]
#![feature(alloc_error_handler)]

extern crate alloc;

use alloc::boxed::Box;
use core::alloc::{GlobalAlloc, Layout};
use core::ffi::{c_char, c_uchar, c_uint, c_void};
use core::fmt;
use core::fmt::Write;
use core::panic::PanicInfo;
use core::slice::from_raw_parts_mut;
use os_terminal::font::TrueTypeFont;
use os_terminal::{DrawTarget, Rgb888, Terminal};
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
        crate::_print(format_args!($($arg)*))
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

#[no_mangle]
#[allow(static_mut_refs)]
pub unsafe extern "C" fn terminal_init(
    width: c_uint,
    height: c_uint,
    screen: *mut u32,
    malloc: Option<extern "C" fn(usize) -> *mut c_void>,
    free: Option<extern "C" fn(*mut c_void)>,
    serial_print: Option<extern "C" fn(*const c_char)>,
) -> bool {
    let width = width as usize;
    let height = height as usize;
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

    let font_buffer = include_bytes!("../fonts/SourceHanMonoSC-Min3500.otf");
    terminal.set_font_manager(Box::new(TrueTypeFont::new(10.0, font_buffer)));

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
pub extern "C" fn terminal_set_auto_flush(auto_flush: c_uint) {
    if let Some(terminal) = TERMINAL.lock().as_mut() {
        terminal.set_auto_flush(auto_flush != 0);
    }
}

#[no_mangle]
pub extern "C" fn terminal_advance_state(s: *const c_char) {
    let s = unsafe { core::ffi::CStr::from_ptr(s) };
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
pub extern "C" fn terminal_handle_keyboard(scancode: c_uchar) -> *const c_char {
    if let Some(terminal) = TERMINAL.lock().as_mut() {
        if let Some(s) = terminal.handle_keyboard(scancode) {
            return s.as_ptr() as *const c_char;
        }
    }
    core::ptr::null()
}
