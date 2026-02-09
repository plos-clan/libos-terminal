#![no_std]
#![no_main]
#![feature(linkage)]
#![feature(alloc_error_handler)]
#![allow(static_mut_refs)]
#![allow(unsafe_op_in_unsafe_fn)]
#![allow(clippy::not_unsafe_ptr_arg_deref)]

extern crate alloc;

use alloc::boxed::Box;
use alloc::ffi::CString;
use alloc::string::String;
use core::alloc::{GlobalAlloc, Layout};
use core::ffi::{CStr, c_char, c_void};
use core::panic::PanicInfo;
use os_terminal::font::TrueTypeFont;
use os_terminal::{ClipboardHandler, DrawTarget};
use os_terminal::{MouseInput, Palette, Rgb, Terminal};

#[global_allocator]
static ALLOCATOR: Allocator = Allocator;

static mut MALLOC: extern "C" fn(usize) -> *mut c_void = null_malloc;
static mut FREE: extern "C" fn(*mut c_void) = null_free;

extern "C" fn null_malloc(_: usize) -> *mut c_void {
    core::ptr::null_mut()
}
extern "C" fn null_free(_: *mut c_void) {}

struct Allocator;

unsafe impl GlobalAlloc for Allocator {
    #[inline]
    unsafe fn alloc(&self, layout: Layout) -> *mut u8 {
        (MALLOC)(layout.size()) as *mut u8
    }

    #[inline]
    unsafe fn dealloc(&self, ptr: *mut u8, _layout: Layout) {
        (FREE)(ptr as *mut c_void);
    }
}

#[panic_handler]
unsafe fn panic(_info: &PanicInfo) -> ! {
    loop {}
}

#[alloc_error_handler]
fn alloc_error_handler(_layout: Layout) -> ! {
    panic!();
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

pub struct Display {
    width: usize,
    height: usize,
    stride: usize,
    buffer: *mut u32,
    r_shift: u8,
    g_shift: u8,
    b_shift: u8,
}

#[repr(C)]
pub struct TerminalDisplay {
    width: usize,
    height: usize,
    buffer: *mut u32,
    pitch: usize,
    red_mask_size: u8,
    red_mask_shift: u8,
    green_mask_size: u8,
    green_mask_shift: u8,
    blue_mask_size: u8,
    blue_mask_shift: u8,
}

impl DrawTarget for Display {
    #[inline]
    fn size(&self) -> (usize, usize) {
        (self.width, self.height)
    }

    #[inline(always)]
    fn draw_pixel(&mut self, x: usize, y: usize, color: Rgb) {
        let pixel = ((color.0 as u32) << self.r_shift)
            | ((color.1 as u32) << self.g_shift)
            | ((color.2 as u32) << self.b_shift);

        unsafe { *self.buffer.add(y * self.stride + x) = pixel };
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
        const fn u32_to_rgb(val: u32) -> Rgb {
            ((val >> 16) as u8, (val >> 8) as u8, val as u8)
        }

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

unsafe fn terminal_new_impl(
    display_info: *const TerminalDisplay,
    font_bytes: &'static [u8],
    font_size: f32,
    malloc: extern "C" fn(usize) -> *mut c_void,
    free: extern "C" fn(*mut c_void),
) -> *mut c_void {
    if malloc as usize == 0 || free as usize == 0 || display_info.is_null() {
        return core::ptr::null_mut();
    }

    MALLOC = malloc;
    FREE = free;

    let info = &*display_info;
    let display = Display {
        width: info.width,
        height: info.height,
        buffer: info.buffer,
        stride: info.pitch / size_of::<u32>(),
        r_shift: info.red_mask_shift + info.red_mask_size.saturating_sub(8),
        g_shift: info.green_mask_shift + info.green_mask_size.saturating_sub(8),
        b_shift: info.blue_mask_shift + info.blue_mask_size.saturating_sub(8),
    };

    let mut terminal = Terminal::new(display);
    let font = Box::new(TrueTypeFont::new(font_size, font_bytes));
    terminal.set_font_manager(font);

    Box::into_raw(Box::new(terminal)) as *mut c_void
}

#[unsafe(no_mangle)]
#[cfg(feature = "embedded-font")]
pub unsafe extern "C" fn terminal_new(
    display: *const TerminalDisplay,
    font_size: f32,
    malloc: extern "C" fn(usize) -> *mut c_void,
    free: extern "C" fn(*mut c_void),
) -> *mut c_void {
    let font_slice = include_bytes!(env!("FONT_PATH"));
    terminal_new_impl(display, font_slice, font_size, malloc, free)
}

#[unsafe(no_mangle)]
#[cfg(not(feature = "embedded-font"))]
pub unsafe extern "C" fn terminal_new(
    display: *const TerminalDisplay,
    font_buffer: *const u8,
    font_buffer_size: usize,
    font_size: f32,
    malloc: extern "C" fn(usize) -> *mut c_void,
    free: extern "C" fn(*mut c_void),
) -> *mut c_void {
    if font_buffer.is_null() {
        return core::ptr::null_mut();
    }
    let font_slice = core::slice::from_raw_parts(font_buffer, font_buffer_size);
    terminal_new_impl(display, font_slice, font_size, malloc, free)
}

#[unsafe(no_mangle)]
pub extern "C" fn terminal_destroy(terminal: *mut c_void) {
    if terminal.is_null() {
        return;
    }
    unsafe {
        let _ = Box::from_raw(terminal as *mut Terminal<Display>);
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn terminal_rows(terminal: *mut c_void) -> usize {
    if terminal.is_null() {
        return 0;
    }
    unsafe {
        let terminal = Box::from_raw(terminal as *mut Terminal<Display>);
        terminal.rows()
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn terminal_columns(terminal: *mut c_void) -> usize {
    if terminal.is_null() {
        return 0;
    }
    unsafe {
        let terminal = Box::from_raw(terminal as *mut Terminal<Display>);
        terminal.columns()
    }
}

macro_rules! with_terminal {
    ($ptr:expr, $term:ident => $block:block) => {
        if !$ptr.is_null() {
            let $term = unsafe { &mut *($ptr as *mut Terminal<Display>) };
            $block
        }
    };
}

#[unsafe(no_mangle)]
pub extern "C" fn terminal_flush(terminal: *mut c_void) {
    with_terminal!(terminal, t => { t.flush(); });
}

#[unsafe(no_mangle)]
pub extern "C" fn terminal_process(terminal: *mut c_void, s: *const c_char) {
    with_terminal!(terminal, t => {
        if let Ok(s) = unsafe { CStr::from_ptr(s).to_str() } {
            t.process(s.as_bytes());
        }
    });
}

#[unsafe(no_mangle)]
pub extern "C" fn terminal_process_byte(terminal: *mut c_void, c: u8) {
    with_terminal!(terminal, t => { t.process(&[c]); });
}

#[unsafe(no_mangle)]
pub extern "C" fn terminal_handle_keyboard(terminal: *mut c_void, scancode: u8) {
    with_terminal!(terminal, t => { t.handle_keyboard(scancode); });
}

#[unsafe(no_mangle)]
pub extern "C" fn terminal_handle_mouse_scroll(terminal: *mut c_void, delta: isize) {
    with_terminal!(terminal, t => { t.handle_mouse(MouseInput::Scroll(delta)); });
}

#[unsafe(no_mangle)]
pub extern "C" fn terminal_set_history_size(terminal: *mut c_void, size: usize) {
    with_terminal!(terminal, t => { t.set_history_size(size); });
}

#[unsafe(no_mangle)]
pub extern "C" fn terminal_set_color_cache_size(terminal: *mut c_void, size: usize) {
    with_terminal!(terminal, t => { t.set_color_cache_size(size); });
}

#[unsafe(no_mangle)]
pub extern "C" fn terminal_set_scroll_speed(terminal: *mut c_void, speed: usize) {
    with_terminal!(terminal, t => { t.set_scroll_speed(speed); });
}

#[unsafe(no_mangle)]
pub extern "C" fn terminal_set_auto_flush(terminal: *mut c_void, auto_flush: bool) {
    with_terminal!(terminal, t => { t.set_auto_flush(auto_flush); });
}

#[unsafe(no_mangle)]
pub extern "C" fn terminal_set_crnl_mapping(terminal: *mut c_void, auto_crnl: bool) {
    with_terminal!(terminal, t => { t.set_crnl_mapping(auto_crnl); });
}

#[unsafe(no_mangle)]
pub extern "C" fn terminal_set_custom_color_scheme(
    terminal: *mut c_void,
    palette: *const TerminalPalette,
) {
    with_terminal!(terminal, t => {
        let palette = unsafe { &*palette };
        t.set_custom_color_scheme(&palette.into());
    });
}

#[unsafe(no_mangle)]
pub extern "C" fn terminal_set_pty_writer(
    terminal: *mut c_void,
    writer: extern "C" fn(*const u8, usize),
) {
    with_terminal!(terminal, t => {
        let callback = move |s: &str| {
            writer(s.as_ptr(), s.len());
        };
        t.set_pty_writer(Box::new(callback));
    });
}

#[unsafe(no_mangle)]
pub extern "C" fn terminal_set_clipboard(
    terminal: *mut c_void,
    get_fn: extern "C" fn() -> *const c_char,
    set_fn: extern "C" fn(*const c_char),
) {
    struct Clipboard(
        extern "C" fn() -> *const c_char,
        extern "C" fn(*const c_char),
    );

    impl ClipboardHandler for Clipboard {
        fn get_text(&mut self) -> Option<String> {
            let ptr = (self.0)();
            if ptr.is_null() {
                return None;
            }
            let s = unsafe { CStr::from_ptr(ptr) };
            Some(s.to_string_lossy().into_owned())
        }
        fn set_text(&mut self, text: String) {
            if let Ok(c) = CString::new(text) {
                (self.1)(c.as_ptr())
            }
        }
    }

    with_terminal!(terminal, t => {
        t.set_clipboard(Box::new(Clipboard(get_fn, set_fn)));
    });
}

#[unsafe(no_mangle)]
pub extern "C" fn terminal_set_color_scheme(terminal: *mut c_void, palette_index: usize) {
    with_terminal!(terminal, t => { t.set_color_scheme(palette_index); });
}

#[unsafe(no_mangle)]
pub extern "C" fn terminal_set_bell_handler(terminal: *mut c_void, handler: extern "C" fn()) {
    with_terminal!(terminal, t => { t.set_bell_handler(Box::new(move || handler())); });
}
