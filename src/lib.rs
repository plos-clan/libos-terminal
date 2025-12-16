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
use core::panic::PanicInfo;
use core::slice;
use os_terminal::font::TrueTypeFont;
use os_terminal::{ClipboardHandler, DrawTarget};
use os_terminal::{MouseInput, Palette, Rgb, Terminal};

#[panic_handler]
unsafe fn panic(_info: &PanicInfo) -> ! {
    loop {}
}

#[global_allocator]
static ALLOCATOR: Allocator = Allocator;

#[alloc_error_handler]
fn alloc_error_handler(_layout: Layout) -> ! {
    panic!();
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

type Shifts = (u8, u8, u8);

static mut TERMINAL: Option<Terminal<Display>> = None;

pub struct Display {
    width: usize,
    height: usize,
    stride: usize,
    buffer: *mut u32,
    shifts: Shifts,
    convert_color: fn(Shifts, Rgb) -> u32,
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

impl Display {
    fn from(info: &TerminalDisplay) -> Self {
        let shifts = (
            info.red_mask_shift + info.red_mask_size - 8,
            info.green_mask_shift + info.green_mask_size - 8,
            info.blue_mask_shift + info.blue_mask_size - 8,
        );

        let convert_color = |shifts: Shifts, color: Rgb| {
            ((color.0 as u32) << shifts.0)
                | ((color.1 as u32) << shifts.1)
                | ((color.2 as u32) << shifts.2)
        };

        Self {
            shifts,
            convert_color,
            width: info.width,
            height: info.height,
            buffer: info.buffer,
            stride: info.pitch / size_of::<u32>(),
        }
    }
}

impl DrawTarget for Display {
    fn size(&self) -> (usize, usize) {
        (self.width, self.height)
    }

    #[inline(always)]
    fn draw_pixel(&mut self, x: usize, y: usize, color: Rgb) {
        let color = (self.convert_color)(self.shifts, color);
        unsafe { self.buffer.add(y * self.stride + x).write(color) }
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
) -> TerminalInitResult {
    let font_buffer = include_bytes!(env!("FONT_PATH"));
    terminal_init_internal(
        display,
        font_buffer.as_ptr(),
        font_buffer.len(),
        font_size,
        malloc,
        free,
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
) -> TerminalInitResult {
    terminal_init_internal(
        display,
        font_buffer,
        font_buffer_size,
        font_size,
        malloc,
        free,
    )
}

fn terminal_init_internal(
    display: *const TerminalDisplay,
    font_buffer: *const u8,
    font_buffer_size: usize,
    font_size: f32,
    malloc: extern "C" fn(usize) -> *mut c_void,
    free: extern "C" fn(*mut c_void),
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

        let mut terminal = Terminal::new(Display::from(&*display));

        let font_buffer = slice::from_raw_parts(font_buffer, font_buffer_size);
        let truetype_font = Box::new(TrueTypeFont::new(font_size, font_buffer));
        terminal.set_font_manager(truetype_font);

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
    if let Ok(s) = unsafe { CStr::from_ptr(s).to_str() }
        && let Some(terminal) = unsafe { TERMINAL.as_mut() }
    {
        terminal.process(s.as_bytes());
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn terminal_process_byte(c: u8) {
    if let Some(terminal) = unsafe { TERMINAL.as_mut() } {
        terminal.process(&[c]);
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

#[unsafe(no_mangle)]
pub extern "C" fn terminal_set_history_size(size: usize) {
    if let Some(terminal) = unsafe { TERMINAL.as_mut() } {
        terminal.set_history_size(size);
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn terminal_set_color_cache_size(size: usize) {
    if let Some(terminal) = unsafe { TERMINAL.as_mut() } {
        terminal.set_color_cache_size(size);
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
pub extern "C" fn terminal_set_crnl_mapping(auto_crnl: bool) {
    if let Some(terminal) = unsafe { TERMINAL.as_mut() } {
        terminal.set_crnl_mapping(auto_crnl);
    }
}

#[unsafe(no_mangle)]
#[allow(improper_ctypes_definitions)]
pub extern "C" fn terminal_set_bell_handler(handler: fn()) {
    if let Some(terminal) = unsafe { TERMINAL.as_mut() } {
        terminal.set_bell_handler(handler as fn());
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
pub extern "C" fn terminal_set_pty_writer(writer: extern "C" fn(*const u8, usize)) {
    if let Some(terminal) = unsafe { TERMINAL.as_mut() } {
        let callback = move |s: &str| {
            writer(s.as_ptr(), s.len());
        };
        terminal.set_pty_writer(Box::new(callback));
    }
}
