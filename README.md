# libos-terminal

C binding of `os-terminal` crate for x86 or x86_64 OS that not written in Rust!

## Usage

Download the header file and your prefered version of lib from [releases](https://github.com/plos-clan/libos-terminal/releases/tag/release).

Link the library to your project.

Remember to define `TERMINAL_EMBEDDED_FONT` when using either embedded-font library.

## Build

The default build enables `swash` and `woff2` and accepts a font buffer from the caller:

```bash
cargo build --release
```

The production libraries will be in the `target/<target>/release/` directories.

And use `cbindgen` to generate the header file:

```bash
cargo install cbindgen
cbindgen --output os_terminal.h
```
