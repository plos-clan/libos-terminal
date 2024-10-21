# libos-terminal

C binding of `os-terminal` crate for x86 or x86_64 OS that not written in Rust!

## Usage

Download the header file and your prefered version of lib from [releases](https://github.com/plos-clan/libos-terminal/releases/tag/release).

Link the library to your project.

Remember to set `TERMINAL_EMBEDDED_FONT` to your compiler based on the version of the lib you are using.

## Build

Build directly to get the two target files:

```bash
cargo build --release
```

The production build will be in `target/release/<target>/` directory.

And use `cbindgen` to generate the header file:

```bash
cargo install cbindgen
cbindgen --output os_terminal.h
```
