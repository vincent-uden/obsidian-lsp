# Agent Guidelines for Obsidian LSP

## Build/Test Commands
- Build: `cargo build` or `cargo build --release`
- Run: `cargo run`
- Test all: `cargo test`
- Test single: `cargo test test_name`
- Check: `cargo check`
- Lint: `cargo clippy`
- Format: `cargo fmt`

## Code Style
- **Language**: Rust (edition 2024)
- **Imports**: Use `use` statements, group std first, then external crates, then local modules
- **Naming**: snake_case for functions/variables, PascalCase for types/structs, SCREAMING_SNAKE_CASE for constants
- **Types**: Prefer explicit types, use lifetimes when needed (e.g., `Document<'a>`)
- **Error Handling**: Use `Result<T>` for fallible operations, `unwrap()` sparingly
- **Async**: Use tokio runtime, prefer async/await over futures directly
- **Comments**: Use `//` for single line, `///` for doc comments, keep TODOs inline

## Dependencies
- Core: tokio, tower-lsp-server, dashmap, tracing
- Testing: Use `#[cfg(test)]` modules with `#[test]` functions
- No additional deps without justification

## Architecture Notes
- LSP server for Obsidian markdown files
- Document parsing via AST module
- Thread-safe document storage with DashMap