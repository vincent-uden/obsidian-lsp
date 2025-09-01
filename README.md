# Obsidian LSP

A Language Server Protocol (LSP) implementation for Obsidian markdown files, providing features like go-to-definition, references, completion, and more for enhanced editing experience in editors like Neovim.

## Installation

### Via Cargo (crates.io)
```bash
cargo install obsidian-lsp
```

### From Source
```bash
git clone https://github.com/vincent-uden/obsidian-lsp
cd obsidian-lsp
cargo build --release
```

## Features

## Roadmap
- [x] Rename links -> rename files and all other links
- [x] Go to definition (or declaration)
    - [x] Link -> Navigate to the file
- [x] References
    - [x] Link -> Show all other links to the same file
    - [x] Tag -> Show all other places where the tag is used
- [x] Completion
    - [x] Links
    - [x] Tags
    - [x] Properties
- [x] Document symbols
- [x] Hover
- [ ] Code actions?
    - [x] Align table
    - [ ] More?
- [ ] New daily notes
- [ ] Create note from template

## Development setup (neovim 0.11+)
Create a file for the lsp configuration at `nvim/lsp/obsidian.lua`. It should contain:
```lua
return {
  cmd = { 'path/to/repo/obsidian-lsp/target/debug/obsidian-lsp' },
  filetypes = { 'markdown', 'md' },
  root_markers = { '.obsidian-marker' },
}
```
Modify the path with backslahes on Windows.

Then, in `init.lua` add
```lua
vim.lsp.config('obsidian', {})
```
and optionally a keybinding to restart the lsp
```lua
vim.keymap.set('n', '<leader>m', function()
  vim.lsp.enable('obsidian', false)
  vim.lsp.enable('obsidian', true)
end, { desc = 'Restart obsidian lsp' })
```
