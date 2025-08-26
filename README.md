# Obsidian LSP

## Roadmap
- [ ] Rename links -> rename files and all other links
- [ ] Go to definition (or declaration)
    - [ ] Link -> Navigate to the file
- [ ] References
    - [ ] Link -> Show all other links to the same file
- [ ] Completion
    - [ ] Links
    - [ ] Tags
    - [ ] Properties
- [x] Document symbols
- [ ] Hover?
- [ ] Code actions?
    - [ ] Align table

## Dev setup (neovim 0.11+)
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
