use dashmap::DashMap;
use std::{
    path::{Path, PathBuf},
    time::Instant,
};
use tokio::fs;
use tracing::{debug, warn};
use walkdir::WalkDir;

#[derive(Debug)]
pub enum FileKind {
    Markdown,
    Attachment,
}

#[derive(Debug)]
pub struct ObsidianFile {
    pub name: String,
    pub path: PathBuf,
    pub stem: String,
    pub aliases: Vec<String>,
    pub checked_at: Instant,
    pub kind: FileKind,
}

#[derive(Debug)]
pub struct VaultIndex {
    pub files: Vec<ObsidianFile>,
    pub link_map: DashMap<String, PathBuf>,
}

impl VaultIndex {
    pub fn new() -> Self {
        Self {
            files: Vec::new(),
            link_map: DashMap::new(),
        }
    }

    pub fn find_file(&self, link_text: &str) -> Option<PathBuf> {
        let normalized = normalize_link_text(link_text);
        self.link_map.get(&normalized).map(|entry| entry.clone())
    }
}

fn normalize_link_text(text: &str) -> String {
    text.trim()
        .to_lowercase()
        .replace(' ', "-")
        .replace('_', "-")
}

fn classify_file(path: &Path) -> Option<FileKind> {
    match path.extension()?.to_str()? {
        "md" => Some(FileKind::Markdown),
        "png" | "jpg" | "jpeg" | "gif" | "pdf" | "mp4" | "webp" | "svg" => {
            Some(FileKind::Attachment)
        }
        _ => None,
    }
}

fn extract_file_stem(path: &Path) -> String {
    path.file_stem()
        .and_then(|s| s.to_str())
        .unwrap_or("")
        .to_string()
}

async fn extract_aliases(path: &Path) -> Vec<String> {
    if let Ok(content) = fs::read_to_string(path).await {
        if content.starts_with("---") {
            if let Some(end) = content[3..].find("---") {
                let frontmatter = &content[3..end + 3];
                return parse_aliases_from_frontmatter(frontmatter);
            }
        }
    }
    Vec::new()
}

fn parse_aliases_from_frontmatter(frontmatter: &str) -> Vec<String> {
    let mut aliases = Vec::new();
    let mut in_aliases = false;
    
    for line in frontmatter.lines() {
        let line = line.trim();
        if line.starts_with("aliases:") || line.starts_with("alias:") {
            in_aliases = true;
            if let Some(inline_value) = line.split(':').nth(1) {
                let inline_value = inline_value.trim();
                if !inline_value.is_empty() {
                    if inline_value.starts_with('[') && inline_value.ends_with(']') {
                        let list_content = &inline_value[1..inline_value.len()-1];
                        for alias in list_content.split(',') {
                            let clean_alias = alias.trim().trim_matches('"').trim_matches('\'');
                            if !clean_alias.is_empty() {
                                aliases.push(clean_alias.to_string());
                            }
                        }
                    } else {
                        aliases.push(inline_value.trim_matches('"').trim_matches('\'').to_string());
                    }
                }
            }
        } else if in_aliases && line.starts_with("- ") {
            let alias = line[2..].trim().trim_matches('"').trim_matches('\'');
            if !alias.is_empty() {
                aliases.push(alias.to_string());
            }
        } else if in_aliases && !line.starts_with("- ") && !line.is_empty() {
            in_aliases = false;
        }
    }
    
    aliases
}

pub async fn index_vault(root: &Path) -> Result<VaultIndex, Box<dyn std::error::Error>> {
    let mut index = VaultIndex::new();
    let start = Instant::now();
    
    debug!("Starting vault indexing at: {}", root.display());
    
    for entry in WalkDir::new(root)
        .follow_links(false)
        .into_iter()
        .filter_entry(|e| {
            // Don't filter out the root directory itself
            if e.path() == root {
                return true;
            }
            let name = e.file_name().to_string_lossy();
            !name.starts_with('.') && name != "node_modules"
        })
    {
        let entry = match entry {
            Ok(entry) => entry,
            Err(e) => {
                warn!("Skipping inaccessible path: {}", e);
                continue;
            }
        };
        
        if !entry.file_type().is_file() {
            continue;
        }
        
        let path = entry.path();
        let kind = match classify_file(path) {
            Some(kind) => kind,
            None => continue,
        };
        
        let name = path
            .file_name()
            .and_then(|n| n.to_str())
            .unwrap_or("")
            .to_string();
        
        let stem = extract_file_stem(path);
        let aliases = if matches!(kind, FileKind::Markdown) {
            extract_aliases(path).await
        } else {
            Vec::new()
        };
        
        let obsidian_file = ObsidianFile {
            name: name.clone(),
            path: path.to_path_buf(),
            stem: stem.clone(),
            aliases: aliases.clone(),
            checked_at: Instant::now(),
            kind,
        };
        
        let normalized_stem = normalize_link_text(&stem);
        index.link_map.insert(normalized_stem.clone(), path.to_path_buf());
        index.link_map.insert(normalize_link_text(&name), path.to_path_buf());
        
        for alias in &aliases {
            index.link_map.insert(normalize_link_text(alias), path.to_path_buf());
        }
        
        index.files.push(obsidian_file);
    }
    
    let duration = start.elapsed();
    debug!(
        "Indexed {} files in {:?}",
        index.files.len(),
        duration
    );
    
    Ok(index)
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;
    use tempfile::TempDir;

    async fn create_test_vault() -> TempDir {
        let temp_dir = TempDir::new().unwrap();
        let root = temp_dir.path();

        fs::write(root.join("Note One.md"), "# Note One\nSome content").unwrap();
        fs::write(root.join("note-two.md"), "# Note Two\nMore content").unwrap();
        fs::write(
            root.join("with-alias.md"),
            "---\naliases: [\"Alternative Name\", \"Alt\"]\n---\n# With Alias"
        ).unwrap();
        fs::write(root.join("image.png"), &[0u8; 100]).unwrap();
        fs::write(root.join("ignored.txt"), "Should be ignored").unwrap();

        let subdir = root.join("subfolder");
        fs::create_dir(&subdir).unwrap();
        fs::write(subdir.join("nested.md"), "# Nested Note").unwrap();

        temp_dir
    }

    #[tokio::test]
    async fn test_index_vault_basic() {
        let temp_dir = create_test_vault().await;
        let index = index_vault(temp_dir.path()).await.unwrap();

        assert_eq!(index.files.len(), 5); // 4 markdown + 1 image

        let markdown_files: Vec<_> = index.files.iter()
            .filter(|f| matches!(f.kind, FileKind::Markdown))
            .collect();
        assert_eq!(markdown_files.len(), 4);

        let attachment_files: Vec<_> = index.files.iter()
            .filter(|f| matches!(f.kind, FileKind::Attachment))
            .collect();
        assert_eq!(attachment_files.len(), 1);
    }

    #[tokio::test]
    async fn test_link_resolution() {
        let temp_dir = create_test_vault().await;
        let index = index_vault(temp_dir.path()).await.unwrap();

        assert!(index.find_file("Note One").is_some());
        assert!(index.find_file("note one").is_some()); // case insensitive
        assert!(index.find_file("note-one").is_some()); // dash variant
        assert!(index.find_file("note_one").is_some()); // underscore variant

        assert!(index.find_file("note-two").is_some());
        assert!(index.find_file("Note Two").is_some());

        assert!(index.find_file("nested").is_some());

        assert!(index.find_file("nonexistent").is_none());
    }

    #[tokio::test]
    async fn test_alias_resolution() {
        let temp_dir = create_test_vault().await;
        let index = index_vault(temp_dir.path()).await.unwrap();

        let with_alias_file = index.files.iter()
            .find(|f| f.stem == "with-alias")
            .unwrap();
        assert_eq!(with_alias_file.aliases.len(), 2);
        assert!(with_alias_file.aliases.contains(&"Alternative Name".to_string()));
        assert!(with_alias_file.aliases.contains(&"Alt".to_string()));

        assert!(index.find_file("Alternative Name").is_some());
        assert!(index.find_file("alternative-name").is_some()); // normalized
        assert!(index.find_file("Alt").is_some());
        assert!(index.find_file("alt").is_some()); // case insensitive
    }

    #[test]
    fn test_normalize_link_text() {
        assert_eq!(normalize_link_text("My Note"), "my-note");
        assert_eq!(normalize_link_text("my_note"), "my-note");
        assert_eq!(normalize_link_text("  My Note  "), "my-note");
        assert_eq!(normalize_link_text("Already-normalized"), "already-normalized");
    }

    #[test]
    fn test_parse_aliases_from_frontmatter() {
        let frontmatter = r#"
aliases: ["Alternative Name", "Alt"]
other: value
"#;
        let aliases = parse_aliases_from_frontmatter(frontmatter);
        assert_eq!(aliases.len(), 2);
        assert!(aliases.contains(&"Alternative Name".to_string()));
        assert!(aliases.contains(&"Alt".to_string()));

        let frontmatter_list = r#"
aliases:
  - "First Alias"
  - Second Alias
"#;
        let aliases = parse_aliases_from_frontmatter(frontmatter_list);
        assert_eq!(aliases.len(), 2);
        assert!(aliases.contains(&"First Alias".to_string()));
        assert!(aliases.contains(&"Second Alias".to_string()));
    }
}
