use dashmap::DashMap;
use std::{
    collections::HashMap,
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
    pub tags: Vec<String>,
    pub properties: HashMap<String, String>,
    pub checked_at: Instant,
    pub kind: FileKind,
}

#[derive(Debug)]
pub struct VaultIndex {
    pub files: Vec<ObsidianFile>,
    pub link_map: DashMap<String, PathBuf>,
    pub reverse_link_map: DashMap<String, Vec<PathBuf>>,
    pub tag_map: DashMap<String, Vec<PathBuf>>,
    pub property_map: DashMap<String, Vec<PathBuf>>,
}

impl VaultIndex {
    pub fn new() -> Self {
        Self {
            files: Vec::new(),
            link_map: DashMap::new(),
            reverse_link_map: DashMap::new(),
            tag_map: DashMap::new(),
            property_map: DashMap::new(),
        }
    }

    pub fn find_file(&self, link_text: &str) -> Option<PathBuf> {
        let normalized = normalize_link_text(link_text);
        self.link_map.get(&normalized).map(|entry| entry.clone())
    }
}

pub fn normalize_link_text(text: &str) -> String {
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

async fn extract_metadata(path: &Path) -> (Vec<String>, Vec<String>, Vec<String>, HashMap<String, String>) {
    let mut aliases = Vec::new();
    let mut tags = Vec::new();
    let mut links = Vec::new();
    let mut properties = HashMap::new();

    if let Ok(content) = fs::read_to_string(path).await {
        if content.starts_with("---") {
            if let Some(end) = content[3..].find("---") {
                let frontmatter = &content[3..end + 3];
                let (fm_aliases, fm_tags, fm_properties) = parse_frontmatter(frontmatter);
                aliases = fm_aliases;
                tags.extend(fm_tags);
                properties = fm_properties;
            }
        }

        // Extract inline tags and links from content body
        let body = if content.starts_with("---") {
            if let Some(end) = content[3..].find("---") {
                &content[end + 6..] // Skip past closing ---
            } else {
                &content
            }
        } else {
            &content
        };

        let inline_tags = extract_inline_tags(body);
        tags.extend(inline_tags);

        let inline_links = extract_links(body);
        links.extend(inline_links);
    }

    (aliases, tags, links, properties)
}

fn parse_frontmatter(frontmatter: &str) -> (Vec<String>, Vec<String>, HashMap<String, String>) {
    let mut aliases = Vec::new();
    let mut tags = Vec::new();
    let mut properties = HashMap::new();
    let mut current_key = String::new();
    let mut in_list = false;
    let mut current_list_items = Vec::new();

    for line in frontmatter.lines() {
        let line = line.trim();

        if line.is_empty() {
            continue;
        }

        if let Some(colon_pos) = line.find(':') {
            if in_list && !current_key.is_empty() {
                match current_key.as_str() {
                    "aliases" | "alias" => aliases.extend(current_list_items.clone()),
                    "tags" | "tag" => tags.extend(current_list_items.clone()),
                    _ => {
                        if !current_list_items.is_empty() {
                            properties.insert(current_key.clone(), current_list_items.join(", "));
                        }
                    }
                }
                current_list_items.clear();
            }

            let key = line[..colon_pos].trim().to_string();
            let value = line[colon_pos + 1..].trim();

            current_key = key.clone();
            in_list = false;

            if !value.is_empty() {
                if value.starts_with('[') && value.ends_with(']') {
                    // Inline array format: key: [item1, item2]
                    let list_content = &value[1..value.len() - 1];
                    let items: Vec<String> = list_content
                        .split(',')
                        .map(|item| item.trim().trim_matches('"').trim_matches('\'').to_string())
                        .filter(|item| !item.is_empty())
                        .collect();

                    match key.as_str() {
                        "aliases" | "alias" => aliases.extend(items),
                        "tags" | "tag" => tags.extend(items),
                        _ => {
                            properties.insert(key, items.join(", "));
                        }
                    }
                } else {
                    let clean_value = value.trim_matches('"').trim_matches('\'').to_string();
                    match key.as_str() {
                        "aliases" | "alias" => aliases.push(clean_value),
                        "tags" | "tag" => tags.push(clean_value),
                        _ => {
                            properties.insert(key, clean_value);
                        }
                    }
                }
            } else {
                in_list = true;
            }
        } else if line.starts_with("- ") {
            let item = line[2..].trim().trim_matches('"').trim_matches('\'');
            if !item.is_empty() {
                current_list_items.push(item.to_string());
                in_list = true;
            }
        }
    }

    // Handle final list if we ended in one
    if in_list && !current_key.is_empty() {
        match current_key.as_str() {
            "aliases" | "alias" => aliases.extend(current_list_items),
            "tags" | "tag" => tags.extend(current_list_items),
            _ => {
                if !current_list_items.is_empty() {
                    properties.insert(current_key, current_list_items.join(", "));
                }
            }
        }
    }

    (aliases, tags, properties)
}

pub fn extract_inline_tags(content: &str) -> Vec<String> {
    use regex::Regex;
    // Match hashtags that are not part of URLs
    let tag_regex = Regex::new(r"(?:^|[^/])#([a-zA-Z][a-zA-Z0-9_/-]*)").unwrap();

    tag_regex
        .captures_iter(content)
        .filter_map(|cap| {
            let full_match = cap.get(0).unwrap().as_str();
            // If the match starts with a character other than #, we matched too much
            if full_match.starts_with('#') {
                Some(cap[1].to_string())
            } else {
                // Skip the first character and check if what's left starts with # in case of
                // leading space
                if full_match.len() > 1 && &full_match[1..2] == "#" {
                    Some(cap[1].to_string())
                } else {
                    None
                }
            }
        })
        .collect()
}

pub fn extract_links(content: &str) -> Vec<String> {
    use regex::Regex;
    let link_regex = Regex::new(r"\[\[([^\]]+)\]\]").unwrap();

    link_regex
        .captures_iter(content)
        .filter_map(|cap| {
            let link_text = &cap[1];
            // Handle pipe links by extracting the address part
            if let Some(pipe_pos) = link_text.find('|') {
                Some(link_text[..pipe_pos].trim().to_string())
            } else {
                Some(link_text.trim().to_string())
            }
        })
        .collect()
}

pub async fn index_vault(root: &Path) -> Result<VaultIndex, String> {
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
        let (aliases, tags, links, properties) = if matches!(kind, FileKind::Markdown) {
            extract_metadata(path).await
        } else {
            (Vec::new(), Vec::new(), Vec::new(), HashMap::new())
        };

        let obsidian_file = ObsidianFile {
            name: name.clone(),
            path: path.to_path_buf(),
            stem: stem.clone(),
            aliases: aliases.clone(),
            tags: tags.clone(),
            properties: properties.clone(),
            checked_at: Instant::now(),
            kind,
        };

        let normalized_stem = normalize_link_text(&stem);
        index
            .link_map
            .insert(normalized_stem.clone(), path.to_path_buf());
        index
            .link_map
            .insert(normalize_link_text(&name), path.to_path_buf());

        for alias in &aliases {
            index
                .link_map
                .insert(normalize_link_text(alias), path.to_path_buf());
        }

        for tag in &tags {
            let normalized_tag = tag.to_lowercase();
            index
                .tag_map
                .entry(normalized_tag)
                .or_insert_with(Vec::new)
                .push(path.to_path_buf());
        }

        for link in &links {
            let normalized_link = normalize_link_text(link);
            index
                .reverse_link_map
                .entry(normalized_link)
                .or_insert_with(Vec::new)
                .push(path.to_path_buf());
        }

        for (property_key, _property_value) in &properties {
            let normalized_key = property_key.to_lowercase();
            index
                .property_map
                .entry(normalized_key)
                .or_insert_with(Vec::new)
                .push(path.to_path_buf());
        }

        index.files.push(obsidian_file);
    }

    let duration = start.elapsed();
    debug!("Indexed {} files in {:?}", index.files.len(), duration);

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
            "---\naliases: [\"Alternative Name\", \"Alt\"]\ntags: [\"test\", \"example\"]\nauthor: \"Test Author\"\nstatus: \"draft\"\n---\n# With Alias\n\nThis has inline #tag-inline and #another-tag in content."
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

        let markdown_files: Vec<_> = index
            .files
            .iter()
            .filter(|f| matches!(f.kind, FileKind::Markdown))
            .collect();
        assert_eq!(markdown_files.len(), 4);

        let attachment_files: Vec<_> = index
            .files
            .iter()
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
    async fn test_metadata_extraction() {
        let temp_dir = create_test_vault().await;
        let index = index_vault(temp_dir.path()).await.unwrap();

        let with_alias_file = index.files.iter().find(|f| f.stem == "with-alias").unwrap();

        // Test aliases
        assert_eq!(with_alias_file.aliases.len(), 2);
        assert!(
            with_alias_file
                .aliases
                .contains(&"Alternative Name".to_string())
        );
        assert!(with_alias_file.aliases.contains(&"Alt".to_string()));

        // Test tags (frontmatter + inline)
        assert!(with_alias_file.tags.len() >= 4); // 2 frontmatter + 2 inline
        assert!(with_alias_file.tags.contains(&"test".to_string()));
        assert!(with_alias_file.tags.contains(&"example".to_string()));
        assert!(with_alias_file.tags.contains(&"tag-inline".to_string()));
        assert!(with_alias_file.tags.contains(&"another-tag".to_string()));

        // Test properties
        assert_eq!(
            with_alias_file.properties.get("author"),
            Some(&"Test Author".to_string())
        );
        assert_eq!(
            with_alias_file.properties.get("status"),
            Some(&"draft".to_string())
        );

        // Test alias resolution still works
        assert!(index.find_file("Alternative Name").is_some());
        assert!(index.find_file("alternative-name").is_some()); // normalized
        assert!(index.find_file("Alt").is_some());
        assert!(index.find_file("alt").is_some()); // case insensitive

        // Test tag indexing
        assert!(index.tag_map.get("test").is_some());
        assert!(index.tag_map.get("example").is_some());
        assert!(index.tag_map.get("tag-inline").is_some());

        // Test property indexing
        assert!(index.property_map.get("author").is_some());
        assert!(index.property_map.get("status").is_some());
    }

    #[test]
    fn test_normalize_link_text() {
        assert_eq!(normalize_link_text("My Note"), "my-note");
        assert_eq!(normalize_link_text("my_note"), "my-note");
        assert_eq!(normalize_link_text("  My Note  "), "my-note");
        assert_eq!(
            normalize_link_text("Already-normalized"),
            "already-normalized"
        );
    }

    #[test]
    fn test_parse_frontmatter() {
        let frontmatter = r#"
aliases: ["Alternative Name", "Alt"]
tags: ["tag1", "tag2"]
author: "Test Author"
other: value
"#;
        let (aliases, tags, properties) = parse_frontmatter(frontmatter);

        assert_eq!(aliases.len(), 2);
        assert!(aliases.contains(&"Alternative Name".to_string()));
        assert!(aliases.contains(&"Alt".to_string()));

        assert_eq!(tags.len(), 2);
        assert!(tags.contains(&"tag1".to_string()));
        assert!(tags.contains(&"tag2".to_string()));

        assert_eq!(properties.get("author"), Some(&"Test Author".to_string()));
        assert_eq!(properties.get("other"), Some(&"value".to_string()));

        let frontmatter_list = r#"
aliases:
  - "First Alias"
  - Second Alias
tags:
  - programming
  - rust
status: draft
"#;
        let (aliases, tags, properties) = parse_frontmatter(frontmatter_list);

        assert_eq!(aliases.len(), 2);
        assert!(aliases.contains(&"First Alias".to_string()));
        assert!(aliases.contains(&"Second Alias".to_string()));

        assert_eq!(tags.len(), 2);
        assert!(tags.contains(&"programming".to_string()));
        assert!(tags.contains(&"rust".to_string()));

        assert_eq!(properties.get("status"), Some(&"draft".to_string()));
    }

    #[test]
    fn test_extract_inline_tags() {
        let content =
            "This is some content with #programming and #rust tags. Also #multi-word-tag.";
        let tags = extract_inline_tags(content);

        assert_eq!(tags.len(), 3);
        assert!(tags.contains(&"programming".to_string()));
        assert!(tags.contains(&"rust".to_string()));
        assert!(tags.contains(&"multi-word-tag".to_string()));

        // Test that hashtags in URLs or other contexts are not captured
        let content_with_url = "Visit #programming or https://example.com/#section";
        let tags = extract_inline_tags(content_with_url);
        assert_eq!(tags.len(), 1);
        assert!(tags.contains(&"programming".to_string()));
    }
}
