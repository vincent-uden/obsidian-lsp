use lsp_types::{Position, Range};
use regex::Regex;
use std::collections::HashMap;

#[derive(Debug, Clone)]
pub struct Node {
    pub node_type: NodeType,
    pub children: Vec<NodeId>,
    pub range: Range,
}

#[derive(Debug, Clone)]
pub enum NodeType {
    Heading(usize),
    Paragraph,
    Link(Link),
    Tag(String),
    Table(Table),
}

#[derive(Debug, Clone)]
pub struct Link {
    pub link_type: LinkType,
    pub address: String,      // The actual link target (before the pipe)
    pub display_text: String, // The display text (after the pipe, or same as address if no pipe)
}

#[derive(Debug, Clone, Copy)]
pub enum LinkType {
    Web,
    Wiki,
}

#[derive(Debug, Clone)]
pub struct Table {
    pub headers: Vec<String>,
    pub rows: Vec<Vec<String>>,
    pub alignments: Vec<ColumnAlignment>,
    pub has_alignment_markers: bool,
}

#[derive(Debug, Clone, Copy)]
pub enum ColumnAlignment {
    Left,
    Center,
    Right,
    None,
}

/// (line, col)
type NodeId = (usize, usize);

#[derive(Debug, Clone)]
pub struct Document {
    pub contents: String,
    pub nodes: HashMap<NodeId, Node>,
}

fn parse_wiki_link_text(link_text: &str) -> (String, String) {
    if let Some(pipe_pos) = link_text.find('|') {
        let address = link_text[..pipe_pos].trim().to_string();
        let display_text = link_text[pipe_pos + 1..].trim().to_string();
        (address, display_text)
    } else {
        (link_text.trim().to_string(), link_text.trim().to_string())
    }
}

impl Document {
    fn extract_wiki_links(&mut self) {
        self.extract_links_and_tags();
    }

    fn extract_tables(&mut self) {
        let lines: Vec<&str> = self.contents.lines().collect();
        let mut i = 0;

        while i < lines.len() {
            if let Some((table, table_end_line)) = self.parse_table_at_line(&lines, i) {
                let table_start_line = i;

                let range = Range {
                    start: Position {
                        line: table_start_line as u32,
                        character: 0,
                    },
                    end: Position {
                        line: table_end_line as u32,
                        character: lines.get(table_end_line).map(|l| l.len() as u32).unwrap_or(0),
                    },
                };

                let node_id = (table_start_line, 0);
                self.nodes.insert(
                    node_id,
                    Node {
                        node_type: NodeType::Table(table),
                        children: vec![],
                        range,
                    },
                );

                // Skip the lines we've processed
                i = table_end_line + 1;
            } else {
                i += 1;
            }
        }
    }

    fn parse_table_at_line(&self, lines: &[&str], start_line: usize) -> Option<(Table, usize)> {
        if start_line >= lines.len() {
            return None;
        }

        // Check if this line looks like a table header
        let header_line = lines[start_line];
        if !self.is_table_row(header_line) {
            return None;
        }

        // Parse header row
        let headers = self.parse_table_row(header_line)?;

        // Check for separator row
        if start_line + 1 >= lines.len() {
            return None;
        }
        let separator_line = lines[start_line + 1];
        if !self.is_separator_row(separator_line) {
            return None;
        }

        // Parse alignments from separator
        let alignments = self.parse_separator_alignments(separator_line, headers.len())?;

        // Check if the original separator had alignment markers
        let has_alignment_markers = self.separator_has_alignment_markers(separator_line);

        // Parse data rows
        let mut rows = Vec::new();
        let mut current_line = start_line + 2;

        while current_line < lines.len() {
            let line = lines[current_line];
            if line.trim().is_empty() {
                break; // Empty line ends the table
            }
            if !self.is_table_row(line) {
                break; // Non-table row ends the table
            }

            if let Some(row) = self.parse_table_row(line) {
                if row.len() == headers.len() {
                    rows.push(row);
                } else {
                    break; // Row with different column count ends the table
                }
            } else {
                break;
            }

            current_line += 1;
        }

        Some((Table {
            headers,
            rows,
            alignments,
            has_alignment_markers,
        }, current_line - 1)) // current_line is the line after the last table row
    }

    fn is_table_row(&self, line: &str) -> bool {
        let trimmed = line.trim();
        trimmed.starts_with('|') && trimmed.ends_with('|') && trimmed.len() > 2
    }

    fn is_separator_row(&self, line: &str) -> bool {
        let trimmed = line.trim();
        if !trimmed.starts_with('|') || !trimmed.ends_with('|') {
            return false;
        }

        // Check if all cells contain only dashes, colons, and spaces
        let cells: Vec<&str> = trimmed.split('|').collect();
        for cell in cells.iter().skip(1).take(cells.len() - 2) {
            let cell_trimmed = cell.trim();
            if cell_trimmed.is_empty() {
                continue;
            }
            if !cell_trimmed.chars().all(|c| c == '-' || c == ':' || c.is_whitespace()) {
                return false;
            }
        }
        true
    }

    fn parse_table_row(&self, line: &str) -> Option<Vec<String>> {
        if !self.is_table_row(line) {
            return None;
        }

        let trimmed = line.trim();
        let content = &trimmed[1..trimmed.len() - 1]; // Remove outer pipes

        let cells: Vec<String> = content
            .split('|')
            .map(|cell| cell.trim().to_string())
            .collect();

        Some(cells)
    }

    fn parse_separator_alignments(&self, line: &str, expected_columns: usize) -> Option<Vec<ColumnAlignment>> {
        if !self.is_separator_row(line) {
            return None;
        }

        let trimmed = line.trim();
        let content = &trimmed[1..trimmed.len() - 1];
        let cells: Vec<&str> = content.split('|').collect();

        if cells.len() != expected_columns {
            return None;
        }

        let mut alignments = Vec::new();

        for cell in &cells {
            let trimmed_cell = cell.trim();
            if trimmed_cell.is_empty() {
                alignments.push(ColumnAlignment::Left);
                continue;
            }

            let starts_with_colon = trimmed_cell.starts_with(':');
            let ends_with_colon = trimmed_cell.ends_with(':');

            let alignment = match (starts_with_colon, ends_with_colon) {
                (true, true) => ColumnAlignment::Center,
                (false, true) => ColumnAlignment::Right,
                (true, false) => ColumnAlignment::Left,
                (false, false) => ColumnAlignment::Left,
            };

            alignments.push(alignment);
        }

        Some(alignments)
    }

    fn separator_has_alignment_markers(&self, line: &str) -> bool {
        if !self.is_separator_row(line) {
            return false;
        }

        let trimmed = line.trim();
        let content = &trimmed[1..trimmed.len() - 1];
        let cells: Vec<&str> = content.split('|').collect();

        for cell in &cells {
            let trimmed_cell = cell.trim();
            if trimmed_cell.starts_with(':') || trimmed_cell.ends_with(':') {
                return true;
            }
        }

        false
    }

    fn extract_links_and_tags(&mut self) {
        let wiki_regex = Regex::new(r"\[\[([^\]]+)\]\]").unwrap();
        let tag_regex = Regex::new(r"#([a-zA-Z][a-zA-Z0-9_/-]*)").unwrap();
        let lines: Vec<&str> = self.contents.lines().collect();
        let mut additional_nodes = HashMap::new();

        let node_ids: Vec<NodeId> = self.nodes.keys().copied().collect();

        for &node_id in &node_ids {
            let (line_num, _) = node_id;

            if let Some(node) = self.nodes.get(&node_id) {
                match node.node_type {
                    NodeType::Heading(_) => {
                        if let Some(line) = lines.get(line_num) {
                            for mat in wiki_regex.find_iter(line) {
                                let link_text = &line[mat.start() + 2..mat.end() - 2];
                                let (address, display_text) = parse_wiki_link_text(link_text);
                                let link_node_id = (line_num, mat.start());

                                let range = Range {
                                    start: Position {
                                        line: line_num as u32,
                                        character: mat.start() as u32,
                                    },
                                    end: Position {
                                        line: line_num as u32,
                                        character: mat.end() as u32,
                                    },
                                };

                                additional_nodes.insert(
                                    link_node_id,
                                    Node {
                                        node_type: NodeType::Link(Link {
                                            link_type: LinkType::Wiki,
                                            address,
                                            display_text,
                                        }),
                                        children: vec![],
                                        range,
                                    },
                                );

                                if let Some(parent_node) = self.nodes.get_mut(&node_id) {
                                    parent_node.children.push(link_node_id);
                                }
                            }

                                let mut tag_match_count = 0;
                                for mat in tag_regex.find_iter(line) {
                                    tag_match_count += 1;
                                    if tag_match_count > 100 {
                                        eprintln!("EMERGENCY: Too many tag matches in heading line: {}", tag_match_count);
                                        break;
                                    }

                                    let tag_name = mat.as_str()[1..].to_string(); // Remove the # prefix
                                    let tag_node_id = (line_num, mat.start());

                                let range = Range {
                                    start: Position {
                                        line: line_num as u32,
                                        character: mat.start() as u32,
                                    },
                                    end: Position {
                                        line: line_num as u32,
                                        character: mat.end() as u32,
                                    },
                                };

                                additional_nodes.insert(
                                    tag_node_id,
                                    Node {
                                        node_type: NodeType::Tag(tag_name),
                                        children: vec![],
                                        range,
                                    },
                                );

                                if let Some(parent_node) = self.nodes.get_mut(&node_id) {
                                    parent_node.children.push(tag_node_id);
                                }
                            }
                        }
                    }
                    NodeType::Paragraph => {
                        let mut current_line = line_num;
                        let mut loop_count = 0;
                        while current_line < lines.len()
                            && !lines[current_line].is_empty()
                            && !lines[current_line].trim_start().starts_with('#')
                        {
                            loop_count += 1;

                            if loop_count > 1000 {
                                eprintln!("EMERGENCY: Paragraph parsing loop exceeded 1000 iterations");
                                break;
                            }
                            if let Some(line) = lines.get(current_line) {
                                for mat in wiki_regex.find_iter(line) {
                                    let link_text = &line[mat.start() + 2..mat.end() - 2];
                                    let (address, display_text) = parse_wiki_link_text(link_text);
                                    let link_node_id = (current_line, mat.start());

                                    let range = Range {
                                        start: Position {
                                            line: current_line as u32,
                                            character: mat.start() as u32,
                                        },
                                        end: Position {
                                            line: current_line as u32,
                                            character: mat.end() as u32,
                                        },
                                    };

                                    additional_nodes.insert(
                                        link_node_id,
                                        Node {
                                            node_type: NodeType::Link(Link {
                                                link_type: LinkType::Wiki,
                                                address,
                                                display_text,
                                            }),
                                            children: vec![],
                                            range,
                                        },
                                    );

                                    if let Some(parent_node) = self.nodes.get_mut(&node_id) {
                                        parent_node.children.push(link_node_id);
                                    }
                                }

                                let mut para_tag_match_count = 0;
                                for mat in tag_regex.find_iter(line) {
                                    para_tag_match_count += 1;
                                    if para_tag_match_count > 100 {
                                        eprintln!("EMERGENCY: Too many tag matches in paragraph line: {}", para_tag_match_count);
                                        break;
                                    }

                                    let tag_name = mat.as_str()[1..].to_string(); // Remove the # prefix
                                    let tag_node_id = (current_line, mat.start());

                                    let range = Range {
                                        start: Position {
                                            line: current_line as u32,
                                            character: mat.start() as u32,
                                        },
                                        end: Position {
                                            line: current_line as u32,
                                            character: mat.end() as u32,
                                        },
                                    };

                                    additional_nodes.insert(
                                        tag_node_id,
                                        Node {
                                            node_type: NodeType::Tag(tag_name),
                                            children: vec![],
                                            range,
                                        },
                                    );

                                    if let Some(parent_node) = self.nodes.get_mut(&node_id) {
                                        parent_node.children.push(tag_node_id);
                                    }
                                }
                            }
                            current_line += 1;
                        }
                    }
                    _ => {}
                }
            }
        }

        self.nodes.extend(additional_nodes);
    }
}

impl From<String> for Document {
    fn from(value: String) -> Self {
        let mut nodes = HashMap::new();
        let mut node_stack: Vec<NodeId> = vec![];
        let lines: Vec<&str> = value.lines().collect();
        let mut i = 0;

        let mut main_loop_count = 0;
        while i < lines.len() {
            main_loop_count += 1;
            if main_loop_count > 10000 {
                eprintln!("EMERGENCY: Main document parsing loop exceeded 10000 iterations");
                break;
            }

            let line = lines[i];
            let original_i_for_line = i;

            if line.is_empty() {
                i += 1;
                continue;
            }

            let col = 0;
            let mut words = line.split_whitespace();
            match words.next() {
                // Headings
                Some(word) => match word {
                    "#" | "##" | "###" | "####" | "#####" | "######" => {
                        let level = word.len();
                        let current_node_id = (i, col);

                        let range = Range {
                            start: Position {
                                line: i as u32,
                                character: 0,
                            },
                            end: Position {
                                line: i as u32,
                                character: line.len() as u32,
                            },
                        };

                        nodes.insert(
                            current_node_id,
                            Node {
                                node_type: NodeType::Heading(level),
                                children: vec![],
                                range,
                            },
                        );
                        let mut heading_loop_count = 0;
                        let mut heading_processed = false;
                        while let Some(&parent_id) = node_stack.last() {
                            heading_loop_count += 1;
                            if heading_loop_count > 100 {
                                eprintln!("EMERGENCY: Heading hierarchy loop exceeded 100 iterations");
                                break;
                            }

                            if let Some(parent_node) = nodes.get(&parent_id) {
                                match parent_node.node_type {
                                    NodeType::Heading(parent_level) => {
                                        if parent_level < level {
                                            nodes
                                                .get_mut(&parent_id)
                                                .unwrap()
                                                .children
                                                .push(current_node_id);
                                            heading_processed = true;
                                            break;
                                        } else {
                                            node_stack.pop();
                                        }
                                    }
                                    _ => {
                                        node_stack.pop();
                                    }
                                }
                            } else {
                                node_stack.pop();
                            }
                        }

                        if !heading_processed {
                            // If we didn't find a proper parent, just add to root level
                        }

                        node_stack.push(current_node_id);
                        i += 1;
                    }
                    // Paragraphs
                    _ => {
                        let paragraph_start = i;
                        let current_node_id = (paragraph_start, col);
                        let start_line = i;

                        let mut para_boundary_loop_count = 0;
                        let _original_i = i;
                        while i < lines.len()
                            && !lines[i].is_empty()
                            && !lines[i].trim_start().starts_with('#')
                        {
                            para_boundary_loop_count += 1;
                            if para_boundary_loop_count > 1000 {
                                eprintln!("EMERGENCY: Paragraph boundary loop exceeded 1000 iterations");
                                break;
                            }
                            i += 1;
                        }



                        let end_line = if i > 0 { i - 1 } else { 0 };
                        let end_char = if let Some(last_line) = lines.get(end_line) {
                            last_line.len() as u32
                        } else {
                            0
                        };

                        let range = Range {
                            start: Position {
                                line: start_line as u32,
                                character: 0,
                            },
                            end: Position {
                                line: end_line as u32,
                                character: end_char,
                            },
                        };

                        nodes.insert(
                            current_node_id,
                            Node {
                                node_type: NodeType::Paragraph,
                                children: vec![],
                                range,
                            },
                        );

                        if let Some(&parent_id) = node_stack.last() {
                            nodes
                                .get_mut(&parent_id)
                                .unwrap()
                                .children
                                .push(current_node_id);
                        }
                    }
                },
                None => {
                    i += 1;
                }
            }

            // Safety: Ensure we always advance i, even if processing failed
            if i == original_i_for_line {
                i += 1;
            }
        }

        let mut document = Document {
            contents: value,
            nodes,
        };

        document.extract_wiki_links();
        document.extract_tables();
        document
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_headings() {
        let src = include_str!("../assets/tests/headings.md");
        let doc: Document = src.to_string().into();

        assert!(doc.nodes.len() == 5);

        // # H1 Heading 1
        assert!(matches!(
            &doc.nodes[&(0, 0)].node_type,
            NodeType::Heading(1)
        ));
        assert!(
            doc.nodes[&(0, 0)].children.len() == 2,
            "H1 Heading 1 should have 2 direct children (H2s), it has {}",
            doc.nodes[&(0, 0)].children.len()
        );

        // ## H2 Heading 2 should have 1 child (H3)
        assert!(matches!(
            &doc.nodes[&(4, 0)].node_type,
            NodeType::Heading(2)
        ));
        assert!(
            doc.nodes[&(4, 0)].children.len() == 1,
            "H2 Heading 2 should have 1 child (H3), it has {}",
            doc.nodes[&(4, 0)].children.len()
        );
    }

    #[test]
    fn parse_paragraphs() {
        let src = include_str!("../assets/tests/paragraphs.md");
        let doc: Document = src.to_string().into();

        // Should have 3 headings + 5 paragraphs = 8 nodes total
        assert!(
            doc.nodes.len() == 8,
            "Expected 8 nodes, got {}",
            doc.nodes.len()
        );

        // # Main Heading should have 3 children (2 paragraphs + 1 subheading)
        assert!(matches!(
            &doc.nodes[&(0, 0)].node_type,
            NodeType::Heading(1)
        ));
        assert!(
            doc.nodes[&(0, 0)].children.len() == 3,
            "Main Heading should have 3 children, it has {}",
            doc.nodes[&(0, 0)].children.len()
        );

        // First paragraph under Main Heading
        assert!(matches!(&doc.nodes[&(2, 0)].node_type, NodeType::Paragraph));

        // ## Subheading should have 2 children (2 paragraphs)
        assert!(matches!(
            &doc.nodes[&(6, 0)].node_type,
            NodeType::Heading(2)
        ));
        assert!(
            doc.nodes[&(6, 0)].children.len() == 2,
            "Subheading should have 2 paragraph children, it has {}",
            doc.nodes[&(6, 0)].children.len()
        );
    }

    #[test]
    fn parse_wiki_links() {
        let src = include_str!("../assets/tests/wikilinks.md");
        let doc: Document = src.to_string().into();

        // Should have 2 headings + 3 paragraphs + 6 wiki links = 11 nodes total
        assert!(
            doc.nodes.len() == 11,
            "Expected 11 nodes (2 headings + 3 paragraphs + 6 wiki links), got {}",
            doc.nodes.len()
        );

        // Check heading with wiki link
        let heading_node = &doc.nodes[&(0, 0)];
        assert!(matches!(heading_node.node_type, NodeType::Heading(1)));
        assert!(
            heading_node.children.len() >= 2,
            "Heading should have paragraph + wiki link as children, got {}",
            heading_node.children.len()
        );

        // Find the wiki link in the heading
        let wiki_link_in_heading = heading_node
            .children
            .iter()
            .find(|&&child_id| matches!(doc.nodes[&child_id].node_type, NodeType::Link(_)));
        assert!(
            wiki_link_in_heading.is_some(),
            "Should find wiki link in heading"
        );

        // Check the wiki link content
        if let Some(&link_id) = wiki_link_in_heading {
            if let NodeType::Link(link) = &doc.nodes[&link_id].node_type {
                assert_eq!(link.address, "Wiki Link");
                assert!(matches!(link.link_type, LinkType::Wiki));
            }
        }

        // Check paragraph with multiple wiki links
        let paragraph_node = &doc.nodes[&(2, 0)];
        assert!(matches!(paragraph_node.node_type, NodeType::Paragraph));

        let wiki_links_in_paragraph: Vec<_> = paragraph_node
            .children
            .iter()
            .filter(|&&child_id| matches!(doc.nodes[&child_id].node_type, NodeType::Link(_)))
            .collect();
        assert_eq!(
            wiki_links_in_paragraph.len(),
            2,
            "First paragraph should have 2 wiki links, got {}",
            wiki_links_in_paragraph.len()
        );

        // Verify all wiki links have correct content
        let all_links: Vec<_> = doc
            .nodes
            .values()
            .filter_map(|node| match &node.node_type {
                NodeType::Link(link) => Some(&link.address),
                _ => None,
            })
            .collect();

        assert_eq!(all_links.len(), 6, "Should have 6 wiki links total");
        assert!(all_links.contains(&&"Wiki Link".to_string()));
        assert!(all_links.contains(&&"another link".to_string()));
        assert!(all_links.contains(&&"second link".to_string()));
        assert!(all_links.contains(&&"link in second line".to_string()));
        assert!(all_links.contains(&&"nested link".to_string()));
        assert!(all_links.contains(&&"final link".to_string()));
    }

    #[test]
    fn test_exclamation_links_debug() {
        let content = r#"# Test Document

This has an exclamation link:

![[voith_charge_and_discharge_no_sensor.svg|center]]

End of document."#;

        println!("Testing document content:\n{}", content);
        let doc: Document = content.to_string().into();

        println!("Parsed document has {} nodes", doc.nodes.len());

        // Print all nodes
        for (id, node) in &doc.nodes {
            println!("Node {:?}: {:?}", id, node.node_type);
        }

        // Look for link nodes specifically
        let link_nodes: Vec<_> = doc
            .nodes
            .values()
            .filter_map(|node| match &node.node_type {
                NodeType::Link(link) => Some(link),
                _ => None,
            })
            .collect();

        println!("Found {} link nodes", link_nodes.len());
        for link in &link_nodes {
            println!(
                "  Link: address='{}', display='{}'",
                link.address, link.display_text
            );
        }

        // This should find the exclamation link
        assert!(!link_nodes.is_empty(), "Should find the exclamation link");
    }

    #[test]
    fn parse_pipe_links() {
        let src = include_str!("../assets/tests/pipe-links.md");
        let doc: Document = src.to_string().into();

        // Should have 1 heading + 3 paragraphs + 3 wiki links = 7 nodes total
        assert_eq!(
            doc.nodes.len(),
            7,
            "Expected 7 nodes, got {}",
            doc.nodes.len()
        );

        // Get all link nodes
        let links: Vec<_> = doc
            .nodes
            .values()
            .filter_map(|node| match &node.node_type {
                NodeType::Link(link) => Some(link),
                _ => None,
            })
            .collect();

        assert_eq!(links.len(), 3, "Expected 3 links, got {}", links.len());

        // Check pipe link with custom display text
        let pipe_link = links
            .iter()
            .find(|link| link.address == "target-file")
            .expect("Should find target-file link");
        assert_eq!(pipe_link.display_text, "Custom Display Text");

        // Check simple link without pipe
        let simple_link = links
            .iter()
            .find(|link| link.address == "simple-link")
            .expect("Should find simple-link");
        assert_eq!(simple_link.display_text, "simple-link");

        // Check complex path with pipe
        let complex_link = links
            .iter()
            .find(|link| link.address == "complex/path")
            .expect("Should find complex/path link");
        assert_eq!(complex_link.display_text, "Simplified Name");
    }

    #[test]
    fn test_parse_tags() {
        let src = include_str!("../assets/tests/tags.md");
        let doc: Document = src.to_string().into();

        // Find all tag nodes
        let tag_nodes: Vec<_> = doc
            .nodes
            .values()
            .filter_map(|node| match &node.node_type {
                NodeType::Tag(tag_name) => Some(tag_name),
                _ => None,
            })
            .collect();

        // Should have inline tags: inline-tag, programming, rust, lsp, test, example, demo, final-tag
        assert!(
            tag_nodes.len() >= 8,
            "Expected at least 8 tag nodes, got {}",
            tag_nodes.len()
        );

        assert!(tag_nodes.contains(&&"inline-tag".to_string()));
        assert!(tag_nodes.contains(&&"programming".to_string()));
        assert!(tag_nodes.contains(&&"rust".to_string()));
        assert!(tag_nodes.contains(&&"lsp".to_string()));
        assert!(tag_nodes.contains(&&"test".to_string()));
        assert!(tag_nodes.contains(&&"example".to_string()));
        assert!(tag_nodes.contains(&&"demo".to_string()));
        assert!(tag_nodes.contains(&&"final-tag".to_string()));
    }

    #[test]
    fn test_parse_wiki_link_text() {
        // Test link without pipe
        let (address, display) = parse_wiki_link_text("simple-link");
        assert_eq!(address, "simple-link");
        assert_eq!(display, "simple-link");

        // Test link with pipe
        let (address, display) = parse_wiki_link_text("target-file|Custom Display");
        assert_eq!(address, "target-file");
        assert_eq!(display, "Custom Display");

        // Test link with pipe and whitespace
        let (address, display) = parse_wiki_link_text("  folder/file  |  Nice Name  ");
        assert_eq!(address, "folder/file");
        assert_eq!(display, "Nice Name");

        // Test edge case: pipe at end
        let (address, display) = parse_wiki_link_text("file-name|");
        assert_eq!(address, "file-name");
        assert_eq!(display, "");

        // Test edge case: pipe at start
        let (address, display) = parse_wiki_link_text("|Display Only");
        assert_eq!(address, "");
        assert_eq!(display, "Display Only");
    }

    #[test]
    fn test_parse_tables() {
        let src = include_str!("../assets/tests/tables.md");
        let doc: Document = src.to_string().into();

        // Should have 5 headings + 5 tables + some paragraphs
        let table_count = doc.nodes.values()
            .filter(|node| matches!(node.node_type, NodeType::Table(_)))
            .count();

        assert_eq!(table_count, 5, "Expected 5 tables, got {}", table_count);

        // Test basic table
        let basic_table = doc.nodes.values()
            .find(|node| {
                if let NodeType::Table(table) = &node.node_type {
                    table.headers.contains(&"Name".to_string())
                } else {
                    false
                }
            })
            .expect("Should find basic table");

        if let NodeType::Table(table) = &basic_table.node_type {
            assert_eq!(table.headers, vec!["Name", "Age", "City"]);
            assert_eq!(table.rows.len(), 2);
            assert_eq!(table.rows[0], vec!["John", "25", "New York"]);
            assert_eq!(table.rows[1], vec!["Jane", "30", "Los Angeles"]);
            assert!(!table.has_alignment_markers, "Basic table should not have alignment markers");
        }

        // Test table with alignment markers
        let alignment_table = doc.nodes.values()
            .find(|node| {
                if let NodeType::Table(table) = &node.node_type {
                    table.headers.contains(&"Left".to_string())
                } else {
                    false
                }
            })
            .expect("Should find alignment table");

        if let NodeType::Table(table) = &alignment_table.node_type {
            assert!(table.has_alignment_markers, "Alignment table should have alignment markers");
        }

        // Test alignment table
        let alignment_table = doc.nodes.values()
            .find(|node| {
                if let NodeType::Table(table) = &node.node_type {
                    table.headers.contains(&"Left".to_string())
                } else {
                    false
                }
            })
            .expect("Should find alignment table");

        if let NodeType::Table(table) = &alignment_table.node_type {
            assert_eq!(table.alignments.len(), 3);
            // Should have Left, Center, Right alignments
            assert!(matches!(table.alignments[0], ColumnAlignment::Left));
            assert!(matches!(table.alignments[1], ColumnAlignment::Center));
            assert!(matches!(table.alignments[2], ColumnAlignment::Right));
        }
    }

    #[test]
    fn test_table_row_parsing() {
        let doc = Document::from("".to_string());

        // Test basic row parsing
        let row = doc.parse_table_row("| A | B | C |").unwrap();
        assert_eq!(row, vec!["A", "B", "C"]);

        // Test row with empty cells
        let row = doc.parse_table_row("| A |   | C |").unwrap();
        assert_eq!(row, vec!["A", "", "C"]);

        // Test row with extra whitespace
        let row = doc.parse_table_row("|  A  |  B  |  C  |").unwrap();
        assert_eq!(row, vec!["A", "B", "C"]);

        // Test invalid row
        assert!(doc.parse_table_row("Not a table row").is_none());
        assert!(doc.parse_table_row("| Missing end pipe").is_none());
    }

    #[test]
    fn test_separator_alignment_parsing() {
        let doc = Document::from("".to_string());

        // Test left alignment
        let alignments = doc.parse_separator_alignments("| :---- | ----- | ----- |", 3).unwrap();
        assert!(matches!(alignments[0], ColumnAlignment::Left));
        assert!(matches!(alignments[1], ColumnAlignment::Left));
        assert!(matches!(alignments[2], ColumnAlignment::Left));

        // Test center alignment
        let alignments = doc.parse_separator_alignments("| :----: | :----: | :----: |", 3).unwrap();
        assert!(matches!(alignments[0], ColumnAlignment::Center));
        assert!(matches!(alignments[1], ColumnAlignment::Center));
        assert!(matches!(alignments[2], ColumnAlignment::Center));

        // Test right alignment
        let alignments = doc.parse_separator_alignments("| -----: | -----: | -----: |", 3).unwrap();
        assert!(matches!(alignments[0], ColumnAlignment::Right));
        assert!(matches!(alignments[1], ColumnAlignment::Right));
        assert!(matches!(alignments[2], ColumnAlignment::Right));

        // Test mixed alignments
        let alignments = doc.parse_separator_alignments("| :---- | :----: | -----: |", 3).unwrap();
        assert!(matches!(alignments[0], ColumnAlignment::Left));
        assert!(matches!(alignments[1], ColumnAlignment::Center));
        assert!(matches!(alignments[2], ColumnAlignment::Right));
    }
}
