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
        let wiki_regex = Regex::new(r"\[\[([^\]]+)\]\]").unwrap();
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
                        }
                    }
                    NodeType::Paragraph => {
                        let mut current_line = line_num;
                        while current_line < lines.len()
                            && !lines[current_line].is_empty()
                            && !lines[current_line].trim_start().starts_with('#')
                        {
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

        while i < lines.len() {
            let line = lines[i];

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
                        while let Some(&parent_id) = node_stack.last() {
                            if let Some(parent_node) = nodes.get(&parent_id) {
                                match parent_node.node_type {
                                    NodeType::Heading(parent_level) => {
                                        if parent_level < level {
                                            nodes
                                                .get_mut(&parent_id)
                                                .unwrap()
                                                .children
                                                .push(current_node_id);
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

                        node_stack.push(current_node_id);
                        i += 1;
                    }
                    // Paragraphs
                    _ => {
                        let paragraph_start = i;
                        let current_node_id = (paragraph_start, col);
                        let start_line = i;

                        while i < lines.len()
                            && !lines[i].is_empty()
                            && !lines[i].trim_start().starts_with('#')
                        {
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
        }

        let mut document = Document {
            contents: value,
            nodes,
        };

        document.extract_wiki_links();
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
    fn parse_pipe_links() {
        let src = include_str!("../assets/tests/pipe-links.md");
        let doc: Document = src.to_string().into();



        // Should have 1 heading + 3 paragraphs + 3 wiki links = 7 nodes total
        assert_eq!(doc.nodes.len(), 7, "Expected 7 nodes, got {}", doc.nodes.len());

        // Get all link nodes
        let links: Vec<_> = doc.nodes.values()
            .filter_map(|node| match &node.node_type {
                NodeType::Link(link) => Some(link),
                _ => None,
            })
            .collect();

        assert_eq!(links.len(), 3, "Expected 3 links, got {}", links.len());

        // Check pipe link with custom display text
        let pipe_link = links.iter()
            .find(|link| link.address == "target-file")
            .expect("Should find target-file link");
        assert_eq!(pipe_link.display_text, "Custom Display Text");

        // Check simple link without pipe
        let simple_link = links.iter()
            .find(|link| link.address == "simple-link")
            .expect("Should find simple-link");
        assert_eq!(simple_link.display_text, "simple-link");

        // Check complex path with pipe
        let complex_link = links.iter()
            .find(|link| link.address == "complex/path")
            .expect("Should find complex/path link");
        assert_eq!(complex_link.display_text, "Simplified Name");
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
}
