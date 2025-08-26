use std::collections::HashMap;

#[derive(Debug, Clone)]
pub struct Node<'a> {
    node_type: NodeType<'a>,
    children: Vec<NodeId>,
}

#[derive(Debug, Clone, Copy)]
pub enum NodeType<'a> {
    Heading(usize),
    Paragraph,
    Link(Link<'a>),
}

#[derive(Debug, Clone, Copy)]
pub struct Link<'a> {
    link_type: LinkType,
    address: &'a str,
}

#[derive(Debug, Clone, Copy)]
pub enum LinkType {
    Web,
    Wiki,
}

/// (line, col)
type NodeId = (usize, usize);

#[derive(Debug, Clone)]
pub struct Document<'a> {
    contents: String,
    nodes: HashMap<NodeId, Node<'a>>,
}

impl<'a> Into<Document<'a>> for String {
    fn into(self) -> Document<'a> {
        let mut nodes = HashMap::new();
        let mut node_stack: Vec<NodeId> = vec![];
        for (i, line) in self.lines().enumerate() {
            if line.is_empty() {
                continue;
            }

            let mut col = 0;
            let mut words = line.split_whitespace();
            match words.next() {
                Some(word) => match word {
                    "#" | "##" | "###" | "####" | "#####" | "######" => {
                        let level = word.len();
                        let current_node_id = (i, col);
                        nodes.insert(
                            current_node_id,
                            Node {
                                node_type: NodeType::Heading(level),
                                children: vec![],
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
                        col += level;
                    }
                    _ => {}
                },
                None => {
                    // Empty lines cant contain any nodes
                    continue;
                }
            }
        }

        Document {
            contents: self,
            nodes,
        }
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
}
