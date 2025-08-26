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
                        nodes.insert(
                            (i, col),
                            Node {
                                node_type: NodeType::Heading(level),
                                children: vec![],
                            },
                        );
                        if let Some(id) = node_stack.last() {
                            let outer_node = nodes.get_mut(id).unwrap();
                            if match outer_node.node_type {
                                NodeType::Heading(prev_level) => prev_level >= level,
                                NodeType::Paragraph => true,
                                NodeType::Link(_) => true,
                            } {
                                // TODO: The stack should be popped, not cleared, and this needs to
                                // be in a loop since it might require us to pop multiple headings
                                node_stack.clear();
                                node_stack.push((i, col));
                            } else {
                                node_stack.push((i, col));
                                outer_node.children.push((i, col));
                            }
                        } else {
                            node_stack.push((i, col));
                        }
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
            doc.nodes[&(0, 0)].children.len() == 3,
            "H1 Heading 1 should have 3 children, it has {}",
            doc.nodes[&(0, 0)].children.len()
        );
    }
}
