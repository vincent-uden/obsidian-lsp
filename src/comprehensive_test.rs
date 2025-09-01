use crate::ast::Document;
use std::time::{Instant, Duration};

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_comprehensive_scenarios() {
        let test_cases = vec![
            ("#area/tribo", "Single tag"),
            ("#area/tribo #another/tag", "Multiple tags on one line"),
            ("# Test\n\n#area/tribo in paragraph.", "Tag in paragraph after heading"),
            ("# H1\n\n## H2\n\n#area/tribo here.", "Tag with nested headings"),
            ("---\ntags: [frontmatter]\n---\n\n#area/tribo", "Tag with frontmatter"),
            ("#area/tribo\n\n#another/tag\n\n#third/tag", "Multiple paragraphs with tags"),
        ];

        for (content, description) in test_cases {
            println!("=== Testing: {} ===", description);
            println!("Content: {:?}", content);

            let start = Instant::now();
            let doc: Document = content.to_string().into();
            let duration = start.elapsed();

            println!("Duration: {:?}, Nodes: {}", duration, doc.nodes.len());

            // Should complete in under 100ms
            assert!(duration < Duration::from_millis(100),
                   "Test '{}' took too long: {:?}", description, duration);

            // Should have at least one node
            assert!(!doc.nodes.is_empty(),
                   "Test '{}' produced no nodes", description);

            println!("✓ Passed\n");
        }
    }

    #[test]
    fn test_edge_cases() {
        let edge_cases = vec![
            ("", "Empty string"),
            ("\n\n\n", "Only newlines"),
            ("   ", "Only spaces"),
            ("#", "Just hash"),
            ("#a", "Single char tag"),
            ("#area/tribo/extra/deep", "Very deep tag"),
            ("#123tag", "Tag starting with number"),
            ("#tag-with-dashes", "Tag with dashes"),
            ("#tag_with_underscores", "Tag with underscores"),
        ];

        for (content, description) in edge_cases {
            println!("=== Edge case: {} ===", description);

            let start = Instant::now();
            let doc: Document = content.to_string().into();
            let duration = start.elapsed();

            println!("Duration: {:?}, Nodes: {}", duration, doc.nodes.len());

            // Should not take more than 50ms even for edge cases
            assert!(duration < Duration::from_millis(50),
                   "Edge case '{}' took too long: {:?}", description, duration);

            println!("✓ Passed\n");
        }
    }

    #[test]
    fn test_large_document() {
        let mut content = "# Large Document\n\n".to_string();

        // Add 100 paragraphs with tags
        for i in 0..100 {
            content.push_str(&format!("This is paragraph {} with #tag{} and #area/tribo.\n\n", i, i));
        }

        println!("=== Testing large document ({} chars) ===", content.len());

        let start = Instant::now();
        let doc: Document = content.into();
        let duration = start.elapsed();

        println!("Duration: {:?}, Nodes: {}", duration, doc.nodes.len());

        // Should complete in under 1 second even for large documents
        assert!(duration < Duration::from_millis(1000),
               "Large document test took too long: {:?}", duration);

        // Should have many nodes
        assert!(doc.nodes.len() > 50,
               "Large document should have many nodes, got {}", doc.nodes.len());

        println!("✓ Passed\n");
    }
}