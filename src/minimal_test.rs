use crate::ast::Document;

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_minimal_area_tribo() {
        let content = r#"# Test Document

This is a paragraph with #area/tribo tag in it.

Another paragraph."#;

        println!("=== STARTING MINIMAL TEST ===");
        println!("Content length: {} chars", content.len());
        println!("Content:\n{}", content);

        let start_time = std::time::Instant::now();
        let doc: Document = content.to_string().into();
        let duration = start_time.elapsed();

        println!("=== TEST COMPLETED ===");
        println!("Duration: {:?}", duration);
        println!("Final node count: {}", doc.nodes.len());

        for (id, node) in &doc.nodes {
            println!("Node {:?}: {:?}", id, node.node_type);
        }

        assert!(duration.as_millis() < 1000, "Test took too long: {:?}", duration);
    }

    #[test]
    fn test_just_tag() {
        let content = "#area/tribo";

        println!("=== TESTING JUST TAG ===");
        let start_time = std::time::Instant::now();
        let doc: Document = content.to_string().into();
        let duration = start_time.elapsed();

        println!("Duration: {:?}", duration);
        println!("Node count: {}", doc.nodes.len());

        assert!(duration.as_millis() < 100, "Tag test took too long: {:?}", duration);
    }
}