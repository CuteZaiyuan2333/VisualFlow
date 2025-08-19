//! Node operations for the window editor
//!
//! This module provides operations for manipulating window nodes,
//! including copy, cut, paste, delete, and duplicate operations.

use super::WindowNode;

/// Node clipboard for copy/cut/paste operations
#[derive(Debug, Clone)]
pub struct NodeClipboard {
    pub nodes: Vec<WindowNode>,
    pub operation: ClipboardOperation,
}

/// Clipboard operation type
#[derive(Debug, Clone, PartialEq)]
pub enum ClipboardOperation {
    Copy,
    Cut,
}

/// Node operations manager
pub struct NodeOperations {
    clipboard: Option<NodeClipboard>,
    next_id_counter: u32,
}

impl NodeOperations {
    /// Create a new node operations manager
    pub fn new() -> Self {
        Self {
            clipboard: None,
            next_id_counter: 1000, // Start from 1000 to avoid conflicts with existing IDs
        }
    }
    
    /// Generate a unique node ID
    pub fn generate_id(&mut self) -> String {
        self.next_id_counter += 1;
        format!("node_{}", self.next_id_counter)
    }
    
    /// Copy nodes to clipboard
    pub fn copy_nodes(&mut self, nodes: Vec<WindowNode>) {
        self.clipboard = Some(NodeClipboard {
            nodes,
            operation: ClipboardOperation::Copy,
        });
    }
    
    /// Cut nodes to clipboard
    pub fn cut_nodes(&mut self, nodes: Vec<WindowNode>) {
        self.clipboard = Some(NodeClipboard {
            nodes,
            operation: ClipboardOperation::Cut,
        });
    }
    
    /// Paste nodes from clipboard
    pub fn paste_nodes(&mut self) -> Option<Vec<WindowNode>> {
        if let Some(clipboard) = self.clipboard.take() {
            let mut pasted_nodes = Vec::new();
            let is_cut_operation = clipboard.operation == ClipboardOperation::Cut;
            
            for node in &clipboard.nodes {
                let mut new_node = node.clone();
                self.assign_new_ids(&mut new_node);
                
                // Offset position slightly to avoid overlap
                new_node.position[0] += 20.0;
                new_node.position[1] += 20.0;
                
                pasted_nodes.push(new_node);
            }
            
            // Restore clipboard if it was a copy operation
            if !is_cut_operation {
                self.clipboard = Some(clipboard);
            }
            
            Some(pasted_nodes)
        } else {
            None
        }
    }
    
    /// Check if clipboard has content
    pub fn has_clipboard_content(&self) -> bool {
        self.clipboard.is_some()
    }
    

    
    /// Duplicate a node
    pub fn duplicate_node(&mut self, node: &WindowNode) -> WindowNode {
        let mut duplicated = node.clone();
        self.assign_new_ids(&mut duplicated);
        
        // Offset position to avoid overlap
        duplicated.position[0] += 20.0;
        duplicated.position[1] += 20.0;
        
        // Update name to indicate it's a duplicate
        if !duplicated.name.contains("Copy") {
            duplicated.name = format!("{} Copy", duplicated.name);
        } else {
            // Find the next copy number
            let copy_count = self.count_copies(&duplicated.name);
            duplicated.name = format!("{} {}", duplicated.name, copy_count + 1);
        }
        
        duplicated
    }
    
    /// Assign new IDs to a node and all its children recursively
    fn assign_new_ids(&mut self, node: &mut WindowNode) {
        node.id = self.generate_id();
        
        for child in &mut node.children {
            self.assign_new_ids(child);
        }
    }
    
    /// Count existing copies of a node name
    fn count_copies(&self, name: &str) -> u32 {
        // Simple implementation - in a real scenario, you'd check against existing nodes
        if name.contains("Copy") {
            // Extract number from "Name Copy 2" format
            if let Some(last_space) = name.rfind(' ') {
                if let Ok(num) = name[last_space + 1..].parse::<u32>() {
                    return num;
                }
            }
            return 1;
        }
        0
    }
    
    /// Create a new container node
    pub fn create_container(&mut self, name: String, position: [f32; 2], size: [f32; 2]) -> WindowNode {
        WindowNode::new_container(name, position, size)
    }
    

}

impl Default for NodeOperations {
    fn default() -> Self {
        Self::new()
    }
}