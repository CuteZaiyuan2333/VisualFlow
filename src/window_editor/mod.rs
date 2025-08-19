//! Window editor module for visual UI design
//!
//! This module provides the core data structures and functionality for the window editor,
//! allowing users to create and edit .vfwindow files with a visual interface.

use serde::{Deserialize, Serialize};
use std::collections::HashMap;

pub mod widget_library;
pub mod node_operations;

/// Window node type enumeration
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub enum NodeType {
    Window,      // Root window node
    Container,   // Empty container node
    Widget(WidgetType), // Component node
}

/// Widget type enumeration
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub enum WidgetType {
    Button,
    Label,
    TextInput,
    Panel,
    Image,
    Checkbox,
    RadioButton,
    Slider,
    ProgressBar,
    ComboBox,
    ListBox,
    Separator,
    Group,
    ScrollArea,
}

/// Window node data structure
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct WindowNode {
    pub id: String,
    pub name: String,
    pub node_type: NodeType,
    pub position: [f32; 2],
    pub size: [f32; 2],
    pub visible: bool,
    pub properties: HashMap<String, serde_json::Value>,
    pub children: Vec<WindowNode>,
}

/// Window document structure
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct WindowDocument {
    pub version: String,
    pub window_size: [f32; 2],
    pub root_node: WindowNode,
    pub metadata: HashMap<String, String>,
}

/// Widget information for the component library
#[derive(Debug, Clone)]
pub struct WidgetInfo {
    pub widget_type: WidgetType,
    pub name: String,
    pub description: String,
    pub icon: String,
    pub category: String,

}

impl WindowNode {

    
    /// Create a new container node
    pub fn new_container(name: String, position: [f32; 2], size: [f32; 2]) -> Self {
        let mut properties = HashMap::new();
        properties.insert("layout".to_string(), serde_json::Value::String("vertical".to_string()));
        properties.insert("padding".to_string(), serde_json::Value::Number(serde_json::Number::from(10)));
        
        Self {
            id: uuid::Uuid::new_v4().to_string(),
            name,
            node_type: NodeType::Container,
            position,
            size,
            visible: true,
            properties,
            children: Vec::new(),
        }
    }
    
    /// Create a new widget node
    pub fn new_widget(widget_type: WidgetType, name: String, position: [f32; 2], size: [f32; 2]) -> Self {
        let properties = widget_type.default_properties();
        
        Self {
            id: uuid::Uuid::new_v4().to_string(),
            name,
            node_type: NodeType::Widget(widget_type),
            position,
            size,
            visible: true,
            properties,
            children: Vec::new(),
        }
    }
    
    /// Find a node by ID recursively
    pub fn find_node_by_id(&self, id: &str) -> Option<&WindowNode> {
        if self.id == id {
            return Some(self);
        }
        
        for child in &self.children {
            if let Some(found) = child.find_node_by_id(id) {
                return Some(found);
            }
        }
        
        None
    }
    
    /// Find a node by ID recursively (mutable)
    pub fn find_node_by_id_mut(&mut self, id: &str) -> Option<&mut WindowNode> {
        if self.id == id {
            return Some(self);
        }
        
        for child in &mut self.children {
            if let Some(found) = child.find_node_by_id_mut(id) {
                return Some(found);
            }
        }
        
        None
    }
    
    /// Add a child node
    pub fn add_child(&mut self, child: WindowNode) {
        self.children.push(child);
    }
    
    /// Remove a child node by ID
    pub fn remove_child(&mut self, id: &str) -> bool {
        if let Some(pos) = self.children.iter().position(|child| child.id == id) {
            self.children.remove(pos);
            return true;
        }
        
        for child in &mut self.children {
            if child.remove_child(id) {
                return true;
            }
        }
        
        false
    }
}

impl WidgetType {
    /// Get the display name for this widget type
    pub fn display_name(&self) -> &str {
        match self {
            WidgetType::Button => "Button",
            WidgetType::Label => "Label",
            WidgetType::TextInput => "Text Input",
            WidgetType::Panel => "Panel",
            WidgetType::Image => "Image",
            WidgetType::Checkbox => "Checkbox",
            WidgetType::RadioButton => "Radio Button",
            WidgetType::Slider => "Slider",
            WidgetType::ProgressBar => "Progress Bar",
            WidgetType::ComboBox => "Combo Box",
            WidgetType::ListBox => "List Box",
            WidgetType::Separator => "Separator",
            WidgetType::Group => "Group",
            WidgetType::ScrollArea => "Scroll Area",
        }
    }
    
    /// Get the icon for this widget type
    pub fn icon(&self) -> &str {
        match self {
            WidgetType::Button => "🔘",
            WidgetType::Label => "🏷️",
            WidgetType::TextInput => "📝",
            WidgetType::Panel => "📋",
            WidgetType::Image => "🖼️",
            WidgetType::Checkbox => "☑️",
            WidgetType::RadioButton => "🔘",
            WidgetType::Slider => "🎚️",
            WidgetType::ProgressBar => "📊",
            WidgetType::ComboBox => "📋",
            WidgetType::ListBox => "📋",
            WidgetType::Separator => "➖",
            WidgetType::Group => "📦",
            WidgetType::ScrollArea => "📜",
        }
    }
    
    /// Get the category for this widget type
    pub fn category(&self) -> &str {
        match self {
            WidgetType::Button | WidgetType::Checkbox | WidgetType::RadioButton => "Input",
            WidgetType::Label | WidgetType::Image | WidgetType::ProgressBar => "Display",
            WidgetType::TextInput | WidgetType::ComboBox | WidgetType::ListBox | WidgetType::Slider => "Input",
            WidgetType::Panel | WidgetType::Group | WidgetType::ScrollArea | WidgetType::Separator => "Layout",
        }
    }
    
    /// Get default properties for this widget type
    pub fn default_properties(&self) -> HashMap<String, serde_json::Value> {
        let mut props = HashMap::new();
        
        match self {
            WidgetType::Button => {
                props.insert("text".to_string(), serde_json::Value::String("Button".to_string()));
                props.insert("enabled".to_string(), serde_json::Value::Bool(true));
                props.insert("background_color".to_string(), serde_json::Value::String("#0078d4".to_string()));
                props.insert("text_color".to_string(), serde_json::Value::String("#ffffff".to_string()));
            }
            WidgetType::Label => {
                props.insert("text".to_string(), serde_json::Value::String("Label".to_string()));
                props.insert("font_size".to_string(), serde_json::Value::Number(serde_json::Number::from(14)));
                props.insert("text_color".to_string(), serde_json::Value::String("#000000".to_string()));
                props.insert("alignment".to_string(), serde_json::Value::String("left".to_string()));
            }
            WidgetType::TextInput => {
                props.insert("placeholder".to_string(), serde_json::Value::String("Enter text...".to_string()));
                props.insert("multiline".to_string(), serde_json::Value::Bool(false));
                props.insert("readonly".to_string(), serde_json::Value::Bool(false));
                props.insert("password".to_string(), serde_json::Value::Bool(false));
            }
            WidgetType::Panel => {
                props.insert("background_color".to_string(), serde_json::Value::String("#f0f0f0".to_string()));
                props.insert("border_color".to_string(), serde_json::Value::String("#cccccc".to_string()));
                props.insert("border_width".to_string(), serde_json::Value::Number(serde_json::Number::from(1)));
            }
            WidgetType::Checkbox => {
                props.insert("text".to_string(), serde_json::Value::String("Checkbox".to_string()));
                props.insert("checked".to_string(), serde_json::Value::Bool(false));
                props.insert("enabled".to_string(), serde_json::Value::Bool(true));
            }
            WidgetType::Slider => {
                props.insert("min_value".to_string(), serde_json::Value::Number(serde_json::Number::from(0)));
                props.insert("max_value".to_string(), serde_json::Value::Number(serde_json::Number::from(100)));
                props.insert("value".to_string(), serde_json::Value::Number(serde_json::Number::from(50)));
                props.insert("step".to_string(), serde_json::Value::Number(serde_json::Number::from(1)));
            }
            _ => {
                // Default properties for other widget types
                props.insert("enabled".to_string(), serde_json::Value::Bool(true));
            }
        }
        
        props
    }
}

impl WindowDocument {

    
    /// Load from JSON string
    pub fn from_json(json: &str) -> Result<Self, serde_json::Error> {
        serde_json::from_str(json)
    }
    
    /// Save to JSON string
    pub fn to_json(&self) -> Result<String, serde_json::Error> {
        serde_json::to_string_pretty(self)
    }
    
    /// Update the modified timestamp
    pub fn update_modified_time(&mut self) {
        self.metadata.insert("modified_at".to_string(), chrono::Utc::now().to_rfc3339());
    }
}