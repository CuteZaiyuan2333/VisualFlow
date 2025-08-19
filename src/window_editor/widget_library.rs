//! Widget library for the window editor
//!
//! This module provides the widget library dialog and widget management functionality.

use super::{WidgetType, WidgetInfo};
use std::collections::HashMap;

/// Widget library manager
pub struct WidgetLibrary {
    widgets: Vec<WidgetInfo>,
    categories: HashMap<String, Vec<WidgetType>>,
    search_filter: String,
    selected_category: Option<String>,
}

impl WidgetLibrary {
    /// Create a new widget library
    pub fn new() -> Self {
        let mut library = Self {
            widgets: Vec::new(),
            categories: HashMap::new(),
            search_filter: String::new(),
            selected_category: None,
        };
        
        library.initialize_widgets();
        library
    }
    
    /// Initialize the widget library with default widgets
    fn initialize_widgets(&mut self) {
        let widget_types = [
            WidgetType::Button,
            WidgetType::Label,
            WidgetType::TextInput,
            WidgetType::Panel,
            WidgetType::Image,
            WidgetType::Checkbox,
            WidgetType::RadioButton,
            WidgetType::Slider,
            WidgetType::ProgressBar,
            WidgetType::ComboBox,
            WidgetType::ListBox,
            WidgetType::Separator,
            WidgetType::Group,
            WidgetType::ScrollArea,
        ];
        
        for widget_type in widget_types {
            let widget_info = WidgetInfo {
                name: widget_type.display_name().to_string(),
                description: self.get_widget_description(&widget_type),
                icon: widget_type.icon().to_string(),
                category: widget_type.category().to_string(),
                widget_type: widget_type.clone(),
            };
            
            self.widgets.push(widget_info);
            
            // Add to category
            let category = widget_type.category().to_string();
            self.categories.entry(category).or_insert_with(Vec::new).push(widget_type);
        }
    }
    
    /// Get widget description
    fn get_widget_description(&self, widget_type: &WidgetType) -> String {
        match widget_type {
            WidgetType::Button => "A clickable button widget".to_string(),
            WidgetType::Label => "A text label for displaying information".to_string(),
            WidgetType::TextInput => "An input field for text entry".to_string(),
            WidgetType::Panel => "A container panel for grouping widgets".to_string(),
            WidgetType::Image => "An image display widget".to_string(),
            WidgetType::Checkbox => "A checkbox for boolean input".to_string(),
            WidgetType::RadioButton => "A radio button for single selection".to_string(),
            WidgetType::Slider => "A slider for numeric input".to_string(),
            WidgetType::ProgressBar => "A progress bar for showing completion".to_string(),
            WidgetType::ComboBox => "A dropdown selection box".to_string(),
            WidgetType::ListBox => "A list selection widget".to_string(),
            WidgetType::Separator => "A visual separator line".to_string(),
            WidgetType::Group => "A visual grouping container".to_string(),
            WidgetType::ScrollArea => "A scrollable container area".to_string(),
        }
    }
    
    /// Get widgets by category
    pub fn get_widgets_by_category(&self, category: &str) -> Vec<&WidgetInfo> {
        self.widgets.iter()
            .filter(|widget| widget.category == category)
            .collect()
    }
    
    /// Get all categories
    pub fn get_categories(&self) -> Vec<&String> {
        self.categories.keys().collect()
    }
    
    /// Set search filter
    pub fn set_search_filter(&mut self, filter: String) {
        self.search_filter = filter;
    }
    
    /// Get search filter
    pub fn get_search_filter(&self) -> &str {
        &self.search_filter
    }
    
    /// Set selected category
    pub fn set_selected_category(&mut self, category: Option<String>) {
        self.selected_category = category;
    }
    
    /// Get selected category
    pub fn get_selected_category(&self) -> &Option<String> {
        &self.selected_category
    }
    
    /// Get filtered widgets based on current search and category filters
    pub fn get_filtered_widgets(&self) -> Vec<&WidgetInfo> {
        let mut widgets = if let Some(category) = &self.selected_category {
            self.get_widgets_by_category(category)
        } else {
            self.widgets.iter().collect()
        };
        
        if !self.search_filter.is_empty() {
            let query_lower = self.search_filter.to_lowercase();
            widgets.retain(|widget| {
                widget.name.to_lowercase().contains(&query_lower) ||
                widget.description.to_lowercase().contains(&query_lower)
            });
        }
        
        widgets
    }
}

impl Default for WidgetLibrary {
    fn default() -> Self {
        Self::new()
    }
}