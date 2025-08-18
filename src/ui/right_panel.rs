use eframe::egui;
use crate::{EguiCodeGeneratorApp, UiNode};

impl EguiCodeGeneratorApp {
    pub fn render_right_panel(&mut self, ctx: &egui::Context) {
        if self.show_properties {
            egui::SidePanel::right("properties_panel")
                .resizable(true)
                .default_width(250.0)
                .min_width(150.0)
                .show(ctx, |ui| {
                    ui.group(|ui| {
                        ui.heading(&self.localization.ui_text.properties_panel_title);
                        ui.separator();
                        
                        if let Some(selected_id) = &self.selected_node {
                            ui.label(&format!("Selected: {}", selected_id));
                            ui.separator();
                            
                            // Find the selected node and show its properties
                            if let Some(node) = self.find_node_by_id(&self.node_tree, selected_id) {
                                ui.label(&format!("Type: {}", node.node_type));
                                ui.separator();
                                
                                for (key, value) in &node.properties {
                                    ui.horizontal(|ui| {
                                        ui.label(&format!("{}:", key));
                                        ui.label(value);
                                    });
                                }
                            }
                        } else {
                            ui.label("No node selected");
                            ui.label("Select a node from the tree to edit its properties");
                        }
                    });
                });
        }
    }
    
    pub(crate) fn find_node_by_id<'a>(&self, nodes: &'a [UiNode], id: &str) -> Option<&'a UiNode> {
        for node in nodes {
            if node.id == id {
                return Some(node);
            }
            if let Some(found) = self.find_node_by_id(&node.children, id) {
                return Some(found);
            }
        }
        None
    }
}