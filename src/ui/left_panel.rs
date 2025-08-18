use eframe::egui;
use crate::{EguiCodeGeneratorApp, UiNode, FileSystemEntry};

impl EguiCodeGeneratorApp {
    pub fn render_left_panel(&mut self, ctx: &egui::Context) {
        // Calculate dynamic max_width to ensure proper panel priority
        let screen_width = ctx.screen_rect().width();
        let right_panel_min_width = if self.show_properties { 150.0 } else { 0.0 };
        let center_panel_min_width = 300.0; // Minimum width for editor area
        let max_left_width = screen_width - right_panel_min_width - center_panel_min_width - 20.0; // 20px margin
        
        // Left Panel with resizable node tree and file system sections
        egui::SidePanel::left("left_panel")
            .resizable(true)
            .default_width(280.0)
            .min_width(200.0)
            .max_width(max_left_width.max(200.0)) // Ensure max is at least min
            .show(ctx, |ui| {
                // Node Tree section (top part)
                egui::TopBottomPanel::top("node_tree_panel")
                    .resizable(true)
                    .default_height(ui.available_height() * 0.5)
                    .min_height(100.0)
                    .show_inside(ui, |ui| {
                        ui.vertical(|ui| {
                            // Node tree header
                            ui.horizontal(|ui| {
                                ui.label("🌳");
                                ui.heading(&self.localization.ui_text.node_tree_title);
                            });
                            
                            // Add widget button
                            ui.horizontal(|ui| {
                                if ui.button("🔧 Add Widget").clicked() {
                                    self.console_messages.push("Widget added".to_string());
                                }
                            });
                            
                            ui.separator();
                            
                            // Node tree content with scroll
                            egui::ScrollArea::vertical()
                                .id_source("node_tree_scroll")
                                .auto_shrink([false, false])
                                .show(ui, |ui| {
                                    Self::render_node_tree(ui, &mut self.selected_node, &self.node_tree);
                                });
                        });
                    });
                
                // File System section (bottom part - uses remaining space)
                egui::CentralPanel::default().show_inside(ui, |ui| {
                    ui.vertical(|ui| {
                        // File system header
                        ui.horizontal(|ui| {
                            ui.label("📁");
                            ui.heading(&self.localization.ui_text.file_system_title);
                        });
                        ui.separator();
                        
                        // File system content with scroll
                        egui::ScrollArea::vertical()
                            .id_source("file_system_scroll")
                            .auto_shrink([false, false])
                            .show(ui, |ui| {
                                self.render_file_system(ui);
                            });
                    });
                });
            });
    }

    pub fn render_file_system(&mut self, ui: &mut egui::Ui) {
        Self::render_file_system_entries(ui, &self.file_system_tree);
    }

    pub(crate) fn render_file_system_entries(ui: &mut egui::Ui, entries: &[FileSystemEntry]) {
        for entry in entries {
            match entry {
                FileSystemEntry::File { name } => {
                    if ui.selectable_label(false, format!("📄 {}", name)).clicked() {}
                }
                FileSystemEntry::Dir { name, children } => {
                    ui.collapsing(format!("📁 {}", name), |ui| {
                        Self::render_file_system_entries(ui, children);
                    });
                }
            }
        }
    }

    pub(crate) fn render_node_tree(ui: &mut egui::Ui, selected_node: &mut Option<String>, nodes: &[UiNode]) {
        for node in nodes {
            let is_selected = selected_node.as_ref() == Some(&node.id);
            if ui.selectable_label(is_selected, &format!("🔧 {}", node.name)).clicked() {
                *selected_node = Some(node.id.clone());
            }
            
            if !node.children.is_empty() {
                ui.indent(&node.id, |ui| {
                    Self::render_node_tree(ui, selected_node, &node.children);
                });
            }
        }
    }
}