use eframe::egui;
use crate::{EguiCodeGeneratorApp, EditorTab};

impl EguiCodeGeneratorApp {
    pub fn render_layout(&mut self, ctx: &egui::Context) {
        // Isolated layout manager with independent panel containers
        egui::CentralPanel::default().show(ctx, |ui| {
            let total_rect = ui.available_rect_before_wrap();
            let total_width = total_rect.width();
            let total_height = total_rect.height();
            let current_window_size = egui::Vec2::new(total_width, total_height);
            
            // Check if window size has changed and invalidate cache if needed
            if (current_window_size - self.last_window_size).length() > 1.0 {
                self.layout_cache_valid = false;
                self.center_panel_cache_valid = false;
                self.left_panel_cache_valid = false;
                self.last_window_size = current_window_size;
            }
            
            // Cache layout calculations to prevent recalculation jitter
            if !self.layout_cache_valid {
                self.update_layout_cache(total_width, total_height);
            }
            
            // Use cached fixed pixel values instead of dynamic ratios
            let left_width = self.cached_left_width;
            let right_width = self.cached_right_width;
            let center_width = self.cached_center_width;
            
            ui.horizontal(|ui| {
                // Left panel - completely isolated container
                if self.show_node_tree || self.show_file_system {
                    let left_rect = egui::Rect::from_min_size(
                        ui.cursor().min,
                        egui::Vec2::new(left_width, total_height)
                    );
                    
                    ui.allocate_ui_at_rect(left_rect, |ui| {
                        self.render_left_content_isolated(ui, left_width, total_height);
                    });
                    
                    // Left draggable separator with improved stability
                    self.render_vertical_separator_stable(ui, total_height, "left_separator", total_width);
                }
                
                // Center panel - isolated from left/right panel changes
                let center_rect = egui::Rect::from_min_size(
                    ui.cursor().min,
                    egui::Vec2::new(center_width, total_height)
                );
                
                ui.allocate_ui_at_rect(center_rect, |ui| {
                    self.render_center_content_isolated(ui, center_width, total_height);
                });
                
                // Right draggable separator and panel
                if self.show_properties {
                    self.render_vertical_separator_stable(ui, total_height, "right_separator", total_width);
                    
                    let right_rect = egui::Rect::from_min_size(
                        ui.cursor().min,
                        egui::Vec2::new(right_width, total_height)
                    );
                    
                    ui.allocate_ui_at_rect(right_rect, |ui| {
                        self.render_right_content_isolated(ui, right_width, total_height);
                    });
                }
            });
        });
    }
    
    // Layout cache update function with stable sizing strategy
    fn update_layout_cache(&mut self, total_width: f32, _total_height: f32) {
        let min_left_width = 200.0;
        let min_right_width = 150.0;
        let min_center_width = 300.0;
        
        // Preferred stable sizes for sidebars (not fixed, but stable)
        let preferred_left_width = 250.0;
        let preferred_right_width = 200.0;
        let stability_threshold = 20.0; // Tolerance for using preferred size
        
        // Calculate sidebar widths with stability priority
        self.cached_left_width = if self.show_node_tree || self.show_file_system {
            let ratio_width = total_width * self.left_panel_ratio;
            let stable_width = if (ratio_width - preferred_left_width).abs() < stability_threshold {
                // If ratio is close to preferred size, use preferred size for stability
                preferred_left_width
            } else {
                // Use ratio-based width when user has manually adjusted significantly
                ratio_width
            };
            stable_width.max(min_left_width).min(total_width - min_center_width - min_right_width).round()
        } else { 0.0 };
        
        self.cached_right_width = if self.show_properties {
            let ratio_width = total_width * self.right_panel_ratio;
            let stable_width = if (ratio_width - preferred_right_width).abs() < stability_threshold {
                // If ratio is close to preferred size, use preferred size for stability
                preferred_right_width
            } else {
                // Use ratio-based width when user has manually adjusted significantly
                ratio_width
            };
            stable_width.max(min_right_width).min(total_width - min_center_width - self.cached_left_width).round()
        } else { 0.0 };
        
        // Center width gets all remaining space (this is where scaling happens)
        self.cached_center_width = (total_width - self.cached_left_width - self.cached_right_width).max(min_center_width).round();
        self.layout_cache_valid = true;
    }
    
    // Improved vertical separator with anti-jitter mechanism
    fn render_vertical_separator_stable(&mut self, ui: &mut egui::Ui, height: f32, id: &str, total_width: f32) {
        let separator_width = 4.0;
        let (rect, response) = ui.allocate_exact_size(
            egui::Vec2::new(separator_width, height),
            egui::Sense::drag()
        );
        
        // Visual feedback
        if response.hovered() || response.dragged() {
            ui.painter().rect_filled(
                rect,
                egui::Rounding::same(2.0),
                ui.style().visuals.selection.bg_fill
            );
        } else {
            ui.painter().rect_filled(
                rect,
                egui::Rounding::same(1.0),
                ui.style().visuals.widgets.inactive.bg_fill
            );
        }
        
        // Handle dragging by aligning separator to mouse position
        if response.dragged() {
            if let Some(mouse_pos) = ui.ctx().pointer_latest_pos() {
                let relative_x = mouse_pos.x - ui.min_rect().min.x;
                
                match id {
                    "left_separator" => {
                        let new_ratio = relative_x / total_width;
                        let clamped_ratio = new_ratio.clamp(0.15, 0.4);
                        
                        if clamped_ratio != self.left_panel_ratio {
                            self.left_panel_ratio = clamped_ratio;
                            self.layout_cache_valid = false; // Invalidate cache
                        }
                    },
                    "right_separator" => {
                        let right_edge_x = total_width - relative_x;
                        let new_ratio = right_edge_x / total_width;
                        let clamped_ratio = new_ratio.clamp(0.15, 0.35);
                        
                        if clamped_ratio != self.right_panel_ratio {
                            self.right_panel_ratio = clamped_ratio;
                            self.layout_cache_valid = false; // Invalidate cache
                        }
                    },
                    _ => {}
                }
            }
        }
        
        // Change cursor when hovering
        if response.hovered() {
            ui.ctx().set_cursor_icon(egui::CursorIcon::ResizeHorizontal);
        }
    }
    

    
    // Isolated left panel rendering with fixed pixel calculations
    fn render_left_content_isolated(&mut self, ui: &mut egui::Ui, panel_width: f32, panel_height: f32) {
        // Cache left panel internal layout to prevent recalculation
        if !self.left_panel_cache_valid {
            self.update_left_panel_cache(panel_height);
        }
        
        ui.vertical(|ui| {
            // Node Tree section with cached height
            if self.show_node_tree {
                let node_rect = egui::Rect::from_min_size(
                    ui.cursor().min,
                    egui::Vec2::new(panel_width, self.cached_node_tree_height)
                );
                
                ui.allocate_ui_at_rect(node_rect, |ui| {
                    ui.vertical(|ui| {
                        ui.horizontal(|ui| {
                            ui.label("🌳");
                            ui.heading(&self.localization.ui_text.node_tree_title);
                        });
                        ui.separator();
                        
                        // Add Widget button for node tree
                        ui.horizontal(|ui| {
                            if ui.button("🔧 Add Widget").clicked() {
                                self.console_messages.push("Widget added from node tree".to_string());
                            }
                        });
                        ui.separator();
                        
                        egui::ScrollArea::vertical()
                            .id_source("node_tree_scroll")
                            .auto_shrink([false, false])
                            .show(ui, |ui| {
                                Self::render_node_tree(ui, &mut self.selected_node, &self.node_tree);
                            });
                    });
                });
                
                // Internal horizontal separator with enhanced stability
                if self.show_file_system {
                    self.render_left_panel_separator(ui, panel_width, panel_height);
                }
            }
            
            // File System section with cached height
            if self.show_file_system {
                let file_rect = egui::Rect::from_min_size(
                    ui.cursor().min,
                    egui::Vec2::new(panel_width, self.cached_file_system_height)
                );
                
                ui.allocate_ui_at_rect(file_rect, |ui| {
                    ui.vertical(|ui| {
                        ui.horizontal(|ui| {
                            ui.label("📁");
                            ui.heading(&self.localization.ui_text.file_system_title);
                        });
                        ui.separator();
                        
                        egui::ScrollArea::vertical()
                            .id_source("file_system_scroll")
                            .auto_shrink([false, false])
                            .show(ui, |ui| {
                                self.render_file_system(ui);
                            });
                    });
                });
            }
        });
    }
    
    // Update left panel internal layout cache
    fn update_left_panel_cache(&mut self, panel_height: f32) {
        let separator_height = 4.0;
        
        if self.show_node_tree && self.show_file_system {
            let node_height = (panel_height * self.left_split_ratio).max(100.0).min(panel_height - 100.0 - separator_height);
            self.cached_node_tree_height = node_height.round();
            self.cached_file_system_height = (panel_height - node_height - separator_height).max(100.0).round();
        } else if self.show_node_tree {
            self.cached_node_tree_height = panel_height;
            self.cached_file_system_height = 0.0;
        } else if self.show_file_system {
            self.cached_node_tree_height = 0.0;
            self.cached_file_system_height = panel_height;
        }
        
        self.left_panel_cache_valid = true;
    }
    
    // Enhanced left panel separator that doesn't affect global layout
    fn render_left_panel_separator(&mut self, ui: &mut egui::Ui, panel_width: f32, panel_height: f32) {
        let separator_height = 4.0;
        let (rect, response) = ui.allocate_exact_size(
            egui::Vec2::new(panel_width, separator_height),
            egui::Sense::drag()
        );
        
        // Visual feedback
        if response.hovered() || response.dragged() {
            ui.painter().rect_filled(
                rect,
                egui::Rounding::same(2.0),
                ui.style().visuals.selection.bg_fill
            );
        } else {
            ui.painter().rect_filled(
                rect,
                egui::Rounding::same(1.0),
                ui.style().visuals.widgets.inactive.bg_fill
            );
        }
        
        // Handle dragging by aligning separator to mouse position
        if response.dragged() {
            if let Some(mouse_pos) = ui.ctx().pointer_latest_pos() {
                let relative_y = mouse_pos.y - ui.min_rect().min.y;
                let new_ratio = relative_y / panel_height;
                let clamped_ratio = new_ratio.clamp(0.2, 0.8);
                
                if clamped_ratio != self.left_split_ratio {
                    self.left_split_ratio = clamped_ratio;
                    self.left_panel_cache_valid = false; // Invalidate internal cache only
                }
            }
        }
        
        // Change cursor when hovering
        if response.hovered() {
            ui.ctx().set_cursor_icon(egui::CursorIcon::ResizeVertical);
        }
    }
    
    // Isolated center panel rendering with fixed pixel calculations
    fn render_center_content_isolated(&mut self, ui: &mut egui::Ui, panel_width: f32, panel_height: f32) {
        // Cache center panel layout to prevent recalculation
        if !self.center_panel_cache_valid {
            self.update_center_panel_cache(panel_height);
        }
        
        ui.vertical(|ui| {
            // Editor area with cached height
            let editor_rect = egui::Rect::from_min_size(
                ui.cursor().min,
                egui::Vec2::new(panel_width, self.cached_editor_height)
            );
            
            ui.allocate_ui_at_rect(editor_rect, |ui| {
                ui.vertical(|ui| {

                    
                    // Tab system
                    ui.horizontal(|ui| {
                        ui.selectable_value(&mut self.current_tab, EditorTab::WindowEditor, "Window Editor");
                        ui.selectable_value(&mut self.current_tab, EditorTab::CodeEditor, "Code Editor");
                        ui.selectable_value(&mut self.current_tab, EditorTab::NodeEditor, "Node Editor");
                    });
                    
                    ui.separator();
                    
                    // Editor content
                    match self.current_tab {
                        EditorTab::WindowEditor => self.render_window_editor(ui),
                        EditorTab::CodeEditor => self.render_code_editor(ui),
                        EditorTab::NodeEditor => self.render_node_editor(ui),
                    }
                });
            });
            
            // Center panel separator with enhanced stability
            if self.show_console {
                self.render_center_panel_separator(ui, panel_width, panel_height);
                
                // Console area with cached height
                let console_rect = egui::Rect::from_min_size(
                    ui.cursor().min,
                    egui::Vec2::new(panel_width, self.cached_console_height)
                );
                
                ui.allocate_ui_at_rect(console_rect, |ui| {
                    ui.vertical(|ui| {
                        ui.horizontal(|ui| {
                            ui.label("📟");
                            ui.heading("Console");
                        });
                        ui.separator();
                        
                        // Use ScrollArea to prevent height changes due to content
                        egui::ScrollArea::vertical()
                            .id_source("console_scroll")
                            .auto_shrink([false, false])
                            .stick_to_bottom(true)
                            .show(ui, |ui| {
                                // Display console messages as individual labels for better formatting
                                for message in &self.console_messages {
                                    ui.horizontal(|ui| {
                                        ui.label(egui::RichText::new(message)
                                            .font(egui::FontId::monospace(12.0))
                                            .color(ui.style().visuals.text_color()));
                                    });
                                }
                                
                                // Add some padding at the bottom
                                ui.add_space(8.0);
                            });
                    });
                });
            }
        });
    }
    
    // Update center panel internal layout cache with stable console sizing
    fn update_center_panel_cache(&mut self, panel_height: f32) {
        let separator_height = 4.0;
        let min_console_height = 100.0;
        let min_editor_height = 200.0;
        
        // Preferred stable height for console
        let preferred_console_height = 150.0;
        let stability_threshold = 15.0; // Tolerance for using preferred size
        
        if self.show_console {
            let ratio_height = panel_height * self.console_ratio;
            let stable_height = if (ratio_height - preferred_console_height).abs() < stability_threshold {
                // If ratio is close to preferred size, use preferred size for stability
                preferred_console_height
            } else {
                // Use ratio-based height when user has manually adjusted significantly
                ratio_height
            };
            
            let console_height = stable_height.max(min_console_height).min(panel_height - min_editor_height - separator_height);
            self.cached_console_height = console_height.round();
            // Editor gets all remaining space (this is where scaling happens)
            self.cached_editor_height = (panel_height - console_height - separator_height).max(min_editor_height).round();
        } else {
            self.cached_editor_height = panel_height;
            self.cached_console_height = 0.0;
        }
        
        self.center_panel_cache_valid = true;
    }
    
    // Enhanced center panel separator that doesn't affect global layout
    fn render_center_panel_separator(&mut self, ui: &mut egui::Ui, panel_width: f32, panel_height: f32) {
        let separator_height = 4.0;
        let (rect, response) = ui.allocate_exact_size(
            egui::Vec2::new(panel_width, separator_height),
            egui::Sense::drag()
        );
        
        // Visual feedback
        if response.hovered() || response.dragged() {
            ui.painter().rect_filled(
                rect,
                egui::Rounding::same(2.0),
                ui.style().visuals.selection.bg_fill
            );
        } else {
            ui.painter().rect_filled(
                rect,
                egui::Rounding::same(1.0),
                ui.style().visuals.widgets.inactive.bg_fill
            );
        }
        
        // Handle dragging by aligning separator to mouse position
        if response.dragged() {
            if let Some(mouse_pos) = ui.ctx().pointer_latest_pos() {
                let relative_y = mouse_pos.y - ui.min_rect().min.y;
                let distance_from_bottom = panel_height - relative_y;
                let new_ratio = distance_from_bottom / panel_height;
                let clamped_ratio = new_ratio.clamp(0.1, 0.6);
                
                if clamped_ratio != self.console_ratio {
                    self.console_ratio = clamped_ratio;
                    self.center_panel_cache_valid = false; // Invalidate internal cache only
                }
            }
        }
        
        // Change cursor when hovering
        if response.hovered() {
            ui.ctx().set_cursor_icon(egui::CursorIcon::ResizeVertical);
        }
    }
    
    // Isolated right panel rendering with simplified layout
    fn render_right_content_isolated(&mut self, ui: &mut egui::Ui, panel_width: f32, panel_height: f32) {
        // Right panel uses a simple vertical layout: title -> separator -> content
        let content_rect = egui::Rect::from_min_size(
            ui.cursor().min,
            egui::Vec2::new(panel_width, panel_height)
        );
        
        ui.allocate_ui_at_rect(content_rect, |ui| {
            ui.vertical(|ui| {
                // Add padding from top
                ui.add_space(8.0);
                
                // Properties title
                ui.heading(&self.localization.ui_text.properties_panel_title);
                
                // Horizontal separator line
                ui.separator();
                
                // Add some space after separator
                ui.add_space(8.0);
                
                // Content area
                if let Some(selected_id) = &self.selected_node {
                    ui.label(&format!("Selected: {}", selected_id));
                    ui.add_space(4.0);
                    
                    if let Some(node) = self.find_node_by_id(&self.node_tree, selected_id) {
                        ui.label(&format!("Type: {}", node.node_type));
                        ui.add_space(8.0);
                        
                        // Node properties
                        for (key, value) in &node.properties {
                            ui.horizontal(|ui| {
                                ui.label(&format!("{}:", key));
                                ui.label(value);
                            });
                        }
                    }
                } else {
                    ui.label("No node selected");
                    ui.add_space(4.0);
                    ui.label("Select a node from the tree to edit its properties");
                }
            });
        });
    }
}