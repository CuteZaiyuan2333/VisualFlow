use eframe::egui;
use crate::{EguiCodeGeneratorApp, EditorTab, GraphNode, NodePort};

impl EguiCodeGeneratorApp {
    pub fn render_center_panel(&mut self, ctx: &egui::Context) {
        // Main editor area with console inside (shares right boundary with right panel)
        egui::CentralPanel::default().show(ctx, |ui| {
            ui.vertical(|ui| {
                let total_height = ui.available_height();
                let console_height = if self.show_console { total_height * 0.3 } else { 0.0 };
                let editor_height = total_height - console_height;
                
                // Editor area (top part)
                ui.allocate_ui_with_layout(
                    egui::Vec2::new(ui.available_width(), editor_height),
                    egui::Layout::top_down(egui::Align::LEFT),
                    |ui| {

                        
                        // Tab system
                        ui.horizontal(|ui| {
                            ui.selectable_value(&mut self.current_tab, EditorTab::WindowEditor, "Window Editor");
                            ui.selectable_value(&mut self.current_tab, EditorTab::CodeEditor, "Code Editor");
                            ui.selectable_value(&mut self.current_tab, EditorTab::NodeEditor, "Node Editor");
                        });
                        
                        ui.separator();
                        
                        // Editor content area
                        match self.current_tab {
                            EditorTab::WindowEditor => self.render_window_editor(ui),
                            EditorTab::CodeEditor => self.render_code_editor(ui),
                            EditorTab::NodeEditor => self.render_node_editor(ui),
                        }
                    },
                );
                
                // Resizable separator between editor and console
                if self.show_console {
                    ui.separator();
                }
                
                // Console area (bottom part)
                if self.show_console {
                    ui.allocate_ui_with_layout(
                        egui::Vec2::new(ui.available_width(), console_height.max(100.0)),
                        egui::Layout::top_down(egui::Align::LEFT),
                        |ui| {
                            // Console header
                            ui.horizontal(|ui| {
                                ui.label("📟");
                                ui.heading("Console");
                            });
                            ui.separator();
                            
                            // Console output area with readonly text edit that fills available height
                            let console_text = self.console_messages.join("\n");
                            let mut console_text_copy = console_text.clone();
                            
                            ui.add_sized(
                                [ui.available_width(), ui.available_height()],
                                egui::TextEdit::multiline(&mut console_text_copy)
                                    .interactive(false)
                                    .font(egui::TextStyle::Monospace)
                            );
                        },
                    );
                }
            });
        });
    }
    
    pub fn render_window_editor(&mut self, ui: &mut egui::Ui) {
        ui.allocate_ui_with_layout(
            ui.available_size(),
            egui::Layout::top_down(egui::Align::LEFT),
            |ui| {
                // Toolbar
                ui.horizontal(|ui| {
                    ui.label("Window Editor Tools:");
                    ui.separator();
                    
                    // File operations
                    if ui.button("💾 Save").clicked() {
                        self.save_window_document();
                    }
                    
                    if ui.button("📁 Save As...").clicked() {
                        self.save_window_document_as();
                    }
                    
                    ui.separator();
                    
                    if ui.button("🔧 Add Widget").clicked() {
                        self.show_widget_library = true;
                    }
                    
                    ui.separator();
                    ui.label(format!("Zoom: {:.1}%", self.window_editor_zoom * 100.0));
                    
                    // Show current file name
                    if let Some(path) = &self.current_window_file_path {
                        ui.separator();
                        ui.label(format!("File: {}", path.file_name().unwrap_or_default().to_string_lossy()));
                    }
                });
                
                ui.separator();
                
                // Check if we have a window document loaded
                if let Some(_document) = &self.current_window_document {
                    // Create a custom area for the window editor that fills the remaining space
                    let available_rect = ui.available_rect_before_wrap();
                    let canvas_rect = available_rect;
                    
                    // Use a child UI with proper clipping to prevent overflow
                    ui.allocate_new_ui(egui::UiBuilder::new().max_rect(canvas_rect), |ui| {
                        // Set clipping to prevent drawing outside canvas bounds
                        ui.set_clip_rect(canvas_rect);
                        
                        // Handle interactions first to capture input
                        let response = ui.allocate_rect(canvas_rect, egui::Sense::click_and_drag());
                        self.handle_window_editor_interactions(ui, &response, canvas_rect);
                        
                        // Draw background (light gray) - lowest layer
                        let painter = ui.painter();
                        painter.rect_filled(
                            canvas_rect,
                            egui::Rounding::ZERO,
                            egui::Color32::from_gray(240)
                        );
                        
                        // Draw grid (gray dots) - second layer
                        self.draw_window_editor_grid(ui, canvas_rect);
                        
                        // Draw window content based on the loaded document
                        self.draw_window_from_document(ui, canvas_rect);
                    });
                } else {
                    // Show message when no window file is loaded
                    ui.vertical_centered(|ui| {
                        ui.add_space(50.0);
                        ui.heading("🪟 Window Editor");
                        ui.add_space(20.0);
                        ui.label("Please open a .vfwindow file to start editing.");
                        ui.add_space(10.0);
                        ui.label("You can:");
                        ui.label("• Double-click a .vfwindow file in the file system");
                        ui.label("• Create a new .vfwindow file from the file system context menu");
                    });
                }
                
                // Handle widget library dialog
                self.handle_widget_library_dialog(ui.ctx());
            },
        );
    }
    
    pub fn render_code_editor(&mut self, ui: &mut egui::Ui) {
        // Handle keyboard shortcuts
        let mut save_triggered = false;
        let mut open_triggered = false;
        let mut new_triggered = false;
        
        ui.input(|i| {
            if i.modifiers.ctrl {
                if i.key_pressed(egui::Key::S) {
                    save_triggered = true;
                }
                if i.key_pressed(egui::Key::O) {
                    open_triggered = true;
                }
                if i.key_pressed(egui::Key::N) {
                    new_triggered = true;
                }
            }
        });
        
        ui.group(|ui| {
            // Header with file name and status
            ui.horizontal(|ui| {
                ui.label("Code Editor");
                ui.separator();
                
                if let Some(path) = &self.current_file_path {
                    ui.label(format!("File: {}", path.file_name().unwrap_or_default().to_string_lossy()));
                } else {
                    ui.label("Untitled");
                }
                
                ui.with_layout(egui::Layout::right_to_left(egui::Align::Center), |ui| {
                    if ui.small_button("💾").on_hover_text("Save (Ctrl+S)").clicked() || save_triggered {
                        if let Some(path) = &self.current_file_path.clone() {
                            match std::fs::write(path, &self.code_content) {
                                Ok(_) => {
                                    self.console_messages.push(format!("Saved file: {}", path.display()));
                                },
                                Err(e) => {
                                    self.console_messages.push(format!("Failed to save file: {}", e));
                                }
                            }
                        } else {
                            // If no file path, trigger save as
                            if let Some(path) = rfd::FileDialog::new()
                                .set_title("Save File As")
                                .add_filter("Text files", &["txt", "rs", "py", "js", "ts", "json", "md", "toml", "yaml", "yml"])
                                .add_filter("All files", &["*"])
                                .save_file() {
                                match std::fs::write(&path, &self.code_content) {
                                    Ok(_) => {
                                        self.current_file_path = Some(path.clone());
                                        self.console_messages.push(format!("Saved file as: {}", path.display()));
                                    },
                                    Err(e) => {
                                        self.console_messages.push(format!("Failed to save file: {}", e));
                                    }
                                }
                            }
                        }
                    }
                    
                    if ui.small_button("📁").on_hover_text("Open File (Ctrl+O)").clicked() || open_triggered {
                        if let Some(path) = rfd::FileDialog::new()
                            .set_title("Open File")
                            .add_filter("Text files", &["txt", "rs", "py", "js", "ts", "json", "md", "toml", "yaml", "yml"])
                            .add_filter("All files", &["*"])
                            .pick_file() {
                            match std::fs::read_to_string(&path) {
                                Ok(content) => {
                                    self.code_content = content;
                                    self.current_file_path = Some(path.clone());
                                    self.console_messages.push(format!("Opened file: {}", path.display()));
                                },
                                Err(e) => {
                                    self.console_messages.push(format!("Failed to open file: {}", e));
                                }
                            }
                        }
                    }
                    
                    if ui.small_button("📄").on_hover_text("New File (Ctrl+N)").clicked() || new_triggered {
                        self.current_file_path = None;
                        self.code_content = String::new();
                        self.console_messages.push("New file created".to_string());
                    }
                });
            });
            
            ui.separator();
            
            // Fill the available space with the text editor
            let available_rect = ui.available_rect_before_wrap();
            ui.add_sized(
                [available_rect.width(), available_rect.height()],
                egui::TextEdit::multiline(&mut self.code_content)
                    .font(egui::TextStyle::Monospace)
                    .code_editor()
            );
        });
    }
    
    pub fn render_node_editor(&mut self, ui: &mut egui::Ui) {
        ui.allocate_ui_with_layout(
            ui.available_size(),
            egui::Layout::top_down(egui::Align::LEFT),
            |ui| {
                // Toolbar
                ui.horizontal(|ui| {
                    ui.label("Node Editor Tools:");
                    ui.separator();
                    
                    let snap_button_text = if self.node_graph.snap_to_grid {
                        "🔒 Snap to Grid: ON"
                    } else {
                        "🔓 Snap to Grid: OFF"
                    };
                    
                    if ui.button(snap_button_text).clicked() {
                        self.node_graph.snap_to_grid = !self.node_graph.snap_to_grid;
                    }
                    
                    ui.separator();
                    ui.label(format!("Zoom: {:.1}%", self.node_graph.canvas_zoom * 100.0));
                });
                
                ui.separator();
                
                // Create a custom area for the node graph that fills the remaining space
                let available_rect = ui.available_rect_before_wrap();
                let canvas_rect = available_rect;
                
                // Use a child UI with proper clipping to prevent overflow
                ui.allocate_new_ui(egui::UiBuilder::new().max_rect(canvas_rect), |ui| {
                    // Set clipping to prevent drawing outside canvas bounds
                    ui.set_clip_rect(canvas_rect);
                    
                    // Handle interactions first to capture input
                    let response = ui.allocate_rect(canvas_rect, egui::Sense::click_and_drag());
                    self.handle_canvas_interactions(ui, &response, canvas_rect);
                    
                    // Draw background (white) - lowest layer
                    let painter = ui.painter();
                    painter.rect_filled(
                        canvas_rect,
                        egui::Rounding::ZERO,
                        egui::Color32::WHITE
                    );
                    
                    // Draw grid (gray dots) - second layer
                    self.draw_grid(ui, canvas_rect);
                    
                    // Draw connections - third layer
                    self.draw_connections(ui, canvas_rect);
                    
                    // Draw nodes - fourth layer
                    self.draw_nodes(ui, canvas_rect);
                    
                    // Draw selection box if active - top layer
                    if let Some(selection_box) = &self.node_graph.selection_box {
                        let painter = ui.painter();
                        painter.rect_stroke(
                            *selection_box,
                            egui::Rounding::ZERO,
                            egui::Stroke::new(1.0, egui::Color32::from_rgb(100, 150, 255))
                        );
                        painter.rect_filled(
                            *selection_box,
                            egui::Rounding::ZERO,
                            egui::Color32::from_rgba_unmultiplied(100, 150, 255, 30)
                        );
                    }
                    
                    // Handle context menu - overlay layer
                    self.handle_context_menu(ui, &response, canvas_rect);
                });
            },
        );
    }
     
     // Widget library dialog handler
     fn handle_widget_library_dialog(&mut self, ctx: &egui::Context) {
         if self.show_widget_library {
             let mut close_dialog = false;
             let mut selected_widget: Option<crate::window_editor::WidgetType> = None;
             
             egui::Window::new("Widget Library")
                 .collapsible(false)
                 .resizable(true)
                 .default_size([400.0, 500.0])
                 .anchor(egui::Align2::CENTER_CENTER, egui::vec2(0.0, 0.0))
                 .show(ctx, |ui| {
                     ui.vertical(|ui| {
                         // Search box
                         ui.horizontal(|ui| {
                             ui.label("Search:");
                             let mut search_text = self.widget_library.get_search_filter().to_string();
                             if ui.text_edit_singleline(&mut search_text).changed() {
                                 self.widget_library.set_search_filter(search_text);
                             }
                         });
                         
                         ui.separator();
                         
                         // Category filter
                          ui.horizontal(|ui| {
                              ui.label("Category:");
                              let categories: Vec<String> = self.widget_library.get_categories().into_iter().cloned().collect();
                              let current_category = self.widget_library.get_selected_category().clone();
                              let mut selected_category = current_category.clone();
                              
                              egui::ComboBox::from_label("")
                                  .selected_text(current_category.as_deref().unwrap_or("All"))
                                  .show_ui(ui, |ui| {
                                      if ui.selectable_value(&mut selected_category, None, "All").clicked() {
                                          // Will be handled after the closure
                                      }
                                      for category in &categories {
                                          if ui.selectable_value(&mut selected_category, Some(category.clone()), category).clicked() {
                                              // Will be handled after the closure
                                          }
                                      }
                                  });
                              
                              // Update the widget library after the closure
                              if selected_category != current_category {
                                  self.widget_library.set_selected_category(selected_category);
                              }
                          });
                         
                         ui.separator();
                         
                         // Widget grid
                         egui::ScrollArea::vertical()
                             .auto_shrink([false, false])
                             .show(ui, |ui| {
                                 let filtered_widgets = self.widget_library.get_filtered_widgets();
                                 
                                 ui.columns(3, |columns| {
                                     for (i, widget) in filtered_widgets.iter().enumerate() {
                                         let col = i % 3;
                                         columns[col].vertical(|ui| {
                                             ui.group(|ui| {
                                                 ui.set_min_size([100.0, 80.0].into());
                                                 ui.vertical_centered(|ui| {
                                                     ui.label(&widget.icon);
                                                     ui.label(&widget.name);
                                                     if ui.small_button("Add").clicked() {
                                                         selected_widget = Some(widget.widget_type.clone());
                                                         close_dialog = true;
                                                     }
                                                 });
                                                 ui.label(&widget.description);
                                             });
                                         });
                                     }
                                 });
                             });
                         
                         ui.separator();
                         
                         // Dialog buttons
                         ui.horizontal(|ui| {
                             ui.with_layout(egui::Layout::right_to_left(egui::Align::Center), |ui| {
                                 if ui.button("Cancel").clicked() {
                                     close_dialog = true;
                                 }
                             });
                         });
                     });
                 });
             
             // Handle widget selection
             if let Some(widget_type) = selected_widget {
                 self.add_widget_to_window(widget_type);
             }
             
             // Close dialog if requested
             if close_dialog {
                 self.show_widget_library = false;
             }
         }
     }
     
     fn add_widget_to_window(&mut self, widget_type: crate::window_editor::WidgetType) {
          if let Some(document) = &mut self.current_window_document {
              let new_widget = crate::window_editor::WindowNode::new_widget(
                  widget_type.clone(),
                  format!("New {}", widget_type.display_name()),
                  [100.0, 100.0], // Default position
                  [100.0, 30.0]   // Default size
              );
              
              document.root_node.add_child(new_widget);
              document.update_modified_time();
              
              self.console_messages.push(format!("Added {} widget to window", widget_type.display_name()));
          } else {
              self.console_messages.push("No window document loaded".to_string());
          }
      }
      
      fn save_window_document(&mut self) {
          if let Some(document) = &mut self.current_window_document {
              if let Some(path) = &self.current_window_file_path {
                  // Save to existing file
                  document.update_modified_time();
                  match document.to_json() {
                      Ok(json_content) => {
                          match std::fs::write(path, json_content) {
                              Ok(()) => {
                                  self.console_messages.push(format!("Saved window file: {}", path.display()));
                              }
                              Err(e) => {
                                  self.console_messages.push(format!("Failed to save window file: {}", e));
                              }
                          }
                      }
                      Err(e) => {
                          self.console_messages.push(format!("Failed to serialize window document: {}", e));
                      }
                  }
              } else {
                  // No file path, trigger save as
                  self.save_window_document_as();
              }
          } else {
              self.console_messages.push("No window document to save".to_string());
          }
      }
      
      fn save_window_document_as(&mut self) {
          if let Some(document) = &mut self.current_window_document {
              if let Some(path) = rfd::FileDialog::new()
                  .set_title("Save Window File As")
                  .add_filter("Window files", &["vfwindow"])
                  .add_filter("All files", &["*"])
                  .save_file() {
                  
                  document.update_modified_time();
                  match document.to_json() {
                      Ok(json_content) => {
                          match std::fs::write(&path, json_content) {
                              Ok(()) => {
                                  self.current_window_file_path = Some(path.clone());
                                  self.console_messages.push(format!("Saved window file as: {}", path.display()));
                              }
                              Err(e) => {
                                  self.console_messages.push(format!("Failed to save window file: {}", e));
                              }
                          }
                      }
                      Err(e) => {
                          self.console_messages.push(format!("Failed to serialize window document: {}", e));
                      }
                  }
              }
          } else {
              self.console_messages.push("No window document to save".to_string());
          }
      }
     
     // Window editor helper functions
     fn handle_window_editor_interactions(&mut self, ui: &mut egui::Ui, response: &egui::Response, canvas_rect: egui::Rect) {
         let _offset = self.window_editor_offset;
         let _zoom = self.window_editor_zoom;
         
         // Handle zoom with mouse wheel (centered on mouse position)
         if response.hovered() {
             let scroll_delta = ui.input(|i| i.smooth_scroll_delta.y);
             if scroll_delta != 0.0 {
                 if let Some(mouse_pos) = ui.input(|i| i.pointer.hover_pos()) {
                     let zoom_factor = 1.0 + scroll_delta * 0.001;
                     let new_zoom = (self.window_editor_zoom * zoom_factor).clamp(0.1, 3.0);
                     
                     // Calculate mouse position in canvas coordinates before zoom
                     let mouse_canvas_pos_before = egui::Vec2::new(
                         (mouse_pos.x - canvas_rect.min.x - self.window_editor_offset.x) / self.window_editor_zoom,
                         (mouse_pos.y - canvas_rect.min.y - self.window_editor_offset.y) / self.window_editor_zoom
                     );
                     
                     // Update zoom
                     self.window_editor_zoom = new_zoom;
                     
                     // Recompute offset so that the world point under the cursor stays fixed
                     let new_offset = egui::Vec2::new(
                         mouse_pos.x - canvas_rect.min.x - mouse_canvas_pos_before.x * self.window_editor_zoom,
                         mouse_pos.y - canvas_rect.min.y - mouse_canvas_pos_before.y * self.window_editor_zoom
                     );
                     self.window_editor_offset = new_offset;
                 }
             }
         }

         // Handle middle mouse button panning
         if response.drag_started_by(egui::PointerButton::Middle) {
             if let Some(start_pos) = response.interact_pointer_pos() {
                 self.window_editor_is_panning = true;
                 self.window_editor_pan_start_pos = Some(start_pos);
                 self.window_editor_pan_start_offset = Some(self.window_editor_offset);
             }
         }

         if response.dragged_by(egui::PointerButton::Middle) && self.window_editor_is_panning {
             if let (Some(start_pos), Some(start_offset)) = (self.window_editor_pan_start_pos, self.window_editor_pan_start_offset) {
                 if let Some(current_pos) = response.interact_pointer_pos() {
                     let delta = current_pos - start_pos;
                     self.window_editor_offset = start_offset + delta;
                 }
             }
         }

         if response.drag_stopped_by(egui::PointerButton::Middle) {
             self.window_editor_is_panning = false;
             self.window_editor_pan_start_pos = None;
             self.window_editor_pan_start_offset = None;
         }
         
         // Handle left click for logging
         if response.clicked_by(egui::PointerButton::Primary) {
             self.console_messages.push("Window editor canvas clicked".to_string());
         }
     }
     
     fn draw_window_editor_grid(&self, ui: &mut egui::Ui, canvas_rect: egui::Rect) {
         let base_grid_size = 20.0;
         let offset = self.window_editor_offset;
         let zoom = self.window_editor_zoom;
         
         let painter = ui.painter();
         let grid_color = egui::Color32::from_gray(200); // Light gray for dots
         
         // Window editor coordinate system origin (relative to canvas_rect)
         let editor_origin = egui::Vec2::new(50.0, 50.0);
         
         // Calculate the visible world bounds (accounting for editor origin)
         let world_min_x = -(offset.x + editor_origin.x) / zoom;
         let world_min_y = -(offset.y + editor_origin.y) / zoom;
         let world_max_x = (canvas_rect.width() - offset.x - editor_origin.x) / zoom;
         let world_max_y = (canvas_rect.height() - offset.y - editor_origin.y) / zoom;
         
         // Find the first grid positions in world coordinates
         let start_grid_x = ((world_min_x / base_grid_size).floor() - 1.0) * base_grid_size;
         let start_grid_y = ((world_min_y / base_grid_size).floor() - 1.0) * base_grid_size;
         let end_grid_x = ((world_max_x / base_grid_size).ceil() + 1.0) * base_grid_size;
         let end_grid_y = ((world_max_y / base_grid_size).ceil() + 1.0) * base_grid_size;
         
         // Draw grid dots
         let mut world_x = start_grid_x;
         while world_x <= end_grid_x {
             let mut world_y = start_grid_y;
             while world_y <= end_grid_y {
                 // Transform world coordinates to screen coordinates (with editor origin)
                 let screen_x = canvas_rect.min.x + editor_origin.x + world_x * zoom + offset.x;
                 let screen_y = canvas_rect.min.y + editor_origin.y + world_y * zoom + offset.y;
                 let dot_pos = egui::Pos2::new(screen_x, screen_y);
                 
                 // Only draw if within canvas bounds
                 if canvas_rect.contains(dot_pos) {
                     painter.circle_filled(
                         dot_pos,
                         1.0 * zoom.max(0.5), // Scale dot size with zoom, minimum 0.5
                         grid_color
                     );
                 }
                 world_y += base_grid_size;
             }
             world_x += base_grid_size;
         }
     }
     
     fn draw_window_from_document(&mut self, ui: &mut egui::Ui, canvas_rect: egui::Rect) {
         if let Some(document) = self.current_window_document.clone() {
             self.draw_window_node(ui, canvas_rect, &document.root_node);
         }
     }
     
     fn draw_window_node(&mut self, ui: &mut egui::Ui, canvas_rect: egui::Rect, node: &crate::window_editor::WindowNode) {
         let offset = self.window_editor_offset;
         let zoom = self.window_editor_zoom;
         
         // Window editor coordinate system origin (relative to canvas_rect)
         let editor_origin = egui::Vec2::new(50.0, 50.0);
         
         // Calculate screen position from world coordinates
         let world_pos = egui::Vec2::new(node.position[0], node.position[1]);
         let screen_pos = canvas_rect.min + editor_origin + (world_pos + offset) * zoom;
         
         // Calculate size with zoom
         let world_size = egui::Vec2::new(node.size[0], node.size[1]);
         let screen_size = world_size * zoom;
         
         // Create rect for the node
         let node_rect = egui::Rect::from_min_size(screen_pos, screen_size);
         
         // Only draw if visible in canvas
         if canvas_rect.intersects(node_rect) {
             let painter = ui.painter();
             
             // Draw node based on type
             match &node.node_type {
                 crate::window_editor::NodeType::Window => {
                     // Draw window frame
                     painter.rect_stroke(
                         node_rect,
                         egui::Rounding::same(4.0),
                         egui::Stroke::new(2.0, egui::Color32::from_rgb(100, 100, 100))
                     );
                     
                     // Draw title bar
                     let title_height = 30.0 * zoom;
                     let title_rect = egui::Rect::from_min_size(
                         node_rect.min,
                         egui::Vec2::new(node_rect.width(), title_height)
                     );
                     painter.rect_filled(
                         title_rect,
                         egui::Rounding::same(4.0),
                         egui::Color32::from_rgb(70, 70, 70)
                     );
                     
                     // Draw title text
                     if zoom > 0.5 {
                         painter.text(
                             title_rect.center(),
                             egui::Align2::CENTER_CENTER,
                             &node.name,
                             egui::FontId::proportional(12.0 * zoom),
                             egui::Color32::WHITE
                         );
                     }
                 }
                 crate::window_editor::NodeType::Container => {
                     // Draw container background
                     painter.rect_filled(
                         node_rect,
                         egui::Rounding::same(2.0),
                         egui::Color32::from_rgba_unmultiplied(200, 200, 255, 100)
                     );
                     
                     // Draw container border
                     painter.rect_stroke(
                         node_rect,
                         egui::Rounding::same(2.0),
                         egui::Stroke::new(1.0, egui::Color32::from_rgb(150, 150, 200))
                     );
                     
                     // Draw label
                     if zoom > 0.3 {
                         painter.text(
                             node_rect.min + egui::Vec2::new(5.0, 5.0),
                             egui::Align2::LEFT_TOP,
                             &node.name,
                             egui::FontId::proportional(10.0 * zoom),
                             egui::Color32::from_rgb(100, 100, 150)
                         );
                     }
                 }
                 crate::window_editor::NodeType::Widget(widget_type) => {
                     // Draw widget background
                     painter.rect_filled(
                         node_rect,
                         egui::Rounding::same(2.0),
                         egui::Color32::from_rgb(240, 240, 240)
                     );
                     
                     // Draw widget border
                     painter.rect_stroke(
                         node_rect,
                         egui::Rounding::same(2.0),
                         egui::Stroke::new(1.0, egui::Color32::from_rgb(180, 180, 180))
                     );
                     
                     // Draw widget icon and name
                     if zoom > 0.3 {
                         let icon = widget_type.icon();
                         painter.text(
                             node_rect.center(),
                             egui::Align2::CENTER_CENTER,
                             &format!("{} {}", icon, node.name),
                             egui::FontId::proportional(10.0 * zoom),
                             egui::Color32::BLACK
                         );
                     }
                 }
             }
             
             // Highlight if selected
             if let Some(selected_id) = &self.window_editor_selected_node {
                 if selected_id == &node.id {
                     painter.rect_stroke(
                         node_rect,
                         egui::Rounding::same(2.0),
                         egui::Stroke::new(2.0, egui::Color32::from_rgb(0, 120, 255))
                     );
                 }
             }
         }
         
         // Draw children
         for child in &node.children {
             self.draw_window_node(ui, canvas_rect, child);
         }
     }
     


     // Node graph helper functions
     fn draw_grid(&self, ui: &mut egui::Ui, canvas_rect: egui::Rect) {
         let base_grid_size = 20.0;
         let offset = self.node_graph.canvas_offset;
         let zoom = self.node_graph.canvas_zoom;
         
         let painter = ui.painter();
         let grid_color = egui::Color32::from_gray(180); // Light gray for dots
         
         // Calculate the visible world bounds (what we can see through the viewport)
         let world_min_x = -offset.x / zoom;
         let world_min_y = -offset.y / zoom;
         let world_max_x = (canvas_rect.width() - offset.x) / zoom;
         let world_max_y = (canvas_rect.height() - offset.y) / zoom;
         
         // Find the first grid positions in world coordinates (expand range to ensure coverage)
         let start_grid_x = ((world_min_x / base_grid_size).floor() - 1.0) * base_grid_size;
         let start_grid_y = ((world_min_y / base_grid_size).floor() - 1.0) * base_grid_size;
         let end_grid_x = ((world_max_x / base_grid_size).ceil() + 1.0) * base_grid_size;
         let end_grid_y = ((world_max_y / base_grid_size).ceil() + 1.0) * base_grid_size;
         
         // Draw grid dots
         let mut world_x = start_grid_x;
         while world_x <= end_grid_x {
             let mut world_y = start_grid_y;
             while world_y <= end_grid_y {
                 // Transform world coordinates to screen coordinates
                 let screen_x = canvas_rect.min.x + world_x * zoom + offset.x;
                 let screen_y = canvas_rect.min.y + world_y * zoom + offset.y;
                 let dot_pos = egui::Pos2::new(screen_x, screen_y);
                 
                 // Only draw if within canvas bounds
                 if canvas_rect.contains(dot_pos) {
                     painter.circle_filled(
                         dot_pos,
                         1.0 * zoom.max(0.5), // Scale dot size with zoom, minimum 0.5
                         grid_color
                     );
                 }
                 world_y += base_grid_size;
             }
             world_x += base_grid_size;
         }
     }
     
     fn draw_connections(&self, ui: &mut egui::Ui, canvas_rect: egui::Rect) {
         let painter = ui.painter();
         let offset = self.node_graph.canvas_offset;
         let zoom = self.node_graph.canvas_zoom;
         
         for connection in &self.node_graph.connections {
             if let (Some(from_node), Some(to_node)) = (
                 self.node_graph.nodes.iter().find(|n| n.id == connection.from_node),
                 self.node_graph.nodes.iter().find(|n| n.id == connection.to_node)
             ) {
                 if let (Some(from_port), Some(to_port)) = (
                     from_node.outputs.iter().find(|p| p.id == connection.from_port),
                     to_node.inputs.iter().find(|p| p.id == connection.to_port)
                 ) {
                     // Apply zoom to positions using edge-aligned port local positions
                     let scaled_from_node_pos = from_node.position * zoom;
                     let scaled_to_node_pos = to_node.position * zoom;
                     let from_port_local = self.compute_port_local_pos(from_node, from_port, true) * zoom;
                     let to_port_local = self.compute_port_local_pos(to_node, to_port, false) * zoom;
                     
                     let from_pos = egui::Pos2::new(
                         canvas_rect.min.x + scaled_from_node_pos.x + from_port_local.x + offset.x,
                         canvas_rect.min.y + scaled_from_node_pos.y + from_port_local.y + offset.y
                     );
                     let to_pos = egui::Pos2::new(
                         canvas_rect.min.x + scaled_to_node_pos.x + to_port_local.x + offset.x,
                         canvas_rect.min.y + scaled_to_node_pos.y + to_port_local.y + offset.y
                     );
                     
                     // Draw bezier curve connection
                     let control_offset = (to_pos.x - from_pos.x).abs() * 0.5;
                     let control1 = egui::Pos2::new(from_pos.x + control_offset, from_pos.y);
                     let control2 = egui::Pos2::new(to_pos.x - control_offset, to_pos.y);
                     
                     // Approximate bezier with line segments
                     let steps = 20;
                     for i in 0..steps {
                         let t1 = i as f32 / steps as f32;
                         let t2 = (i + 1) as f32 / steps as f32;
                         
                         let p1 = self.bezier_point(from_pos, control1, control2, to_pos, t1);
                         let p2 = self.bezier_point(from_pos, control1, control2, to_pos, t2);
                         
                         painter.line_segment(
                             [p1, p2],
                             egui::Stroke::new(2.0 * zoom, egui::Color32::from_rgb(100, 150, 255))
                         );
                     }
                 }
             }
         }
         
         // Draw temporary connection if connecting
         if self.node_graph.is_connecting {
             if let (Some((from_node_id, from_port_id, from_is_output)), Some(temp_end)) = 
                 (&self.node_graph.connecting_from, self.node_graph.temp_connection_end) {
                 
                 // Find the starting port position
                 if let Some(from_node) = self.node_graph.nodes.iter().find(|n| n.id == *from_node_id) {
                     let from_port = if *from_is_output {
                         from_node.outputs.iter().find(|p| p.id == *from_port_id)
                     } else {
                         from_node.inputs.iter().find(|p| p.id == *from_port_id)
                     };
                     
                     if let Some(port) = from_port {
                         let scaled_from_node_pos = from_node.position * zoom;
                         let from_port_local = self.compute_port_local_pos(from_node, port, *from_is_output) * zoom;
                         
                         let from_pos = egui::Pos2::new(
                             canvas_rect.min.x + scaled_from_node_pos.x + from_port_local.x + offset.x,
                             canvas_rect.min.y + scaled_from_node_pos.y + from_port_local.y + offset.y
                         );
                         
                         // Draw bezier curve from port to mouse
                         let control_offset = (temp_end.x - from_pos.x).abs() * 0.5;
                         let control1 = egui::Pos2::new(from_pos.x + control_offset, from_pos.y);
                         let control2 = egui::Pos2::new(temp_end.x - control_offset, temp_end.y);
                         
                         // Approximate bezier with line segments
                         let steps = 20;
                         for i in 0..steps {
                             let t1 = i as f32 / steps as f32;
                             let t2 = (i + 1) as f32 / steps as f32;
                             
                             let p1 = self.bezier_point(from_pos, control1, control2, temp_end, t1);
                             let p2 = self.bezier_point(from_pos, control1, control2, temp_end, t2);
                             
                             painter.line_segment(
                                 [p1, p2],
                                 egui::Stroke::new(2.0 * zoom, egui::Color32::from_rgba_unmultiplied(100, 150, 255, 128))
                             );
                         }
                     }
                 }
             }
         }
     }
     
     fn bezier_point(&self, p0: egui::Pos2, p1: egui::Pos2, p2: egui::Pos2, p3: egui::Pos2, t: f32) -> egui::Pos2 {
         let u = 1.0 - t;
         let tt = t * t;
         let uu = u * u;
         let uuu = uu * u;
         let ttt = tt * t;
         
         let x = uuu * p0.x + 3.0 * uu * t * p1.x + 3.0 * u * tt * p2.x + ttt * p3.x;
         let y = uuu * p0.y + 3.0 * uu * t * p1.y + 3.0 * u * tt * p2.y + ttt * p3.y;
         
         egui::Pos2::new(x, y)
     }
     
     // Compute the local position of a port aligned to the node edge
     fn compute_port_local_pos(&self, node: &GraphNode, port: &NodePort, is_output: bool) -> egui::Vec2 {
         // We anchor inputs to the left edge (x = 0.0), outputs to the right edge (x = node.size.x)
         // We take the y from the port's configured position, but clamp to node bounds with small padding
         let padding = 6.0;
         let y = port.position.y.clamp(padding, node.size.y - padding);
         let x = if is_output { node.size.x } else { 0.0 };
         egui::Vec2::new(x, y)
     }
     
     // Hit testing for ports in canvas (world) coordinates
     // Returns (node_id, port_id, is_output)
     fn get_port_at_position(&self, canvas_pos: egui::Vec2) -> Option<(String, String, bool)> {
         let hit_radius = 6.0; // world-space radius
         for node in &self.node_graph.nodes {
             // Check outputs first (right edge), then inputs (left edge)
             for port in &node.outputs {
                 let local = self.compute_port_local_pos(node, port, true);
                 let world = node.position + local;
                 let d2 = (world.x - canvas_pos.x).powi(2) + (world.y - canvas_pos.y).powi(2);
                 if d2 <= hit_radius * hit_radius {
                     return Some((node.id.clone(), port.id.clone(), true));
                 }
             }
             for port in &node.inputs {
                 let local = self.compute_port_local_pos(node, port, false);
                 let world = node.position + local;
                 let d2 = (world.x - canvas_pos.x).powi(2) + (world.y - canvas_pos.y).powi(2);
                 if d2 <= hit_radius * hit_radius {
                     return Some((node.id.clone(), port.id.clone(), false));
                 }
             }
         }
         None
     }
     
     fn draw_nodes(&mut self, ui: &mut egui::Ui, canvas_rect: egui::Rect) {
         let painter = ui.painter();
         let offset = self.node_graph.canvas_offset;
         let zoom = self.node_graph.canvas_zoom;
         
         for node in &self.node_graph.nodes {
             // Apply zoom to node size and position
             let scaled_size = node.size * zoom;
             let scaled_position = node.position * zoom;
             
             let node_rect = egui::Rect::from_min_size(
                 egui::Pos2::new(
                     canvas_rect.min.x + scaled_position.x + offset.x,
                     canvas_rect.min.y + scaled_position.y + offset.y
                 ),
                 scaled_size
             );
             
             // Check if node is selected (either single or multi-select)
             let is_selected = self.node_graph.selected_node.as_ref() == Some(&node.id) ||
                              self.node_graph.selected_nodes.contains(&node.id);
             
             // Draw node background
             let node_color = if is_selected {
                 egui::Color32::from_rgb(80, 120, 200)
             } else {
                 egui::Color32::from_rgb(220, 220, 220) // Light gray for better contrast on white background
             };
             
             painter.rect_filled(
                 node_rect,
                 egui::Rounding::same(5.0 * zoom),
                 node_color
             );
             
             painter.rect_stroke(
                 node_rect,
                 egui::Rounding::same(5.0 * zoom),
                 egui::Stroke::new(1.0 * zoom, egui::Color32::from_rgb(100, 100, 100))
             );
             
             // Draw node title (scale font size with zoom)
             let font_size = 14.0 * zoom;
             painter.text(
                 egui::Pos2::new(node_rect.min.x + 5.0 * zoom, node_rect.min.y + 5.0 * zoom),
                 egui::Align2::LEFT_TOP,
                 &node.title,
                 egui::FontId::proportional(font_size),
                 egui::Color32::BLACK
             );
             
             // Draw input ports (scale with zoom) at left edge
             for port in &node.inputs {
                 let local = self.compute_port_local_pos(node, port, false) * zoom;
                 let port_pos = egui::Pos2::new(
                     node_rect.min.x + local.x,
                     node_rect.min.y + local.y
                 );
                 
                 let selected = match &self.node_graph.selected_port {
                     Some((nid, pid, is_out)) => nid == &node.id && pid == &port.id && !is_out,
                     None => false,
                 };
                 painter.circle_filled(
                     port_pos,
                     4.0 * zoom,
                     egui::Color32::from_rgb(255, 100, 100)
                 );
                 if selected {
                     painter.circle_stroke(
                         port_pos,
                         6.0 * zoom,
                         egui::Stroke::new(1.5 * zoom, egui::Color32::BLACK)
                     );
                 }
             }
             
             // Draw output ports (scale with zoom) at right edge
             for port in &node.outputs {
                 let local = self.compute_port_local_pos(node, port, true) * zoom;
                 let port_pos = egui::Pos2::new(
                     node_rect.min.x + local.x,
                     node_rect.min.y + local.y
                 );
                 
                 let selected = match &self.node_graph.selected_port {
                     Some((nid, pid, is_out)) => nid == &node.id && pid == &port.id && *is_out,
                     None => false,
                 };
                 painter.circle_filled(
                     port_pos,
                     4.0 * zoom,
                     egui::Color32::from_rgb(100, 255, 100)
                 );
                 if selected {
                     painter.circle_stroke(
                         port_pos,
                         6.0 * zoom,
                         egui::Stroke::new(1.5 * zoom, egui::Color32::BLACK)
                     );
                 }
             }
         }
     }
     
     fn handle_canvas_interactions(&mut self, ui: &mut egui::Ui, response: &egui::Response, canvas_rect: egui::Rect) {
         let offset = self.node_graph.canvas_offset;
         let zoom = self.node_graph.canvas_zoom;
         
         // Check if any text edit has focus to avoid consuming Shift key events
         let text_edit_has_focus = ui.ctx().memory(|mem| {
             mem.focused().map_or(false, |id| {
                 // Check if the focused widget is a text edit
                 mem.data.get_temp::<egui::text_edit::TextEditState>(id).is_some()
             })
         });
         
         // Handle zoom with mouse wheel (centered on mouse position)
         if response.hovered() {
             let scroll_delta = ui.input(|i| i.smooth_scroll_delta.y);
             if scroll_delta != 0.0 {
                 if let Some(mouse_pos) = ui.input(|i| i.pointer.hover_pos()) {
                     let zoom_factor = 1.0 + scroll_delta * 0.001;
                     let new_zoom = (self.node_graph.canvas_zoom * zoom_factor).clamp(0.1, 3.0);
                     
                     // Calculate mouse position in canvas coordinates before zoom
                     let mouse_canvas_pos_before = egui::Vec2::new(
                         (mouse_pos.x - canvas_rect.min.x - self.node_graph.canvas_offset.x) / self.node_graph.canvas_zoom,
                         (mouse_pos.y - canvas_rect.min.y - self.node_graph.canvas_offset.y) / self.node_graph.canvas_zoom
                     );
                     
                     // Update zoom
                     self.node_graph.canvas_zoom = new_zoom;
                     
                     // Recompute offset so that the world point under the cursor stays fixed
                     let new_offset = egui::Vec2::new(
                         mouse_pos.x - canvas_rect.min.x - mouse_canvas_pos_before.x * self.node_graph.canvas_zoom,
                         mouse_pos.y - canvas_rect.min.y - mouse_canvas_pos_before.y * self.node_graph.canvas_zoom
                     );
                     self.node_graph.canvas_offset = new_offset;
                 }
             }
         }

         // Handle middle mouse button panning
         if response.drag_started_by(egui::PointerButton::Middle) {
             if let Some(start_pos) = response.interact_pointer_pos() {
                 self.node_graph.is_panning = true;
                 self.node_graph.pan_start_pos = Some(start_pos);
                 self.node_graph.pan_start_offset = Some(self.node_graph.canvas_offset);
             }
         }

         if response.dragged_by(egui::PointerButton::Middle) && self.node_graph.is_panning {
             if let (Some(start_pos), Some(start_offset)) = (self.node_graph.pan_start_pos, self.node_graph.pan_start_offset) {
                 if let Some(current_pos) = response.interact_pointer_pos() {
                     let delta = current_pos - start_pos;
                     self.node_graph.canvas_offset = start_offset + delta;
                 }
             }
         }

         if response.drag_stopped_by(egui::PointerButton::Middle) {
             self.node_graph.is_panning = false;
             self.node_graph.pan_start_pos = None;
             self.node_graph.pan_start_offset = None;
         }

         // Handle right mouse button for connection deletion and node deletion
         if response.drag_started_by(egui::PointerButton::Secondary) {
             if let Some(start_pos) = response.interact_pointer_pos() {
                 self.node_graph.is_right_dragging = true;
                 self.node_graph.right_drag_start = Some(start_pos);
                 self.node_graph.right_drag_path.clear();
                 self.node_graph.right_drag_path.push(start_pos);
             }
         }

         if response.dragged_by(egui::PointerButton::Secondary) && self.node_graph.is_right_dragging {
             if let Some(current_pos) = response.interact_pointer_pos() {
                 self.node_graph.right_drag_path.push(current_pos);
                 // Delete connections that intersect with the drag path
                 self.check_and_delete_intersecting_connections(canvas_rect);
                 // Delete nodes that intersect with the drag path
                 self.check_and_delete_intersecting_nodes(canvas_rect);
             }
         }

         if response.drag_stopped_by(egui::PointerButton::Secondary) {
            // Clean up right drag state
            self.node_graph.is_right_dragging = false;
            self.node_graph.right_drag_start = None;
            self.node_graph.right_drag_path.clear();
        }
         
         // Begin interactions on primary drag start: either start box selection (with Shift), start node dragging, or start connection dragging
         if response.drag_started_by(egui::PointerButton::Primary) {
             if let Some(start_pos) = response.interact_pointer_pos() {
                 // Only check Shift key if no text edit has focus
                 let shift_held = !text_edit_has_focus && ui.input(|i| i.modifiers.shift);
                 let canvas_pos = egui::Vec2::new(
                     (start_pos.x - canvas_rect.min.x - self.node_graph.canvas_offset.x) / self.node_graph.canvas_zoom,
                     (start_pos.y - canvas_rect.min.y - self.node_graph.canvas_offset.y) / self.node_graph.canvas_zoom
                 );
                 // Save pre-drag selection state
                 self.node_graph.pre_drag_selected_node = self.node_graph.selected_node.clone();
                 self.node_graph.pre_drag_selected_nodes = self.node_graph.selected_nodes.clone();
                 
                 // Check if dragging started on a port
                 let clicked_port = self.get_port_at_position(canvas_pos);
                 
                 if let Some((node_id, port_id, is_output)) = clicked_port {
                     // Start connection dragging from port
                     self.handle_port_click(&node_id, &port_id, is_output, start_pos);
                 } else if shift_held {
                    // Start box selection immediately on drag start
                    self.node_graph.is_selecting = true;
                    self.node_graph.selection_start = Some(start_pos);
                    self.node_graph.selection_box = Some(egui::Rect::from_min_max(start_pos, start_pos));
                    self.node_graph.selection_shift_held = true; // Record Shift state
                 } else if let Some(node_id) = self.get_node_at_position(canvas_pos) {
                     // Start dragging this node - record initial positions for grid snapping
                     self.node_graph.selected_nodes.clear();
                     self.node_graph.selected_node = Some(node_id.clone());
                     self.node_graph.dragging_node = Some(node_id.clone());
                     
                     // Record drag start positions for proper grid snapping
                     self.node_graph.drag_start_mouse_pos = Some(start_pos);
                     if let Some(node) = self.node_graph.nodes.iter().find(|n| n.id == node_id) {
                         self.node_graph.drag_start_node_pos = Some(node.position);
                     }
                 }
             }
         }
         
         // Update temporary connection end position if connecting (always check for hover position)
         if self.node_graph.is_connecting {
             if let Some(hover_pos) = ui.input(|i| i.pointer.hover_pos()) {
                 self.node_graph.temp_connection_end = Some(hover_pos);
             }
         }
         
         // Handle mouse interactions
         if let Some(pointer_pos) = response.interact_pointer_pos() {
             let canvas_pos = egui::Vec2::new(
                 (pointer_pos.x - canvas_rect.min.x - offset.x) / zoom,
                 (pointer_pos.y - canvas_rect.min.y - offset.y) / zoom
             );
             
             // Check if clicking on a node or a port
             let clicked_node = self.get_node_at_position(canvas_pos);
             let clicked_port = self.get_port_at_position(canvas_pos);
             
             // Handle left mouse button
             if response.clicked_by(egui::PointerButton::Primary) {
                 // Only check Shift key if no text edit has focus
                 let shift_held = !text_edit_has_focus && ui.input(|i| i.modifiers.shift);
                 if let Some((node_id, port_id, is_output)) = clicked_port {
                     self.handle_port_click(&node_id, &port_id, is_output, pointer_pos);
                 } else if let Some(ref node_id) = clicked_node {
                     if shift_held {
                         // Shift+click:
                         if self.node_graph.selected_nodes.contains(node_id) {
                             self.node_graph.selected_nodes.retain(|id| id != node_id);
                         } else {
                             self.node_graph.selected_nodes.push(node_id.clone());
                         }
                     } else {
                         // Regular click: select single node
                         self.node_graph.selected_nodes.clear();
                         self.node_graph.selected_node = Some(node_id.clone());
                         self.node_graph.selected_port = None;
                     }
                 } else {
                     // Clicked on empty space
                     if self.node_graph.is_connecting {
                         // Cancel connection
                         self.cancel_connection();
                     } else if !shift_held {
                         // Clear selection only if shift is not held
                         self.node_graph.selected_node = None;
                         self.node_graph.selected_nodes.clear();
                         self.node_graph.selected_port = None;
                     }
                 }
             }
         }
         
         // Handle dragging
         if response.dragged_by(egui::PointerButton::Primary) {
             if self.node_graph.is_connecting {
                 // Update temporary connection end position
                 if let Some(current_pos) = response.interact_pointer_pos() {
                     self.node_graph.temp_connection_end = Some(current_pos);
                 }
             } else if self.node_graph.is_selecting {
                 // Update selection box
                 if let (Some(start), Some(current)) = (self.node_graph.selection_start, response.interact_pointer_pos()) {
                     self.node_graph.selection_box = Some(egui::Rect::from_min_max(start, current));
                 }
             } else if let Some(selected_id) = &self.node_graph.selected_node.clone() {
                 // Drag selected node with improved grid snapping
                 if let Some(node) = self.node_graph.nodes.iter_mut().find(|n| n.id == *selected_id) {
                     if self.node_graph.snap_to_grid {
                         // Use recorded start positions for proper grid snapping behavior
                         if let (Some(start_mouse), Some(start_node)) = (self.node_graph.drag_start_mouse_pos, self.node_graph.drag_start_node_pos) {
                             if let Some(current_mouse) = response.interact_pointer_pos() {
                                 let mouse_delta = current_mouse - start_mouse;
                                 let canvas_delta = mouse_delta / zoom;
                                 let target_pos = start_node + canvas_delta;
                                 node.position = Self::snap_to_grid_position_static(target_pos);
                             }
                         }
                     } else {
                         node.position += response.drag_delta() / zoom;
                     }
                 }
             } else if !self.node_graph.selected_nodes.is_empty() {
                 // Drag multiple selected nodes
                 let selected_ids = self.node_graph.selected_nodes.clone();
                 for node_id in selected_ids {
                     if let Some(node) = self.node_graph.nodes.iter_mut().find(|n| n.id == node_id) {
                         let mut new_pos = node.position + response.drag_delta() / zoom;
                         if self.node_graph.snap_to_grid {
                             new_pos = Self::snap_to_grid_position_static(new_pos);
                         }
                         node.position = new_pos;
                     }
                 }
             }
         }
         
         // Finish selection box: perform selection of nodes inside the box
        if response.drag_stopped_by(egui::PointerButton::Primary) && self.node_graph.is_selecting {
            if let Some(selection_box) = self.node_graph.selection_box {
                // Use the recorded Shift state from when selection started
                let shift_held = self.node_graph.selection_shift_held;
                // If Shift was held when selection started, start from pre-drag multi-selection; otherwise start fresh
                let mut new_selection = if shift_held {
                    self.node_graph.pre_drag_selected_nodes.clone()
                } else {
                    Vec::new()
                };
                 
                 // Convert selection box to canvas coordinates for proper intersection testing
                 let canvas_selection_box = egui::Rect::from_min_max(
                     egui::Pos2::new(
                         (selection_box.min.x - canvas_rect.min.x - offset.x) / zoom,
                         (selection_box.min.y - canvas_rect.min.y - offset.y) / zoom
                     ),
                     egui::Pos2::new(
                         (selection_box.max.x - canvas_rect.min.x - offset.x) / zoom,
                         (selection_box.max.y - canvas_rect.min.y - offset.y) / zoom
                     )
                 );
                 
                 println!("Box selection ended. Canvas selection box: {:?}", canvas_selection_box);
                 for node in &self.node_graph.nodes {
                     let node_rect = egui::Rect::from_min_size(
                         node.position.to_pos2(),
                         node.size
                     );
                     println!("Checking node '{}' at rect {:?}", node.id, node_rect);
                     if canvas_selection_box.intersects(node_rect) {
                         println!("Node '{}' intersects with selection box", node.id);
                         if !new_selection.contains(&node.id) {
                             new_selection.push(node.id.clone());
                         }
                     }
                 }
                 println!("Selected nodes: {:?}", new_selection);
                 self.node_graph.selected_nodes = new_selection;
                 // Keep multi-selection; clear single selection and port selection
                 self.node_graph.selected_node = None;
                 self.node_graph.selected_port = None;
             }
             self.node_graph.is_selecting = false;
            self.node_graph.selection_start = None;
            self.node_graph.selection_box = None;
            self.node_graph.selection_shift_held = false; // Reset Shift state
         }
         
         // Handle connection completion
         if response.drag_stopped_by(egui::PointerButton::Primary) && self.node_graph.is_connecting {
             if let Some(end_pos) = response.interact_pointer_pos() {
                 let canvas_pos = egui::Vec2::new(
                     (end_pos.x - canvas_rect.min.x - offset.x) / zoom,
                     (end_pos.y - canvas_rect.min.y - offset.y) / zoom
                 );
                 let target_port = self.get_port_at_position(canvas_pos);
                 self.handle_connection_end(target_port);
             }
         }
         
         // If a node-drag operation just ended (and not in box-select), restore selection to pre-drag state
         if response.drag_stopped_by(egui::PointerButton::Primary) && !self.node_graph.is_selecting && !self.node_graph.is_connecting && self.node_graph.dragging_node.is_some() {
             self.node_graph.selected_node = self.node_graph.pre_drag_selected_node.clone();
             self.node_graph.selected_nodes = self.node_graph.pre_drag_selected_nodes.clone();
             self.node_graph.dragging_node = None;
             // Clear drag start positions
             self.node_graph.drag_start_mouse_pos = None;
             self.node_graph.drag_start_node_pos = None;
         }
     }
     
     fn get_node_at_position(&self, canvas_pos: egui::Vec2) -> Option<String> {
          for node in &self.node_graph.nodes {
              let node_rect = egui::Rect::from_min_size(
                  node.position.to_pos2(),
                  node.size
              );
              if node_rect.contains(canvas_pos.to_pos2()) {
                  return Some(node.id.clone());
              }
          }
          None
      }
      
      fn check_and_delete_intersecting_connections(&mut self, canvas_rect: egui::Rect) {
          let offset = self.node_graph.canvas_offset;
          let zoom = self.node_graph.canvas_zoom;
          let drag_path = &self.node_graph.right_drag_path;
          
          if drag_path.len() < 2 {
              return;
          }
          
          let mut connections_to_remove = Vec::new();
          
          for (conn_idx, connection) in self.node_graph.connections.iter().enumerate() {
              if let (Some(from_node), Some(to_node)) = (
                  self.node_graph.nodes.iter().find(|n| n.id == connection.from_node),
                  self.node_graph.nodes.iter().find(|n| n.id == connection.to_node)
              ) {
                  if let (Some(from_port), Some(to_port)) = (
                      from_node.outputs.iter().find(|p| p.id == connection.from_port),
                      to_node.inputs.iter().find(|p| p.id == connection.to_port)
                  ) {
                      // Calculate connection line endpoints
                      let scaled_from_node_pos = from_node.position * zoom;
                      let scaled_to_node_pos = to_node.position * zoom;
                      let scaled_from_port_pos = self.compute_port_local_pos(from_node, from_port, true) * zoom;
                      let scaled_to_port_pos = self.compute_port_local_pos(to_node, to_port, false) * zoom;
                       
                       let from_pos = egui::Pos2::new(
                           canvas_rect.min.x + scaled_from_node_pos.x + scaled_from_port_pos.x + offset.x,
                           canvas_rect.min.y + scaled_from_node_pos.y + scaled_from_port_pos.y + offset.y
                       );
                       let to_pos = egui::Pos2::new(
                           canvas_rect.min.x + scaled_to_node_pos.x + scaled_to_port_pos.x + offset.x,
                           canvas_rect.min.y + scaled_to_node_pos.y + scaled_to_port_pos.y + offset.y
                       );
                      
                      // Check if drag path intersects with connection
                      if self.path_intersects_connection(drag_path, from_pos, to_pos) {
                          connections_to_remove.push(conn_idx);
                      }
                  }
              }
          }
          
          // Remove connections in reverse order to maintain indices
          for &idx in connections_to_remove.iter().rev() {
              self.node_graph.connections.remove(idx);
          }
      }
      
      fn check_and_delete_intersecting_nodes(&mut self, canvas_rect: egui::Rect) {
          let offset = self.node_graph.canvas_offset;
          let zoom = self.node_graph.canvas_zoom;
          
          let mut nodes_to_remove = Vec::new();
          
          for (node_idx, node) in self.node_graph.nodes.iter().enumerate() {
              // Convert node position to screen coordinates
              let scaled_size = node.size * zoom;
              let scaled_position = node.position * zoom;
              
              let node_screen_rect = egui::Rect::from_min_size(
                  egui::Pos2::new(
                      canvas_rect.min.x + scaled_position.x + offset.x,
                      canvas_rect.min.y + scaled_position.y + offset.y
                  ),
                  scaled_size
              );
              
              // Check if any segment of the drag path intersects with the node rectangle
              if self.path_intersects_node(&self.node_graph.right_drag_path, node_screen_rect) {
                  nodes_to_remove.push(node_idx);
              }
          }
          
          // Remove nodes in reverse order to maintain indices
          for &idx in nodes_to_remove.iter().rev() {
              let node_id = self.node_graph.nodes[idx].id.clone();
              
              // Remove the node
              self.node_graph.nodes.remove(idx);
              
              // Remove all connections related to this node
              self.node_graph.connections.retain(|conn| {
                  conn.from_node != node_id && conn.to_node != node_id
              });
              
              // Clear selection if the deleted node was selected
              if self.node_graph.selected_node.as_ref() == Some(&node_id) {
                  self.node_graph.selected_node = None;
              }
              self.node_graph.selected_nodes.retain(|id| id != &node_id);
              
              // Clear port selection if it belonged to the deleted node
              if let Some((selected_node_id, _, _)) = &self.node_graph.selected_port {
                  if selected_node_id == &node_id {
                      self.node_graph.selected_port = None;
                  }
              }
          }
      }
      
      fn path_intersects_node(&self, drag_path: &[egui::Pos2], node_rect: egui::Rect) -> bool {
          // Check if any segment of the drag path intersects with the node rectangle
          for i in 0..drag_path.len().saturating_sub(1) {
              let seg_start = drag_path[i];
              let seg_end = drag_path[i + 1];
              
              // Check if line segment intersects with any edge of the rectangle
              let rect_edges = [
                  (node_rect.min, egui::Pos2::new(node_rect.max.x, node_rect.min.y)), // top edge
                  (egui::Pos2::new(node_rect.max.x, node_rect.min.y), node_rect.max), // right edge
                  (node_rect.max, egui::Pos2::new(node_rect.min.x, node_rect.max.y)), // bottom edge
                  (egui::Pos2::new(node_rect.min.x, node_rect.max.y), node_rect.min), // left edge
              ];
              
              for (edge_start, edge_end) in rect_edges.iter() {
                  if self.line_segments_intersect(seg_start, seg_end, *edge_start, *edge_end) {
                      return true;
                  }
              }
              
              // Also check if either endpoint of the segment is inside the rectangle
              if node_rect.contains(seg_start) || node_rect.contains(seg_end) {
                  return true;
              }
          }
          false
      }
      
      #[allow(dead_code)]
      fn path_intersects_connection(&self, drag_path: &[egui::Pos2], conn_start: egui::Pos2, conn_end: egui::Pos2) -> bool {
          // Check if any segment of the drag path intersects with the connection line
          for i in 0..drag_path.len().saturating_sub(1) {
              let seg_start = drag_path[i];
              let seg_end = drag_path[i + 1];
              
              if self.line_segments_intersect(seg_start, seg_end, conn_start, conn_end) {
                  return true;
              }
          }
          false
      }
      
      #[allow(dead_code)]
      fn line_segments_intersect(&self, p1: egui::Pos2, q1: egui::Pos2, p2: egui::Pos2, q2: egui::Pos2) -> bool {
          // Find the four orientations needed for general and special cases
          let o1 = self.orientation(p1, q1, p2);
          let o2 = self.orientation(p1, q1, q2);
          let o3 = self.orientation(p2, q2, p1);
          let o4 = self.orientation(p2, q2, q1);
          
          // General case
          if o1 != o2 && o3 != o4 {
              return true;
          }
          
          // Special cases
          // p1, q1 and p2 are colinear and p2 lies on segment p1q1
          if o1 == 0 && self.on_segment(p1, p2, q1) {
              return true;
          }
          
          // p1, q1 and q2 are colinear and q2 lies on segment p1q1
          if o2 == 0 && self.on_segment(p1, q2, q1) {
              return true;
          }
          
          // p2, q2 and p1 are colinear and p1 lies on segment p2q2
          if o3 == 0 && self.on_segment(p2, p1, q2) {
              return true;
          }
          
          // p2, q2 and q1 are colinear and q1 lies on segment p2q2
          if o4 == 0 && self.on_segment(p2, q1, q2) {
              return true;
          }
          
          false
      }
      
      #[allow(dead_code)]
      fn orientation(&self, p: egui::Pos2, q: egui::Pos2, r: egui::Pos2) -> i32 {
          let val = (q.y - p.y) * (r.x - q.x) - (q.x - p.x) * (r.y - q.y);
          if val.abs() < f32::EPSILON {
              0 // colinear
          } else if val > 0.0 {
              1 // clockwise
          } else {
              2 // counterclockwise
          }
      }
      
      #[allow(dead_code)]
      fn on_segment(&self, p: egui::Pos2, q: egui::Pos2, r: egui::Pos2) -> bool {
           q.x <= p.x.max(r.x) && q.x >= p.x.min(r.x) && q.y <= p.y.max(r.y) && q.y >= p.y.min(r.y)
       }
       
       fn snap_to_grid_position_static(position: egui::Vec2) -> egui::Vec2 {
            let grid_size = 20.0; // Base grid size (unscaled)
            
            // Calculate the nearest grid point
            let snapped_x = (position.x / grid_size).round() * grid_size;
            let snapped_y = (position.y / grid_size).round() * grid_size;
            
            egui::Vec2::new(snapped_x, snapped_y)
        }
      
      fn handle_port_click(&mut self, node_id: &str, port_id: &str, is_output: bool, mouse_pos: egui::Pos2) {
          if self.node_graph.is_connecting {
              // Already connecting, try to complete the connection
              let target_port = Some((node_id.to_string(), port_id.to_string(), is_output));
              self.handle_connection_end(target_port);
          } else {
              // Not currently connecting, check what type of port was clicked
              if is_output {
                  // Clicked on output port - start new connection
                  self.node_graph.is_connecting = true;
                  self.node_graph.connecting_from = Some((node_id.to_string(), port_id.to_string(), true));
                  self.node_graph.temp_connection_end = Some(mouse_pos);
                  // Clear other selections
                  self.node_graph.selected_port = None;
                  self.node_graph.selected_node = None;
                  self.node_graph.selected_nodes.clear();
              } else {
                  // Clicked on input port - only start connection if it's already connected
                  if let Some(existing_conn_idx) = self.node_graph.connections.iter().position(|conn| {
                      conn.to_node == node_id && conn.to_port == port_id
                  }) {
                      // Disconnect and start new connection from the original output port
                      let existing_conn = self.node_graph.connections.remove(existing_conn_idx);
                      self.node_graph.is_connecting = true;
                      self.node_graph.connecting_from = Some((existing_conn.from_node, existing_conn.from_port, true));
                      self.node_graph.temp_connection_end = Some(mouse_pos);
                      // Clear other selections
                      self.node_graph.selected_port = None;
                      self.node_graph.selected_node = None;
                      self.node_graph.selected_nodes.clear();
                  }
                  // If input port is not connected, do nothing (don't start connection)
              }
          }
      }
      
      fn handle_connection_end(&mut self, target_port: Option<(String, String, bool)>) {
          if let Some((from_node, from_port, from_is_output)) = &self.node_graph.connecting_from {
              if let Some((to_node, to_port, to_is_output)) = target_port {
                  // Validate connection (must be from output to input)
                  if self.can_connect(from_node, from_port, *from_is_output, &to_node, &to_port, to_is_output) {
                      // Remove existing connection to target input port if any
                      self.node_graph.connections.retain(|conn| {
                          !(conn.to_node == to_node && conn.to_port == to_port)
                      });
                      
                      // Create new connection (from output to input)
                      let connection_id = format!("conn_{}_{}_{}_{}", from_node, from_port, to_node, to_port);
                      let new_connection = crate::NodeConnection {
                          id: connection_id,
                          from_node: from_node.clone(),
                          from_port: from_port.clone(),
                          to_node: to_node,
                          to_port: to_port,
                      };
                      self.node_graph.connections.push(new_connection);
                  }
              }
          }
          
          // Reset connection state
          self.cancel_connection();
      }
      
      fn can_connect(&self, from_node: &str, _from_port: &str, from_is_output: bool, 
                     to_node: &str, _to_port: &str, to_is_output: bool) -> bool {
          // Cannot connect to same node
          if from_node == to_node {
              return false;
          }
          
          // Must connect output to input (from must be output, to must be input)
          from_is_output && !to_is_output
      }
      
      fn cancel_connection(&mut self) {
          self.node_graph.is_connecting = false;
          self.node_graph.connecting_from = None;
          self.node_graph.temp_connection_end = None;
      }
      
      fn handle_context_menu(&mut self, ui: &mut egui::Ui, response: &egui::Response, _canvas_rect: egui::Rect) {
          if self.node_graph.show_context_menu {
              if let Some(menu_pos) = self.node_graph.context_menu_pos {
                  let menu_rect = egui::Rect::from_min_size(
                      menu_pos,
                      egui::Vec2::new(120.0, 100.0)
                  );
                  
                  // Check if menu should be closed (click outside)
                  if response.clicked() && !menu_rect.contains(response.interact_pointer_pos().unwrap_or_default()) {
                      self.node_graph.show_context_menu = false;
                      self.node_graph.context_menu_pos = None;
                  }
                  
                  // Draw context menu
                   egui::Area::new(egui::Id::new("context_menu"))
                       .fixed_pos(menu_pos)
                       .order(egui::Order::Foreground)
                       .show(ui.ctx(), |ui| {
                          egui::Frame::popup(ui.style())
                              .show(ui, |ui| {
                                  ui.set_min_width(120.0);
                                  
                                  if ui.button("Copy").clicked() {
                                      // TODO: Implement copy functionality
                                      self.node_graph.show_context_menu = false;
                                  }
                                  
                                  if ui.button("Cut").clicked() {
                                      // TODO: Implement cut functionality
                                      self.node_graph.show_context_menu = false;
                                  }
                                  
                                  if ui.button("Paste").clicked() {
                                      // TODO: Implement paste functionality
                                      self.node_graph.show_context_menu = false;
                                  }
                                  
                                  ui.separator();
                                  
                                  if ui.button("Refresh").clicked() {
                                      // Refresh the canvas
                                      self.node_graph.canvas_offset = egui::Vec2::ZERO;
                                      self.node_graph.canvas_zoom = 1.0;
                                      self.node_graph.show_context_menu = false;
                                  }
                              });
                      });
              }
          }
      }
}