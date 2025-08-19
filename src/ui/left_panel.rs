use eframe::egui;
use crate::{EguiCodeGeneratorApp, UiNode, ClipboardOperation};
use crate::filesystem::{FileSystemEntry, FileSystemService};
use crate::ui::dialogs::*;
use crate::ui::file_operations::FileOperations;

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
                                .id_salt("node_tree_scroll")
                                .auto_shrink([false, false])
                                .show(ui, |ui| {
                                    // Check if we're in window editor mode with a loaded document
                                    if self.current_tab == crate::EditorTab::WindowEditor {
                                        if self.current_window_document.is_some() {
                                            self.render_window_node_tree(ui);
                                        } else {
                                            // Show message when no window file is loaded
                                            ui.vertical_centered(|ui| {
                                                ui.add_space(20.0);
                                                ui.label("🪟 No window file opened");
                                                ui.add_space(10.0);
                                                ui.label("Please open a .vfwindow file");
                                                ui.label("to view the node tree.");
                                            });
                                        }
                                    } else {
                                        Self::render_node_tree(ui, &mut self.selected_node, &self.node_tree);
                                    }
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
                            .id_salt("file_system_scroll")
                            .auto_shrink([false, false])
                            .show(ui, |ui| {
                                self.render_file_system(ui);
                            });
                    });
                });
            });
        
        // Handle node context menu
        self.handle_node_context_menu(ctx);
    }
 
     pub fn render_file_system(&mut self, ui: &mut egui::Ui) {
        // Handle right-click context menu
        if self.show_context_menu {
            self.render_context_menu(ui);
        }
        
        // Check if we have a project loaded
        if self.file_system_tree.is_empty() {
            ui.vertical_centered(|ui| {
                ui.add_space(20.0);
                ui.label("📁 No project loaded");
                ui.label("Please create or open a project");
            });
        } else {
            // Render file system entries
            self.render_file_system_entries(ui, &self.file_system_tree.clone());
            
            // Handle right-click on empty space (only if not already showing context menu)
            if !self.show_context_menu {
                let available_rect = ui.available_rect_before_wrap();
                if available_rect.height() > 10.0 { // Only if there's meaningful space
                    let response = ui.allocate_rect(available_rect, egui::Sense::click());
                    if response.secondary_clicked() {
                        self.show_context_menu = true;
                        self.context_menu_pos = response.interact_pointer_pos().unwrap_or_default();
                        self.context_menu_target = None; // No specific target, empty space
                        self.selected_files.clear();
                    }
                }
            }
        }
    }

    fn render_file_system_entries(&mut self, ui: &mut egui::Ui, entries: &[FileSystemEntry]) {
        for entry in entries {
            match entry {
                FileSystemEntry::File { name, path, .. } => {
                    let is_selected = self.selected_files.contains(path);
                    
                    // Check if this file is being renamed inline
                    if let Some(rename_target) = &self.inline_rename_target {
                        if rename_target == path {
                            // Show inline text editor
                            ui.horizontal(|ui| {
                                let file_icon = self.get_file_icon(path);
                                ui.label(file_icon);
                                let mut text_edit = egui::TextEdit::singleline(&mut self.inline_rename_text)
                                    .desired_width(ui.available_width() - 20.0);
                                
                                // Set cursor position if specified
                                 if let Some(_cursor_pos) = self.inline_rename_cursor_pos {
                                     text_edit = text_edit.cursor_at_end(false);
                                 }
                                
                                let response = ui.add(text_edit);
                                
                                // Auto-focus and set cursor position
                                if !response.has_focus() {
                                    response.request_focus();
                                    // Set cursor position after focusing
                                    if let Some(cursor_pos) = self.inline_rename_cursor_pos {
                                        if let Some(mut state) = egui::TextEdit::load_state(ui.ctx(), response.id) {
                                            state.cursor.set_char_range(Some(egui::text::CCursorRange::one(egui::text::CCursor::new(cursor_pos))));
                                            state.store(ui.ctx(), response.id);
                                        }
                                        self.inline_rename_cursor_pos = None; // Clear after setting
                                    }
                                }
                                
                                // Handle Enter key to confirm rename
                                if ui.input(|i| i.key_pressed(egui::Key::Enter)) {
                                    self.confirm_inline_rename();
                                }
                                
                                // Handle Escape key to cancel rename
                                if ui.input(|i| i.key_pressed(egui::Key::Escape)) {
                                    self.cancel_inline_rename();
                                }
                                
                                // Cancel rename if clicked outside
                                if response.lost_focus() && ui.input(|i| i.pointer.any_click()) {
                                    if let Some(pointer_pos) = ui.input(|i| i.pointer.interact_pos()) {
                                        if !response.rect.contains(pointer_pos) {
                                            self.cancel_inline_rename();
                                        }
                                    }
                                }
                            });
                            
                            // Show error message if any
                            if let Some(error) = &self.inline_rename_error {
                                ui.colored_label(egui::Color32::RED, error);
                            }
                            
                            // Continue to render other entries after this file
                            continue;
                        }
                    }
                    
                    // Normal file rendering with file type specific icons
                    let file_icon = self.get_file_icon(path);
                    let response = ui.selectable_label(is_selected, format!("{} {}", file_icon, name));
                    
                    if response.clicked() {
                        if ui.input(|i| i.modifiers.ctrl) {
                            // Ctrl+click for multi-selection
                            if is_selected {
                                self.selected_files.retain(|p| p != path);
                            } else {
                                self.selected_files.push(path.clone());
                            }
                        } else {
                            // Single selection
                            self.selected_files.clear();
                            self.selected_files.push(path.clone());
                        }
                    }
                    
                    // Handle double-click to open files
                    if response.double_clicked() {
                        self.handle_file_double_click(path);
                    }
                    
                    if response.secondary_clicked() {
                        self.show_context_menu = true;
                        self.context_menu_pos = response.interact_pointer_pos().unwrap_or_default();
                        self.context_menu_target = Some(path.clone());
                        if !self.selected_files.contains(path) {
                            self.selected_files.clear();
                            self.selected_files.push(path.clone());
                        }
                    }
                }
                FileSystemEntry::Dir { name, children, path, expanded: _expanded } => {
                    let is_selected = self.selected_files.contains(path);
                    
                    // Check if this folder is being renamed inline
                    if let Some(rename_target) = &self.inline_rename_target {
                        if rename_target == path {
                            // Show inline text editor for folder
                            ui.horizontal(|ui| {
                                ui.label("📁");
                                let text_edit = egui::TextEdit::singleline(&mut self.inline_rename_text)
                                    .desired_width(ui.available_width() - 20.0);
                                let response = ui.add(text_edit);
                                
                                // Auto-focus and select all text
                                if !response.has_focus() {
                                    response.request_focus();
                                }
                                
                                // Handle Enter key to confirm rename
                                if ui.input(|i| i.key_pressed(egui::Key::Enter)) {
                                    self.confirm_inline_rename();
                                }
                                
                                // Handle Escape key to cancel rename
                                if ui.input(|i| i.key_pressed(egui::Key::Escape)) {
                                    self.cancel_inline_rename();
                                }
                                
                                // Cancel rename if clicked outside
                                if response.lost_focus() && ui.input(|i| i.pointer.any_click()) {
                                    if let Some(pointer_pos) = ui.input(|i| i.pointer.interact_pos()) {
                                        if !response.rect.contains(pointer_pos) {
                                            self.cancel_inline_rename();
                                        }
                                    }
                                }
                            });
                            
                            // Show error message if any
                            if let Some(error) = &self.inline_rename_error {
                                ui.colored_label(egui::Color32::RED, error);
                            }
                            
                            // Still show children if expanded, but don't allow folder interaction
                            if !children.is_empty() {
                                ui.indent("folder_children", |ui| {
                                    self.render_file_system_entries(ui, children);
                                });
                            }
                            
                            // Continue to render other entries after this folder
                            continue;
                        }
                    }
                    
                    // Use collapsing header for folders to enable expand/collapse
                    let header_response = ui.collapsing(format!("📁 {}", name), |ui| {
                        // Use existing children data to avoid repeated scanning
                        if !children.is_empty() {
                            self.render_file_system_entries(ui, children);
                        } else {
                            // Only scan if no children data exists
                            if let Ok(child_entries) = FileSystemService::scan_directory(path) {
                                if !child_entries.is_empty() {
                                    self.render_file_system_entries(ui, &child_entries);
                                } else {
                                    ui.label("📂 Empty folder");
                                }
                            } else {
                                ui.label("❌ Cannot access folder");
                            }
                        }
                    });
                    
                    if header_response.header_response.clicked() {
                        if ui.input(|i| i.modifiers.ctrl) {
                            // Ctrl+click for multi-selection
                            if is_selected {
                                self.selected_files.retain(|p| p != path);
                            } else {
                                self.selected_files.push(path.clone());
                            }
                        } else {
                            // Single selection
                            self.selected_files.clear();
                            self.selected_files.push(path.clone());
                        }
                    }
                    
                    if header_response.header_response.secondary_clicked() {
                        self.show_context_menu = true;
                        self.context_menu_pos = header_response.header_response.interact_pointer_pos().unwrap_or_default();
                        self.context_menu_target = Some(path.clone());
                        if !self.selected_files.contains(path) {
                            self.selected_files.clear();
                            self.selected_files.push(path.clone());
                        }
                    }
                }
            }
        }
    }

    pub(crate) fn render_node_tree(ui: &mut egui::Ui, selected_node: &mut Option<String>, nodes: &[UiNode]) {
        for node in nodes {
            let is_selected = selected_node.as_ref() == Some(&node.id);
            
            if node.children.is_empty() {
                // Leaf node - render as selectable label with icon
                ui.horizontal(|ui| {
                    ui.add_space(4.0);
                    ui.label("📄");
                    if ui.selectable_label(is_selected, &node.name).clicked() {
                        *selected_node = Some(node.id.clone());
                    }
                });
            } else {
                // Parent node - render as collapsing header with tree icon
                let header_response = ui.collapsing(
                    egui::RichText::new(&format!("🌳 {}", node.name))
                        .color(if is_selected { ui.visuals().selection.bg_fill } else { ui.visuals().text_color() }),
                    |ui| {
                        ui.indent(&node.id, |ui| {
                            Self::render_node_tree(ui, selected_node, &node.children);
                        });
                    }
                );
                
                // Make the header clickable for selection
                if header_response.header_response.clicked() {
                    *selected_node = Some(node.id.clone());
                }
            }
        }
    }
    
    fn render_context_menu(&mut self, ui: &mut egui::Ui) {
        let mut close_menu = false;
        let mut menu_rect = egui::Rect::NOTHING;
        
        let _area_response = egui::Area::new("file_context_menu".into())
            .fixed_pos(self.context_menu_pos)
            .order(egui::Order::Foreground)
            .show(ui.ctx(), |ui| {
                let frame_response = egui::Frame::popup(ui.style())
                    .show(ui, |ui| {
                        ui.set_min_width(150.0);
                        ui.set_max_width(200.0);
                        ui.spacing_mut().button_padding = egui::vec2(8.0, 4.0);
                        ui.spacing_mut().item_spacing.y = 2.0;
                        
                        // Different menu items based on context
                        let is_target_dir = self.context_menu_target.as_ref().map(|t| t.is_dir()).unwrap_or(false);
                        if self.context_menu_target.is_some() {
                            // Right-clicked on a file or folder
                            ui.vertical(|ui| {
                                // Show new file options for folders
                                if is_target_dir {
                                    ui.horizontal(|ui| {
                                        ui.menu_button("📄 New File", |ui| {
                                            if ui.add_sized([ui.available_width(), 24.0], egui::Button::new("🐍 Python Script (.pyscript)")).clicked() {
                                                self.handle_new_python_script_direct();
                                                close_menu = true;
                                            }
                                            
                                            if ui.add_sized([ui.available_width(), 24.0], egui::Button::new("📦 Module (.module)")).clicked() {
                                                self.handle_new_module_direct();
                                                close_menu = true;
                                            }
                                            
                                            if ui.add_sized([ui.available_width(), 24.0], egui::Button::new("🪟 Window (.vfwindow)")).clicked() {
                                                self.handle_new_window_direct();
                                                close_menu = true;
                                            }
                                            
                                            if ui.add_sized([ui.available_width(), 24.0], egui::Button::new("📄 Custom File")).clicked() {
                                                self.handle_new_custom_file_direct();
                                                close_menu = true;
                                            }
                                        });
                                        
                                        if ui.add_sized([75.0, 24.0], egui::Button::new("📁 New Folder")).clicked() {
                                            self.handle_new_folder_direct();
                                            close_menu = true;
                                        }
                                    });
                                    
                                    ui.separator();
                                }
                                
                                if ui.add_sized([ui.available_width(), 24.0], egui::Button::new(&self.localization.ui_text.copy_button)).clicked() {
                                    self.handle_copy();
                                    close_menu = true;
                                }
                                
                                if ui.add_sized([ui.available_width(), 24.0], egui::Button::new(&self.localization.ui_text.duplicate_button)).clicked() {
                                    self.handle_duplicate();
                                    close_menu = true;
                                }
                                
                                if ui.add_sized([ui.available_width(), 24.0], egui::Button::new(&self.localization.ui_text.cut_button)).clicked() {
                                    self.handle_cut();
                                    close_menu = true;
                                }
                                
                                if self.clipboard_operation.is_some() && !self.clipboard_files.is_empty() {
                                    if ui.add_sized([ui.available_width(), 24.0], egui::Button::new(&self.localization.ui_text.paste_button)).clicked() {
                                        self.handle_paste();
                                        close_menu = true;
                                    }
                                }
                                
                                ui.separator();
                                
                                if ui.add_sized([ui.available_width(), 24.0], egui::Button::new(&self.localization.ui_text.rename_button)).clicked() {
                                    self.handle_rename();
                                    close_menu = true;
                                }
                                
                                if ui.add_sized([ui.available_width(), 24.0], egui::Button::new(&self.localization.ui_text.delete_button)).clicked() {
                                    self.console_messages.push("Delete button clicked in context menu".to_string());
                                    self.handle_delete();
                                    close_menu = true;
                                }
                                
                                ui.separator();
                                
                                if ui.add_sized([ui.available_width(), 24.0], egui::Button::new(&self.localization.ui_text.properties_button)).clicked() {
                                    self.handle_properties();
                                    close_menu = true;
                                }
                            });
                        } else {
                            // Right-clicked on empty space
                            ui.vertical(|ui| {
                                // New File and New Folder buttons side by side
                                ui.horizontal(|ui| {
                                    ui.menu_button("📄 New File", |ui| {
                                        if ui.add_sized([ui.available_width(), 24.0], egui::Button::new("🐍 Python Script (.pyscript)")).clicked() {
                                            self.handle_new_python_script_direct();
                                            close_menu = true;
                                        }
                                        
                                        if ui.add_sized([ui.available_width(), 24.0], egui::Button::new("📦 Module (.module)")).clicked() {
                                            self.handle_new_module_direct();
                                            close_menu = true;
                                        }
                                        
                                        if ui.add_sized([ui.available_width(), 24.0], egui::Button::new("🪟 Window (.vfwindow)")).clicked() {
                                            self.handle_new_window_direct();
                                            close_menu = true;
                                        }
                                        
                                        if ui.add_sized([ui.available_width(), 24.0], egui::Button::new("📄 Custom File")).clicked() {
                                            self.handle_new_custom_file_direct();
                                            close_menu = true;
                                        }
                                    });
                                    
                                    if ui.add_sized([75.0, 24.0], egui::Button::new("📁 New Folder")).clicked() {
                                        self.handle_new_folder_direct();
                                        close_menu = true;
                                    }
                                });
                                
                                ui.separator();
                                
                                if self.clipboard_operation.is_some() && !self.clipboard_files.is_empty() {
                                    ui.separator();
                                    if ui.add_sized([ui.available_width(), 24.0], egui::Button::new(&self.localization.ui_text.paste_button)).clicked() {
                                        self.handle_paste();
                                        close_menu = true;
                                    }
                                }
                            });
                        }
                    });
                menu_rect = frame_response.response.rect;
            });
        
        // Close menu if clicked outside or ESC pressed
        if ui.input(|i| i.key_pressed(egui::Key::Escape)) {
            close_menu = true;
        }
        
        // Check if clicked outside the menu area
        if ui.input(|i| i.pointer.any_click()) {
            if let Some(pointer_pos) = ui.input(|i| i.pointer.interact_pos()) {
                if !menu_rect.contains(pointer_pos) {
                    close_menu = true;
                }
            }
        }
        
        if close_menu {
            self.show_context_menu = false;
        }
    }
    
    fn handle_copy(&mut self) {
        if !self.selected_files.is_empty() {
            self.clipboard_operation = Some(ClipboardOperation::Copy);
            self.clipboard_files = self.selected_files.clone();
            self.console_messages.push(format!("Copied {} item(s)", self.selected_files.len()));
        }
    }
    
    fn handle_duplicate(&mut self) {
        if let Some(target) = &self.context_menu_target {
            let unique_path = FileOperations::generate_unique_name(target);
            match FileOperations::copy(target, &unique_path) {
                Ok(()) => {
                    self.console_messages.push(format!("Duplicated: {} -> {}", 
                        target.file_name().and_then(|n| n.to_str()).unwrap_or("Unknown"),
                        unique_path.file_name().and_then(|n| n.to_str()).unwrap_or("Unknown")
                    ));
                    self.load_project_files();
                }
                Err(e) => {
                    self.console_messages.push(format!("Failed to duplicate: {}", e));
                }
            }
        }
    }
    
    fn handle_cut(&mut self) {
        if !self.selected_files.is_empty() {
            self.clipboard_operation = Some(ClipboardOperation::Cut);
            self.clipboard_files = self.selected_files.clone();
            self.console_messages.push(format!("Cut {} item(s)", self.selected_files.len()));
        }
    }
    
    fn handle_paste(&mut self) {
        if let Some(operation) = &self.clipboard_operation {
            let target_dir = if let Some(target) = &self.context_menu_target {
                if target.is_dir() {
                    target.clone()
                } else {
                    target.parent().unwrap_or(target).to_path_buf()
                }
            } else {
                // Paste in project root
                self.project_manager.get_root_path().cloned().unwrap_or_default()
            };
            
            let mut success_count = 0;
            let mut error_count = 0;
            
            for file_path in &self.clipboard_files {
                let file_name = file_path.file_name()
                    .and_then(|n| n.to_str())
                    .unwrap_or("Unknown");
                let target_path = target_dir.join(file_name);
                
                let result = match operation {
                    ClipboardOperation::Copy => {
                        if target_path.exists() {
                            let unique_path = FileOperations::generate_unique_name(&target_path);
                            FileOperations::copy(file_path, &unique_path)
                        } else {
                            FileOperations::copy(file_path, &target_path)
                        }
                    }
                    ClipboardOperation::Cut => {
                        if target_path.exists() {
                            let unique_path = FileOperations::generate_unique_name(&target_path);
                            FileOperations::move_item(file_path, &unique_path)
                        } else {
                            FileOperations::move_item(file_path, &target_path)
                        }
                    }
                };
                
                match result {
                    Ok(()) => success_count += 1,
                    Err(e) => {
                        error_count += 1;
                        self.console_messages.push(format!("Failed to {} {}: {}", 
                            match operation {
                                ClipboardOperation::Copy => "copy",
                                ClipboardOperation::Cut => "move",
                            },
                            file_name, e
                        ));
                    }
                }
            }
            
            if success_count > 0 {
                self.console_messages.push(format!("{} {} item(s) to {}", 
                    match operation {
                        ClipboardOperation::Copy => "Copied",
                        ClipboardOperation::Cut => "Moved",
                    },
                    success_count, 
                    target_dir.display()
                ));
            }
            
            if error_count > 0 {
                self.console_messages.push(format!("{} operation(s) failed", error_count));
            }
            
            // Clear clipboard after cut operation
            if matches!(operation, ClipboardOperation::Cut) && success_count > 0 {
                self.clipboard_operation = None;
                self.clipboard_files.clear();
            }
            
            // Refresh file system
            self.load_project_files();
        }
    }
    
    fn handle_rename(&mut self) {
        if let Some(target) = &self.context_menu_target {
            // Start inline rename mode
            self.inline_rename_target = Some(target.clone());
            self.inline_rename_text = target.file_name()
                .and_then(|name| name.to_str())
                .unwrap_or("")
                .to_string();
            self.inline_rename_error = None;
        }
    }
    
    fn handle_delete(&mut self) {
        let items_to_delete = if !self.selected_files.is_empty() {
            self.selected_files.clone()
        } else if let Some(target) = &self.context_menu_target {
            vec![target.clone()]
        } else {
            Vec::new()
        };
        
        if !items_to_delete.is_empty() {
            self.delete_dialog = Some(DeleteDialog::new(items_to_delete));
        }
    }
    
    fn handle_properties(&mut self) {
        if let Some(target) = &self.context_menu_target {
            self.properties_dialog = Some(PropertiesDialog::new(target.clone()));
        }
    }
    
    fn confirm_inline_rename(&mut self) {
        if let Some(target) = &self.inline_rename_target {
            // Get original filename for comparison
            let original_name = target.file_name()
                .and_then(|name| name.to_str())
                .unwrap_or("");
            
            // If name hasn't changed, just close the input box
            if self.inline_rename_text == original_name {
                self.cancel_inline_rename();
                return;
            }
            
            // Validate the new name
            if let Err(error) = FileOperations::validate_filename(&self.inline_rename_text) {
                self.inline_rename_error = Some(error.to_string());
                return;
            }
            
            // Create new path
            let mut new_path = target.clone();
            new_path.set_file_name(&self.inline_rename_text);
            
            // Check if file already exists
            if new_path.exists() && new_path != *target {
                self.inline_rename_error = Some("A file with this name already exists".to_string());
                return;
            }
            
            // Perform the rename
            match FileOperations::rename(target, &new_path) {
                Ok(()) => {
                    self.console_messages.push(format!("Renamed: {} -> {}", 
                        target.file_name().and_then(|n| n.to_str()).unwrap_or("Unknown"),
                        new_path.file_name().and_then(|n| n.to_str()).unwrap_or("Unknown")
                    ));
                    self.load_project_files();
                    self.cancel_inline_rename();
                }
                Err(e) => {
                    self.inline_rename_error = Some(format!("Failed to rename: {}", e));
                }
            }
        }
    }
    
    fn cancel_inline_rename(&mut self) {
        self.inline_rename_target = None;
        self.inline_rename_text.clear();
        self.inline_rename_error = None;
        self.inline_rename_cursor_pos = None;
    }
    
    /// Get file icon based on file extension
    fn get_file_icon(&self, path: &std::path::Path) -> &'static str {
        if let Some(extension) = path.extension().and_then(|ext| ext.to_str()) {
            match extension {
                "pyscript" => "🐍",
                "module" => "📦",
                "vfwindow" => "🖼️", // Using framed picture emoji as alternative to 🪟
                "txt" => "📝",
                "json" => "📋",
                "md" => "📖",
                "rs" => "🦀",
                "py" => "🐍",
                "js" => "📜",
                "html" => "🌐",
                "css" => "🎨",
                "xml" => "📄",
                "toml" => "⚙️",
                "yaml" | "yml" => "📝",
                _ => "📄", // Default file icon
            }
        } else {
            "📄" // Default for files without extension
        }
    }
    

    
    // Direct creation methods for inline rename
    fn handle_new_python_script_direct(&mut self) {
        self.create_file_with_inline_rename(".pyscript", "#!/usr/bin/env python3\n# -*- coding: utf-8 -*-\n\ndef main():\n    pass\n\nif __name__ == '__main__':\n    main()\n");
    }
    
    fn handle_new_module_direct(&mut self) {
        self.create_file_with_inline_rename(".module", "{\n  \"name\": \"NewModule\",\n  \"version\": \"1.0.0\",\n  \"exports\": {}\n}");
    }
    
    fn handle_new_window_direct(&mut self) {
        self.create_file_with_inline_rename(".vfwindow", "{\n  \"version\": \"1.0\",\n  \"window_size\": [800, 600],\n  \"metadata\": {},\n  \"root_node\": {\n    \"id\": \"root\",\n    \"name\": \"MainWindow\",\n    \"node_type\": \"Window\",\n    \"position\": [0, 0],\n    \"size\": [800, 600],\n    \"visible\": true,\n    \"properties\": {},\n    \"children\": []\n  }\n}");
    }
    
    fn handle_new_custom_file_direct(&mut self) {
        self.create_file_with_inline_rename(".txt", "");
    }
    
    fn handle_new_folder_direct(&mut self) {
        self.create_folder_with_inline_rename();
    }
    
    fn create_file_with_inline_rename(&mut self, extension: &str, content: &str) {
        let target_dir = if let Some(target) = &self.context_menu_target {
            if target.is_dir() {
                target.clone()
            } else {
                target.parent().unwrap_or(target).to_path_buf()
            }
        } else {
            self.project_manager.get_root_path().cloned().unwrap_or_default()
        };
        
        // Generate unique filename
        let mut counter = 1;
        let base_name = format!("NewFile{}", extension);
        let mut file_path = target_dir.join(&base_name);
        
        while file_path.exists() {
            counter += 1;
            let name = format!("NewFile{}{}", counter, extension);
            file_path = target_dir.join(&name);
        }
        
        // Create the file
        match std::fs::write(&file_path, content) {
            Ok(()) => {
                self.console_messages.push(format!("Created file: {}", 
                    file_path.file_name().and_then(|n| n.to_str()).unwrap_or("Unknown")
                ));
                
                // Refresh file system
                self.load_project_files();
                
                // Start inline rename with extension
                self.inline_rename_target = Some(file_path.clone());
                
                // Set default filename with extension and cursor position before extension
                let base_name = file_path.file_stem()
                    .and_then(|name| name.to_str())
                    .unwrap_or("NewFile");
                self.inline_rename_text = format!("{}{}", base_name, extension);
                
                // Set cursor position before the extension
                self.inline_rename_cursor_pos = Some(base_name.len());
                self.inline_rename_error = None;
                
                // Select the new file
                self.selected_files.clear();
                self.selected_files.push(file_path);
            }
            Err(e) => {
                self.console_messages.push(format!("Failed to create file: {}", e));
            }
        }
    }
    
    fn create_folder_with_inline_rename(&mut self) {
        let target_dir = if let Some(target) = &self.context_menu_target {
            if target.is_dir() {
                target.clone()
            } else {
                target.parent().unwrap_or(target).to_path_buf()
            }
        } else {
            self.project_manager.get_root_path().cloned().unwrap_or_default()
        };
        
        // Generate unique folder name
        let mut counter = 1;
        let base_name = "NewFolder";
        let mut folder_path = target_dir.join(base_name);
        
        while folder_path.exists() {
            counter += 1;
            let name = format!("NewFolder{}", counter);
            folder_path = target_dir.join(&name);
        }
        
        // Create the folder
        match std::fs::create_dir(&folder_path) {
            Ok(()) => {
                self.console_messages.push(format!("Created folder: {}", 
                    folder_path.file_name().and_then(|n| n.to_str()).unwrap_or("Unknown")
                ));
                
                // Refresh file system
                self.load_project_files();
                
                // Start inline rename
                self.inline_rename_target = Some(folder_path.clone());
                self.inline_rename_text = folder_path.file_name()
                    .and_then(|name| name.to_str())
                    .unwrap_or("NewFolder")
                    .to_string();
                self.inline_rename_error = None;
                
                // Select the new folder
                self.selected_files.clear();
                self.selected_files.push(folder_path);
            }
            Err(e) => {
                self.console_messages.push(format!("Failed to create folder: {}", e));
            }
        }
    }
    
    fn handle_file_double_click(&mut self, path: &std::path::PathBuf) {
        if let Some(extension) = path.extension().and_then(|ext| ext.to_str()) {
            match extension {
                "vfwindow" => {
                    // Open .vfwindow file in window editor
                    match std::fs::read_to_string(path) {
                        Ok(content) => {
                            match crate::window_editor::WindowDocument::from_json(&content) {
                                Ok(document) => {
                                    self.current_window_document = Some(document);
                                    self.current_window_file_path = Some(path.clone());
                                    self.current_tab = crate::EditorTab::WindowEditor;
                                    self.console_messages.push(format!("Opened window file: {}", path.display()));
                                }
                                Err(e) => {
                                    self.console_messages.push(format!("Failed to parse window file: {}", e));
                                }
                            }
                        }
                        Err(e) => {
                            self.console_messages.push(format!("Failed to read window file: {}", e));
                        }
                    }
                }
                _ => {
                    // Open other files in code editor
                    match std::fs::read_to_string(path) {
                        Ok(content) => {
                            self.code_content = content;
                            self.current_file_path = Some(path.clone());
                            self.current_tab = crate::EditorTab::CodeEditor;
                            self.console_messages.push(format!("Opened file: {}", path.display()));
                        }
                        Err(e) => {
                            self.console_messages.push(format!("Failed to open file: {}", e));
                        }
                    }
                }
            }
        }
     }

     
     fn handle_node_context_menu(&mut self, ctx: &egui::Context) {
         if self.show_node_context_menu {
             let mut close_menu = false;
             let mut menu_rect = egui::Rect::NOTHING;
             
             let _area_response = egui::Area::new("node_context_menu".into())
                 .fixed_pos(self.node_context_menu_pos)
                 .order(egui::Order::Foreground)
                 .show(ctx, |ui| {
                     let frame_response = egui::Frame::popup(ui.style())
                         .show(ui, |ui| {
                             ui.set_min_width(150.0);
                             ui.set_max_width(200.0);
                             ui.spacing_mut().button_padding = egui::vec2(8.0, 4.0);
                             ui.spacing_mut().item_spacing.y = 2.0;
                             
                             ui.vertical(|ui| {
                                 if ui.add_sized([ui.available_width(), 24.0], egui::Button::new("📋 Copy")).clicked() {
                                     self.handle_node_copy();
                                     close_menu = true;
                                 }
                                 
                                 if ui.add_sized([ui.available_width(), 24.0], egui::Button::new("✂️ Cut")).clicked() {
                                     self.handle_node_cut();
                                     close_menu = true;
                                 }
                                 
                                 if ui.add_sized([ui.available_width(), 24.0], egui::Button::new("📄 Duplicate")).clicked() {
                                     self.handle_node_duplicate();
                                     close_menu = true;
                                 }
                                 
                                 if self.node_operations.has_clipboard_content() {
                                     if ui.add_sized([ui.available_width(), 24.0], egui::Button::new("📌 Paste")).clicked() {
                                         self.handle_node_paste();
                                         close_menu = true;
                                     }
                                 }
                                 
                                 ui.separator();
                                 
                                 if ui.add_sized([ui.available_width(), 24.0], egui::Button::new("🗑️ Delete")).clicked() {
                                     self.handle_node_delete();
                                     close_menu = true;
                                 }
                                 
                                 ui.separator();
                                 
                                 if ui.add_sized([ui.available_width(), 24.0], egui::Button::new("📦 New Container")).clicked() {
                                     self.handle_node_new_container();
                                     close_menu = true;
                                 }
                             });
                         });
                     menu_rect = frame_response.response.rect;
                 });
             
             // Close menu if clicked outside or ESC pressed
             if ctx.input(|i| i.key_pressed(egui::Key::Escape)) {
                 close_menu = true;
             }
             
             // Check if clicked outside the menu area
             if ctx.input(|i| i.pointer.any_click()) {
                 if let Some(pointer_pos) = ctx.input(|i| i.pointer.interact_pos()) {
                     if !menu_rect.contains(pointer_pos) {
                         close_menu = true;
                     }
                 }
             }
             
             if close_menu {
                 self.show_node_context_menu = false;
             }
         }
     }
     
     fn handle_node_copy(&mut self) {
         if let (Some(target_id), Some(document)) = (&self.node_context_menu_target, &self.current_window_document) {
             if let Some(node) = document.root_node.find_node_by_id(target_id) {
                 self.node_operations.copy_nodes(vec![node.clone()]);
                 self.console_messages.push(format!("Copied node: {}", node.name));
             }
         }
     }
     
     fn handle_node_cut(&mut self) {
         if let (Some(target_id), Some(document)) = (&self.node_context_menu_target, &self.current_window_document) {
             if let Some(node) = document.root_node.find_node_by_id(target_id) {
                 self.node_operations.cut_nodes(vec![node.clone()]);
                 self.console_messages.push(format!("Cut node: {}", node.name));
             }
         }
     }
     
     fn handle_node_duplicate(&mut self) {
         if let (Some(target_id), Some(document)) = (&self.node_context_menu_target, &mut self.current_window_document) {
             if let Some(node) = document.root_node.find_node_by_id(target_id) {
                 let duplicated = self.node_operations.duplicate_node(node);
                 
                 // Find parent and add duplicated node
                 if let Some(parent_id) = Self::find_parent_node_id(&document.root_node, target_id) {
                     if let Some(parent_node) = document.root_node.find_node_by_id_mut(&parent_id) {
                         parent_node.add_child(duplicated.clone());
                         document.update_modified_time();
                         self.console_messages.push(format!("Duplicated node: {}", duplicated.name));
                     }
                 }
             }
         }
     }
     
     fn handle_node_paste(&mut self) {
         if let (Some(target_id), Some(document)) = (&self.node_context_menu_target, &mut self.current_window_document) {
             if let Some(pasted_nodes) = self.node_operations.paste_nodes() {
                 if let Some(target_node) = document.root_node.find_node_by_id_mut(target_id) {
                     for node in pasted_nodes {
                         target_node.add_child(node.clone());
                         self.console_messages.push(format!("Pasted node: {}", node.name));
                     }
                     document.update_modified_time();
                 }
             }
         }
     }
     
     fn handle_node_delete(&mut self) {
         if let (Some(target_id), Some(document)) = (&self.node_context_menu_target, &mut self.current_window_document) {
             if target_id != "root" { // Don't allow deleting root node
                 if document.root_node.remove_child(target_id) {
                     document.update_modified_time();
                     self.console_messages.push("Deleted node".to_string());
                     
                     // Clear selection if deleted node was selected
                     if self.window_editor_selected_node.as_ref() == Some(target_id) {
                         self.window_editor_selected_node = None;
                     }
                 }
             } else {
                 self.console_messages.push("Cannot delete root node".to_string());
             }
         }
     }
     
     fn handle_node_new_container(&mut self) {
         if let (Some(target_id), Some(document)) = (&self.node_context_menu_target, &mut self.current_window_document) {
             let new_container = self.node_operations.create_container(
                 "New Container".to_string(),
                 [0.0, 0.0],
                 [200.0, 100.0]
             );
             
             if let Some(target_node) = document.root_node.find_node_by_id_mut(target_id) {
                 target_node.add_child(new_container.clone());
                 document.update_modified_time();
                 self.console_messages.push(format!("Added container: {}", new_container.name));
             }
         }
     }
     
     fn find_parent_node_id(root: &crate::window_editor::WindowNode, target_id: &str) -> Option<String> {
         for child in &root.children {
             if child.id == target_id {
                 return Some(root.id.clone());
             }
             if let Some(parent_id) = Self::find_parent_node_id(child, target_id) {
                 return Some(parent_id);
             }
         }
         None
     }
}