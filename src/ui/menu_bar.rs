use eframe::egui;
use crate::EguiCodeGeneratorApp;


impl EguiCodeGeneratorApp {
    pub fn render_menu_bar(&mut self, ctx: &egui::Context) {
        let mut new_file_clicked = false;
        let mut new_project_clicked = false;
        let mut open_file_clicked = false;
        let mut open_project_clicked = false;
        let mut close_project_clicked = false;
        let mut save_file_clicked = false;
        let mut save_as_file_clicked = false;
        let mut save_project_clicked = false;
        let mut export_project_clicked = false;
        let mut exit_clicked = false;
        
        egui::TopBottomPanel::top("menu_bar").show(ctx, |ui| {
            egui::menu::bar(ui, |ui| {
                ui.menu_button(&self.localization.ui_text.file_menu, |ui| {
                    if ui.button("New File").clicked() {
                        new_file_clicked = true;
                    }
                    if ui.button("New Project").clicked() {
                        new_project_clicked = true;
                    }
                    ui.separator();
                    if ui.button("Open File").clicked() {
                        open_file_clicked = true;
                    }
                    if ui.button("Open Project").clicked() {
                        open_project_clicked = true;
                    }
                    ui.separator();
                    if ui.button("Save").clicked() {
                        save_file_clicked = true;
                    }
                    if ui.button("Save As...").clicked() {
                        save_as_file_clicked = true;
                    }
                    if ui.button(&self.localization.ui_text.save_button).clicked() {
                        save_project_clicked = true;
                    }
                    ui.separator();
                    if ui.button("Export").clicked() {
                        export_project_clicked = true;
                    }
                    ui.separator();
                    if self.project_manager.has_open_project() {
                        if ui.button("Close Project").clicked() {
                            close_project_clicked = true;
                        }
                    }
                    if ui.button("Exit").clicked() {
                        exit_clicked = true;
                    }
                });
                
                ui.menu_button(&self.localization.ui_text.edit_menu, |ui| {
                    if ui.button("Undo").clicked() {}
                    if ui.button("Redo").clicked() {}
                });
                
                ui.menu_button(&self.localization.ui_text.view_menu, |ui| {
                    ui.checkbox(&mut self.show_node_tree, "Node Tree");
                    ui.checkbox(&mut self.show_file_system, "File System");
                    ui.checkbox(&mut self.show_properties, "Properties");
                    ui.checkbox(&mut self.show_console, "Console");
                });
                
                ui.menu_button(&self.localization.ui_text.help_menu, |ui| {
                    if ui.button("About").clicked() {
                        self.console_messages.push("Egui Code Generator v0.1.0".to_string());
                    }
                });
            });
        });
        
        // Handle menu actions after the UI is rendered
        if new_file_clicked {
            // Create new file in code editor
            self.current_file_path = None;
            self.code_content = String::new();
            self.console_messages.push("New file created".to_string());
        }
        if new_project_clicked {
            self.handle_new_project();
        }
        if open_file_clicked {
            self.handle_open_file();
        }
        if open_project_clicked {
            self.handle_open_project();
        }
        if save_file_clicked {
            self.handle_save_file();
        }
        if save_as_file_clicked {
            self.handle_save_as_file();
        }
        if save_project_clicked {
            self.handle_save_project();
        }
        if export_project_clicked {
            self.handle_export_project();
        }
        if close_project_clicked {
            self.handle_close_project();
        }
        if exit_clicked {
            self.handle_exit_application();
        }
    }
    
    // File operation handlers
    fn handle_open_file(&mut self) {
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
    
    fn handle_save_file(&mut self) {
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
            self.handle_save_as_file();
        }
    }
    
    fn handle_save_as_file(&mut self) {
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
    
    // Note: handle_new_project, handle_open_project, handle_save_project, 
    // handle_export_project, and handle_exit_application are implemented in main.rs
    // to avoid duplicate definitions
    
    fn handle_close_project(&mut self) {
        if let Some(name) = self.project_manager.get_project_name() {
            self.console_messages.push(format!("Closed project: {}", name));
        }
        self.project_manager.close_project();
        self.file_system_tree = self.project_manager.get_file_system_tree();
    }
}