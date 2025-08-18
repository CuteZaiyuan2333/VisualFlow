use eframe::egui;
use crate::EguiCodeGeneratorApp;
use std::path::PathBuf;

impl EguiCodeGeneratorApp {
    pub fn render_menu_bar(&mut self, ctx: &egui::Context) {
        let mut new_project_clicked = false;
        let mut open_project_clicked = false;
        let mut close_project_clicked = false;
        let mut save_project_clicked = false;
        
        egui::TopBottomPanel::top("menu_bar").show(ctx, |ui| {
            egui::menu::bar(ui, |ui| {
                ui.menu_button(&self.localization.ui_text.file_menu, |ui| {
                    if ui.button("New Project").clicked() {
                        new_project_clicked = true;
                    }
                    if ui.button("Open Project").clicked() {
                        open_project_clicked = true;
                    }
                    ui.separator();
                    if ui.button(&self.localization.ui_text.save_button).clicked() {
                        save_project_clicked = true;
                    }
                    if self.project_manager.has_open_project() {
                        if ui.button("Close Project").clicked() {
                            close_project_clicked = true;
                        }
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
        if new_project_clicked {
            self.handle_new_project();
        }
        if open_project_clicked {
            self.handle_open_project();
        }
        if save_project_clicked {
            self.console_messages.push("Project saved".to_string());
        }
        if close_project_clicked {
            self.handle_close_project();
        }
    }
    
    fn handle_new_project(&mut self) {
        // For now, create a simple dialog using console
        // In a real implementation, you'd use a proper file dialog
        let project_name = "NewProject".to_string();
        let project_path = PathBuf::from(format!("./projects/{}", project_name));
        
        match self.project_manager.create_project(project_path, project_name.clone()) {
            Ok(()) => {
                self.file_system_tree = self.project_manager.get_file_system_tree();
                self.console_messages.push(format!("Created new project: {}", project_name));
            }
            Err(e) => {
                self.console_messages.push(format!("Failed to create project: {}", e));
            }
        }
    }
    
    fn handle_open_project(&mut self) {
        // For now, try to open a hardcoded path
        // In a real implementation, you'd use a proper file dialog
        let project_path = PathBuf::from("./projects/NewProject");
        
        match self.project_manager.open_project(project_path) {
            Ok(()) => {
                self.file_system_tree = self.project_manager.get_file_system_tree();
                if let Some(name) = self.project_manager.get_project_name() {
                    self.console_messages.push(format!("Opened project: {}", name));
                }
            }
            Err(e) => {
                self.console_messages.push(format!("Failed to open project: {}", e));
            }
        }
    }
    
    fn handle_close_project(&mut self) {
        if let Some(name) = self.project_manager.get_project_name() {
            self.console_messages.push(format!("Closed project: {}", name));
        }
        self.project_manager.close_project();
        self.file_system_tree = self.project_manager.get_file_system_tree();
    }
}