use eframe::egui;
use crate::EguiCodeGeneratorApp;

impl EguiCodeGeneratorApp {
    pub fn render_menu_bar(&mut self, ctx: &egui::Context) {
        egui::TopBottomPanel::top("menu_bar").show(ctx, |ui| {
            egui::menu::bar(ui, |ui| {
                ui.menu_button(&self.localization.ui_text.file_menu, |ui| {
                    if ui.button("New Project").clicked() {
                        self.console_messages.push("New project created".to_string());
                    }
                    if ui.button(&self.localization.ui_text.save_button).clicked() {
                        self.console_messages.push("Project saved".to_string());
                    }
                    if ui.button(&self.localization.ui_text.load_button).clicked() {
                        self.console_messages.push("Project loaded".to_string());
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
    }
}