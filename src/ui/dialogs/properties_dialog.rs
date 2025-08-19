//! Properties dialog for files and directories

use eframe::egui;
use std::path::PathBuf;
use super::{Dialog, DialogResult};

#[derive(Debug)]
pub struct PropertiesDialog {
    is_open: bool,
    target_path: PathBuf,
    /// File name
    name: String,
    /// File size in bytes
    size: u64,
    /// Creation time
    created: String,
    /// Last modified time
    modified: String,
    /// Last accessed time
    accessed: String,
}

impl PropertiesDialog {
    pub fn new(path: PathBuf) -> Self {
        let name = path.file_name()
            .and_then(|n| n.to_str())
            .unwrap_or("Unknown")
            .to_string();
        
        let (size, created, modified, accessed) = if let Ok(metadata) = std::fs::metadata(&path) {
            let size = metadata.len();
            
            let created = metadata.created()
                .ok()
                .and_then(|time| {
                    time.duration_since(std::time::UNIX_EPOCH)
                        .ok()
                        .map(|duration| duration.as_secs())
                })
                .and_then(|secs| {
                    let datetime = chrono::DateTime::from_timestamp(secs as i64, 0)
                        .unwrap_or_default();
                    Some(datetime.format("%Y-%m-%d %H:%M:%S").to_string())
                })
                .unwrap_or_else(|| "Unknown".to_string());
            
            let modified = metadata.modified()
                .ok()
                .and_then(|time| {
                    time.duration_since(std::time::UNIX_EPOCH)
                        .ok()
                        .map(|duration| duration.as_secs())
                })
                .and_then(|secs| {
                    let datetime = chrono::DateTime::from_timestamp(secs as i64, 0)
                        .unwrap_or_default();
                    Some(datetime.format("%Y-%m-%d %H:%M:%S").to_string())
                })
                .unwrap_or_else(|| "Unknown".to_string());
            
            let accessed = metadata.accessed()
                .ok()
                .and_then(|time| {
                    time.duration_since(std::time::UNIX_EPOCH)
                        .ok()
                        .map(|duration| duration.as_secs())
                })
                .and_then(|secs| {
                    let datetime = chrono::DateTime::from_timestamp(secs as i64, 0)
                        .unwrap_or_default();
                    Some(datetime.format("%Y-%m-%d %H:%M:%S").to_string())
                })
                .unwrap_or_else(|| "Unknown".to_string());
            
            (size, created, modified, accessed)
        } else {
            (0, "Unknown".to_string(), "Unknown".to_string(), "Unknown".to_string())
        };
        
        Self {
            is_open: true,
            target_path: path,
            name,
            size,
            created,
            modified,
            accessed,
        }
    }
}

impl Dialog for PropertiesDialog {
    fn show(&mut self, ctx: &egui::Context) -> DialogResult {
        let mut result = DialogResult::Open;
        
        if !self.is_open {
            return DialogResult::None;
        }
        
        egui::Window::new("Properties")
            .collapsible(false)
            .resizable(false)
            .show(ctx, |ui| {
                ui.vertical(|ui| {
                    ui.label(format!("Name: {}", self.name));
                    ui.label(format!("Path: {}", self.target_path.display()));
                    ui.separator();
                    
                    if self.target_path.is_file() {
                        ui.label(format!("Size: {} bytes", self.size));
                    } else {
                        ui.label("Type: Directory");
                    }
                    
                    ui.separator();
                    ui.label(format!("Created: {}", self.created));
                    ui.label(format!("Modified: {}", self.modified));
                    ui.label(format!("Accessed: {}", self.accessed));
                    
                    ui.add_space(10.0);
                    
                    ui.horizontal(|ui| {
                        ui.with_layout(egui::Layout::right_to_left(egui::Align::Center), |ui| {
                            if ui.button("Close").clicked() {
                                self.is_open = false;
                                result = DialogResult::Cancelled;
                            }
                        });
                    });
                });
            });
        
        // Close on Escape
        if ctx.input(|i| i.key_pressed(egui::Key::Escape)) {
            self.is_open = false;
            result = DialogResult::Cancelled;
        }
        
        result
    }
    
    
}