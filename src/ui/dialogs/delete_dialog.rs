//! Delete confirmation dialog

use eframe::egui;
use std::path::PathBuf;
use super::{Dialog, DialogResult};

#[derive(Debug)]
pub struct DeleteDialog {
    is_open: bool,
    items: Vec<PathBuf>,
}

impl DeleteDialog {
    pub fn new(items: Vec<PathBuf>) -> Self {
        Self {
            is_open: true,
            items,
        }
    }
    
    pub fn get_items(&self) -> &[PathBuf] {
        &self.items
    }
}

impl Dialog for DeleteDialog {
    fn show(&mut self, ctx: &egui::Context) -> DialogResult {
        let mut result = DialogResult::Open;
        
        if !self.is_open {
            return DialogResult::None;
        }
        
        // Force a repaint to ensure the dialog is visible
        ctx.request_repaint();
        
        // Create modal background that doesn't interfere with dialog interaction
        let mut dialog_rect = egui::Rect::NOTHING;
        
        // Show the dialog window first to get its rect
        let window_response = egui::Window::new("⚠️ Delete Confirmation")
            .collapsible(false)
            .resizable(false)
            .movable(false)
            .anchor(egui::Align2::CENTER_CENTER, egui::Vec2::ZERO)
            .fixed_size(egui::vec2(400.0, 300.0))
            .show(ctx, |ui| {
                dialog_rect = ui.max_rect(); // Store dialog rect for background calculation
                ui.vertical_centered(|ui| {
                    ui.add_space(10.0);
                    ui.label(egui::RichText::new("Are you sure you want to delete the following items?").size(14.0));
                    ui.add_space(15.0);
                    
                    // Items list with background
                    egui::Frame::none()
                        .fill(egui::Color32::from_gray(240))
                        .stroke(egui::Stroke::new(1.0, egui::Color32::from_gray(200)))
                        .inner_margin(egui::Margin::same(10.0))
                        .show(ui, |ui| {
                            egui::ScrollArea::vertical()
                                .max_height(150.0)
                                .show(ui, |ui| {
                                    for item in &self.items {
                                        ui.horizontal(|ui| {
                                            ui.label("🗑️");
                                            ui.label(item.file_name().and_then(|n| n.to_str()).unwrap_or("Unknown"));
                                        });
                                    }
                                });
                        });
                    
                    ui.add_space(20.0);
                    
                    // Buttons
                    ui.horizontal(|ui| {
                        ui.add_space((ui.available_width() - 160.0) / 2.0); // Center buttons
                        
                        if ui.add_sized([70.0, 30.0], egui::Button::new("Cancel")).clicked() {
                            self.is_open = false;
                            result = DialogResult::Cancelled;
                        }
                        
                        ui.add_space(20.0);
                        
                        let delete_button = egui::Button::new(egui::RichText::new("Delete").color(egui::Color32::WHITE))
                            .fill(egui::Color32::from_rgb(220, 53, 69));
                        if ui.add_sized([70.0, 30.0], delete_button).clicked() {
                            self.is_open = false;
                            result = DialogResult::Confirmed;
                        }
                    });
                    
                    ui.add_space(10.0);
                });
            });
        
        // Create modal background after dialog to ensure proper layering
        if let Some(window_response) = window_response {
            dialog_rect = window_response.response.rect;
            
            // Draw modal background that excludes the dialog area
            egui::Area::new(egui::Id::new("delete_dialog_modal_bg"))
                .order(egui::Order::Background)
                .show(ctx, |ui| {
                    let screen_rect = ctx.screen_rect();
                    let painter = ui.painter();
                    painter.rect_filled(screen_rect, 0.0, egui::Color32::from_black_alpha(128));
                    
                    // Check for clicks outside the dialog
                    if ui.input(|i| i.pointer.any_click()) {
                        if let Some(click_pos) = ui.input(|i| i.pointer.interact_pos()) {
                            if !dialog_rect.contains(click_pos) {
                                self.is_open = false;
                                result = DialogResult::Cancelled;
                            }
                        }
                    }
                });
        }
        
        // Close on Escape
        if ctx.input(|i| i.key_pressed(egui::Key::Escape)) {
            self.is_open = false;
            result = DialogResult::Cancelled;
        }
        
        result
    }
    
    
}