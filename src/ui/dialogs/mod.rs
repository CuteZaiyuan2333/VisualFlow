//! Dialog modules for various UI interactions

pub mod delete_dialog;
pub mod properties_dialog;

// Re-export common dialog types
pub use delete_dialog::*;
pub use properties_dialog::*;

/// Common dialog result enum
#[derive(Debug, Clone, PartialEq)]
pub enum DialogResult {
    #[allow(dead_code)]
    Ok,
    Confirmed,
    Open,
    Cancelled,
    None,
}

/// Common dialog trait
pub trait Dialog {
    fn show(&mut self, ctx: &egui::Context) -> DialogResult;
}