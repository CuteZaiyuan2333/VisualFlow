//! UI module containing all user interface components
//! 
//! This module organizes the UI code into separate, focused components:
//! - menu_bar: Top menu bar with File, Edit, View, Help menus
//! - left_panel: Left sidebar with Node Tree and File System
//! - right_panel: Right sidebar with Properties panel
//! - center_panel: Main content area with editors and console

pub mod menu_bar;
pub mod left_panel;
pub mod right_panel;
pub mod center_panel;
pub mod layout;
pub mod syntax_highlighting;
pub mod file_operations;
pub mod dialogs;