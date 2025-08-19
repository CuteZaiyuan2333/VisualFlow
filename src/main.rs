use eframe::egui;
use std::collections::HashMap;
use serde::Deserialize;
use std::io::Write;
use zip::write::FileOptions;

mod ui;
mod project;
mod filesystem;
mod window_editor;

// Import project management types
use project::ProjectManager;
use crate::filesystem::{FileSystemService, FileSystemEntry as ProjectFileSystemEntry};

#[derive(Deserialize, Debug)]
struct UiText {

    file_menu: String,
    edit_menu: String,
    view_menu: String,
    help_menu: String,

    properties_panel_title: String,


    save_button: String,


    node_tree_title: String,
    file_system_title: String,
    #[allow(dead_code)]
    new_button: String,
    #[allow(dead_code)]
    open_button: String,
    #[allow(dead_code)]
    save_as_button: String,
    #[allow(dead_code)]
    export_button: String,
    #[allow(dead_code)]
    exit_button: String,
    copy_button: String,
    duplicate_button: String,
    cut_button: String,
    paste_button: String,
    delete_button: String,
    rename_button: String,
    properties_button: String,
}

#[derive(Deserialize, Debug)]
struct LocalizationData {
    ui_text: UiText,
}

// Node tree structure for UI components
#[derive(Debug, Clone)]
struct UiNode {
    id: String,
    name: String,
    node_type: String,
    children: Vec<UiNode>,
    properties: HashMap<String, String>,
}



// Editor tab types
#[derive(Debug, Clone, PartialEq)]
enum EditorTab {
    WindowEditor,
    CodeEditor,
    NodeEditor,
}

// Clipboard operation types
#[derive(Debug, Clone, PartialEq)]
enum ClipboardOperation {
    Copy,
    Cut,
}

// Node graph structures for visual programming
#[derive(Debug, Clone)]
pub struct GraphNode {
    pub id: String,
    pub title: String,
    pub position: egui::Vec2,
    pub size: egui::Vec2,
    pub node_type: String,
    pub inputs: Vec<NodePort>,
    pub outputs: Vec<NodePort>,
    pub properties: HashMap<String, String>,
}

#[derive(Debug, Clone)]
pub struct NodePort {
    pub id: String,
    pub name: String,
    pub port_type: String,
    pub position: egui::Vec2, // Relative to node position
}

#[derive(Debug, Clone)]
pub struct NodeConnection {
    pub id: String,
    pub from_node: String,
    pub from_port: String,
    pub to_node: String,
    pub to_port: String,
}

#[derive(Debug, Clone)]
pub struct NodeGraph {
    pub nodes: Vec<GraphNode>,
    pub connections: Vec<NodeConnection>,
    pub canvas_offset: egui::Vec2,
    pub canvas_zoom: f32,
    pub selected_node: Option<String>,
    pub selected_nodes: Vec<String>,
    pub selected_port: Option<(String, String, bool)>, // (node_id, port_id, is_output)
    pub dragging_node: Option<String>,
    pub drag_offset: egui::Vec2,
    pub drag_start_mouse_pos: Option<egui::Pos2>, // Mouse position when drag started
    pub drag_start_node_pos: Option<egui::Vec2>, // Node position when drag started
    pub selection_box: Option<egui::Rect>,
    pub selection_start: Option<egui::Pos2>,
    pub is_selecting: bool,
    pub context_menu_pos: Option<egui::Pos2>,
    pub show_context_menu: bool,
    pub right_drag_start: Option<egui::Pos2>,
    pub is_right_dragging: bool,
    pub right_drag_path: Vec<egui::Pos2>,
    pub snap_to_grid: bool,
    pub pre_drag_selected_node: Option<String>,
    pub pre_drag_selected_nodes: Vec<String>,
    pub is_panning: bool, // For middle mouse button panning
    pub pan_start_pos: Option<egui::Pos2>, // Pan start position
    pub pan_start_offset: Option<egui::Vec2>, // Canvas offset when pan started
    pub selection_shift_held: bool, // Whether Shift was held when selection started
    // Connection state
    pub is_connecting: bool, // Whether currently creating a connection
    pub connecting_from: Option<(String, String, bool)>, // (node_id, port_id, is_output)
    pub temp_connection_end: Option<egui::Pos2>, // Temporary connection end position following mouse
}

// Main application state
pub struct EguiCodeGeneratorApp {
    localization: LocalizationData,
    // UI state
    selected_node: Option<String>,
    node_tree: Vec<UiNode>,
    file_system_tree: Vec<ProjectFileSystemEntry>,
    current_tab: EditorTab,
    console_messages: Vec<String>,
    // Panels visibility
    show_node_tree: bool,
    show_file_system: bool,
    show_properties: bool,
    show_console: bool,
    // Editor content
    code_content: String,
    current_file_path: Option<std::path::PathBuf>,
    // Window editor content
    current_window_document: Option<window_editor::WindowDocument>,
    current_window_file_path: Option<std::path::PathBuf>,
    window_editor_selected_node: Option<String>,
    // Widget library dialog
    show_widget_library: bool,
    widget_library: window_editor::widget_library::WidgetLibrary,
    // Node operations for window editor
    node_operations: window_editor::node_operations::NodeOperations,
    // Node tree context menu
    show_node_context_menu: bool,
    node_context_menu_pos: egui::Pos2,
    node_context_menu_target: Option<String>,
    // Node graph for visual programming
    node_graph: NodeGraph,
    // Panel size ratios for draggable separators
    left_panel_ratio: f32,      // Ratio of left panel width to total width
    right_panel_ratio: f32,     // Ratio of right panel width to total width
    console_ratio: f32,         // Ratio of console height to center panel height
    left_split_ratio: f32,      // Ratio of node tree to file system in left panel
    
    // Layout cache system for isolated panels
    layout_cache_valid: bool,
    cached_left_width: f32,
    cached_right_width: f32,
    cached_center_width: f32,
    
    // Left panel internal cache
    left_panel_cache_valid: bool,
    cached_node_tree_height: f32,
    cached_file_system_height: f32,
    
    // Center panel internal cache
    center_panel_cache_valid: bool,
    cached_editor_height: f32,
    cached_console_height: f32,
    
    // Window size tracking for cache invalidation
    last_window_size: egui::Vec2,
    
    // Window editor state
    window_editor_offset: egui::Vec2,
    window_editor_zoom: f32,
    window_editor_is_panning: bool,
    window_editor_pan_start_pos: Option<egui::Pos2>,
    window_editor_pan_start_offset: Option<egui::Vec2>,
    
    // Project management
    project_manager: ProjectManager,
    #[allow(dead_code)]
    file_system_service: FileSystemService,
    
    // File system context menu
    show_context_menu: bool,
    context_menu_pos: egui::Pos2,
    context_menu_target: Option<std::path::PathBuf>,
    selected_files: Vec<std::path::PathBuf>,
    
    // File operations clipboard
    clipboard_operation: Option<ClipboardOperation>,
    clipboard_files: Vec<std::path::PathBuf>,
    
    // File operation dialogs
    delete_dialog: Option<ui::dialogs::DeleteDialog>,
    properties_dialog: Option<ui::dialogs::PropertiesDialog>,
    
    // Inline rename state
    inline_rename_target: Option<std::path::PathBuf>,
    inline_rename_text: String,
    inline_rename_error: Option<String>,
    inline_rename_cursor_pos: Option<usize>,
}

impl Default for EguiCodeGeneratorApp {
    fn default() -> Self {
        let localization_path = "../i18n.json";
        let localization_content = std::fs::read_to_string(localization_path)
            .unwrap_or_else(|_| r#"{"ui_text":{"main_window_title":"Egui Code Generator","file_menu":"File","edit_menu":"Edit","view_menu":"View","help_menu":"Help","widget_palette_title":"Widget Palette","properties_panel_title":"Properties","code_preview_title":"Code Preview","generate_button":"Generate Code","save_button":"Save Project","load_button":"Load Project","clear_button":"Clear Canvas"}}"#.to_string());
        let localization: LocalizationData = serde_json::from_str(&localization_content)
            .expect("Could not parse localization JSON");
        
        // Initialize with sample node tree
        let mut root_node = UiNode {
            id: "root".to_string(),
            name: "Main Window".to_string(),
            node_type: "Window".to_string(),
            children: vec![],
            properties: HashMap::from([("title".to_string(), "My App".to_string())]),
        };
        
        root_node.children.push(UiNode {
            id: "button1".to_string(),
            name: "Button".to_string(),
            node_type: "Button".to_string(),
            children: vec![],
            properties: HashMap::from([("text".to_string(), "Click Me".to_string())]),
        });
        
        // Initialize with empty file system tree
        // Will be populated when a project is loaded
        let file_system_tree: Vec<ProjectFileSystemEntry> = Vec::new();

        Self {
            localization,
            selected_node: None,
            node_tree: vec![root_node],
            file_system_tree,
            current_tab: EditorTab::WindowEditor,
            console_messages: vec!["Application started".to_string()],
            show_node_tree: true,
            show_file_system: true,
            show_properties: true,
            show_console: true,
            code_content: "// Generated Egui Code\nuse eframe::egui;\n\nfn main() {\n    // Your generated code here\n}".to_string(),
            current_file_path: None,
            // Initialize window editor state
            current_window_document: None,
            current_window_file_path: None,
            window_editor_selected_node: None,
            // Initialize widget library
            show_widget_library: false,
            widget_library: window_editor::widget_library::WidgetLibrary::new(),
            // Initialize node operations
            node_operations: window_editor::node_operations::NodeOperations::new(),
            // Initialize node context menu
            show_node_context_menu: false,
            node_context_menu_pos: egui::Pos2::ZERO,
            node_context_menu_target: None,
            // Initialize node graph with sample nodes
            node_graph: NodeGraph {
                nodes: vec![
                    GraphNode {
                        id: "input_node".to_string(),
                        title: "Input".to_string(),
                        position: egui::Vec2::new(100.0, 100.0),
                        size: egui::Vec2::new(120.0, 80.0),
                        node_type: "Input".to_string(),
                        inputs: vec![],
                        outputs: vec![
                            NodePort {
                                id: "output1".to_string(),
                                name: "Value".to_string(),
                                port_type: "Float".to_string(),
                                position: egui::Vec2::new(115.0, 40.0),
                            }
                        ],
                        properties: HashMap::new(),
                    },
                    GraphNode {
                        id: "process_node".to_string(),
                        title: "Process".to_string(),
                        position: egui::Vec2::new(300.0, 150.0),
                        size: egui::Vec2::new(140.0, 100.0),
                        node_type: "Process".to_string(),
                        inputs: vec![
                            NodePort {
                                id: "input1".to_string(),
                                name: "Input".to_string(),
                                port_type: "Float".to_string(),
                                position: egui::Vec2::new(5.0, 40.0),
                            }
                        ],
                        outputs: vec![
                            NodePort {
                                id: "output1".to_string(),
                                name: "Result".to_string(),
                                port_type: "Float".to_string(),
                                position: egui::Vec2::new(135.0, 50.0),
                            }
                        ],
                        properties: HashMap::new(),
                    },
                ],
                connections: vec![
                    NodeConnection {
                        id: "conn1".to_string(),
                        from_node: "input_node".to_string(),
                        from_port: "output1".to_string(),
                        to_node: "process_node".to_string(),
                        to_port: "input1".to_string(),
                    }
                ],
                canvas_offset: egui::Vec2::ZERO,
                canvas_zoom: 1.0,
                selected_node: None,
                selected_nodes: Vec::new(),
                selected_port: None,
                dragging_node: None,
                drag_offset: egui::Vec2::ZERO,
                drag_start_mouse_pos: None,
                drag_start_node_pos: None,
                selection_box: None,
                selection_start: None,
                is_selecting: false,
                context_menu_pos: None,
                show_context_menu: false,
                right_drag_start: None,
                is_right_dragging: false,
                right_drag_path: Vec::new(),
                snap_to_grid: false,
                // Initialize pre-drag selection state
                pre_drag_selected_node: None,
                pre_drag_selected_nodes: Vec::new(),
                // Initialize panning state
                is_panning: false,
                pan_start_pos: None,
                pan_start_offset: None,
                selection_shift_held: false,
                // Initialize connection state
                is_connecting: false,
                connecting_from: None,
                temp_connection_end: None,
            },
            // Initialize panel ratios
            left_panel_ratio: 0.25,     // 25% of total width
            right_panel_ratio: 0.2,     // 20% of total width
            console_ratio: 0.3,         // 30% of center panel height
            left_split_ratio: 0.5,      // 50/50 split between node tree and file system
            
            // Initialize layout cache system
            layout_cache_valid: false,
            cached_left_width: 0.0,
            cached_right_width: 0.0,
            cached_center_width: 0.0,
            
            // Initialize left panel cache
            left_panel_cache_valid: false,
            cached_node_tree_height: 0.0,
            cached_file_system_height: 0.0,
            
            // Initialize center panel cache
            center_panel_cache_valid: false,
            cached_editor_height: 0.0,
            cached_console_height: 0.0,
            
            // Initialize window size tracking
            last_window_size: egui::Vec2::ZERO,
            
            // Initialize window editor state
            window_editor_offset: egui::Vec2::new(0.0, 0.0), // No initial offset, editor origin handles positioning
            window_editor_zoom: 1.0,
            window_editor_is_panning: false,
            window_editor_pan_start_pos: None,
            window_editor_pan_start_offset: None,
            
            // Initialize project management
            project_manager: ProjectManager::new(),
            file_system_service: FileSystemService::new(),
            
            // Initialize file system context menu
            show_context_menu: false,
            context_menu_pos: egui::Pos2::ZERO,
            context_menu_target: None,
            selected_files: Vec::new(),
            
            // Initialize file operations clipboard
            clipboard_operation: None,
            clipboard_files: Vec::new(),
            
            // Initialize file operation dialogs
            delete_dialog: None,
            properties_dialog: None,
            
            // Initialize inline rename state
            inline_rename_target: None,
            inline_rename_text: String::new(),
            inline_rename_error: None,
            inline_rename_cursor_pos: None,
        }
    }
}

impl EguiCodeGeneratorApp {
    // UI rendering methods are now in separate modules
    
    // Global keyboard event handler
    fn handle_global_keyboard_events(&mut self, ctx: &egui::Context) {
        // Handle Delete key for file deletion
        if ctx.input(|i| i.key_pressed(egui::Key::Delete)) {
            // Only handle delete if we have selected files and no dialog is open
            if !self.selected_files.is_empty() && self.delete_dialog.is_none() {
                // Create delete dialog directly here since we can't call left_panel method
                let items_to_delete = self.selected_files.clone();
                self.delete_dialog = Some(ui::dialogs::delete_dialog::DeleteDialog::new(items_to_delete));
            }
        }
    }
    
    // Handle file operation dialogs
    fn handle_dialogs(&mut self, ctx: &egui::Context) {
        use crate::ui::dialogs::{Dialog, DialogResult};
        use crate::ui::file_operations::FileOperations;
        
        // Handle delete dialog
        if self.delete_dialog.is_some() {
            self.console_messages.push("DeleteDialog exists, processing...".to_string());
        }
        
        if let Some(mut dialog) = self.delete_dialog.take() {
            self.console_messages.push("Delete dialog is being processed".to_string());
            match dialog.show(ctx) {
                DialogResult::Confirmed => {
                    self.console_messages.push("Delete operation confirmed by user".to_string());
                    let items = dialog.get_items().to_vec();
                    let mut success_count = 0;
                    let mut error_count = 0;
                    
                    self.console_messages.push(format!("Starting deletion of {} item(s)", items.len()));
                    
                    for item in &items {
                        let item_name = item.file_name().and_then(|n| n.to_str()).unwrap_or("Unknown");
                        self.console_messages.push(format!("Attempting to delete: {}", item.display()));
                        
                        match FileOperations::delete(item) {
                            Ok(()) => {
                                success_count += 1;
                                self.console_messages.push(format!("Successfully deleted: {}", item_name));
                            }
                            Err(e) => {
                                error_count += 1;
                                self.console_messages.push(format!("Failed to delete {}: {}", item_name, e));
                            }
                        }
                    }
                    
                    if success_count > 0 {
                        self.console_messages.push(format!("Total deleted: {} item(s)", success_count));
                    }
                    
                    if error_count > 0 {
                        self.console_messages.push(format!("Total failed: {} deletion(s)", error_count));
                    }
                    
                    // Clear selection
                    self.selected_files.clear();
                    self.console_messages.push("Refreshing file system after deletion".to_string());
                    self.load_project_files();
                }
                DialogResult::Open => {
                    self.console_messages.push("Delete dialog remains open".to_string());
                    self.delete_dialog = Some(dialog);
                }
                DialogResult::Cancelled => {
                    self.console_messages.push("Delete operation cancelled by user".to_string());
                }
                DialogResult::Ok | DialogResult::None => {
                    // Handle other dialog results
                }
            }
        }
        
        // Handle properties dialog
        if let Some(mut dialog) = self.properties_dialog.take() {
            match dialog.show(ctx) {
                DialogResult::Open => {
                    self.properties_dialog = Some(dialog);
                }
                _ => {
                    // Dialog closed, do nothing
                }
            }
        }
    }
    
    // File menu handlers
    fn handle_new_project(&mut self) {
        // Show new project dialog
        if let Some(path) = rfd::FileDialog::new()
            .set_title("Create New Project")
            .pick_folder() {
            let project_name = path.file_name()
                .and_then(|n| n.to_str())
                .unwrap_or("NewProject")
                .to_string();
            
            match self.project_manager.create_project(path.clone(), project_name.clone()) {
                Ok(_) => {
                    self.console_messages.push(format!("Created new project: {}", project_name));
                    self.load_project_files();
                },
                Err(e) => {
                    self.console_messages.push(format!("Failed to create project: {}", e));
                }
            }
        }
    }
    
    fn handle_open_project(&mut self) {
        // Show open project dialog - directly select project folder
        if let Some(path) = rfd::FileDialog::new()
            .set_title("Open Project Folder")
            .pick_folder() {
            // User selected a folder
            match self.project_manager.open_project(path.clone()) {
                Ok(_) => {
                    if let Some(config) = self.project_manager.get_config() {
                        self.console_messages.push(format!("Opened project: {}", config["name"].as_str().unwrap_or("Unknown")));
                        self.load_project_files();
                    }
                },
                Err(e) => {
                    self.console_messages.push(format!("Failed to open project: {}", e));
                }
            }
        }
    }
    
    fn handle_save_project(&mut self) {
        if let Some(root_path) = self.project_manager.get_root_path() {
            self.console_messages.push(format!("Saved project: {}", root_path.display()));
        } else {
            self.console_messages.push("No project to save".to_string());
        }
    }
    
    #[allow(dead_code)]
    fn handle_save_as_project(&mut self) {
        if let Some(path) = rfd::FileDialog::new()
            .set_title("Save Project As")
            .pick_folder() {
            self.console_messages.push(format!("Saved project as: {}", path.display()));
        }
    }
    
    fn handle_export_project(&mut self) {
        if let Some(root_path) = self.project_manager.get_root_path() {
            if let Some(export_path) = rfd::FileDialog::new()
                .set_title("Export Project")
                .add_filter("ZIP files", &["zip"])
                .save_file() {
                match self.export_project_as_zip(root_path, &export_path) {
                    Ok(_) => {
                        self.console_messages.push(format!("Exported project to: {}", export_path.display()));
                    },
                    Err(e) => {
                        self.console_messages.push(format!("Failed to export project: {}", e));
                    }
                }
            }
        } else {
            self.console_messages.push("No project to export".to_string());
        }
    }
    
    fn handle_exit_application(&mut self) {
        std::process::exit(0);
    }
    
    // Helper methods
    fn load_project_files(&mut self) {
        if let Some(root_path) = self.project_manager.get_root_path() {
            match FileSystemService::scan_directory(root_path) {
                Ok(entries) => {
                    self.file_system_tree = entries;
                },
                Err(e) => {
                    self.console_messages.push(format!("Failed to load project files: {}", e));
                }
            }
        }
    }
    
    fn export_project_as_zip(&self, source_path: &std::path::Path, target_path: &std::path::Path) -> Result<(), Box<dyn std::error::Error>> {
        let file = std::fs::File::create(target_path)?;
        let mut zip = zip::ZipWriter::new(file);
        let options = FileOptions::default().compression_method(zip::CompressionMethod::Deflated);
        
        self.add_directory_to_zip(&mut zip, source_path, source_path, &options)?;
        zip.finish()?;
        Ok(())
    }
    
    fn add_directory_to_zip<W: Write + std::io::Seek>(
        &self,
        zip: &mut zip::ZipWriter<W>,
        dir_path: &std::path::Path,
        base_path: &std::path::Path,
        options: &FileOptions,
    ) -> Result<(), Box<dyn std::error::Error>> {
        for entry in std::fs::read_dir(dir_path)? {
            let entry = entry?;
            let path = entry.path();
            let relative_path = path.strip_prefix(base_path)?;
            
            if path.is_file() {
                zip.start_file(relative_path.to_string_lossy(), *options)?;
                let content = std::fs::read(&path)?;
                zip.write_all(&content)?;
            } else if path.is_dir() {
                zip.add_directory(relative_path.to_string_lossy(), *options)?;
                self.add_directory_to_zip(zip, &path, base_path, options)?;
            }
        }
        Ok(())
    }
}

impl eframe::App for EguiCodeGeneratorApp {
    fn update(&mut self, ctx: &egui::Context, _frame: &mut eframe::Frame) {
        // Handle global keyboard shortcuts
        self.handle_global_keyboard_events(ctx);
        
        self.render_menu_bar(ctx);
        self.render_layout(ctx);
    }
}

fn main() -> eframe::Result<()> {
    let options = eframe::NativeOptions {
        viewport: egui::ViewportBuilder::default()
            .with_inner_size([1200.0, 800.0])
            .with_min_inner_size([800.0, 600.0]),
        ..Default::default()
    };
    eframe::run_native(
        "Egui Code Generator",
        options,
        Box::new(|_cc| Ok(Box::<EguiCodeGeneratorApp>::default())),
    )
}
