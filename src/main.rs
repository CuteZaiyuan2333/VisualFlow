use eframe::egui;
use std::collections::HashMap;
use serde::Deserialize;

mod ui;
mod project;

use project::ProjectManager;
use ui::syntax_highlighting::SyntaxHighlighter;

#[derive(Deserialize, Debug)]
struct UiText {
    #[allow(dead_code)]
    main_window_title: String,
    file_menu: String,
    edit_menu: String,
    view_menu: String,
    help_menu: String,
    #[allow(dead_code)]
    widget_palette_title: String,
    properties_panel_title: String,
    #[allow(dead_code)]
    code_preview_title: String,
    #[allow(dead_code)]
    generate_button: String,
    save_button: String,
    load_button: String,
    #[allow(dead_code)]
    clear_button: String,
    node_tree_title: String,
    file_system_title: String,
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

#[derive(Debug, Clone)]
enum FileSystemEntry {
    File { name: String },
    Dir { name: String, children: Vec<FileSystemEntry> },
}

// Editor tab types
#[derive(Debug, Clone, PartialEq)]
enum EditorTab {
    WindowEditor,
    CodeEditor,
    NodeEditor,
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
    // Project management
    project_manager: ProjectManager,
    // Syntax highlighting
    syntax_highlighter: SyntaxHighlighter,
    // UI state
    selected_node: Option<String>,
    node_tree: Vec<UiNode>,
    file_system_tree: Vec<FileSystemEntry>,
    current_tab: EditorTab,
    console_messages: Vec<String>,
    // Panels visibility
    show_node_tree: bool,
    show_file_system: bool,
    show_properties: bool,
    show_console: bool,
    // Editor content
    code_content: String,
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
        
        let project_manager = ProjectManager::default();
        let file_system_tree = project_manager.get_file_system_tree();

        Self {
            localization,
            project_manager,
            syntax_highlighter: SyntaxHighlighter::default(),
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
        }
    }
}

impl EguiCodeGeneratorApp {
    // UI rendering methods are now in separate modules
    

    

    

}

impl eframe::App for EguiCodeGeneratorApp {
    fn update(&mut self, ctx: &egui::Context, _frame: &mut eframe::Frame) {
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
