use egui::{Ui, WidgetText, Rect, Pos2, Vec2, Color32, Stroke, Align2, FontId, Sense, Id, Rounding};
use crate::{Plugin, AppCommand, TabInstance, Tab, NotificationLevel};
use std::collections::{HashMap, HashSet};
use std::path::{Path, PathBuf};

use serde::{Serialize, Deserialize};

mod compiler;

// ----------------------------------------------------------------------------
// Serde Proxies for egui types
// ----------------------------------------------------------------------------

#[derive(Serialize, Deserialize)]
#[serde(remote = "Pos2")]
struct Pos2Def {
    pub x: f32,
    pub y: f32,
}

#[derive(Serialize, Deserialize)]
#[serde(remote = "Vec2")]
struct Vec2Def {
    pub x: f32,
    pub y: f32,
}

// ----------------------------------------------------------------------------
// Core Types & Data Model
// ----------------------------------------------------------------------------

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct NodeId(pub usize);

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub enum DataType {
    Flow,   // 执行流
    Any,    // 通用类型
    String,
    Number,
    Bool,
}

impl DataType {
    pub fn color(&self) -> Color32 {
        match self {
            DataType::Flow => Color32::WHITE,
            DataType::Any => Color32::from_gray(200),
            DataType::String => Color32::from_rgb(255, 200, 100), // Orange
            DataType::Number => Color32::from_rgb(100, 200, 255), // Blue
            DataType::Bool => Color32::from_rgb(255, 100, 100),   // Red
        }
    }
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct Port {
    pub name: String,
    pub data_type: DataType,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct NodeTemplate {
    pub name: String,
    pub category: String,
    
    // 端口定义
    pub inputs: Vec<Port>,
    pub outputs: Vec<Port>,
    
    // Rhai 逻辑
    pub rhai_fn_name: String,
    pub rhai_body: String, // 完整的函数体
}

impl NodeTemplate {
    pub fn flow_inputs(&self) -> Vec<&Port> { self.inputs.iter().filter(|p| matches!(p.data_type, DataType::Flow)).collect() }
    pub fn flow_outputs(&self) -> Vec<&Port> { self.outputs.iter().filter(|p| matches!(p.data_type, DataType::Flow)).collect() }
    pub fn data_inputs(&self) -> Vec<&Port> { self.inputs.iter().filter(|p| !matches!(p.data_type, DataType::Flow)).collect() }
    pub fn data_outputs(&self) -> Vec<&Port> { self.outputs.iter().filter(|p| !matches!(p.data_type, DataType::Flow)).collect() }
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct NodeInstance {
    pub id: NodeId,
    pub template_name: String,
    #[serde(with = "Pos2Def")]
    pub position: Pos2, 
    #[serde(with = "Vec2Def")]
    pub size: Vec2,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct Connection {
    pub from_node: NodeId,
    pub from_port: usize, // Index in outputs
    pub to_node: NodeId,
    pub to_port: usize,   // Index in inputs
}

// ----------------------------------------------------------------------------
// Parser & Loader
// ----------------------------------------------------------------------------

struct NodeLoader;

impl NodeLoader {
    pub fn load_from_paths(paths: &[PathBuf]) -> Vec<NodeTemplate> {
        let mut templates = Vec::new();
        for path in paths {
            if path.exists() {
                if path.is_file() {
                    if let Some(t) = Self::load_file(path) {
                        templates.extend(t);
                    }
                } else if path.is_dir() {
                    templates.extend(Self::load_from_dir(path));
                }
            }
        }
        templates
    }

    fn load_file(path: &Path) -> Option<Vec<NodeTemplate>> {
        if path.extension().and_then(|s| s.to_str()) == Some("vfnode") {
            if let Some(stem) = path.file_stem().and_then(|s| s.to_str()) {
                if let Ok(content) = std::fs::read_to_string(path) {
                    return Some(Self::parse_vfnode_content(stem, &content));
                }
            }
        }
        None
    }

    /// 扫描目录下的 .vfnode 文件并解析
    fn load_from_dir(dir: &Path) -> Vec<NodeTemplate> {
        let mut templates = Vec::new();
        if let Ok(entries) = std::fs::read_dir(dir) {
            for entry in entries.flatten() {
                let path = entry.path();
                if path.is_file() {
                    if let Some(t) = Self::load_file(&path) {
                        templates.extend(t);
                    }
                }
            }
        }
        templates
    }

    /// 解析模拟的 .vfnode 文件内容
    fn parse_vfnode_content(category: &str, content: &str) -> Vec<NodeTemplate> {
        let mut templates = Vec::new();
        let mut current_comment_block = Vec::new();

        for line in content.lines() {
            let line = line.trim();
            if line.starts_with("///") {
                current_comment_block.push(line.trim_start_matches("///").trim().to_string());
            } else if line.starts_with("fn ") {
                // 解析函数定义
                // fn name(args) {
                if let Some(fn_name_end) = line.find('(') {
                    let fn_name = line[3..fn_name_end].trim().to_string();
                    
                    // 解析元数据
                    let mut name = fn_name.clone();
                    let mut inputs = Vec::new();
                    let mut outputs = Vec::new();

                    for comment in &current_comment_block {
                        if let Some(val) = comment.strip_prefix("@name:") {
                            name = val.trim().to_string();
                        } else if let Some(val) = comment.strip_prefix("@flow:") {
                            // @flow: in -> out1, out2
                            let parts: Vec<&str> = val.split("->").collect();
                            if parts.len() > 0 {
                                for p in parts[0].split(',').map(|s| s.trim()).filter(|s| !s.is_empty()) {
                                    inputs.push(Port { name: p.to_string(), data_type: DataType::Flow });
                                }
                            }
                            if parts.len() > 1 {
                                for p in parts[1].split(',').map(|s| s.trim()).filter(|s| !s.is_empty()) {
                                    outputs.push(Port { name: p.to_string(), data_type: DataType::Flow });
                                }
                            }
                        } else if let Some(val) = comment.strip_prefix("@in:") {
                            // @in: a: Number, b: String
                            Self::parse_data_ports(val, &mut inputs);
                        } else if let Some(val) = comment.strip_prefix("@out:") {
                            // @out: res: Bool
                            Self::parse_data_ports(val, &mut outputs);
                        }
                    }

                    if !inputs.iter().any(|p| matches!(p.data_type, DataType::Flow)) {
                         inputs.insert(0, Port { name: "".into(), data_type: DataType::Flow });
                    }
                    if !outputs.iter().any(|p| matches!(p.data_type, DataType::Flow)) {
                         outputs.insert(0, Port { name: "".into(), data_type: DataType::Flow });
                    }

                    templates.push(NodeTemplate {
                        name,
                        category: category.to_string(),
                        inputs,
                        outputs,
                        rhai_fn_name: fn_name,
                        rhai_body: "".to_string(), // TODO: Capture full body
                    });
                }
                current_comment_block.clear();
            } else if line.is_empty() {
                // Keep comments if next line is not empty
            } else {
                // Not a comment, not a fn start (maybe body), clear comments
                current_comment_block.clear();
            }
        }
        templates
    }

    fn parse_data_ports(def: &str, target: &mut Vec<Port>) {
        // format: "name: Type, name2: Type2"
        for part in def.split(',') {
            let part = part.trim();
            if let Some(idx) = part.find(':') {
                let name = part[..idx].trim().to_string();
                let type_str = part[idx+1..].trim();
                let dtype = match type_str {
                    "String" => DataType::String,
                    "Number" => DataType::Number,
                    "Bool" => DataType::Bool,
                    _ => DataType::Any,
                };
                target.push(Port { name, data_type: dtype });
            }
        }
    }
}

// ----------------------------------------------------------------------------
// Tab Instance
// ----------------------------------------------------------------------------

pub struct VisualFlowCanvasTab {
    nodes: HashMap<NodeId, NodeInstance>,
    connections: Vec<Connection>,
    templates: Vec<NodeTemplate>,
    
    next_node_id: usize,
    pan_offset: Vec2,
    zoom: f32,
    
    dragged_node: Option<NodeId>,
    selected_nodes: HashSet<NodeId>,
    last_selected_node: Option<NodeId>, 
    
    active_drag_source: Option<(NodeId, usize)>, // (Node, PortIndex in outputs)
    selection_start: Option<Pos2>, 
    swipe_last_pos: Option<Pos2>,  
    
    secondary_click_start: Option<Pos2>,
    secondary_started_on_node: bool,
    is_swiping: bool,
    context_menu_pos: Option<Pos2>,
    copied_data: Option<String>,

    is_collapsed: bool,
    dragged_template: Option<NodeTemplate>,
}

impl VisualFlowCanvasTab {
    pub fn new(templates: Vec<NodeTemplate>) -> Self {
        let mut tab = Self {
            nodes: HashMap::new(),
            connections: Vec::new(),
            templates,
            next_node_id: 0, pan_offset: Vec2::ZERO, zoom: 1.0, dragged_node: None,
            selected_nodes: HashSet::new(), last_selected_node: None,
            active_drag_source: None, selection_start: None, swipe_last_pos: None,
            secondary_click_start: None, secondary_started_on_node: false, is_swiping: false, context_menu_pos: None,
            copied_data: None,
            is_collapsed: false, dragged_template: None,
        };
        // Add a default start node
        if tab.templates.iter().any(|t| t.name == "Start") {
             tab.add_node("Start", Pos2::new(50.0, 50.0));
        }
        tab
    }
}

impl Clone for VisualFlowCanvasTab {
    fn clone(&self) -> Self {
        Self {
            nodes: self.nodes.clone(), connections: self.connections.clone(), templates: self.templates.clone(),
            next_node_id: self.next_node_id, pan_offset: self.pan_offset, zoom: self.zoom,
            dragged_node: None, selected_nodes: self.selected_nodes.clone(), last_selected_node: self.last_selected_node,
            active_drag_source: None, selection_start: None, swipe_last_pos: None,
            secondary_click_start: None, secondary_started_on_node: false, is_swiping: false, context_menu_pos: None,
            copied_data: self.copied_data.clone(),
            is_collapsed: self.is_collapsed, dragged_template: None,
        }
    }
}

impl std::fmt::Debug for VisualFlowCanvasTab {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result { f.debug_struct("VisualFlowCanvasTab").finish() }
}

impl VisualFlowCanvasTab {
    fn add_node(&mut self, template_name: &str, pos: Pos2) -> NodeId {
        let id = NodeId(self.next_node_id); self.next_node_id += 1;
        let t = self.templates.iter().find(|t| t.name == template_name).unwrap();
        // Calculate height based on ports
        let max_ports = t.inputs.len().max(t.outputs.len());
        let height = 35.0 + (max_ports as f32 * 22.0).max(22.0);
        
        self.nodes.insert(id, NodeInstance { id, template_name: template_name.to_string(), position: pos, size: Vec2::new(150.0, height) });
        id
    }

    fn world_to_screen(&self, world_pos: Pos2, canvas_rect: Rect) -> Pos2 { canvas_rect.min + (world_pos.to_vec2() + self.pan_offset) * self.zoom }
    fn screen_to_world(&self, screen_pos: Pos2, canvas_rect: Rect) -> Pos2 { ((screen_pos - canvas_rect.min) / self.zoom - self.pan_offset).to_pos2() }
    
    fn get_category_color(&self, category: &str) -> Color32 {
        match category { 
            "Flow" => Color32::from_rgb(180, 60, 60), 
            "IO" => Color32::from_rgb(60, 180, 60), 
            "Math" => Color32::from_rgb(60, 60, 180), 
            _ => Color32::from_gray(120) 
        }
    }

    fn get_port_world_pos(&self, node_id: NodeId, port_index: usize, is_output: bool) -> Pos2 {
        if let Some(node) = self.nodes.get(&node_id) {
            let start_y = node.position.y + 36.0;
            let x = if is_output { node.position.x + node.size.x } else { node.position.x };
            Pos2::new(x, start_y + port_index as f32 * 22.0)
        } else {
            Pos2::ZERO
        }
    }

    fn draw_port_shape(&self, painter: &egui::Painter, center: Pos2, dtype: &DataType, size: f32) {
        match dtype {
            DataType::Flow => {
                let h = size * 0.9; let w = size * 0.8;
                painter.add(egui::Shape::convex_polygon(vec![center + Vec2::new(h, 0.0), center + Vec2::new(-h, -w), center + Vec2::new(-h, w)], Color32::WHITE, Stroke::new(1.0, Color32::from_gray(50))));
            },
            _ => {
                painter.circle_filled(center, size * 0.8, dtype.color());
            }
        }
    }

    fn draw_node_template_preview(&self, ui: &mut Ui, template: &NodeTemplate) -> egui::Response {
        let row_count = template.inputs.len().max(template.outputs.len()).max(1);
        let row_h = 15.0;
        let preview_height = 25.0 + (row_count as f32 * row_h);
        let (rect, response) = ui.allocate_exact_size(Vec2::new(ui.available_width(), preview_height + 10.0), Sense::click_and_drag());
        
        if ui.is_rect_visible(rect) {
            let painter = ui.painter();
            let n_rect = Rect::from_center_size(rect.center(), Vec2::new(rect.width() - 20.0, preview_height));
            
            painter.rect_filled(n_rect, 4.0, Color32::from_gray(40));
            painter.rect_stroke(n_rect, 4.0, Stroke::new(1.0, Color32::from_gray(70)));
            
            let h_rect = Rect::from_min_size(n_rect.min, Vec2::new(n_rect.width(), 14.0));
            painter.rect_filled(h_rect, Rounding { nw: 4.0, ne: 4.0, sw: 0.0, se: 0.0 }, self.get_category_color(&template.category));
            painter.text(h_rect.center(), Align2::CENTER_CENTER, &template.name, FontId::proportional(9.0), Color32::WHITE);

            let row_y = n_rect.min.y + 20.0;
            let f_id = FontId::proportional(8.0);

            // Draw Inputs
            for (i, port) in template.inputs.iter().enumerate() {
                let py = row_y + i as f32 * row_h;
                self.draw_port_shape(painter, Pos2::new(n_rect.left(), py), &port.data_type, 3.0);
                if !port.name.is_empty() {
                    painter.text(Pos2::new(n_rect.left() + 6.0, py), Align2::LEFT_CENTER, &port.name, f_id.clone(), Color32::WHITE);
                }
            }
            // Draw Outputs
            for (i, port) in template.outputs.iter().enumerate() {
                let py = row_y + i as f32 * row_h;
                self.draw_port_shape(painter, Pos2::new(n_rect.right(), py), &port.data_type, 3.0);
                if !port.name.is_empty() {
                    painter.text(Pos2::new(n_rect.right() - 6.0, py), Align2::RIGHT_CENTER, &port.name, f_id.clone(), Color32::WHITE);
                }
            }
        }
        response
    }

    fn draw_connection(&self, painter: &egui::Painter, from: Pos2, to: Pos2, color: Color32) {
        let cp1 = from + Vec2::new(50.0 * self.zoom, 0.0);
        let cp2 = to + Vec2::new(-50.0 * self.zoom, 0.0);
        painter.add(egui::Shape::CubicBezier(egui::epaint::CubicBezierShape {
            points: [from, cp1, cp2, to],
            closed: false,
            fill: Color32::TRANSPARENT,
            stroke: Stroke::new(2.0 * self.zoom, color).into(),
        }));
    }
}

#[derive(Serialize, Deserialize)]
struct ClipboardData {
    nodes: Vec<NodeInstance>,
    connections: Vec<Connection>,
}

impl TabInstance for VisualFlowCanvasTab {
    fn title(&self) -> WidgetText { "VisualFlowCanvas".into() }

    fn ui(&mut self, ui: &mut Ui, control: &mut Vec<AppCommand>) {
        // --- 1. Top Bar ---
        egui::TopBottomPanel::top(ui.id().with("top_bar")).show_inside(ui, |ui| {
            ui.horizontal(|ui| {
                if ui.selectable_label(!self.is_collapsed, "☰ Toolbox").clicked() { self.is_collapsed = !self.is_collapsed; }
                ui.separator();
                ui.label(format!("Zoom: {:.1}x | Selected: {}", self.zoom, self.selected_nodes.len()));
                
                ui.separator();
                if ui.button("▶ Generate Code").clicked() {
                    let mut compiler = compiler::FlowCompiler::new(&self.nodes, &self.connections, &self.templates);
                    match compiler.compile() {
                        Ok(code) => {
                            println!("--- Generated Rhai Code ---\n{}\n---------------------------", code);
                            control.push(AppCommand::Notify { 
                                message: "Code generated successfully (check console)".into(), 
                                level: NotificationLevel::Success 
                            });
                        }
                        Err(e) => {
                            eprintln!("Compilation Error: {}", e);
                            control.push(AppCommand::Notify { 
                                message: format!("Compilation failed: {}", e), 
                                level: NotificationLevel::Error 
                            });
                        }
                    }
                }
            });
        });

        // --- 2. Sidebar ---
        let toolbox_id = ui.id().with("toolbox");
        egui::SidePanel::left(toolbox_id).resizable(true).default_width(180.0).show_animated_inside(ui, !self.is_collapsed, |ui| {
            ui.vertical(|ui| {
                ui.heading("Toolbox"); ui.separator();
                egui::ScrollArea::vertical().show(ui, |ui| {
                    let mut categories: HashMap<String, Vec<NodeTemplate>> = HashMap::new();
                    for t in &self.templates { categories.entry(t.category.clone()).or_default().push(t.clone()); }
                    let mut sorted_cat: Vec<_> = categories.keys().cloned().collect(); sorted_cat.sort();
                    for cat in sorted_cat {
                        ui.collapsing(format!("📁 {}", cat), |ui| {
                            for template in &categories[&cat] {
                                if self.draw_node_template_preview(ui, template).drag_started() { self.dragged_template = Some(template.clone()); }
                                ui.add_space(4.0);
                            }
                        });
                    }
                });
            });
        });

        // --- 3. Canvas ---
        egui::CentralPanel::default().show_inside(ui, |ui| {
            let canvas_rect = ui.available_rect_before_wrap();
            let canvas_resp = ui.interact(canvas_rect, ui.id().with("canvas"), Sense::click_and_drag());
            let m_pos = ui.input(|i| i.pointer.hover_pos()).unwrap_or(Pos2::ZERO);
            let world_m_pos = self.screen_to_world(m_pos, canvas_rect);

            // 交互：缩放
            let scroll = ui.input(|i| i.raw_scroll_delta.y);
            if scroll != 0.0 && canvas_resp.hovered() {
                let zoom_delta = if scroll > 0.0 { 1.1 } else { 0.9 };
                let old_zoom = self.zoom;
                self.zoom = (self.zoom * zoom_delta).clamp(0.2, 3.0);
                let world_anchor = ((m_pos - canvas_rect.min) / old_zoom - self.pan_offset).to_pos2();
                self.pan_offset = (m_pos - canvas_rect.min) / self.zoom - world_anchor.to_vec2();
            }

            // --- 右键交互处理 (Slice) ---
            let is_secondary_down = ui.input(|i| i.pointer.button_down(egui::PointerButton::Secondary));
            let is_secondary_pressed = ui.input(|i| i.pointer.button_pressed(egui::PointerButton::Secondary));
            let is_secondary_released = ui.input(|i| i.pointer.button_released(egui::PointerButton::Secondary));

            if is_secondary_pressed && canvas_rect.contains(m_pos) {
                self.secondary_click_start = Some(m_pos);
                self.secondary_started_on_node = self.nodes.values().any(|n| {
                    let sn_pos = self.world_to_screen(n.position, canvas_rect);
                    let sn_size = n.size * self.zoom;
                    Rect::from_min_size(sn_pos, sn_size).contains(m_pos)
                });
                self.is_swiping = false;
                self.context_menu_pos = None;
            }

            if is_secondary_down {
                if let Some(start) = self.secondary_click_start {
                    let dist = (m_pos - start).length();
                    if dist > 10.0 && !self.secondary_started_on_node { self.is_swiping = true; }
                }

                if self.is_swiping {
                    if let Some(last) = self.swipe_last_pos {
                        let swipe_rect = Rect::from_two_pos(last, world_m_pos).expand(2.0);
                        self.nodes.retain(|_, node| !swipe_rect.intersects(Rect::from_min_size(node.position, node.size)));
                        
                        let mut connections_to_remove = Vec::new();
                                                for (i, conn) in self.connections.iter().enumerate() {
                                                    // If node was deleted above, remove the connection
                                                    if !self.nodes.contains_key(&conn.from_node) || !self.nodes.contains_key(&conn.to_node) {
                                                        connections_to_remove.push(i);
                                                        continue;
                                                    }
                        
                                                    let p1 = self.get_port_world_pos(conn.from_node, conn.from_port, true);
                                                    let p2 = self.get_port_world_pos(conn.to_node, conn.to_port, false);
                                                    if Rect::from_two_pos(p1, p2).intersects(swipe_rect) { 
                                                        connections_to_remove.push(i); 
                                                    }
                                                }
                        for &i in connections_to_remove.iter().rev() { self.connections.remove(i); }
                    }
                    self.swipe_last_pos = Some(world_m_pos);
                }
            }

            if is_secondary_released {
                if !self.is_swiping {
                    if let Some(start) = self.secondary_click_start {
                        if (m_pos - start).length() < 5.0 { self.context_menu_pos = Some(m_pos); }
                    }
                }
                self.swipe_last_pos = None; self.secondary_click_start = None; self.is_swiping = false;
            }

            if canvas_resp.dragged_by(egui::PointerButton::Middle) { self.pan_offset += canvas_resp.drag_delta() / self.zoom; }
            if ui.input(|i| i.modifiers.shift && i.pointer.button_down(egui::PointerButton::Primary)) {
                if self.selection_start.is_none() { self.selection_start = Some(world_m_pos); }
            }

            let mut painter = ui.painter_at(canvas_rect);
            painter.set_clip_rect(canvas_rect);
            painter.rect_filled(canvas_rect, 0.0, Color32::from_gray(30));

            // Grid
            let grid_spacing = 20.0 * self.zoom;
            let off_x = (self.pan_offset.x * self.zoom) % grid_spacing;
            let off_y = (self.pan_offset.y * self.zoom) % grid_spacing;
            for x in std::iter::successors(Some(off_x), |&x| Some(x + grid_spacing)).take_while(|&x| x < canvas_rect.width()) {
                for y in std::iter::successors(Some(off_y), |&y| Some(y + grid_spacing)).take_while(|&y| y < canvas_rect.height()) {
                    painter.circle_filled(canvas_rect.min + Vec2::new(x, y), 1.0 * self.zoom.max(0.5), Color32::from_gray(60));
                }
            }

            // Draw Connections
            for conn in &self.connections {
                let p1 = self.world_to_screen(self.get_port_world_pos(conn.from_node, conn.from_port, true), canvas_rect);
                let p2 = self.world_to_screen(self.get_port_world_pos(conn.to_node, conn.to_port, false), canvas_rect);
                
                // Determine color based on source port type
                let mut color = Color32::WHITE;
                if let Some(node) = self.nodes.get(&conn.from_node) {
                     if let Some(tmpl) = self.templates.iter().find(|t| t.name == node.template_name) {
                         if let Some(port) = tmpl.outputs.get(conn.from_port) {
                             color = port.data_type.color();
                         }
                     }
                }
                self.draw_connection(&painter, p1, p2, color);
            }

            // --- Node Logic & Drawing ---
            let mut node_to_drag = None;
            let mut node_to_select = None;
            let mut drag_delta = Vec2::ZERO;
            let mut hover_port = None;
            let mut start_drag_port = None;

            let mut sorted_ids: Vec<NodeId> = self.nodes.keys().cloned().collect();
            sorted_ids.sort_by(|a, b| {
                if Some(*a) == self.last_selected_node { std::cmp::Ordering::Greater }
                else if Some(*b) == self.last_selected_node { std::cmp::Ordering::Less }
                else { std::cmp::Ordering::Equal }
            });

            for id in sorted_ids {
                let (template, position, size) = {
                    let node = &self.nodes[&id];
                    let t = self.templates.iter().find(|t| t.name == node.template_name).unwrap();
                    (t.clone(), node.position, node.size)
                };

                let screen_pos = self.world_to_screen(position, canvas_rect);
                let screen_size = size * self.zoom;
                let node_rect = Rect::from_min_size(screen_pos, screen_size);
                if !canvas_rect.intersects(node_rect) { continue; }

                let node_resp = ui.interact(node_rect, Id::new(id.0), Sense::drag());
                if node_resp.clicked() { node_to_select = Some(id); }
                if node_resp.drag_started() { node_to_drag = Some(id); node_to_select = Some(id); }
                if node_resp.dragged() { drag_delta = node_resp.drag_delta(); }

                let is_selected = self.selected_nodes.contains(&id);
                if is_selected { painter.rect_stroke(node_rect.expand(2.0), 5.0 * self.zoom, Stroke::new(2.0 * self.zoom, Color32::from_rgb(200, 180, 0))); }
                painter.rect_filled(node_rect, 5.0 * self.zoom, Color32::from_gray(45));
                painter.rect_stroke(node_rect, 5.0 * self.zoom, Stroke::new(1.0, Color32::from_gray(80)));
                
                let header_h = 24.0 * self.zoom;
                let h_rect = Rect::from_min_size(screen_pos, Vec2::new(screen_size.x, header_h));
                painter.rect_filled(h_rect, Rounding { nw: 5.0 * self.zoom, ne: 5.0 * self.zoom, sw: 0.0, se: 0.0 }, self.get_category_color(&template.category));
                painter.text(h_rect.center(), Align2::CENTER_CENTER, &template.name, FontId::proportional(13.0 * self.zoom), Color32::WHITE);

                let row_h = 22.0 * self.zoom;
                let start_y = screen_pos.y + 36.0 * self.zoom;

                // Helper to draw port interactions
                let mut draw_p = |p_idx: usize, is_output: bool, port: &Port, align: Align2, x: f32| {
                    let p_pos = Pos2::new(x, start_y + p_idx as f32 * row_h);
                    self.draw_port_shape(&painter, p_pos, &port.data_type, 5.0 * self.zoom);
                    
                    if !port.name.is_empty() {
                        let off = if align == Align2::LEFT_CENTER { 12.0 } else { -12.0 } * self.zoom;
                        painter.text(p_pos + Vec2::new(off, 0.0), align, &port.name, FontId::proportional(11.0 * self.zoom), Color32::LIGHT_GRAY);
                    }
                    
                    let p_resp = ui.interact(Rect::from_center_size(p_pos, Vec2::splat(20.0 * self.zoom)), ui.id().with(p_idx).with(is_output).with(id), Sense::drag());
                    if p_resp.hovered() { hover_port = Some((id, p_idx, is_output)); }
                    if p_resp.drag_started() && is_output { start_drag_port = Some((id, p_idx)); }
                };

                for (i, p) in template.inputs.iter().enumerate() { draw_p(i, false, p, Align2::LEFT_CENTER, node_rect.left()); }
                for (i, p) in template.outputs.iter().enumerate() { draw_p(i, true, p, Align2::RIGHT_CENTER, node_rect.right()); }
            }

            // Logic Updates
            if let Some(p) = start_drag_port { self.active_drag_source = Some(p); self.dragged_node = None; }
            if let Some(id) = node_to_select {
                if ui.input(|i| i.modifiers.ctrl) { if self.selected_nodes.contains(&id) { self.selected_nodes.remove(&id); } else { self.selected_nodes.insert(id); } }
                else { if !self.selected_nodes.contains(&id) { self.selected_nodes.clear(); self.selected_nodes.insert(id); } }
                self.last_selected_node = Some(id);
            }
            if let Some(id) = node_to_drag { self.dragged_node = Some(id); }
            if let Some(drag_id) = self.dragged_node {
                let delta = drag_delta / self.zoom;
                if self.selected_nodes.contains(&drag_id) {
                    for &sid in &self.selected_nodes { if let Some(sn) = self.nodes.get_mut(&sid) { sn.position += delta; } }
                } else { if let Some(n) = self.nodes.get_mut(&drag_id) { n.position += delta; } }
            }

            // Selection Box
            if let Some(start) = self.selection_start {
                let r = Rect::from_two_pos(self.world_to_screen(start, canvas_rect), m_pos);
                painter.rect_filled(r, 0.0, Color32::from_rgba_unmultiplied(100, 150, 255, 30));
                painter.rect_stroke(r, 0.0, Stroke::new(1.0, Color32::from_rgb(100, 150, 255)));
                if ui.input(|i| i.pointer.button_released(egui::PointerButton::Primary)) {
                    let wr = Rect::from_two_pos(start, world_m_pos);
                    if !ui.input(|i| i.modifiers.ctrl) { self.selected_nodes.clear(); }
                    for (id, node) in &self.nodes { if wr.intersects(Rect::from_min_size(node.position, node.size)) { self.selected_nodes.insert(*id); } }
                    self.selection_start = None;
                }
            }

            if let Some(last) = self.swipe_last_pos { painter.line_segment([self.world_to_screen(last, canvas_rect), m_pos], Stroke::new(2.0, Color32::RED)); }
            
            // Drag Connection Line
            if let Some((sn, sp_idx)) = self.active_drag_source {
                let p1 = self.world_to_screen(self.get_port_world_pos(sn, sp_idx, true), canvas_rect);
                // Get color from template
                let mut color = Color32::YELLOW;
                if let Some(node) = self.nodes.get(&sn) {
                     if let Some(tmpl) = self.templates.iter().find(|t| t.name == node.template_name) {
                         if let Some(port) = tmpl.outputs.get(sp_idx) {
                             color = port.data_type.color();
                         }
                     }
                }
                self.draw_connection(&painter, p1, m_pos, color);
            }

            if ui.input(|i| i.pointer.any_released()) {
                if let Some((sn, sp_idx)) = self.active_drag_source {
                    if let Some((dn, dp_idx, is_output)) = hover_port {
                        if !is_output && sn != dn {
                            // Valid connection: Source Output -> Target Input
                            // Type check could happen here
                            self.connections.push(Connection { from_node: sn, from_port: sp_idx, to_node: dn, to_port: dp_idx });
                        }
                    }
                }
                if let Some(t) = self.dragged_template.take() { if canvas_rect.contains(m_pos) { self.add_node(&t.name, world_m_pos - Vec2::new(75.0, 10.0)); } }
                self.dragged_node = None; self.active_drag_source = None; self.selection_start = None;
            }

            if let Some(t) = &self.dragged_template {
                let pr = Rect::from_center_size(m_pos, Vec2::new(150.0, 40.0) * self.zoom);
                painter.rect_filled(pr, 5.0 * self.zoom, self.get_category_color(&t.category).gamma_multiply(0.5));
                painter.text(m_pos, Align2::CENTER_CENTER, &t.name, FontId::proportional(13.0 * self.zoom), Color32::WHITE);
            }
            if canvas_resp.clicked() && node_to_select.is_none() && !ui.input(|i| i.modifiers.shift) { self.selected_nodes.clear(); }

            // Context Menu (Simplified)
            if let Some(pos) = self.context_menu_pos {
                let world_pos = self.screen_to_world(pos, canvas_rect);
                let mut close_menu = false;
                
                egui::Area::new(ui.id().with("context_menu"))
                    .fixed_pos(pos)
                    .order(egui::Order::Foreground)
                    .show(ui.ctx(), |ui| {
                        egui::Frame::menu(ui.style()).show(ui, |ui| {
                            ui.set_width(120.0);
                            
                             if ui.add_enabled(!self.selected_nodes.is_empty(), egui::Button::new("✂ Cut")).clicked() {
                                // Logic omitted for brevity, similar to original but needs struct update
                                close_menu = true;
                            }
                            
                            if ui.add_enabled(!self.selected_nodes.is_empty(), egui::Button::new("🗑 Delete")).clicked() {
                                for id in self.selected_nodes.drain() {
                                    self.nodes.remove(&id);
                                    self.connections.retain(|c| c.from_node != id && c.to_node != id);
                                }
                                close_menu = true;
                            }

                             if ui.button("Reset View").clicked() {
                                self.pan_offset = Vec2::ZERO;
                                self.zoom = 1.0;
                                close_menu = true;
                            }
                        });
                    });
                 if close_menu || (ui.input(|i| i.pointer.any_pressed()) && !Rect::from_min_size(pos, Vec2::new(120.0, 50.0)).contains(m_pos)) {
                    self.context_menu_pos = None;
                }
            }
        });
    }
    fn box_clone(&self) -> Box<dyn TabInstance> { Box::new(self.clone()) }
}

// ----------------------------------------------------------------------------
// Plugin Implementation
// ----------------------------------------------------------------------------

#[derive(Default)]
struct VisualFlowConfig {
    package_paths: Vec<PathBuf>,
}

pub struct VisualFlowCanvasPlugin {
    config: VisualFlowConfig,
    template_cache: Vec<NodeTemplate>,
}

impl VisualFlowCanvasPlugin {
    fn new() -> Self {
        let mut config = VisualFlowConfig::default();
        // Add default local path
        let default_path = PathBuf::from("src/plugins/visual_flow_canvas/nodes");
        if default_path.exists() {
            config.package_paths.push(default_path);
        } else {
             // Fallback for full path if running from root
             let fallback = PathBuf::from("verbium/src/plugins/visual_flow_canvas/nodes");
             if fallback.exists() {
                 config.package_paths.push(fallback);
             }
        }

        let mut plugin = Self {
            config,
            template_cache: Vec::new(),
        };
        plugin.reload_templates();
        plugin
    }

    fn reload_templates(&mut self) {
        self.template_cache = NodeLoader::load_from_paths(&self.config.package_paths);
        if self.template_cache.is_empty() {
             // Fallback if nothing loaded
             let core_src = r#"
                /// @name: Start
                /// @flow: -> next
                fn start() {}
                
                /// @name: If
                /// @flow: in -> true, false
                /// @in: condition: Bool
                fn branch(condition) {}
            "#;
            self.template_cache.extend(NodeLoader::parse_vfnode_content("Flow", core_src));
        }
    }
}

impl Plugin for VisualFlowCanvasPlugin {
    fn name(&self) -> &str { crate::plugins::PLUGIN_NAME_VISUAL_FLOW_CANVAS }
    
    fn on_tab_menu(&mut self, ui: &mut Ui, control: &mut Vec<AppCommand>) {
        if ui.button("VisualFlowCanvas").clicked() {
            // Pass the currently loaded templates to the new tab
            control.push(AppCommand::OpenTab(Tab::new(Box::new(VisualFlowCanvasTab::new(self.template_cache.clone())))));
            ui.close_menu();
        }
    }

    fn on_settings_ui(&mut self, ui: &mut Ui) {
        ui.heading("VisualFlow Node Packages");
        ui.separator();

        let mut path_to_remove = None;
        for (i, path) in self.config.package_paths.iter().enumerate() {
            ui.horizontal(|ui| {
                ui.label(path.to_string_lossy());
                if ui.button("Remove").clicked() {
                    path_to_remove = Some(i);
                }
            });
        }
        if let Some(i) = path_to_remove {
            self.config.package_paths.remove(i);
        }

        ui.horizontal(|ui| {
            if ui.button("Add Folder").clicked() {
                 if let Some(path) = rfd::FileDialog::new().pick_folder() {
                     self.config.package_paths.push(path);
                 }
            }
            if ui.button("Add File").clicked() {
                 if let Some(path) = rfd::FileDialog::new().add_filter("VisualFlow Node", &["vfnode"]).pick_file() {
                     self.config.package_paths.push(path);
                 }
            }
        });
        
        ui.separator();
        if ui.button("Reload Templates").clicked() {
            self.reload_templates();
        }
        ui.label(format!("Loaded {} templates", self.template_cache.len()));
    }
}

pub fn create() -> VisualFlowCanvasPlugin { VisualFlowCanvasPlugin::new() }

