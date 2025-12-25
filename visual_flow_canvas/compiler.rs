use super::{NodeId, NodeInstance, Connection, NodeTemplate, DataType};
use std::collections::{HashMap, HashSet};

pub struct FlowCompiler<'a> {
    nodes: &'a HashMap<NodeId, NodeInstance>,
    connections: &'a Vec<Connection>,
    templates: &'a [NodeTemplate],
    
    /// 记录已生成代码的节点及其输出变量名
    /// Key: (NodeId, OutputPortIndex) -> Value: VariableName
    generated_outputs: HashMap<(NodeId, usize), String>,
    
    /// 记录已处理过的节点，防止重复生成或循环引用
    visited_nodes: HashSet<NodeId>,
    
    /// 生成的代码行缓冲区
    lines: Vec<String>,
}

impl<'a> FlowCompiler<'a> {
    pub fn new(nodes: &'a HashMap<NodeId, NodeInstance>, connections: &'a Vec<Connection>, templates: &'a [NodeTemplate]) -> Self {
        Self { 
            nodes, 
            connections, 
            templates, 
            generated_outputs: HashMap::new(),
            visited_nodes: HashSet::new(),
            lines: Vec::new(),
        }
    }

    pub fn compile(&mut self) -> Result<String, String> {
        // 1. 寻找入口节点 (Start)
        let start_node = self.nodes.values() 
            .find(|n| n.template_name == "Start")
            .ok_or("No 'Start' node found.")?;

        self.lines.push("fn main_flow() {".to_string());

        // 2. 从 Start 开始沿着执行流遍历
        let mut current_node_id = Some(start_node.id);
        
        // 为了避免死循环，简单的步数限制
        let mut steps = 0;
        let max_steps = 1000;

        while let Some(node_id) = current_node_id {
            if steps > max_steps {
                return Err("Flow execution limit reached (possible loop?).".to_string());
            }
            steps += 1;

            // 生成当前节点的代码（如果之前被作为数据依赖递归生成过，这里会跳过）
            if !self.visited_nodes.contains(&node_id) {
                self.generate_node_logic(node_id)?;
            }

            // 寻找下一个执行节点
            current_node_id = self.get_next_flow_node(node_id)?;
        }

        self.lines.push("}".to_string());
        Ok(self.lines.join("\n"))
    }

    /// 生成单个节点的逻辑代码
    /// 如果该节点依赖的数据尚未生成，会递归生成上游数据节点
    fn generate_node_logic(&mut self, node_id: NodeId) -> Result<(), String> {
        if self.visited_nodes.contains(&node_id) {
            return Ok(());
        }
        
        let node = self.nodes.get(&node_id).ok_or("Node not found")?;
        let template = self.templates.iter().find(|t| t.name == node.template_name)
            .ok_or(format!("Template '{}' not found", node.template_name))?;

        // 1. 解析所有数据输入端口
        let mut arg_vars = Vec::new();
        
        for (i, port) in template.inputs.iter().enumerate() {
            // 跳过执行流端口
            if port.data_type == DataType::Flow { continue; }

            // 寻找连入此端口的线
            // Connection target: (to_node: node_id, to_port: i)
            if let Some(conn) = self.connections.iter().find(|c| c.to_node == node_id && c.to_port == i) {
                // 检查源节点是否已生成
                let src_key = (conn.from_node, conn.from_port);
                
                if let Some(var_name) = self.generated_outputs.get(&src_key) {
                    arg_vars.push(var_name.clone());
                } else {
                    // 数据依赖未生成 -> 递归生成源节点
                    // 检测循环引用
                    // 注意：这里简单的递归可能在环状数据流中死循环，需要改进 Visited 检查机制
                    // 暂时假设数据流是 DAG
                    self.generate_node_logic(conn.from_node)?;
                    
                    // 递归回来后，再次尝试获取
                    if let Some(var_name) = self.generated_outputs.get(&src_key) {
                        arg_vars.push(var_name.clone());
                    } else {
                        return Err(format!("Failed to resolve dependency from node {:?} port {}", conn.from_node, conn.from_port));
                    }
                }
            } else {
                // 没有连线 -> 使用默认值
                // TODO: 未来应从 NodeInstance 中读取默认值配置
                let default_val = match port.data_type {
                    DataType::String => "\"\"",
                    DataType::Number => "0",
                    DataType::Bool => "false",
                    _ => "()",
                };
                arg_vars.push(default_val.to_string());
            }
        }

        // 2. 生成函数调用语句
        // 忽略 Start 节点的生成（它是入口，没有逻辑，或者逻辑为空）
        if node.template_name != "Start" {
            let fn_call = format!("{}({})", template.rhai_fn_name, arg_vars.join(", "));
            
            // 3. 处理输出变量
            let mut data_outputs = Vec::new();
            for (i, port) in template.outputs.iter().enumerate() {
                if port.data_type == DataType::Flow { continue; }
                data_outputs.push((i, format!("_v{}_{}", node_id.0, i)));
            }

            if data_outputs.is_empty() {
                // 无返回值调用
                self.lines.push(format!("    {};", fn_call));
            } else if data_outputs.len() == 1 {
                // 单返回值
                let (idx, var_name) = &data_outputs[0];
                self.lines.push(format!("    let {} = {};", var_name, fn_call));
                self.generated_outputs.insert((node_id, *idx), var_name.clone());
            } else {
                // 多返回值 (暂不支持 Rhai 的解构，假设 Rhai 函数返回 Array 或 Map?)
                // MVP 暂时只支持取第一个返回值，或者假设函数名_outputN 这种魔改
                // 这里为了稳健，暂时只取第一个作为主返回值，其他忽略或报错
                let (idx, var_name) = &data_outputs[0];
                 self.lines.push(format!("    let {} = {}; // Warn: Multi-output partially supported", var_name, fn_call));
                 self.generated_outputs.insert((node_id, *idx), var_name.clone());
            }
        }

        self.visited_nodes.insert(node_id);
        Ok(())
    }

    fn get_next_flow_node(&self, current_node_id: NodeId) -> Result<Option<NodeId>, String> {
        let node = self.nodes.get(&current_node_id).ok_or("Node not found")?;
        let template = self.templates.iter().find(|t| t.name == node.template_name)
            .ok_or("Template not found")?;

        // 找到第一个 Flow 类型的输出端口
        if let Some(flow_port_idx) = template.outputs.iter().position(|p| p.data_type == DataType::Flow) {
            // 找到连接该端口的线
            if let Some(conn) = self.connections.iter().find(|c| c.from_node == current_node_id && c.from_port == flow_port_idx) {
                return Ok(Some(conn.to_node));
            }
        }
        
        Ok(None)
    }
}
