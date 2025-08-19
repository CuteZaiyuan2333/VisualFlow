//! Project management module
//! Handles project creation, loading, and file system operations

use std::path::PathBuf;
use std::fs;
use crate::filesystem::FileSystemEntry;



pub struct ProjectManager {
    pub current_project_path: Option<PathBuf>,
    pub project_name: Option<String>,
}

impl Default for ProjectManager {
    fn default() -> Self {
        Self {
            current_project_path: None,
            project_name: None,
        }
    }
}

impl ProjectManager {
    /// Create a new ProjectManager instance
    pub fn new() -> Self {
        Self::default()
    }
    
    /// Get the root path of the current project
    pub fn get_root_path(&self) -> Option<&PathBuf> {
        self.current_project_path.as_ref()
    }
    
    /// Get project configuration
    pub fn get_config(&self) -> Option<serde_json::Value> {
        if let Some(project_path) = &self.current_project_path {
            let config_path = project_path.join("project.json");
            if let Ok(content) = fs::read_to_string(&config_path) {
                serde_json::from_str(&content).ok()
            } else {
                None
            }
        } else {
            None
        }
    }
    
    /// Create a new project in the specified directory
    pub fn create_project(&mut self, project_path: PathBuf, project_name: String) -> Result<(), String> {
        // Check if project.json already exists in the directory
        let config_path = project_path.join("project.json");
        if config_path.exists() {
            return Err("Directory already contains a VisualFlow project".to_string());
        }
        
        // Create project directory if it doesn't exist
        fs::create_dir_all(&project_path)
            .map_err(|e| format!("Failed to create project directory: {}", e))?;
        
        // Create basic project structure
        let src_dir = project_path.join("src");
        fs::create_dir_all(&src_dir)
            .map_err(|e| format!("Failed to create src directory: {}", e))?;
        
        // Create main.py file
        let main_py_content = "# Main Python file\nprint('Hello, VisualFlow!')\n";
        fs::write(src_dir.join("main.py"), main_py_content)
            .map_err(|e| format!("Failed to create main.py: {}", e))?;
        
        // Create project config file
        let config_content = format!(
            r#"{{
    "name": "{}",
    "version": "1.0.0",
    "language": "python",
    "entry_point": "src/main.py"
}}
"#,
            project_name
        );
        fs::write(project_path.join("project.json"), config_content)
            .map_err(|e| format!("Failed to create project.json: {}", e))?;
        
        self.current_project_path = Some(project_path);
        self.project_name = Some(project_name);
        
        Ok(())
    }
    
    /// Open an existing project
    pub fn open_project(&mut self, project_path: PathBuf) -> Result<(), String> {
        if !project_path.exists() {
            return Err("Project directory does not exist".to_string());
        }
        
        // Check if it's a valid project (has project.json)
        let config_path = project_path.join("project.json");
        if !config_path.exists() {
            return Err("Not a valid VisualFlow project (missing project.json)".to_string());
        }
        
        // Read project name from config
        let config_content = fs::read_to_string(&config_path)
            .map_err(|e| format!("Failed to read project config: {}", e))?;
        
        let config: serde_json::Value = serde_json::from_str(&config_content)
            .map_err(|e| format!("Invalid project config: {}", e))?;
        
        let project_name = config["name"].as_str()
            .unwrap_or("Unknown Project")
            .to_string();
        
        self.current_project_path = Some(project_path);
        self.project_name = Some(project_name);
        
        Ok(())
    }
    
    /// Get the current project's file system tree
    pub fn get_file_system_tree(&self) -> Vec<FileSystemEntry> {
        if let Some(project_path) = &self.current_project_path {
            use crate::filesystem::FileSystemService;
            FileSystemService::scan_directory(project_path).unwrap_or_default()
        } else {
            // Return default/empty structure if no project is open
            vec![
                FileSystemEntry::Dir {
                    name: "No project open".to_string(),
                    path: std::path::PathBuf::from("."),
                    children: vec![],
                    expanded: false,
                },
            ]
        }
    }
    

    
    /// Check if a project is currently open
    pub fn has_open_project(&self) -> bool {
        self.current_project_path.is_some()
    }
    
    /// Get the current project name
    pub fn get_project_name(&self) -> Option<&str> {
        self.project_name.as_deref()
    }
    
    /// Close the current project
    pub fn close_project(&mut self) {
        self.current_project_path = None;
        self.project_name = None;
    }
}