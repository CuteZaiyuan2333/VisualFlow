//! File system operations and structures

use std::path::{Path, PathBuf};
use std::fs;

/// Enhanced file system entry with metadata
#[derive(Debug, Clone)]
pub enum FileSystemEntry {
    File {
        name: String,
        path: PathBuf,
    },
    Dir {
        name: String,
        path: PathBuf,
        children: Vec<FileSystemEntry>,
        expanded: bool,
    },
}



/// File system service for scanning and managing project files
pub struct FileSystemService;

impl FileSystemService {
    pub fn new() -> Self {
        Self
    }
    
    /// Scan a directory and return file system entries
    pub fn scan_directory(dir_path: &Path) -> Result<Vec<FileSystemEntry>, std::io::Error> {
        let mut entries = Vec::new();
        
        if !dir_path.exists() {
            return Ok(entries);
        }
        
        let read_dir = fs::read_dir(dir_path)?;
        let mut items: Vec<_> = read_dir.collect::<Result<Vec<_>, _>>()?;
        
        // Sort: directories first, then files, both alphabetically
        items.sort_by(|a, b| {
            let a_is_dir = a.file_type().map(|ft| ft.is_dir()).unwrap_or(false);
            let b_is_dir = b.file_type().map(|ft| ft.is_dir()).unwrap_or(false);
            
            match (a_is_dir, b_is_dir) {
                (true, false) => std::cmp::Ordering::Less,
                (false, true) => std::cmp::Ordering::Greater,
                _ => a.file_name().cmp(&b.file_name()),
            }
        });
        
        for entry in items {
            let file_name = entry.file_name().to_string_lossy().to_string();
            let path = entry.path();
            
            // Skip hidden files and common build/cache directories
            if file_name.starts_with('.') || 
               file_name == "__pycache__" || 
               file_name == "node_modules" || 
               file_name == "target" {
                continue;
            }
            
            if let Ok(file_type) = entry.file_type() {
                if file_type.is_dir() {
                    let children = Self::scan_directory(&path).unwrap_or_default();
                    entries.push(FileSystemEntry::Dir {
                        name: file_name,
                        path,
                        children,
                        expanded: false,
                    });
                } else {
                    entries.push(FileSystemEntry::File {
                        name: file_name,
                        path,
                    });
                }
            }
        }
        
        Ok(entries)
    }
}

impl Default for FileSystemService {
    fn default() -> Self {
        Self::new()
    }
}