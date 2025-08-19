//! File operations service
//! 
//! This module provides actual file system operations for the UI

use std::path::{Path, PathBuf};
use std::fs;
use std::io;

/// Result type for file operations
pub type FileOpResult<T> = Result<T, FileOpError>;

/// File operation errors
#[derive(Debug, Clone)]
pub enum FileOpError {
    /// IO error
    Io(String),
    /// File already exists
    AlreadyExists(PathBuf),
    /// File not found
    NotFound(PathBuf),
    /// Permission denied
    PermissionDenied(PathBuf),
    /// Invalid file name
    InvalidName(String),
    /// Operation not supported
    #[allow(dead_code)]
    NotSupported(String),
}

impl std::fmt::Display for FileOpError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            FileOpError::Io(msg) => write!(f, "IO error: {}", msg),
            FileOpError::AlreadyExists(path) => write!(f, "File already exists: {}", path.display()),
            FileOpError::NotFound(path) => write!(f, "File not found: {}", path.display()),
            FileOpError::PermissionDenied(path) => write!(f, "Permission denied: {}", path.display()),
            FileOpError::InvalidName(name) => write!(f, "Invalid file name: {}", name),
            FileOpError::NotSupported(op) => write!(f, "Operation not supported: {}", op),
        }
    }
}

impl From<io::Error> for FileOpError {
    fn from(error: io::Error) -> Self {
        match error.kind() {
            io::ErrorKind::NotFound => FileOpError::NotFound(PathBuf::new()),
            io::ErrorKind::PermissionDenied => FileOpError::PermissionDenied(PathBuf::new()),
            io::ErrorKind::AlreadyExists => FileOpError::AlreadyExists(PathBuf::new()),
            _ => FileOpError::Io(error.to_string()),
        }
    }
}

/// File operations service
pub struct FileOperations;

impl FileOperations {
    /// Rename a file or directory
    pub fn rename<P: AsRef<Path>>(from: P, to: P) -> FileOpResult<()> {
        let from = from.as_ref();
        let to = to.as_ref();
        
        if !from.exists() {
            return Err(FileOpError::NotFound(from.to_path_buf()));
        }
        
        if to.exists() {
            return Err(FileOpError::AlreadyExists(to.to_path_buf()));
        }
        
        fs::rename(from, to).map_err(|e| match e.kind() {
            io::ErrorKind::PermissionDenied => FileOpError::PermissionDenied(from.to_path_buf()),
            _ => FileOpError::Io(e.to_string()),
        })
    }
    
    /// Copy a file or directory recursively
    pub fn copy<P: AsRef<Path>>(from: P, to: P) -> FileOpResult<()> {
        let from = from.as_ref();
        let to = to.as_ref();
        
        if !from.exists() {
            return Err(FileOpError::NotFound(from.to_path_buf()));
        }
        
        if from.is_dir() {
            Self::copy_dir_recursive(from, to)
        } else {
            Self::copy_file(from, to)
        }
    }
    
    /// Copy a single file
    fn copy_file<P: AsRef<Path>>(from: P, to: P) -> FileOpResult<()> {
        let from = from.as_ref();
        let to = to.as_ref();
        
        // Create parent directory if it doesn't exist
        if let Some(parent) = to.parent() {
            fs::create_dir_all(parent).map_err(FileOpError::from)?;
        }
        
        fs::copy(from, to).map_err(|e| match e.kind() {
            io::ErrorKind::PermissionDenied => FileOpError::PermissionDenied(from.to_path_buf()),
            _ => FileOpError::Io(e.to_string()),
        })?;
        
        Ok(())
    }
    
    /// Copy a directory recursively
    fn copy_dir_recursive<P: AsRef<Path>>(from: P, to: P) -> FileOpResult<()> {
        let from = from.as_ref();
        let to = to.as_ref();
        
        // Create the target directory
        fs::create_dir_all(to).map_err(FileOpError::from)?;
        
        // Copy all entries
        for entry in fs::read_dir(from).map_err(FileOpError::from)? {
            let entry = entry.map_err(FileOpError::from)?;
            let entry_path = entry.path();
            let target_path = to.join(entry.file_name());
            
            if entry_path.is_dir() {
                Self::copy_dir_recursive(&entry_path, &target_path)?;
            } else {
                Self::copy_file(&entry_path, &target_path)?;
            }
        }
        
        Ok(())
    }
    
    /// Move a file or directory
    pub fn move_item<P: AsRef<Path>>(from: P, to: P) -> FileOpResult<()> {
        let from = from.as_ref();
        let to = to.as_ref();
        
        if !from.exists() {
            return Err(FileOpError::NotFound(from.to_path_buf()));
        }
        
        // Try rename first (fastest for same filesystem)
        match fs::rename(from, to) {
            Ok(()) => Ok(()),
            Err(_) => {
                // If rename fails, try copy + delete
                Self::copy(from, to)?;
                Self::delete(from)?;
                Ok(())
            }
        }
    }
    
    /// Delete a file or directory
    pub fn delete<P: AsRef<Path>>(path: P) -> FileOpResult<()> {
        let path = path.as_ref();
        
        // Check if file exists before deletion
        if !path.exists() {
            return Err(FileOpError::NotFound(path.to_path_buf()));
        }
        
        // Attempt deletion
        let result = if path.is_dir() {
            fs::remove_dir_all(path).map_err(|e| {
                match e.kind() {
                    io::ErrorKind::PermissionDenied => FileOpError::PermissionDenied(path.to_path_buf()),
                    _ => FileOpError::Io(e.to_string()),
                }
            })
        } else {
            fs::remove_file(path).map_err(|e| {
                match e.kind() {
                    io::ErrorKind::PermissionDenied => FileOpError::PermissionDenied(path.to_path_buf()),
                    _ => FileOpError::Io(e.to_string()),
                }
            })
        };
        
        // Verify deletion
        if result.is_ok() && path.exists() {
            return Err(FileOpError::Io("File still exists after deletion".to_string()));
        }
        
        result
    }
    

    
    /// Create a new directory
    #[allow(dead_code)]
    pub fn create_dir<P: AsRef<Path>>(path: P) -> FileOpResult<()> {
        let path = path.as_ref();
        
        if path.exists() {
            return Err(FileOpError::AlreadyExists(path.to_path_buf()));
        }
        
        fs::create_dir_all(path).map_err(|e| match e.kind() {
            io::ErrorKind::PermissionDenied => FileOpError::PermissionDenied(path.to_path_buf()),
            _ => FileOpError::Io(e.to_string()),
        })
    }
    
    /// Generate a unique file name if the original already exists
    pub fn generate_unique_name<P: AsRef<Path>>(path: P) -> PathBuf {
        let path = path.as_ref();
        
        if !path.exists() {
            return path.to_path_buf();
        }
        
        let parent = path.parent().unwrap_or(Path::new(""));
        let stem = path.file_stem().and_then(|s| s.to_str()).unwrap_or("file");
        let extension = path.extension().and_then(|s| s.to_str());
        
        for i in 1..1000 {
            let new_name = if let Some(ext) = extension {
                format!("{} ({}).{}", stem, i, ext)
            } else {
                format!("{} ({})", stem, i)
            };
            
            let new_path = parent.join(new_name);
            if !new_path.exists() {
                return new_path;
            }
        }
        
        // Fallback
        path.to_path_buf()
    }
    
    /// Validate file name
    pub fn validate_filename(name: &str) -> FileOpResult<()> {
        if name.trim().is_empty() {
            return Err(FileOpError::InvalidName("Name cannot be empty".to_string()));
        }
        
        // Check for invalid characters
        let invalid_chars = ['<', '>', ':', '"', '|', '?', '*', '/', '\\'];
        for ch in invalid_chars.iter() {
            if name.contains(*ch) {
                return Err(FileOpError::InvalidName(format!("Name cannot contain '{}'", ch)));
            }
        }
        
        // Check for reserved names on Windows
        let reserved_names = [
            "CON", "PRN", "AUX", "NUL",
            "COM1", "COM2", "COM3", "COM4", "COM5", "COM6", "COM7", "COM8", "COM9",
            "LPT1", "LPT2", "LPT3", "LPT4", "LPT5", "LPT6", "LPT7", "LPT8", "LPT9",
        ];
        
        let name_upper = name.to_uppercase();
        for reserved in reserved_names.iter() {
            if name_upper == *reserved || name_upper.starts_with(&format!("{}.", reserved)) {
                return Err(FileOpError::InvalidName(format!("'{}' is a reserved name", name)));
            }
        }
        
        Ok(())
    }
}