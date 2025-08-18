//! Syntax highlighting module using syntect

use syntect::easy::HighlightLines;
use syntect::highlighting::{ThemeSet, Style};
use syntect::parsing::SyntaxSet;
use syntect::util::{as_24_bit_terminal_escaped, LinesWithEndings};
use eframe::egui;

pub struct SyntaxHighlighter {
    syntax_set: SyntaxSet,
    theme_set: ThemeSet,
}

impl Default for SyntaxHighlighter {
    fn default() -> Self {
        Self {
            syntax_set: SyntaxSet::load_defaults_newlines(),
            theme_set: ThemeSet::load_defaults(),
        }
    }
}

impl SyntaxHighlighter {
    /// Highlight code and return formatted text for egui
    pub fn highlight_code(&self, code: &str, language: &str) -> Vec<(String, egui::Color32)> {
        let syntax = self.syntax_set
            .find_syntax_by_extension(language)
            .or_else(|| self.syntax_set.find_syntax_by_name(language))
            .unwrap_or_else(|| self.syntax_set.find_syntax_plain_text());
        
        let theme = &self.theme_set.themes["base16-ocean.dark"];
        let mut highlighter = HighlightLines::new(syntax, theme);
        
        let mut result = Vec::new();
        
        for line in LinesWithEndings::from(code) {
            let ranges = highlighter.highlight_line(line, &self.syntax_set)
                .unwrap_or_else(|_| vec![(syntect::highlighting::Style::default(), line)]);
            
            for (style, text) in ranges {
                let color = style_to_egui_color(style);
                result.push((text.to_string(), color));
            }
        }
        
        result
    }
    
    /// Get available syntax names
    pub fn get_syntax_names(&self) -> Vec<String> {
        self.syntax_set.syntaxes()
            .iter()
            .map(|s| s.name.clone())
            .collect()
    }
}

/// Convert syntect Style to egui Color32
fn style_to_egui_color(style: Style) -> egui::Color32 {
    let fg = style.foreground;
    egui::Color32::from_rgb(fg.r, fg.g, fg.b)
}

/// Simple Python syntax highlighter for basic cases
pub fn highlight_python_simple(code: &str) -> Vec<(String, egui::Color32)> {
    let mut result = Vec::new();
    let lines = code.lines();
    
    for line in lines {
        let trimmed = line.trim();
        
        // Comments
        if trimmed.starts_with('#') {
            result.push((format!("{}\n", line), egui::Color32::from_rgb(128, 128, 128)));
            continue;
        }
        
        // Keywords
        let keywords = [
            "def", "class", "if", "else", "elif", "for", "while", "try", "except", 
            "finally", "with", "as", "import", "from", "return", "yield", "break", 
            "continue", "pass", "and", "or", "not", "in", "is", "lambda", "global", 
            "nonlocal", "assert", "del", "raise", "True", "False", "None"
        ];
        
        let mut line_parts = Vec::new();
        let words: Vec<&str> = line.split_whitespace().collect();
        let mut current_pos = 0;
        
        for word in words {
            // Find the word position in the original line
            if let Some(word_start) = line[current_pos..].find(word) {
                let actual_start = current_pos + word_start;
                
                // Add any whitespace before the word
                if actual_start > current_pos {
                    line_parts.push((line[current_pos..actual_start].to_string(), egui::Color32::WHITE));
                }
                
                // Determine word color
                let color = if keywords.contains(&word) {
                    egui::Color32::from_rgb(86, 156, 214) // Blue for keywords
                } else if word.starts_with('"') || word.starts_with('\'') {
                    egui::Color32::from_rgb(206, 145, 120) // Orange for strings
                } else if word.chars().all(|c| c.is_ascii_digit() || c == '.') {
                    egui::Color32::from_rgb(181, 206, 168) // Green for numbers
                } else {
                    egui::Color32::WHITE // Default color
                };
                
                line_parts.push((word.to_string(), color));
                current_pos = actual_start + word.len();
            }
        }
        
        // Add any remaining characters
        if current_pos < line.len() {
            line_parts.push((line[current_pos..].to_string(), egui::Color32::WHITE));
        }
        
        // Add the line parts to result
        for (text, color) in line_parts {
            result.push((text, color));
        }
        result.push(("\n".to_string(), egui::Color32::WHITE));
    }
    
    result
}