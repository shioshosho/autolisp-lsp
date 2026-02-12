use std::path::Path;

use serde::Deserialize;

#[derive(Debug, Clone, Deserialize)]
pub struct Config {
    #[serde(default)]
    pub format: FormatConfig,
}

impl Default for Config {
    fn default() -> Self {
        Self {
            format: FormatConfig::default(),
        }
    }
}

#[derive(Debug, Clone, Deserialize)]
#[serde(rename_all = "kebab-case")]
pub struct FormatConfig {
    #[serde(default = "default_true")]
    pub force_convert_case: bool,
}

impl Default for FormatConfig {
    fn default() -> Self {
        Self {
            force_convert_case: default_true(),
        }
    }
}

fn default_true() -> bool {
    true
}

pub fn load_config(workspace_root: Option<&Path>) -> Config {
    // Try project-level config first (higher priority)
    if let Some(root) = workspace_root {
        let project_config = root.join(".autolisp-lsp.yml");
        if let Some(cfg) = try_load(&project_config) {
            return cfg;
        }
    }

    // Fall back to global config
    if let Some(home) = std::env::var_os("HOME") {
        let global_config = Path::new(&home).join(".autolisp-lsp.yml");
        if let Some(cfg) = try_load(&global_config) {
            return cfg;
        }
    }

    Config::default()
}

fn try_load(path: &Path) -> Option<Config> {
    let content = std::fs::read_to_string(path).ok()?;
    serde_yaml::from_str(&content).ok()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_config_parse_yaml() {
        let yaml = "format:\n  force-convert-case: false\n";
        let cfg: Config = serde_yaml::from_str(yaml).unwrap();
        assert!(!cfg.format.force_convert_case);
    }

    #[test]
    fn test_config_parse_yaml_true() {
        let yaml = "format:\n  force-convert-case: true\n";
        let cfg: Config = serde_yaml::from_str(yaml).unwrap();
        assert!(cfg.format.force_convert_case);
    }

    #[test]
    fn test_config_default() {
        let cfg = Config::default();
        assert!(cfg.format.force_convert_case);
    }

    #[test]
    fn test_config_empty_yaml() {
        let yaml = "";
        let cfg: Config = serde_yaml::from_str(yaml).unwrap();
        assert!(cfg.format.force_convert_case);
    }

    #[test]
    fn test_config_partial_yaml() {
        let yaml = "format: {}\n";
        let cfg: Config = serde_yaml::from_str(yaml).unwrap();
        assert!(cfg.format.force_convert_case);
    }
}
