//! SR5 interface specifications for IDE/analysis use.
//!
//! Provides interface specs without requiring hardware, for use in
//! tools like the IDE that only need parsing/analysis support.

#![allow(dead_code)]

use std::sync::OnceLock;

use rpl::interface::InterfaceSpec;

const GRAPHICS_INTERFACE: &str = include_str!("sr5-graphics.rpli");
const INPUT_INTERFACE: &str = include_str!("sr5-input.rpli");
const SYSTEM_INTERFACE: &str = include_str!("sr5-system.rpli");
const SPRITES_INTERFACE: &str = include_str!("sr5-sprites.rpli");
const TILES_INTERFACE: &str = include_str!("sr5-tiles.rpli");

/// Get the SR5 Graphics interface spec.
pub fn graphics_interface() -> &'static InterfaceSpec {
    static SPEC: OnceLock<InterfaceSpec> = OnceLock::new();
    SPEC.get_or_init(|| {
        InterfaceSpec::from_dsl(GRAPHICS_INTERFACE).expect("invalid sr5-graphics interface")
    })
}

/// Get the SR5 Input interface spec.
pub fn input_interface() -> &'static InterfaceSpec {
    static SPEC: OnceLock<InterfaceSpec> = OnceLock::new();
    SPEC.get_or_init(|| {
        InterfaceSpec::from_dsl(INPUT_INTERFACE).expect("invalid sr5-input interface")
    })
}

/// Get the SR5 System interface spec.
pub fn system_interface() -> &'static InterfaceSpec {
    static SPEC: OnceLock<InterfaceSpec> = OnceLock::new();
    SPEC.get_or_init(|| {
        InterfaceSpec::from_dsl(SYSTEM_INTERFACE).expect("invalid sr5-system interface")
    })
}

/// Get the SR5 Sprites interface spec.
pub fn sprites_interface() -> &'static InterfaceSpec {
    static SPEC: OnceLock<InterfaceSpec> = OnceLock::new();
    SPEC.get_or_init(|| {
        InterfaceSpec::from_dsl(SPRITES_INTERFACE).expect("invalid sr5-sprites interface")
    })
}

/// Get the SR5 Tiles interface spec.
pub fn tiles_interface() -> &'static InterfaceSpec {
    static SPEC: OnceLock<InterfaceSpec> = OnceLock::new();
    SPEC.get_or_init(|| {
        InterfaceSpec::from_dsl(TILES_INTERFACE).expect("invalid sr5-tiles interface")
    })
}

/// Register all SR5 interfaces with an interface registry.
pub fn register_interfaces(registry: &mut rpl::registry::InterfaceRegistry) {
    registry.add(graphics_interface().clone());
    registry.add(input_interface().clone());
    registry.add(system_interface().clone());
    registry.add(sprites_interface().clone());
    registry.add(tiles_interface().clone());
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn sprites_interface_loads() {
        let spec = sprites_interface();
        assert_eq!(spec.id(), 203);
        assert_eq!(spec.name(), "SR5Sprites");
        // Check that SPR command exists
        let commands = spec.commands();
        let spr = commands
            .iter()
            .find(|c| c.names.contains(&"SPR".to_string()));
        assert!(spr.is_some(), "SPR command should exist");
    }

    #[test]
    fn tiles_interface_loads() {
        let spec = tiles_interface();
        assert_eq!(spec.id(), 204);
        assert_eq!(spec.name(), "SR5Tiles");
        // Check that LSCALE command exists
        let commands = spec.commands();
        let lscale = commands
            .iter()
            .find(|c| c.names.contains(&"LSCALE".to_string()));
        assert!(lscale.is_some(), "LSCALE command should exist");
    }

    #[test]
    fn all_interfaces_load() {
        let _ = graphics_interface();
        let _ = input_interface();
        let _ = system_interface();
        let _ = sprites_interface();
        let _ = tiles_interface();
    }
}
