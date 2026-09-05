use std::collections::HashMap;

use super::LIB_MAP;
use super::LIBS;

struct Node {
    references: Vec<&'static str>,
}

fn read_default_libs() -> HashMap<&'static str, Node> {
    let dir = project_root::get_project_root()
        .unwrap()
        .join("crates/libs/src/declared_file");
    let map = LIBS
        .iter()
        .map(|&lib| {
            assert!(lib.starts_with("lib.") && lib.ends_with(".d.ts"));
            let path = dir.join(lib);
            let content = std::fs::read_to_string(&path)
                .unwrap_or_else(|_| panic!("Failed to read default lib file: {}", path.display()));
            let references = content
                .lines()
                .filter_map(|line| {
                    if line.starts_with("/// <reference lib=\"") {
                        let lib_name = line
                            .trim_start_matches("/// <reference lib=\"")
                            .trim_end_matches("\" />");
                        LIB_MAP.get(&lib_name).copied()
                    } else {
                        None
                    }
                })
                .collect::<Vec<_>>();
            (lib, Node { references })
        })
        .collect::<HashMap<_, _>>();
    assert!(map.len() == LIBS.len());
    map
}

fn toposort(map: &HashMap<&'static str, Node>) -> Vec<&'static str> {
    let mut sorted = Vec::new();
    let mut visited = HashMap::new();

    fn visit<'a>(
        lib: &'static str,
        map: &'a HashMap<&'static str, Node>,
        visited: &mut HashMap<&'a str, bool>,
        sorted: &mut Vec<&'static str>,
    ) {
        if let Some(&true) = visited.get(lib) {
            return;
        }
        if let Some(&false) = visited.get(lib) {
            panic!("Circular dependency detected for lib: {}", lib);
        }
        visited.insert(lib, false);
        if let Some(node) = map.get(lib) {
            for &reference in &node.references {
                visit(reference, map, visited, sorted);
            }
        }
        visited.insert(lib, true);
        sorted.push(lib);
    }

    for &lib in LIBS.iter() {
        visit(lib, map, &mut visited, &mut sorted);
    }

    sorted
}

fn build_bitset(map: HashMap<&'static str, Node>) -> Vec<u128> {
    assert!(map.len() == LIBS.len());
    let mut result = vec![0; map.len()];
    let sorted_libs = toposort(&map);
    for lib in sorted_libs {
        let i = LIBS.iter().position(|&l| l == lib).unwrap();
        let node = &map[lib];
        for &reference in &node.references {
            let j = LIBS
                .iter()
                .position(|&l| l == reference)
                .unwrap_or_else(|| panic!("Reference {} not found in DEFAULT_LIBS", reference));
            result[i] |= result[j];
        }
        result[i] |= 1 << i; // include self
    }
    result
}

#[allow(unused)]
pub(super) fn bitset() -> Vec<u128> {
    let map = read_default_libs();
    build_bitset(map)
}
