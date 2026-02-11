use std::collections::HashMap;

use super::{TempDir, tempdir};

fn is_symlink_tag(s: &str) -> bool {
    s.starts_with("symlink:")
}

pub fn build_temp_files(map: serde_json::Value) -> TempDir {
    let map: HashMap<String, String> = serde_json::from_value(map).unwrap();
    assert!(map.keys().all(|v| v.starts_with("./")));
    let base = tempdir("bolt-ts");

    for (k, v) in &map {
        if is_symlink_tag(v) {
            continue;
        }
        let p = base.join(k);
        if p.as_os_str().as_encoded_bytes().ends_with(&[b'/']) {
            assert!(v.is_empty());
            std::fs::create_dir_all(&p).unwrap();
        } else {
            std::fs::create_dir_all(p.parent().unwrap()).unwrap();
            std::fs::File::create(&p)
                .unwrap_or_else(|_| panic!("Failed to create file: {}", p.display()));
            std::fs::write(&p, v).unwrap();
        }
    }
    for (k, v) in &map {
        if !is_symlink_tag(v) {
            continue;
        }
        let target = v.strip_prefix("symlink:").unwrap();
        let p = base.join(k);
        symlink::symlink_auto(target, p).unwrap();
    }
    base
}

#[cfg(test)]
fn realpath(p: &std::path::Path) -> std::path::PathBuf {
    std::fs::canonicalize(p).unwrap()
}

#[test]
fn test_build_tmpdir() {
    let tmp = build_temp_files(serde_json::json!({
      "./a": "",
      "./b": "symlink:./",
      "./c": "symlink:./a",
      "./d/e/f/g": ""
    }));
    let tmp_a = tmp.join("./a");
    assert!(tmp_a.is_file());
    let tmp_b = tmp.join("./b");
    assert!(tmp_b.is_symlink());
    assert!(tmp_b.is_dir());
    assert!(realpath(&tmp_b) == realpath(tmp.path()));
    assert!(tmp_b.join("./a").is_file());
    assert!(tmp_b.join("./b").is_dir());
    let tmp_c = tmp.join("./c");
    assert!(tmp_c.is_symlink());
    assert!(realpath(&tmp_c) == realpath(&tmp_a));
    let tmp_g = tmp.join("./d/e/f/g");
    assert!(tmp_g.is_file());
}
