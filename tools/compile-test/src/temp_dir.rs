use rand::Rng;

#[track_caller]
pub fn tempdir(prefix: &str) -> TempDir {
    assert!(!prefix.is_empty());
    let p = std::env::temp_dir();
    let mut r = rand::rng();
    let path = format!("{}-{}", prefix, r.next_u32());
    let ret = p.join(&path);
    std::fs::create_dir(&ret).unwrap();
    TempDir(ret)
}

pub struct TempDir(std::path::PathBuf);

impl TempDir {
    pub fn join(&self, path: &str) -> std::path::PathBuf {
        let TempDir(ref p) = *self;
        p.join(path)
    }

    pub fn path(&self) -> &std::path::Path {
        let TempDir(ref p) = *self;
        p
    }
}
