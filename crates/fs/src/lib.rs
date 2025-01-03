mod memory;
mod real;

pub use memory::MemoryFS;
pub use real::LocalFS;

pub trait FileSystem {}

use std::fs::File;
use std::io;

pub fn read_file_with_encoding(file: &std::path::Path) -> io::Result<String> {
    let file = File::open(file)?;
    let size = file.metadata().map(|m| m.len() as usize).ok();
    read_content_with_encoding(file, size)
}

pub fn read_content_with_encoding(
    mut read: impl std::io::Read,
    size: Option<usize>,
) -> Result<String, std::io::Error> {
    let mut buffer = Vec::new();
    buffer.try_reserve_exact(size.unwrap_or(1024))?;
    read.read_to_end(&mut buffer)?;

    let len = buffer.len();
    if len >= 2 && buffer[0] == 0xFE && buffer[1] == 0xFF {
        // Big endian UTF-16 byte order mark detected. Since big endian is not supported by Rust,
        // flip all byte pairs and treat as little endian.
        let mut i = 0;
        while i + 1 < len {
            buffer.swap(i, i + 1);
            i += 2;
        }
        let utf16_buffer: &[u16] =
            unsafe { std::slice::from_raw_parts(buffer.as_ptr() as *const u16, len / 2) };
        Ok(String::from_utf16_lossy(&utf16_buffer[1..]))
    } else if len >= 2 && buffer[0] == 0xFF && buffer[1] == 0xFE {
        // Little endian UTF-16 byte order mark detected
        let utf16_buffer: &[u16] =
            unsafe { std::slice::from_raw_parts(buffer.as_ptr() as *const u16, (len - 2) / 2) };
        Ok(String::from_utf16_lossy(utf16_buffer))
    } else if len >= 3 && buffer[0] == 0xEF && buffer[1] == 0xBB && buffer[2] == 0xBF {
        // UTF-8 byte order mark detected
        Ok(String::from_utf8_lossy(&buffer[3..]).to_string())
    } else {
        // Default is UTF-8 with no byte order mark
        Ok(String::from_utf8_lossy(&buffer).to_string())
    }
}
