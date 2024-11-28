// Prevents additional console window on Windows in release, DO NOT REMOVE!!
#![cfg_attr(not(debug_assertions), windows_subsystem = "windows")]

use {
    aargvark::Aargvark,
    indexmap::IndexMap,
    serde_json::value::RawValue,
    std::{
        fs::read,
        io::ErrorKind,
        os::unix::ffi::OsStrExt,
        path::PathBuf,
    },
    tauri_lib::{
        default_data,
        State,
    },
};

#[derive(Aargvark)]
struct Args {
    file: PathBuf,
}

fn main() -> Result<(), String> {
    {
        let args = aargvark::vark::<Args>();
        let initial_data;
        match read(&args.file) {
            Err(e) => {
                if e.kind() == ErrorKind::NotFound {
                    initial_data = default_data();
                } else {
                    return Err(format!("Failed to read file [{}]: {}", args.file.to_string_lossy(), e));
                }
            },
            Ok(raw_data) => {
                match args.file.extension().map(|x| x.as_bytes()) {
                    Some(b"jsv") => {
                        initial_data =
                            serde_json::from_slice::<Vec<IndexMap<String, Box<RawValue>>>>(
                                &raw_data,
                            ).map_err(|e| format!("Failed to parse file json into expected structure: {}", e))?;
                    },
                    _ => {
                        return Err(
                            format!(
                                "Missing or unknown extension for specified filename {}",
                                args.file.to_string_lossy()
                            ).into(),
                        );
                    },
                }
            },
        };
        *tauri_lib::STATE.lock().unwrap() = Some(State {
            file_path: args.file,
            initial_data: Some(Box::new(initial_data)),
        });
    }
    tauri_lib::run();
    return Ok(());
}
