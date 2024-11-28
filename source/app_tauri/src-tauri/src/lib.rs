use {
    indexmap::IndexMap,
    serde_json::value::RawValue,
    std::{
        fs::{
            read,
            write,
        },
        os::unix::ffi::OsStrExt,
        path::{
            Path,
            PathBuf,
        },
        sync::{
            LazyLock,
            Mutex,
        },
    },
    tauri::Manager,
    tauri_plugin_dialog::DialogExt,
    tokio::sync::oneshot,
};

pub struct State {
    pub file_path: PathBuf,
    pub initial_data: Option<Box<Vec<IndexMap<String, Box<RawValue>>>>>,
}

pub static STATE: LazyLock<Mutex<Option<State>>> = LazyLock::new(|| Mutex::new(None));

fn format_title(file_path: &Path) -> String {
    return format!("{} - Sheetman", file_path.file_name().unwrap_or(file_path.as_os_str()).to_string_lossy());
}

fn update_current_file_path(app_handle: &tauri::AppHandle, new: PathBuf) {
    let mut state = STATE.lock().unwrap();
    let file_path = &mut state.as_mut().unwrap().file_path;
    if let Some(w) = app_handle.webview_windows().values().next() {
        w.set_title(&format_title(&new)).unwrap();
    }
    *file_path = new;
}

pub fn default_data() -> Vec<IndexMap<String, Box<RawValue>>> {
    return vec![
        [
            (
                "".to_string(),
                RawValue::from_string(
                    serde_json::to_string(&serde_json::Value::String("".to_string())).unwrap(),
                ).unwrap(),
            ),
        ]
            .into_iter()
            .collect()
    ];
}

#[tauri::command]
async fn command_open(
    app_handle: tauri::AppHandle,
) -> Result<Option<Vec<IndexMap<String, Box<RawValue>>>>, String> {
    let (res_tx, res_rx) = oneshot::channel();
    app_handle
        .dialog()
        .file()
        .add_filter("JSV", &["jsv"])
        .pick_file(move |file_path| res_tx.send(file_path).unwrap());
    let Some(file_path) = res_rx.await.unwrap() else {
        return Ok(None);
    };
    let file_path = file_path.into_path().unwrap();
    let rows;
    let raw_data =
        read(&file_path).map_err(|e| format!("Failed to read file [{}]: {}", file_path.to_string_lossy(), e))?;
    match file_path.extension().map(|x| x.as_bytes()) {
        Some(b"jsv") => {
            rows =
                serde_json::from_slice::<Vec<IndexMap<String, Box<RawValue>>>>(
                    &raw_data,
                ).map_err(|e| format!("Failed to parse file json into expected structure: {}", e))?;
        },
        _ => {
            return Err(
                format!("Missing or unknown extension for specified filename {}", file_path.to_string_lossy()).into(),
            );
        },
    }
    update_current_file_path(&app_handle, file_path);
    return Ok(Some(rows));
}

#[tauri::command]
fn command_take_initial_data() -> Vec<IndexMap<String, Box<RawValue>>> {
    return *STATE.lock().unwrap().as_mut().unwrap().initial_data.take().unwrap();
}

fn save(filename: &PathBuf, data: Vec<IndexMap<String, Box<RawValue>>>) -> Result<(), String> {
    write(
        &filename,
        serde_json::to_vec_pretty(&data).unwrap(),
    ).map_err(|e| format!("Error saving file at [{}]: {}", filename.to_string_lossy(), e))?;
    return Ok(());
}

#[tauri::command]
fn command_save(data: Vec<IndexMap<String, Box<RawValue>>>) -> Result<(), String> {
    let state = STATE.lock().unwrap();
    let filename = &state.as_ref().unwrap().file_path;
    save(&filename, data)?;
    return Ok(());
}

#[tauri::command]
fn command_save_as(
    app_handle: tauri::AppHandle,
    data: Vec<IndexMap<String, Box<RawValue>>>,
) -> Result<(), String> {
    app_handle.dialog().file().add_filter("JSV", &["jsv"]).save_file(move |file_path| {
        if let Some(file_path) = file_path {
            let file_path = file_path.into_path().unwrap();
            match save(&file_path, data) {
                Ok(_) => { },
                Err(e) => {
                    eprintln!("Error saving file at [{}]: {}", file_path.to_string_lossy(), e);
                },
            }
            update_current_file_path(&app_handle, file_path);
        }
    });
    return Ok(());
}

#[cfg_attr(mobile, tauri::mobile_entry_point)]
pub fn run() {
    tauri::Builder::default()
        .plugin(tauri_plugin_shell::init())
        .plugin(tauri_plugin_dialog::init())
        .invoke_handler(
            tauri::generate_handler![command_take_initial_data, command_open, command_save, command_save_as],
        )
        .setup(|app| {
            let mut state = STATE.lock().unwrap();
            if state.is_none() {
                *state = Some(State {
                    file_path: PathBuf::from("sheet.jsv"),
                    initial_data: Some(Box::new(default_data())),
                });
            }
            if let Some(w) = app.webview_windows().values().next() {
                w.set_title(&format_title(&state.as_ref().unwrap().file_path)).unwrap();
            }
            Ok(())
        })
        .run(tauri::generate_context!())
        .expect("error while running tauri application");
}
