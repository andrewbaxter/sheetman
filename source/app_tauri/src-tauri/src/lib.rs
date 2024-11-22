use {
    std::{
        fs::{
            read,
            write,
        },
        os::unix::ffi::OsStrExt,
        path::PathBuf,
        sync::{
            LazyLock,
            Mutex,
        },
    },
    tauri_plugin_dialog::DialogExt,
    tokio::sync::oneshot,
};

pub struct State {
    pub filename: PathBuf,
    pub initial_data: Option<Box<Vec<serde_json::Value>>>,
}

pub static STATE: LazyLock<Mutex<Option<State>>> = LazyLock::new(|| Mutex::new(None));

pub fn default_data() -> Vec<serde_json::Value> {
    return vec![
        serde_json::Value::Object(
            [("".to_string(), serde_json::Value::String("".to_string()))].into_iter().collect(),
        )
    ];
}

#[tauri::command]
async fn command_open(app_handle: tauri::AppHandle) -> Result<Option<Vec<serde_json::Value>>, String> {
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
                serde_json::from_slice::<Vec<serde_json::Value>>(
                    &raw_data,
                ).map_err(|e| format!("Failed to parse file json into expected structure: {}", e))?;
        },
        _ => {
            return Err(
                format!("Missing or unknown extension for specified filename {}", file_path.to_string_lossy()).into(),
            );
        },
    }
    let mut state = STATE.lock().unwrap();
    let filename = &mut state.as_mut().unwrap().filename;
    *filename = file_path;
    return Ok(Some(rows));
}

#[tauri::command]
fn command_take_initial_data() -> Vec<serde_json::Value> {
    return *STATE.lock().unwrap().as_mut().unwrap().initial_data.take().unwrap();
}

fn save(filename: &PathBuf, data: Vec<serde_json::Value>) -> Result<(), String> {
    write(
        &filename,
        serde_json::to_vec_pretty(&data).unwrap(),
    ).map_err(|e| format!("Error saving file at [{}]: {}", filename.to_string_lossy(), e))?;
    return Ok(());
}

#[tauri::command]
fn command_save(data: Vec<serde_json::Value>) -> Result<(), String> {
    let state = STATE.lock().unwrap();
    let filename = &state.as_ref().unwrap().filename;
    save(&filename, data)?;
    return Ok(());
}

#[tauri::command]
fn command_save_as(app_handle: tauri::AppHandle, data: Vec<serde_json::Value>) -> Result<(), String> {
    app_handle.dialog().file().add_filter("JSV", &["jsv"]).save_file(|file_path| {
        if let Some(file_path) = file_path {
            let mut state = STATE.lock().unwrap();
            let filename = &mut state.as_mut().unwrap().filename;
            *filename = file_path.into_path().unwrap();
            match save(&filename, data) {
                Ok(_) => { },
                Err(e) => {
                    eprintln!("Error saving file at [{}]: {}", filename.to_string_lossy(), e);
                },
            }
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
        .setup(|_app| {
            let mut state = STATE.lock().unwrap();
            if state.is_none() {
                *state = Some(State {
                    filename: PathBuf::from("sheet.jsv"),
                    initial_data: Some(Box::new(default_data())),
                });
            }
            Ok(())
        })
        .run(tauri::generate_context!())
        .expect("error while running tauri application");
}
