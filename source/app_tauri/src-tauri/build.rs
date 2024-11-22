use {
    resvg::{
        tiny_skia,
        usvg,
    },
    std::{
        env,
        fs::{
            create_dir_all,
            read,
            remove_dir_all,
        },
        path::PathBuf,
    },
};

fn main() {
    println!("cargo:rerun-if-changed=build.rs");

    // Tauri doing tauri things
    tauri_build::build();

    // Generate icons
    let root_path = PathBuf::from(env::var("CARGO_MANIFEST_DIR").unwrap()).canonicalize().unwrap();
    let out_icons_path = root_path.join("dist_icons");
    _ = remove_dir_all(&out_icons_path);
    create_dir_all(&out_icons_path).unwrap();
    let svg_path = root_path.join("../../wasm/prestatic/logo.svg");
    println!("cargo:rerun-if-changed={}", svg_path.to_string_lossy());
    let svg = usvg::Tree::from_data(&read(&svg_path).unwrap(), &{
        let mut opt = usvg::Options::default();
        opt.resources_dir = svg_path.parent().map(|p| p.to_path_buf());
        opt
    }).unwrap();
    let svg_size = svg.size();
    for (x, y, stem) in [(32, 32, "32x32"), (128, 128, "128x128"), (256, 256, "128x128@2x")] {
        let mut pixmap = tiny_skia::Pixmap::new(x, y).unwrap();
        resvg::render(
            &svg,
            tiny_skia::Transform::from_scale(x as f32 / svg_size.width(), y as f32 / svg_size.height()),
            &mut pixmap.as_mut(),
        );
        pixmap.save_png(&out_icons_path.join(format!("{}.png", stem))).unwrap();
    }
}
