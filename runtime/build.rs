pub fn main() {
    cc::Build::new()
    .include("bdwgc/include")
    .file("cutils.c")
    .file("libregexp.c")
    .file("libunicode.c")
    .file("c_api.c")
    .compile("c_api");
}
