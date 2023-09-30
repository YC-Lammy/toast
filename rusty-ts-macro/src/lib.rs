use std::{str::FromStr, hash::{Hash, Hasher}};

use proc_macro::TokenStream;

#[proc_macro]
pub fn tssting(input:proc_macro::TokenStream) -> proc_macro::TokenStream{

    let s = syn::parse_macro_input!(input as syn::LitStr);

    let s = s.value();

    let mut buffer = String::new();

    for i in s.as_bytes(){
        buffer.push(',');
        buffer.push_str(i.to_string().as_str());
    };
    
    let splited:[u8;8] = unsafe{std::mem::transmute(s.len() as u64)};
    
    let mut new_buf = "JSString(unsafe{static O:&[u8]=&[".to_string();
    new_buf.push_str(splited.iter().map(|v|v.to_string()).collect::<Vec<String>>().join(",").as_str());
    new_buf.push_str(&buffer);
    new_buf.push_str("];core::mem::transmute(O.as_ptr())})");

    return TokenStream::from_str(&new_buf).unwrap();
}

#[proc_macro]
pub fn hash(input:proc_macro::TokenStream) -> TokenStream{
    let s = syn::parse_macro_input!(input as syn::LitStr);

    let result:u64 = cityhasher::hash(s.value().as_bytes());
    return TokenStream::from_str(&result.to_string()).unwrap()
}