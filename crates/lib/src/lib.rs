macro_rules! lib {
    ( $($lib_name: literal),* $(,)? ) => {
      $(
        ($lib_name, {
          let concat = concat!("lib.", $lib_name, ".d.ts");
          let _ = include_str!(concat!("./declared_file/", concat!("lib.", $lib_name, ".d.ts")));
          concat
        })
      )*
    };
}
pub const LIB_ENTIRES: [(&str, &str); 1] = [lib!("es5")];
