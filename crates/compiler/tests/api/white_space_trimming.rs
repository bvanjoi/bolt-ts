use crate::compile_single_input;

#[test]
fn white_space_trimming() {
    const CODE: &str = r#"
if (true) {     
  //    
   /*err*/}
"#
    .trim_ascii();
    let mut db = compile_single_input(CODE);
    db.goto_marker("err");
    db.edit_insert("\n");
    db.verify_current_file_content_is(
        r#"
if (true) {     
  //    
   
}
"#
        .trim_ascii(),
    );
}
