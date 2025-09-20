use crate::compile_single_input;

#[test]
fn white_space_trimming() {
    // From `github.com/microsoft/TypeScript/blob/v5.9.2/tests/cases/fourslash/whiteSpaceTrimming.ts`, Apache-2.0 License
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

#[test]
fn white_space_trimming2() {
    // From `github.com/microsoft/TypeScript/blob/v5.9.2/tests/cases/fourslash/whiteSpaceTrimming2.ts`, Apache-2.0 License
    const CODE: &str = r#"
let noSubTemplate = `/*    /*1*/`;
let templateHead = `/*    /*2*/${1 + 2}`;
let templateMiddle = `/*    ${1 + 2    /*3*/}`;
let templateTail = `/*    ${1 + 2}    /*4*/`;
"#
    .trim_ascii();

    let mut db = compile_single_input(CODE);
    db.goto_marker("1");
    db.edit_insert("\n");
    db.goto_marker("2");
    db.edit_insert("\n");
    db.goto_marker("3");
    db.edit_insert("\n");
    db.goto_marker("4");
    db.edit_insert("\n");

    db.verify_current_file_content_is(
        r#"
let noSubTemplate = `/*    
`;
let templateHead = `/*    
${1 + 2}`;
let templateMiddle = `/*    ${1 + 2    
}`;
let templateTail = `/*    ${1 + 2}    
`;
"#
        .trim_ascii(),
    );
}

#[test]
fn white_space_trimming3() {
    // From `github.com/microsoft/TypeScript/blob/v5.9.2/tests/cases/fourslash/whiteSpaceTrimming3.ts`, Apache-2.0 License
    const CODE: &str = r#"
let t = "foo \
bar     \   
"/*1*/
"#
    .trim_ascii();

    let mut db = compile_single_input(CODE);
    db.goto_marker("1");
    db.edit_insert(";");
    db.verify_current_file_content_is(
        r#"
let t = "foo \
bar     \   
";
        "#
        .trim_ascii(),
    );
}
