
use compile_test::run_tests::run;
use rts::eval;

#[test]
fn run_tests() {
    let project_root = project_root::get_project_root().unwrap();
    let sub = "tests/cases/compiler";
    let cases = compile_test::fixtures(&project_root, sub);
    let runner = |case: &std::path::Path| {
        let input = std::fs::read_to_string(case).unwrap();
        eval(&input);
        Ok(())
    };
    for case in cases.into_iter() {
        run(&case.path(), runner);
    }
}
