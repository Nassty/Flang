mod parser;
mod span;
mod type_checker;
use parser::lang::parser;
use span::Span;

fn main() {
    let file_contents = std::fs::read_to_string("test.txt").unwrap();
    let parsed = match parser(&file_contents) {
        Ok(parsed) => parsed,
        Err(e) => {
            println!("Error({}): {}", e.location, e.expected);
            return;
        }
    };
    println!("{:#?}", parsed);
    let mut type_checker = type_checker::TypeChecker::new();
    match type_checker.check(&parsed) {
        Ok(()) => {}
        Err(e) => {
            println!("Error({:#?})", e);
            return;
        }
    }
}
