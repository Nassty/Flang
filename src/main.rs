mod parser;
mod span;
mod type_checker;
use span::Span;
use parser::lang::parser;

fn main() {
    let file_contents = std::fs::read_to_string("test.txt").unwrap();
    let parsed = match parser(&file_contents) {
        Ok(parsed) => parsed,
        Err(e) => {
            println!("Error: {}", e);
            return;
        }
    };
    let mut type_checker = type_checker::TypeChecker::new();
    println!("{:#?}", type_checker.check(&parsed));
}
