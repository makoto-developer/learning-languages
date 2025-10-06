use std::io;

fn main() {
    println!("Hello, world!");
    let mut guess = String::new();
    io::stdin().read_line(&mut guess)
        .expect("Failed to read line");
    println!("You guessed: {}", guess);
    let x = 5;
    let y = 3;
    println!("x = {}, y = {}", x, y);

}
