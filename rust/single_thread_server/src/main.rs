use std::io::prelude::*;
use std::net::TcpStream;
use std::net::TcpListener;
use std::fs::File;

fn main() {
    print!("boot rust server...\n");
    start_rust_server();
}

fn start_rust_server() {
    let server_port = 29000;
    let server_url = format!("127.0.0.1:{}", &server_port);
    let listener = TcpListener::bind(&server_url).unwrap();
    println!("Starting Rust Web server URL: {}", &server_url);

    for _stream in listener.incoming() {
        println!("Connection established!");
        let stream = _stream.unwrap();
        handle_request(stream)

    }
}

fn handle_request(mut stream: TcpStream) {
    let mut buffer = [0; 1024];
    stream.read(&mut buffer).unwrap();

    let get = b"GET / HTTP/1.1\r\n";

    // go 404 page if server recieve beside get method.
    let (status_line, filename) = if buffer.starts_with(get) {
        ("HTTP/1.1 200 OK\r\n\r\n", "src/index.html")
    } else {
        ("HTTP/1.1 404 NOT FOUND\r\n\r\n", "src/404.html")
    };

    let mut file = File::open(filename).unwrap();
    let mut contents = String::new();
    file.read_to_string(&mut contents).unwrap();
    let response = format!("{}{}", status_line, contents);
    stream.write(response.as_bytes()).unwrap();
    stream.flush().unwrap();
}
