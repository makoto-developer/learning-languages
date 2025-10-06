# CargoでシングルスレッドWebサーバを作成

## note

create rust app command 
```shell
cargo new single_thread_server --bin
```

## Starting

starting rust web server.
```shell
cargo run
```

## URL

|Path|Note|
|:---|:---|
|/|Top page|
|/sleep| access /, wait 5 sec|
|other|404 status|

# Reference
- https://doc.rust-jp.rs/book-ja/ch20-01-single-threaded.html


