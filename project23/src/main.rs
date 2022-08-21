// MariaDB [test]> describe student;
// +-------------+--------------+------+-----+---------+----------------+
// | Field       | Type         | Null | Key | Default | Extra          |
// +-------------+--------------+------+-----+---------+----------------+
// | id          | int(11)      | NO   | PRI | NULL    | auto_increment |
// | name        | varchar(128) | NO   |     | NULL    |                |
// | age         | int(11)      | NO   |     | NULL    |                |
// | id_card     | varchar(128) | NO   |     | NULL    |                |
// | last_update | date         | NO   |     | NULL    |                |
// +-------------+--------------+------+-----+---------+----------------+

#![allow(unused)]

use mysql::prelude::*;
use mysql::*;

fn main() {
    let url = "mysql://<username>:<password>@localhost:3306/<database>";
    let pool = Pool::new(url).unwrap();
    let mut conn = pool.get_conn().unwrap();

    //INSERT
    conn.exec_drop(
        "INSERT INTO student (name, age, id_card, last_update) VALUES (:name, :age, :id_card, :last_update)",
        params! {
            "name" => "Hello",
            "age" => 18,
            "id_card" => "123456789xyz",
            "last_update" => "2022-12-31",
        },
    ).unwrap();
    let last_inserted_id = conn.last_insert_id();
    println!("Last generated key: {}", last_inserted_id);

    //LIST
    let res: Vec<(i32, String, i32, String, String)> = conn.query("SELECT * FROM student").unwrap();
    for r in res {
        println!("{}, {},{},{}, {:?}", r.0, r.1, r.2, r.3, r.4);
    }

    //UPDATE
    let stmt = conn
        .prep("UPDATE student SET name=:name WHERE id=:last_inserted_id")
        .unwrap();
    conn.exec_drop(
        &stmt,
        params! {
            "name" => "World",
            "last_inserted_id" => last_inserted_id,
        },
    )
    .unwrap();

    //READ
    let res_read: Option<String> = conn.query_first("SELECT name FROM student ORDER BY id desc").unwrap();
    let updated_name = res_read.unwrap();
    println!("{}", updated_name);
}