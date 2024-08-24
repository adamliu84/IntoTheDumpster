const express = require("express");
// const users = require("./users.json");

const app = express();

const PORT = 3001;

app.get("/api/users", (req, res) => {
    return res.json({
        users:
            [
                {                    
                    "id": 1,
                    "desc": "quia et suscipit\nsuscipit recusandae consequuntur expedita et cum\nreprehenderit molestiae ut ut quas totam\nnostrum rerum est autem sunt rem eveniet architecto"
                },
                {                 
                    "id": 2,                    
                    "desc": "est rerum tempore vitae\nsequi sint nihil reprehenderit dolor beatae ea dolores neque\nfugiat blanditiis voluptate porro vel nihil molestiae ut reiciendis\nqui aperiam non debitis possimus qui neque nisi nulla"
                }
            ]
    });
});

app.listen(PORT, () => console.log(`Listening on port ${PORT}`));