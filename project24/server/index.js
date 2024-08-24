const express = require("express");
const axios = require('axios');
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

app.get("/api/posts/", async (req, res) => {
    try {
        const response = await axios.get('https://jsonplaceholder.typicode.com/posts');
        return res.json(response.data)
    } catch (error) {
        console.error('Error calling API:', error);
        return res.status(500).send('Error')
    }
});

app.get("/api/post/:id", async (req, res) => {
    try {
        const id = req.params.id;
        const response = await axios.get('https://jsonplaceholder.typicode.com/posts/' + id);
        return res.json(response.data)
    } catch (error) {
        console.error(error);
        return res.status(500).send('Error')
    }
});

app.post("/api/post", async (req, res) => {
    const randomNum = Math.floor(Math.random() * 100);
    try {
        const response = await axios.post('https://jsonplaceholder.typicode.com/posts', {
            title: 'Title ' + randomNum,
            body: 'Body ' + randomNum,
            userId: randomNum
        });
        return res.json(response.data)
    } catch (error) {
        console.error(error);
        return res.status(500).send('Error')
    }
});

app.put("/api/post/:id", async (req, res) => {
    const randomNum = Math.floor(Math.random() * 100);
    try {
        const id = req.params.id;
        const response = await axios.put('https://jsonplaceholder.typicode.com/posts/' + id, {
            id: id,
            title: 'Title ' + randomNum,
            body: 'Body ' + randomNum,
            userId: randomNum
        });
        return res.json(response.data)
    } catch (error) {
        console.error(error);
        return res.status(500).send('Error')
    }
});

app.delete("/api/post/:id", async (req, res) => {
    try {
        const id = req.params.id;
        const response = await axios.delete('https://jsonplaceholder.typicode.com/posts/' + id);
        return res.json(response.data)
    } catch (error) {
        console.error(error);
        return res.status(500).send('Error')
    }
});
app.listen(PORT, () => console.log(`Listening on port ${PORT}`));