const express = require("express");
const axios = require('axios');
const bodyParser = require('body-parser')
const { PrismaClient } = require('@prisma/client')
const prisma = new PrismaClient()
require('dotenv').config();
const PRISMA_FLAG = process.env.PRISMA_FLAG === "TRUE"

const app = express();
app.use(bodyParser.json());
app.use(bodyParser.urlencoded({ extended: true }));

const PORT = 3001;

app.get("/", (req, res) => {
    res.send("Hello World from Server!");
});

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
        if (PRISMA_FLAG) {
            prisma.post.count().then(nCount => { console.log(nCount) })
        }
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
        console.log("Getting post:" + JSON.stringify(response.data))
        return res.json(response.data)
    } catch (error) {
        console.error(error);
        return res.status(500).send('Error')
    }
});

app.post("/api/post", async (req, res) => {
    const newTitle = req.body.title;
    const newBody = req.body.body;
    try {
        const response = await axios.post('https://jsonplaceholder.typicode.com/posts', {
            title: newTitle,
            body: newBody,
            userId: Math.floor(Math.random() * 100)
        });
        console.log("Creating post:" + JSON.stringify(response.data))
        if (PRISMA_FLAG) {
            const newPost = await prisma.post.create({
                data: {
                    title: newTitle,
                    body: newBody,
                    userId: Math.floor(Math.random() * 100)
                }
            })
            console.log(newPost);
        }
        return res.json(response.data)
    } catch (error) {
        console.error(error);
        return res.status(500).send('Error')
    }
});

app.put("/api/post/:id", async (req, res) => {
    const newTitle = req.body.title;
    const newBody = req.body.body;
    try {
        const id = req.params.id;
        const response = await axios.put('https://jsonplaceholder.typicode.com/posts/' + id, {
            id: id,
            title: newTitle,
            body: newBody,
            userId: Math.floor(Math.random() * 100)
        });
        console.log("Updating post:" + JSON.stringify(response.data))
        if (PRISMA_FLAG) {
            const oldPost = await prisma.post.findFirst({
                orderBy: {
                    id: 'asc',
                },
                take: 1
            });
            if (!oldPost) {
                console.log("No existing post to mock update")
            } else {
                const updatePost = await prisma.post.update({
                    where: { id: oldPost.id },
                    data: {
                        title: newTitle,
                        body: newBody,
                    }
                }
                )
                console.log(updatePost);
            }
        }
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
        console.log("Deleting post:" + JSON.stringify(response.data))
        if (PRISMA_FLAG) {
            const oldPost = await prisma.post.findFirst({
                orderBy: {
                    id: 'asc',
                },
                take: 1
            });
            if (!oldPost) {
                console.log("No existing post to mock delete")
            } else {
                const deletePost = await prisma.post.delete({
                    where: { id: oldPost.id },
                }
                )
                console.log(deletePost);
            }
        }
        return res.json(response.data)
    } catch (error) {
        console.error(error);
        return res.status(500).send('Error')
    }
});
app.listen(PORT, () => console.log(`Listening on port ${PORT}`));