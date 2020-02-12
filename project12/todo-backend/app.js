const express = require('express');
const app = express();
const bodyParser = require('body-parser');
const mongoose = require('mongoose');
require('dotenv/config');

app.use(bodyParser.json());

const taskRoute = require('./routes/task');
app.use('/task', taskRoute);

app.get('/', (req, res) => {
    res.send('Invalid');
})

app.listen(3001);

// Connect to db
mongoose.connect(
    process.env.DB_CONNECTION,
    { useNewUrlParser: true, useUnifiedTopology: true },
    () => {
        console.log("Connected to DB");
    }
);