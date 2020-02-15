const express = require('express');
const router = express.Router();
const jwt = require('jsonwebtoken');

const TEMP_USERNAME = "tester";
const TEMP_USERPASSWORD = "password";

router.all('/logout', async (req, res) => {
    res.send("Logging out");
})

router.post('/login', async (req, res) => {
    try {
        const { username, password } = req.body;
        if (username === TEMP_USERNAME && password === TEMP_USERPASSWORD) {
            const access_token = generateAccessToken({ user: username });
            res.json({ access_token: access_token });
        } else {
            res.sendStatus(403);
        }
    }
    catch (err) {
        res.send(err.message);
    }
})

function generateAccessToken(user) {
    return jwt.sign(user, process.env.ACCESS_TOKEN_SECRET);
}

global.authenticateToken = function (req, res, next) {
    try {
        const authHeader = req.headers['authorization'];
        const token = authHeader && authHeader.split(' ')[1]
        jwt.verify(token, process.env.ACCESS_TOKEN_SECRET, (err, jwtdata) => {
            if (err) {
                console.log(err.message);
                return res.sendStatus(403);
            }
            //Draft user checking logic
            if (jwtdata.user !== TEMP_USERNAME) {
                return res.sendStatus(401);
            }
            req.user = jwtdata.user;
            next();
        })
    }
    catch (err) {
        console.log(err.message);
        return res.sendStatus(403);
    }
}

module.exports = router;