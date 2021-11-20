const express = require('express');
const app = express();
const bodyParser = require('body-parser');
const fileupload = require("express-fileupload");
const cors = require('cors');
const axios = require('axios');
const fs = require('fs');
const FormData = require('form-data');
const PORT = 3001

const PINATA_URL = `https://api.pinata.cloud/pinning/pinFileToIPFS`;
const PINATA_GATEWAY = `https://gateway.pinata.cloud/ipfs/`;
const PINATA_API_KEY = 'aaabbbccc'
const PINATA_SECRET_API_KEY = 'xxxyyyzzz'

app.use(cors());
app.use(fileupload({
    useTempFiles: true,  //In-memory file do not seem to work on Pinata API
    tempFileDir: '/tmp/' //Likely some field missing/different from temp file.
}));
app.use(bodyParser.json());

//Root routing
app.get('/test', async (req, res) => {
    res.json({
        'message': "Hello World"
    });
});

app.post('/upload', async (req, res) => {
    //REFERENCE: https://medium.com/pinata/how-to-pin-to-ipfs-effortlessly-ba3437b33885
    const file = req.files.file;
    let data = new FormData();
    data.append('file', fs.createReadStream(file.tempFilePath));
    axios.post(PINATA_URL,
        data,
        {
            headers: {
                'Content-Type': `multipart/form-data; boundary=${data._boundary}`,
                'pinata_api_key': PINATA_API_KEY,
                'pinata_secret_api_key': PINATA_SECRET_API_KEY
            }
        }
    ).then(function (response) {
        res.json({ ipfshash: response.data.IpfsHash, gatewayurl: PINATA_GATEWAY + response.data.IpfsHash });
    }).catch(function (error) {
        res.status(400).send({ error: "Something is just wrong!" });
    });
});

console.log("START BACKEND SERVER");
app.listen(PORT);
