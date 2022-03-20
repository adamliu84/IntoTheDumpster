const express = require('express');
const app = express();
const bodyParser = require('body-parser');
const cors = require('cors');
const PORT = 3001;
const serverenv = require('./serverenv.json');
const stripe = require("stripe")(serverenv.SECRET_KEY);


app.use(cors());
app.use(bodyParser.json());
//Root routing
app.get('/test', async (req, res) => {
    res.json({
        'message': "Hello World",
    });
});

app.post('/test', async (req, res) => {
    const { amount } = req.body;

    // Create a PaymentIntent with the order amount and currency
    const paymentIntent = await stripe.paymentIntents.create({
        amount: amount * 100,
        currency: "sgd",
        automatic_payment_methods: {
            enabled: true,
        },
    });
    console.log(paymentIntent.client_secret);
    res.send({
        clientSecret: paymentIntent.client_secret,
    });
});

console.log("START BACKEND SERVER");
app.listen(PORT);