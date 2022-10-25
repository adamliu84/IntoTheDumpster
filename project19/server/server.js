const express = require('express')
const app = express()
const bodyParser = require('body-parser')
const cors = require('cors')
const PORT = 3001
const serverenv = require('./serverenv.json')
const stripe = require('stripe')(serverenv.SECRET_KEY)

app.use(cors())
app.use(bodyParser.json())
//Root routing
app.get('/test', async (req, res) => {
  res.json({
    message: 'Hello World',
  })
})

app.post('/test', async (req, res) => {
  const { amount } = req.body

  // Create a PaymentIntent with the order amount and currency
  const paymentIntent = await stripe.paymentIntents.create({
    amount: amount * 100,
    currency: 'sgd',
    automatic_payment_methods: {
      enabled: true,
    },
  })
  console.log(paymentIntent.client_secret)
  res.send({
    clientSecret: paymentIntent.client_secret,
  })
})

app.post('/createcustomerwithsavingcard', async (req, res) => {
  const { name } = req.body
  const customer = await stripe.customers.create({
    name,
  })
  const customer_id = customer.id
  const intent = await stripe.setupIntents.create({
    payment_method_types: ['card_present'],
    customer: customer_id,
  })
  const intent_id = intent.id
  const reader = await stripe.terminal.readers.processSetupIntent(
    serverenv.TERMINAL_ID,
    { setup_intent: intent_id, customer_consent_collected: true },
  )
  res.send({
    message: 'OK',
  })
})

console.log('START BACKEND SERVER')
app.listen(PORT)
