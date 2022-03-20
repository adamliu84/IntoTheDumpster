import * as React from 'react';
import Button from '@mui/material/Button';
import Dialog from '@mui/material/Dialog';
import DialogActions from '@mui/material/DialogActions';
import DialogContent from '@mui/material/DialogContent';
import DialogTitle from '@mui/material/DialogTitle';
import { loadStripe } from "@stripe/stripe-js";
import { Elements } from "@stripe/react-stripe-js";
import CheckoutForm from "./CheckoutForm";
const clientenv = require('./clientenv.json');
const stripePromise = loadStripe(clientenv.PUBLISHABLE_KEY);

function HelloDialog(props) {
    const appearance = {
        theme: 'stripe',
    };
    const options = {
        clientSecret: props.clientSecret,
        appearance,
    };

    return (
        <div>
            <Dialog open={props.open} onClose={props.handleClose}>
                <DialogTitle>Hello!</DialogTitle>
                <DialogContent>
                    <div className="App">
                        {props.clientSecret && (
                            <Elements options={options} stripe={stripePromise}>
                                <CheckoutForm />
                            </Elements>
                        )}
                    </div>
                </DialogContent>
                <DialogActions>
                    <Button onClick={props.handleClose}>Cancel</Button>
                </DialogActions>
            </Dialog>
        </div>
    );
}

export default HelloDialog;