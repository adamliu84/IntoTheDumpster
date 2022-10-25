import React, { useState } from 'react'
import AppBar from '@mui/material/AppBar'
import Button from '@mui/material/Button'
import CssBaseline from '@mui/material/CssBaseline'
import Grid from '@mui/material/Grid'
import Toolbar from '@mui/material/Toolbar'
import Typography from '@mui/material/Typography'
import Link from '@mui/material/Link'
import GlobalStyles from '@mui/material/GlobalStyles'
import Container from '@mui/material/Container'
import WatchmenImg from './image/watchmen.png'
import Slider from '@mui/material/Slider'
import HelloDialog from './HelloDialog'
import axios from 'axios'
import TextField from '@mui/material/TextField'

function Copyright(props) {
  return (
    <Typography
      variant="body2"
      color="text.secondary"
      align="center"
      {...props}
    >
      {'Copyright © '}
      <Link color="inherit" href="https://mui.com/">
        Your Website
      </Link>{' '}
      {new Date().getFullYear()}
      {'.'}
    </Typography>
  )
}

function TerminalSaveCard() {
  const [customername, setCustomername] = useState(null)
  const [clientSecret, setClientSecret] = useState(null)

  const [open, setOpen] = React.useState(false)

  const handleClose = () => {
    setClientSecret(null)
    setOpen(false)
  }

  const createCustomer = () => {
    axios
      .post('http://localhost:3001/createcustomerwithsavingcard', {
        name: customername,
      })
      .then(function (response) {
        console.log(response)
      })
  }

  return (
    <React.Fragment>
      <GlobalStyles
        styles={{ ul: { margin: 0, padding: 0, listStyle: 'none' } }}
      />
      <CssBaseline />
      <AppBar
        position="static"
        color="default"
        elevation={0}
        sx={{ borderBottom: (theme) => `1px solid ${theme.palette.divider}` }}
      >
        <Toolbar sx={{ flexWrap: 'wrap' }}>
          <Typography variant="h6" color="inherit" noWrap sx={{ flexGrow: 1 }}>
            Save card to customer
          </Typography>
        </Toolbar>
      </AppBar>
      <Container maxWidth="md" component="main" sx={{ pt: 8, pb: 6 }}>
        <Grid container spacing={5} alignItems="flex-end">
          {/* Donation */}
          <Grid item key="donation" xs={12} sm={6} md={6}>
            <TextField
              id="outlined-basic"
              label="Customer name"
              variant="outlined"
              onChange={(evt) => {
                setCustomername(evt.target.value)
              }}
            />
          </Grid>
          {/* Subscription */}
          <Grid item key={'pricing'} xs={12} sm={6} md={4}>
            <Button variant="contained" onClick={createCustomer}>
              Create customer & request save card
            </Button>
          </Grid>
        </Grid>
      </Container>
      {/* Footer */}
      <Container
        maxWidth="md"
        component="footer"
        sx={{
          borderTop: (theme) => `1px solid ${theme.palette.divider}`,
          mt: 8,
          py: [3, 6],
        }}
      >
        <Copyright sx={{ mt: 5 }} />
      </Container>
      {/* End footer */}
    </React.Fragment>
  )
}

export default TerminalSaveCard
