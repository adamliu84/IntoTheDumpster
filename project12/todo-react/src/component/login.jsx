import React, { Component } from 'react';
import './tasks.css';
import auth from '../auth';

class Login extends Component {

    state = {
        username: '',
        password: '',
        errormessage: '',
    }


    handleSubmit = (e) => {
        const self = this;
        const { username, password } = this.state;
        auth.login(username, password).then(
            function () {
                self.props.history.push('/tasks');
            }
        ).catch(function (err) {
            self.setState({ errormessage: err.message });
        })
    }

    handleChange = (e) => {
        const { name, value } = e.target;
        this.setState({ [name]: value });
    }

    render() {
        const pStyle = {
            paddingTop: 200,
        };
        return (
            <div className="Tasks">
                <header style={pStyle} className="Tasks-header">
                    <font color="red">{this.state.errormessage}</font>
                    <form>
                        <label>Username: </label>
                        <input type="text" name="username" onChange={(e) => this.handleChange(e)} />
                        <br />
                        <label>Password: </label>
                        <input type="password" name="password" onChange={(e) => this.handleChange(e)} />
                        <br /><hr />
                        <button type="button" className="loginbutton" onClick={(e) => this.handleSubmit(e)}>Login</button>
                    </form>
                </header>
            </div >
        );
    }
}

export default Login;