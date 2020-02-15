import React, { Component } from 'react';
import './tasks.css';

class Login extends Component {
    render() {
        const pStyle = {
            paddingTop: 200,
        };
        return (
            <div className="Tasks">
                <header style={pStyle} className="Tasks-header">
                    <button
                        className="loginbutton"
                        onClick={() => { this.props.history.push("/tasks"); }}
                    >
                        Login
                    </button>
                </header>
            </div >
        );
    }
}

export default Login;