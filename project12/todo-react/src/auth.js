import axios from 'axios';

class Auth {
    constructor() {
        this.authenticated = false;
    }

    login(username, password) {
        return new Promise(function (resolve, reject) {
            axios.post('user/login/', {
                username: username,
                password: password
            })
                .then(response => {
                    localStorage.setItem('access_token', response.data.access_token);
                    resolve();
                    return;
                })
                .catch((error) => {
                    reject(error);
                    return;
                })
        });

    }

    logout() {
        return new Promise(function (resolve, reject) {
            localStorage.removeItem('access_token');
            resolve();
            return;
        });
    }

    isAuthenticated() {
        return this.authenticated;
    }
}

export default new Auth();