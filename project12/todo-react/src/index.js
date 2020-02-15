import React from 'react';
import ReactDOM from 'react-dom';
import './index.css';
import Tasks from './component/tasks';
import Login from './component/login';
import * as serviceWorker from './serviceWorker';
import { BrowserRouter, Route, Switch } from 'react-router-dom';

function App() {
    return (
        <React.Fragment>
            <Switch>
                <Route exact path="/" component={Login} />
                <Route exact path="/tasks" component={Tasks} />
                <Route path="*" component={() => "404"} />
            </Switch>
        </React.Fragment>
    );
}

ReactDOM.render(<BrowserRouter><App /></BrowserRouter>, document.getElementById('root'));

// If you want your app to work offline and load faster, you can change
// unregister() to register() below. Note this comes with some pitfalls.
// Learn more about service workers: https://bit.ly/CRA-PWA
serviceWorker.unregister();
