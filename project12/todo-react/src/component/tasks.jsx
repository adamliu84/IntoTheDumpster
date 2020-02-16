import React, { Component } from 'react';
import logo from '../logo.svg';
import './tasks.css';
import Task from './task'
import axios from 'axios';
import auth from '../auth';

class Tasks extends Component {

  state = {
    tasks: [],
  }

  componentDidMount() {
    axios.get('task/',
      auth.getAuthorizationheader())
      .then(response => {
        this.setState({ tasks: response.data });
      })
      .catch((error) => {
        console.log(error);
      })
  }

  handleTaskCreate = () => {
    const self = this;
    axios.post('task/', {
      title: 'New Title',
      desc: 'New Desc',
    }, auth.getAuthorizationheader())
      .then(function (response) {
        let newID = response.data._id
        const tasks = [...self.state.tasks];
        const newTask = { _id: newID, title: 'New Title', desc: 'New Desc' };
        tasks.push(newTask);
        self.setState({ tasks });
      })
      .catch(function (error) {
        console.log(error);
      });
  }

  handleTaskUpdate = (id, newTitle, newDesc) => {
    const self = this;
    axios.patch(`task/${id}`, {
      title: newTitle,
      desc: newDesc,
    }, auth.getAuthorizationheader())
      .then(function (response) {
        const tasks = [...self.state.tasks];
        const index = tasks.findIndex(t => t._id === id);
        const task = tasks[index];
        task.title = newTitle;
        task.desc = newDesc;
        self.setState({ tasks });
      })
      .catch(function (error) {
        console.log(error);
      });
  }

  handleTaskDelete = (id) => {
    const self = this;
    axios.delete(`task/${id}`,
      auth.getAuthorizationheader())
      .then(function (response) {
        const tasks = self.state.tasks.filter(t => t._id !== id);
        self.setState({ tasks });
      })
      .catch(function (error) {
        console.log(error);
      });
  }

  handleLogout = () => {
    auth.logout()
      .then(() => {
        this.props.history.push("/");
      })
      .catch(() => {

      });
  }

  render() {
    return (
      <div className="Tasks">
        <header className="Tasks-header">
          <button
            className="loginbutton"
            onClick={() => this.handleLogout()}>
            Logout
            </button>
          <table>
            <tbody>
              <tr>
                <td>{this.generateTask()}
                  <button onClick={this.handleTaskCreate}>Add Template Task</button></td>
                <td>
                  <img src={logo} className="Tasks-logo" alt="logo" />
                </td>
              </tr>
            </tbody>
          </table>
        </header>
      </div>
    );
  }

  generateTask() {
    return (
      <div>
        {this.state.tasks.map(task =>
          <Task
            onTaskUpdate={this.handleTaskUpdate}
            onTaskDelete={this.handleTaskDelete}
            key={task._id}
            task={task}
          />
        )}
      </div>
    );
  }

}

export default Tasks;
