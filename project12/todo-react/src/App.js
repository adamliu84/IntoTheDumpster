import React, { Component } from 'react';
import logo from './logo.svg';
import './App.css';
import Task from './component/task'
import axios from 'axios';

class App extends Component {

  state = {
    tasks: [],
  }

  componentDidMount() {
    axios.get('task/')
      .then(response => {
        this.setState({ tasks: response.data });
      })
      .catch((error) => {
        console.log(error);
      })
  }

  handleTaskCreate = () => {
    axios.post('task/', {
      title: 'New Title',
      desc: 'New Desc',
    })
      .then(function (response) {
        console.log(response);
      })
      .catch(function (error) {
        console.log(error);
      });
    // const tasks = [...this.state.tasks];
    // let newID = 0;
    // if (tasks.length !== 0) {
    //   newID = (Math.max.apply(Math, tasks.map(function (o) { return o._id; }))) + 1;
    // }
    // const newTask = { id: newID, title: 'New Title', desc: 'New Desc' };
    // tasks.push(newTask);
    // this.setState({ tasks });
  }

  handleTaskUpdate = (id, newTitle, newDesc) => {
    axios.patch(`task/${id}`, {
      title: newTitle,
      desc: newDesc,
    })
      .then(function (response) {
        console.log(response);
      })
      .catch(function (error) {
        console.log(error);
      });
    // const tasks = [...this.state.tasks];
    // const index = tasks.findIndex(t => t._id === id);
    // const task = tasks[index];
    // task.title = newTitle;
    // task.desc = newDesc;
    // this.setState({ tasks });
  }

  handleTaskDelete = (id) => {
    axios.delete(`task/${id}`)
      .then(function (response) {
        console.log(response);
      })
      .catch(function (error) {
        console.log(error);
      });
    // const tasks = this.state.tasks.filter(t => t._id !== id);
    // this.setState({ tasks });
  }

  render() {
    return (
      <div className="App">
        <header className="App-header">
          <table>
            <tbody>
              <tr>
                <td>{this.generateTask()}
                  <button onClick={this.handleTaskCreate}>Add Template Task</button></td>
                <td>
                  <img src={logo} className="App-logo" alt="logo" />
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

export default App;
