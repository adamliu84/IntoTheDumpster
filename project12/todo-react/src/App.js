import React, { Component } from 'react';
import logo from './logo.svg';
import './App.css';
import Task from './component/task'

class App extends Component {

  state = {
    tasks: [
      { id: 1, title: 'React CRUD', desc: 'Create a simple CRUD in React' },
      { id: 2, title: 'Git Push', desc: 'Git push on React CRUD' },
      { id: 3, title: 'Backend CRUD', desc: 'Create a simple CRUD in Backend' },
      { id: 4, title: 'Git Push', desc: 'Git push on Backend CRUD' },
    ],
  }

  handleTaskCreate = () => {
    const tasks = [...this.state.tasks];
    let newID = 0;
    if (tasks.length !== 0) {
      newID = (Math.max.apply(Math, tasks.map(function (o) { return o.id; }))) + 1;
    }
    const newTask = { id: newID, title: 'New Title', desc: 'New Desc' };
    tasks.push(newTask);
    this.setState({ tasks });
  }

  handleTaskUpdate = (id, newTitle, newDesc) => {
    const tasks = [...this.state.tasks];
    const index = tasks.findIndex(t => t.id === id);
    const task = tasks[index];
    task.title = newTitle;
    task.desc = newDesc;
    this.setState({ tasks });
  }

  handleTaskDelete = (id) => {
    const tasks = this.state.tasks.filter(t => t.id !== id);
    this.setState({ tasks });
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
            key={task.id}
            task={task}
          />
        )}
      </div>
    );
  }

}

export default App;
