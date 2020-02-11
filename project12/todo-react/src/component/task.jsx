import React, { Component } from 'react';
import './task.css';

class Task extends Component {

    state = {
        isEdit: false,
        curTitle: this.props.task.title,
        curDesc: this.props.task.desc,
    }

    render() {
        return (
            <div className="Task">
                {this.editDeleteFragment()}
                <hr />
            </div>
        );
    }

    handleEdit = (e) => {
        this.setState({ isEdit: true });
    }

    editDeleteFragment() {
        let { title, desc } = this.props.task;
        if (this.state.isEdit) {
            return (
                <div>
                    <form onSubmit={this.handleSubmit}>
                        <label>
                            <input type="text" defaultValue={title} onChange={(e) => this.setState({ curTitle: e.target.value })} />
                            <input type="text" defaultValue={desc} onChange={(e) => this.setState({ curDesc: e.target.value })} />
                        </label>
                        <br />
                        <button onClick={(e) => { this.setState({ isEdit: false }); this.props.onTaskUpdate(this.props.task.id, this.state.curTitle, this.state.curDesc); }}>Update</button>
                        <button onClick={(e) => this.setState({ isEdit: false })}>Cancel</button>
                    </form>
                </div>
            );
        } else {
            return (
                <React.Fragment>
                    {title} | {desc}
                    <div className="EditRemove">
                        <a onClick={this.handleEdit}>Edit</a>
                        &nbsp;|&nbsp;
                        <a onClick={() => this.props.onTaskDelete(this.props.task.id)}>
                            Delete
                        </a>
                    </div>
                </React.Fragment>
            );
        }

    }
}

export default Task;