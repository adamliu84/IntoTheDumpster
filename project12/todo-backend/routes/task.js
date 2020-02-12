const express = require('express');
const router = express.Router();

const Task = require('../model/Task');
const mongoose = require('mongoose');

router.get('/', async (req, res) => {
    try {
        const tasks = await Task.find();
        res.json(tasks);
    } catch (err) {
        res.json({ message: err });
    }
})

router.get('/:taskId', async (req, res) => {
    try {
        const taskId = req.params.taskId;
        const task = await Task.findById(taskId);
        if (null === task) {
            throw new Error('Invalid task');
        }
        res.json(task);
    }
    catch (err) {
        res.json({ error: err.message });
    }
})

router.post('/', async (req, res) => {
    try {
        const task = new Task({
            title: req.body.title,
            desc: req.body.desc,
        });
        const savedTask = await task.save();
        res.json(savedTask);
    }
    catch (err) {
        res.json({ error: err.message });
    }
})

router.patch('/:taskId', async (req, res) => {
    try {
        const taskId = req.params.taskId;
        const updateTask = await Task.updateOne(
            { _id: taskId, },
            {
                $set: {
                    title: req.body.title,
                    desc: req.body.desc,
                }
            }
        );
        res.json(updateTask);
    }
    catch (err) {
        res.json({ error: err.message });
    }
})

router.delete('/:taskId', async (req, res) => {
    try {
        const taskId = req.params.taskId;
        const removeTask = await Task.deleteOne(
            { _id: taskId, },
        );
        res.json(removeTask);
    }
    catch (err) {
        res.json({ error: err.message });
    }
})

module.exports = router;