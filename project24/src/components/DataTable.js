import * as React from 'react';
import { styled } from '@mui/material/styles';
import Box from '@mui/material/Box';
import List from '@mui/material/List';
import ListItem from '@mui/material/ListItem';
import ListItemText from '@mui/material/ListItemText';
import IconButton from '@mui/material/IconButton';
import DeleteIcon from '@mui/icons-material/Delete';
import EditIcon from '@mui/icons-material/Edit';

const Demo = styled('div')(({ theme }) => ({
    backgroundColor: theme.palette.background.paper,
}));

export default function InteractiveList({ posts, selectPostCallback, updatePostCallback, deletePostCallback }) {
    const postListItems = posts.map(post =>
        <ListItem key={post.id}>
            <ListItemText
                primary={"[" + post.id + "] " + post.title}
                secondary={post.body}
                onClick={(_) => { selectPostCallback(post.id) }}
            />
            <IconButton edge="end" aria-label="delete" onClick={(_) => { updatePostCallback(post.id) }}>
                <EditIcon />
            </IconButton>
            <IconButton edge="end" aria-label="delete" onClick={(_) => { deletePostCallback(post.id) }}>
                <DeleteIcon />
            </IconButton>
        </ListItem>,
    )

    return (
        <Box sx={{ flexGrow: 1, maxWidth: 752 }}>
            <Demo>
                <List dense={true}>
                    {postListItems}
                </List>
            </Demo>
        </Box >
    );
}
