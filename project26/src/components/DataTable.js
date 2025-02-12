import * as React from 'react';
import { styled } from '@mui/material/styles';
import Box from '@mui/material/Box';
import List from '@mui/material/List';
import ListItem from '@mui/material/ListItem';
import ListItemText from '@mui/material/ListItemText';
import Link from '@mui/material/Link'

const Demo = styled('div')(({ theme }) => ({
    backgroundColor: theme.palette.background.paper,
}));

export default function InteractiveList({ videos, }) {
    const videoListItems = videos.map(video =>
        <ListItem key={video.id.videoId}>
            <ListItemText
                primary={video.snippet.title}
                secondary={video.snippet.channelTitle}
            />
            <img src={video.snippet.thumbnails.default.url} alt="Example image" />
            <Link href={"https://www.youtube.com/v/" + video.id.videoId} target="_blank">Watch Video</Link>
        </ListItem>,
    )

    return (
        <Box sx={{ flexGrow: 1, maxWidth: 752 }}>
            <Demo>
                <List dense={true}>
                    {videoListItems}
                </List>
            </Demo>
        </Box >
    );
}
