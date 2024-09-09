import { useEffect, useState } from "react";
import './App.css';
import DataTable from "./components/DataTable";
import Typography from '@mui/material/Typography';
import Table from '@mui/material/Table';
import TableBody from '@mui/material/TableBody';
import TableCell from '@mui/material/TableCell';
import TableContainer from '@mui/material/TableContainer';
import TableHead from '@mui/material/TableHead';
import TableRow from '@mui/material/TableRow';
import Paper from '@mui/material/Paper';


function App() {

  const [posts, setPosts] = useState([]);
  const [lastPostId, setLastPostId] = useState(null);

  useEffect(() => {
    fetch(`/api/posts`)
      .then((res) => res.json())
      .then((data) => {
        const fdata = data.filter(function (post) {
          return post.id <= 10;
        })
        setPosts(fdata)
      });
    const temp = JSON.parse(localStorage.getItem('lastId'));
    setLastPostId(temp);
  }, []);

  const [formData, setFormData] = useState({ title: "", body: "" });

  const handleChange = (event) => {
    const { name, value } = event.target;
    setFormData((prevFormData) => ({ ...prevFormData, [name]: value }));
  };

  const handleSubmit = (event) => {
    event.preventDefault();
    const requestOptions = {
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({ title: formData.title, body: formData.body })
    };
    fetch(`/api/post/`, requestOptions)
      .then((res) => res.json())
      .then((data) => {
        localStorage.setItem('lastId', JSON.stringify({ "id": data.id }));
      });
  }

  const selectPost = (id) => {
    fetch(`/api/post/${id}`, { method: "GET" })
      .then((res) => res.json())
      .then((data) => console.log(data));
  }

  const updatePost = (id) => {
    const requestOptions = {
      method: 'PUT',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({ title: formData.title, body: formData.body })
    };
    fetch(`/api/post/${id}`, requestOptions)
      .then((res) => res.json())
      .then((data) => console.log(data));
    localStorage.setItem('lastId', JSON.stringify({ "id": id }));
  }

  const deletePost = (id) => {
    fetch(`/api/post/${id}`, { method: "DELETE" })
      .then((res) => res.json())
      .then((data) => console.log(data));
    localStorage.setItem('lastId', JSON.stringify({ "id": id }));
  }

  const getLocalStorageTable = () => {
    return (
      <TableContainer component={Paper}>
        <Table sx={{ minWidth: 650 }} aria-label="simple table">
          <TableHead>
            <TableRow>
              <TableCell>Key</TableCell>
              <TableCell align="right">Value</TableCell>
            </TableRow>
          </TableHead>
          <TableBody>
            <TableRow
              key="lastid"
              sx={{ '&:last-child td, &:last-child th': { border: 0 } }}
            >
              <TableCell component="th" scope="row">
                lastId
              </TableCell>
              <TableCell align="right">
                {JSON.stringify(lastPostId, null)}
              </TableCell>
            </TableRow>
          </TableBody>
        </Table>
      </TableContainer>
    )
  }

  return (
    <div className="App">
      <form onSubmit={handleSubmit}>
        <label htmlFor="Title">Title:</label>
        <input type="text" id="title" name="title" value={formData.name} onChange={handleChange} />
        <br />
        <label htmlFor="body">Body:</label>
        <textarea id="body" name="body" value={formData.message} onChange={handleChange} />
        <br />
        <button type="submit">Submit</button>
      </form>

      <Typography sx={{ mt: 4, mb: 2 }} variant="h6" component="div">
        Avatar with text and icon
      </Typography>
      <DataTable posts={posts} selectPostCallback={selectPost} updatePostCallback={updatePost} deletePostCallback={deletePost} />
      <Typography sx={{ mt: 4, mb: 2 }} variant="h6" component="div">
        Local Storage
      </Typography>
      {getLocalStorageTable()}
    </div >

  );
}

export default App;
