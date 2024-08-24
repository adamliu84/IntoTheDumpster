import { useEffect, useState } from "react";
import './App.css';

function App() {

  const [posts, setPosts] = useState([]);

  useEffect(() => {
    fetch(`/api/posts`)
      .then((res) => res.json())
      .then((data) => setPosts(data));
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
      .then((data) => console.log(data));
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
  }

  const deletePost = (id) => {
    fetch(`/api/post/${id}`, { method: "DELETE" })
      .then((res) => res.json())
      .then((data) => console.log(data));
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

      <h1>Posts</h1>
      <table>
        <tbody>
          {posts.map(function (post) {
            return (
              <tr key={post.id} style={{ 'outline': 'solid' }}>
                <td><div onClick={() => updatePost(post.id)}>UPDATE</div></td>
                <td><div onClick={() => deletePost(post.id)}>DELETE</div></td>
                <td><div onClick={() => selectPost(post.id)}>{post.id}</div></td>
                <td>{post.title}</td >
              </tr>
            )
          })}
        </tbody>
      </table>
    </div >
  );
}

export default App;
