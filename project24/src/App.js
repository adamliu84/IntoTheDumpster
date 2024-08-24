import { useEffect, useState } from "react";
import './App.css';

function App() {

  const [posts, setPosts] = useState([]);

  useEffect(() => {
    fetch(`/api/posts`)
      .then((res) => res.json())
      .then((data) => setPosts(data));
  }, []);

  const updatePost = (id) => {
    fetch(`/api/post/${id}`, { method: "PUT" })
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
      <h1>Posts</h1>
      <table>
        <tbody>
          {posts.map(function (post) {
            return (
              <tr key={post.id} style={{ 'outline': 'solid' }}>
                <td><div onClick={() => updatePost(post.id)}>UPDATE</div></td>
                <td><div onClick={() => deletePost(post.id)}>DELETE</div></td>
                <td>{post.id}</td>
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
