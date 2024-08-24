import { useEffect, useState } from "react";
import './App.css';

function App() {

  const [posts, setPosts] = useState([]);

  useEffect(() => {
    fetch(`/api/posts`)
      .then((res) => res.json())
      .then((data) => setPosts(data));
  }, []);

  return (
    <div className="App">
      <h1>Posts</h1>
      {posts.map((post) => (
        <p key={post.id}>{post.id} → {post.title}</p>
      ))}
    </div>
  );
}

export default App;
