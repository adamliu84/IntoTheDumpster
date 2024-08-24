import { useEffect, useState } from "react";
import './App.css';

function App() {

  const [users, setUsers] = useState([]);

  useEffect(() => {
    fetch(`/api/users`)
      .then((res) => res.json())
      .then((data) => setUsers(data['users']));
  }, []);

  return (
    <div className="App">
      <h1>Users</h1>
      {users.map((user) => (
        <p key={user.id}>{user.id} → {user.desc}</p>
      ))}
    </div>
  );
}

export default App;
