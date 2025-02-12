import { useEffect, useState } from "react";
import './App.css';
import DataTable from "./components/DataTable";
import TextField from "@mui/material/TextField";
import Button from "@mui/material/Button";
import { Container } from "@mui/material";

function App() {

  const [videos, setVideos] = useState(null);
  const [keywords, setKeywords] = useState(null);
  const [thinkingFlag, setThinkingFlag] = useState(false);

  const [formData, setFormData] = useState({ age: "", interest: "" });

  const handleChange = (event) => {
    const { name, value } = event.target;
    setFormData((prevFormData) => ({ ...prevFormData, [name]: value }));
  };

  const handleSubmit = () => {
    setThinkingFlag(true);
    const requestOptions = {
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({ age: formData.age, interest: formData.interest })
    };
    fetch(`/api/post/`, requestOptions)
      .then((res) => res.json())
      .then((data) => {
        setThinkingFlag(false);
        setKeywords(data.keywords)
        setVideos(data.video_results)
      })
      .catch((error) => {
        alert("Error return!")
        setThinkingFlag(false);
        setKeywords(null)
        setVideos(null)
      });
  }

  return (
    <div className="App">
      <Container>
        <TextField id="age" name="age" label="Age" variant="filled" onChange={handleChange} />
        <br />
        <TextField id="interest" name="interest" label="Interested Character" variant="filled" onChange={handleChange} />
        <p></p>
        <Button variant="contained" onClick={handleSubmit}>Search education videos</Button>
      </Container>
      <Container>
        {thinkingFlag ? (
          <p>Thinking...</p> // Render element when thinkingFlag is true
        ) : (
          <></>
        )}
      </Container>
      <Container>
        {keywords ? (
          <>
            <p>Searched keyword: {keywords.join('🔺')}</p>
            <p>Current draft version is just using the [0] from ollam suggested keywords</p>
          </>
        ) : (
          <></>
        )}
      </Container>
      <Container>
        {videos ? (
          <DataTable videos={videos} />
        ) : (
          <></>
        )}
      </Container>
    </div >
  );
}

export default App;
