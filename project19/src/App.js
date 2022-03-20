import React from "react";
import { BrowserRouter as Router, Route, Routes } from 'react-router-dom'
import Hello from './Hello';
import Thx from './Thx'

function App() {
  return (
    <Router>
      <Routes>
        <Route exact path="/hello" element={<Hello />} />        
        <Route exact path="/thx" element={<Thx />} />
        <Route path="*" element={<>Not Found</>} />
      </Routes>
    </Router>
  );
}

export default App;
