import logo from './logo.svg';
import './App.css';
import UploadButton from './component/UploadButton'

function App() {
  return (
    <div className="App">
      <header className="App-header">
        <img src={logo} className="App-logo" alt="logo" />      
          <UploadButton />        
      </header>
    </div>
  );
}

export default App;
