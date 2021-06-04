import './App.css';
import React, { useState, useRef } from "react"
import { Base64 } from 'js-base64';
import QRCode from 'qrcode.react';

const GenBase64 = ({ sourceText }) => {
  const encodedText = Base64.encode(sourceText, true);
  const decodedText = Base64.decode(encodedText);

  return (
    <>
      <h3>Base64</h3>
      <div>Encoded Text: {encodedText}</div>
      <div>Decoded Text: {decodedText}</div>
    </>
  );
}

const GenQrCode = ({ sourceText }) => {
  let qrcode = <>QR Code to be generate after source text is input</>;
  if ("" !== sourceText) {
    qrcode = <QRCode
      id="123456"
      value={sourceText}
      size={290}
      level={"H"}
      includeMargin={true}
    />
  }
  return (
    <>
      <h3>QR Code</h3>
      <div>
        {qrcode}
      </div>
    </>
  );
}

function App() {

  const [sourceText, setSourceText] = useState("");
  const inputRef = useRef();

  return (
    <div className="App">
      <header className="App-header">
        Source Text: <input type="text" name="name" ref={inputRef} />
        <input type="submit" value="Submit" onClick={(e) => setSourceText(inputRef.current.value)} />
      </header>
      <hr />
      <GenBase64 sourceText={sourceText} />
      <hr />
      <GenQrCode sourceText={sourceText} />
      <hr />
    </div>
  );
}

export default App;
