import './App.css';
import React, { useState, useRef, useEffect } from "react"
import { Base64 } from 'js-base64';
import QRCode from 'qrcode.react';
import QuickEncrypt from 'quick-encrypt';

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

const GenQuickEncrypt = ({ sourceText }) => {

  let keys = QuickEncrypt.generate(2048) // Use either 2048 bits or 1024 bits.
  const [encryptedText, setEncryptedText] = useState("");
  const [decryptedText, setDecryptedText] = useState("");

  useEffect(() => {
    if ("" !== sourceText) {
      const encrypted = QuickEncrypt.encrypt(sourceText, keys.public)
      setEncryptedText(encrypted);
      setDecryptedText(QuickEncrypt.decrypt(encrypted, keys.private));
    }
  }, [sourceText])

  return (
    <>
      <h3>QuickEncrypt</h3>
      Public: <textarea id="qe_public" name="qe_public" rows="4" cols="50" defaultValue={keys.public} /><br />
      Private: <textarea id="qe_private" name="qe_private" rows="4" cols="50" defaultValue={keys.private} />
      <div>Encrypted Text: {encryptedText}</div>
      <div>Decrypted Text: {decryptedText}</div>
    </>
  );
}

const GenHtmlJS = () => {
  useEffect(() => {
    //https://stackoverflow.com/questions/53396307/how-do-i-use-external-script-that-i-add-to-react-js
    //https://betterprogramming.pub/4-ways-of-adding-external-js-files-in-reactjs-823f85de3668
    const script = document.createElement('script');
    script.src = "https://code.jquery.com/jquery-3.4.1.min.js";
    script.async = true;
    document.body.appendChild(script);
  })

  let body = `
    <button id="this_button" onclick='(async () => {
      console.log("The button is clicked");
      $("#this_button").text("Now this is a button!");
    })()'>
      This is a button
    </button>
  `;

  return (
    <>
      <div dangerouslySetInnerHTML={{ __html: body }} />
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
      <GenHtmlJS />
      <hr />
      <GenBase64 sourceText={sourceText} />
      <hr />
      <GenQrCode sourceText={sourceText} />
      <hr />
      <GenQuickEncrypt sourceText={sourceText} />
      <hr />
    </div>
  );
}

export default App;
