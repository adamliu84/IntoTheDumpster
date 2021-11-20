import React, { useRef, useState, ChangeEvent } from 'react'
const FormData = require('form-data');
const axios = require('axios');

const UploadButton = () => {
  const [message, setMessage] = useState<String | null>(null)
  const uploadRef = useRef<HTMLInputElement>(null)

  const handleUpload = (
    e: ChangeEvent<HTMLInputElement>
  ) => {
    if (e.target.files === null) {
      return
    }
    const file = e.target.files[0];
    const formData = new FormData();
    formData.append('file', file);
    axios.post('http://localhost:3001/upload', formData, {
      headers: {
        'Content-Type': 'multipart/form-data'
      }
    }).then((response: any) => {
      setMessage(response.data.gatewayurl);
    })
      .catch((error: any) => {
        setMessage(error.response.data.error);
      });
  }

  return (
    <>
      {message ? (
        <p style={{ fontSize: '15px' }}>{message}</p>
      ) : (
        <>
          <button onClick={() => uploadRef.current?.click()}>
            Upload file
          </button>
          <input
            type="file"
            ref={uploadRef}
            onChange={handleUpload}
            style={{ display: 'none' }}
          />
        </>
      )}
    </>
  )
}

export default UploadButton;