const express = require("express");
const axios = require('axios');
const bodyParser = require('body-parser')
const { default: ollama } = require('ollama');
const { placeholderlocaldata } = require("./placeholderlocaldata");

const app = express();
app.use(bodyParser.json());
app.use(bodyParser.urlencoded({ extended: true }));

const PORT = 3001;
const GOOGLE_API_KEY = "XYZ"

app.post("/api/post", async (req, res) => {
    try {
        //Ollama prompt
        const kidInterest = req.body.interest
        const yearsold = req.body.age
        const prompt = `You are a friendly teacher helping children learn math by relating it to their favorite cartoon characters. The child is interested in ${kidInterest} and ${yearsold} years old. 
        Suggest a engaging math-related keywords that align with this interest to generate a youtube search (limit to 3). Return your answer just in a json object with the "keywords" as key and values in array`;
        const ollama_response = await ollama.generate({
            model: 'llama3.2',
            prompt: prompt,
            format: 'json',

        });
        let keyboardJson = JSON.parse(ollama_response.response);

        // Google search
        let video_array = []
        for await (const keyword of keyboardJson.keywords) {
            const searchQuery = keyboardJson.keywords[0]
            const options = {
                method: 'GET',
                url: `https://www.googleapis.com/youtube/v3/search?part=id,snippet&q=${searchQuery}&key=${GOOGLE_API_KEY}`,
                headers: {
                    'User-Agent': 'Node.js App'
                }
            };
            let current_result = null;
            if ("XYZ" === GOOGLE_API_KEY) {
                current_result = placeholderlocaldata.data.items
            } else {
                const google_response = await axios(options)
                current_result = google_response.data.items
            }

            current_result.forEach(i => {
                if (!(video_array.some(j => j.id.videoId === i.id.videoId))) {
                    video_array.push(i)
                }
            });
        }



        // Format final response
        const frontend_response = {
            keywords: keyboardJson.keywords,
            video_results: video_array
        }
        return res.json(frontend_response)

    } catch (error) {
        console.error(error);
        return res.status(500).send('Error')
    }
});

app.get("/", (req, res) => {
    res.send("Hello World from Server!");
});

app.listen(PORT, () => console.log(`Listening on port ${PORT}`));
