import axios from 'axios';
import * as cheerio from 'cheerio';
import { Ollama } from '@langchain/ollama'
import { loadSummarizationChain } from 'langchain/chains';
import { PromptTemplate } from '@langchain/core/prompts';
import { RecursiveCharacterTextSplitter } from 'langchain/text_splitter';

// Initialize Ollama
const ollama = new Ollama({
    baseUrl: "http://localhost:11434", // Local Ollama server
    model: "llama3.2", // Use the appropriate model name (check with `ollama list`)
    temperature: 0.9,
});

// /**
//  * Fetch the HTML content of a given URL.
//  * @param {string} url - The URL of the forum page.
//  * @returns {Promise<string>} - The HTML content.
//  */
async function fetchThreadPage(url) {
    const response = await axios.get(url);
    return response.data;
}

function removeSpecialCharacters(input) {
    // Regular expression to match whitespace and control characters
    // const regex = /\s+/g;
    const regex = /[\t\n]/g;

    // Replace all occurrences of the pattern with an empty string
    return input.replace(regex, '');
}

// /**
//  * Parse the forum page to extract individual post texts.
//  * Adjust the selector (e.g. 'n') to match the actual forum structure.
//  * @param {string} html - The HTML content of the forum page.
//  * @returns {string[]} - Array of post texts.
//  */
async function parseForumPage(html) {
    const $ = cheerio.load(html);
    const posts = [];
    $('.message-userContent').each((i, el) => {
        posts.push(removeSpecialCharacters($(el).text().trim()));
    });
    return posts;
}

// /**
//  * Fetch and aggregate posts from all pages of the thread.
//  * @param {string} baseUrl - Base URL of the thread.
//  * @param {number} totalPages - Total number of pages to fetch.
//  * @returns {Promise<string[]>} - Combined list of posts.
//  */
async function fetchAllPages(baseUrl, totalPages) {
    let allPosts = [];
    // for (let page = 1; page <= totalPages; page++) {    
    for (let page = 1; page <= totalPages; page++) {
        const url = `${baseUrl}?page=${page}`;
        console.log(`Fetching page ${page}: ${url}`);
        const html = await fetchThreadPage(url);
        const posts = await parseForumPage(html);
        allPosts = allPosts.concat(posts);
    }
    return allPosts;
}

// /**
//  * Uses LangChain's LLMChain to summarize the forum discussion and analyze sentiment.
//  * @param {string[]} posts - Array of post texts.
//  * @returns {Promise<string>} - Summary and sentiment analysis.
//  */
async function summarizeAndAnalyze(posts) {
    // Concatenate all posts into a single string
    const text = posts.join("\n\n");
    const textSplitter = new RecursiveCharacterTextSplitter({ chunkSize: 2000 });
    const docs = await textSplitter.createDocuments([text])

    // Create a prompt template for summarization and sentiment analysis
    const summarizationPrompt = new PromptTemplate({
        template:
            `You are an expert summarizer. Summarize the following forum discussion in concise point form.
             Provide a general summary of the thread, indicating its overall direction.
             Analyze the overall popular sentiment (e.g., positive, negative, neutral) and return only one word: Positive, Negative, or Neutral.
             Focus on identifying the main themes, any important data, significant conclusions.

             Document content:
             {text}            
            `,
        inputVariables: ["text"],
    });

    const chain = loadSummarizationChain(ollama, {
        type: "stuff",
        combinePrompt: summarizationPrompt,
        returnIntermediateSteps: false,
        verbose: false,
    });

    const result = await chain.invoke({
        input_documents: docs,
    });

    return result.text;
}

(async () => {
    // Define the base thread URL (adjust to your specific thread) & Define how many pages the thread spans; adjust as needed
    const baseUrl = "https://forums.hardwarezone.com.sg/threads/news-jack-neo-hits-back-at-criticisms-of-his-cny-movie-a-hwz-forum-netizen-says-a-lot-of-people-left-halfway.7101813/";
    const totalPages = 11;

    try {
        // Fetch all posts from the thread pages
        const posts = await fetchAllPages(baseUrl, totalPages);
        if (!posts.length) {
            console.error("No posts found. Check the CSS selectors or URL.");
            return;
        }
        // Summarize discussion and perform sentiment analysis
        const summary = await summarizeAndAnalyze(posts);
        console.log("Summary and Sentiment Analysis:\n", summary);
    } catch (err) {
        console.error("Error processing thread:", err);
    }
})();
