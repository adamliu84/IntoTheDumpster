// const { Ollama } = require('langchain/llms/ollama');
const { Ollama } = require('@langchain/community/llms/ollama');
const { PDFLoader } = require('langchain/document_loaders/fs/pdf');
const { RecursiveCharacterTextSplitter } = require('langchain/text_splitter');
const { loadSummarizationChain } = require('langchain/chains');
// const { PromptTemplate } = require('langchain/prompts');
const { PromptTemplate } = require('@langchain/core/prompts');

// Initialize Ollama
const ollama = new Ollama({
    baseUrl: "http://localhost:11434", // Local Ollama server
    model: "llama3.2", // Use the appropriate model name (check with `ollama list`)
    temperature: 0.5,
});

async function analyzePDF(pdfPath) {
    try {
        // Load PDF document
        const loader = new PDFLoader(pdfPath);
        const docs = await loader.load();

        // Split document into chunks
        const textSplitter = new RecursiveCharacterTextSplitter({
            chunkSize: 90000,
            chunkOverlap: 200,
        });
        const splitDocs = await textSplitter.splitDocuments(docs);

        // Create analysis prompt
        const prompt = new PromptTemplate({
            template: `Analyze this document and provide a detailed summary with key points.
            Focus on identifying main themes, important data, and significant conclusions.

            Document content:
            {text}

            Analysis:`,
            inputVariables: ["text"],
        });
        // const prompt = new PromptTemplate({
        //     template: `Answer these questions about the document:
        //   1. What is the main purpose of this document?
        //   2. List 3 key findings
        //   3. What recommendations are suggested?

        //   Document content:
        //   {text}

        //   Answers:`,
        //     inputVariables: ["text"],
        // });

        // Create analysis chain
        const chain = loadSummarizationChain(ollama, {
            type: "map_reduce",
            combinePrompt: prompt,
            returnIntermediateSteps: false,
            verbose: false
        });

        // Run analysis
        console.log("Running analysis...");
        const result = await chain.invoke({
            input_documents: splitDocs,
        });

        console.log("Analysis Result:\n", result.text);

    } catch (error) {
        console.error("Error analyzing PDF:", error);
    }
}

// Usage: node index.js ./your-file.pdf
const pdfPath = process.argv[2];
if (!pdfPath) {
    console.error("Please provide a PDF file path");
    process.exit(1);
}

analyzePDF(pdfPath);