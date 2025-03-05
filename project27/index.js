// const { Ollama } = require('langchain/llms/ollama');
// import { Ollama } from '@langchain/community/llms/ollama';
// const { PromptTemplate } = require('langchain/prompts');
import { Ollama } from '@langchain/ollama'
import { PDFLoader } from 'langchain/document_loaders/fs/pdf';
import { RecursiveCharacterTextSplitter } from 'langchain/text_splitter';
import { loadSummarizationChain } from 'langchain/chains';
import { PromptTemplate } from '@langchain/core/prompts';
import yoctoSpinner from 'yocto-spinner';
//Temp fix for UND_ERR_HEADERS_TIMEOUT at node:internal/deps/undici/undici:12500:13
//https://github.com/langchain-ai/langchainjs/issues/1856#issuecomment-1793436310
import { Agent } from "undici";
globalThis[Symbol.for("undici.globalDispatcher.1")] = new Agent({
    headersTimeout: 1000 * 60 * 60 * 24,
});

// Initialize Ollama
const ollama = new Ollama({
    baseUrl: "http://localhost:11434", // Local Ollama server
    model: "llama3.2", // Use the appropriate model name (check with `ollama list`)
    temperature: 0.5,
});

async function analyzePDF(pdfPath) {
    const spinner = yoctoSpinner({ text: 'Running analysis...' });
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
        spinner.start();
        const result = await chain.invoke({
            input_documents: splitDocs,
        });

        spinner.success('Analysis done!');
        console.log("Analysis Result:\n", result.text);

    } catch (error) {
        spinner.error("ERROR!")
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