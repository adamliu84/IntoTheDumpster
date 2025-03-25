### Key elements of AI Agents - ResearchPlayground.py
https://learn.deeplearning.ai/courses/multi-ai-agent-systems-with-crewai/lesson/eusjw/create-agents-to-research-and-write-an-article-(code)
Basic crewai frameowork - Agent > Task > Crew

### Mental framework of agent - CustomerSupportPlayground.py
https://learn.deeplearning.ai/courses/multi-ai-agent-systems-with-crewai/lesson/nk13s/multi-agent-customer-support-automation-(code)
Self improvement via memory

### Tool of agent - CustomerOutreachPlayground.py
https://learn.deeplearning.ai/courses/multi-ai-agent-systems-with-crewai/lesson/ksejw/tools-for-a-customer-outreach-campaign-(code)
DirectoryReadTool,FileReadTool,SerperDevTool (serper.dev),BaseTool


### Execution flow & Export of agent - EventPlanningPlayground.py
https://learn.deeplearning.ai/courses/multi-ai-agent-systems-with-crewai/lesson/kq8ls/automate-event-planning-(code)
async_execution, pydantic, output_file (including tool ScrapeWebsiteTool), ask for user's input

### Multi agent collaboration - FinancialAnalysisPlayground.py
https://learn.deeplearning.ai/courses/multi-ai-agent-systems-with-crewai/lesson/ixy19/mutli-agent-collaboration-for-financial-analysis-(code)
Process, manager_llm

### Multi agent collaboration - JobApplicationPlayground.py
https://learn.deeplearning.ai/courses/multi-ai-agent-systems-with-crewai/lesson/a8ecj/build-a-crew-to-trailor-job-applications-(code)
MDXSearchTool, RAG, Gen recommendation file output

**TODO/WARNING** .py not really running/working as expected due to the local Ollama not working on MDXSearchTool

### YAML configuration - AutomatedProjectPlayground.py
https://learn.deeplearning.ai/courses/practical-multi-ai-agents-and-advanced-use-cases-with-crewai/lesson/oqusr/automated-project:-planning,-estimation,-and-allocation
.yaml

### API (External) integration - ProjectProgressReportPlayground.py
https://learn.deeplearning.ai/courses/practical-multi-ai-agents-and-advanced-use-cases-with-crewai/lesson/xi5md/building-project-progress-report
BaseTool, .env loader

### CrewAI Flow - AgenticSalesPipelinePlayground.py
https://learn.deeplearning.ai/courses/practical-multi-ai-agents-and-advanced-use-cases-with-crewai/lesson/l04k1/agentic-sales-pipeline
Flow

**TODO/WARNING** .py not really running/working as expected due 400 Failed to convert text into a pydantic model due to the following error: litellm.BadRequestError: VertexAIException BadRequestError Invalid JSON payload received. Unknown name \"default\" at 'tools[0].function_declarations[0].parameters.properties[1].value.properties[3].value': Cannot find field.\nInvalid JSON payload received. Unknown name \"default\" at 'tools[0].function_declarations[0].parameters.properties[2].value.properties[2].value': Cannot find field.

### Multi-Model - ContentCreationPlayground.py
https://learn.deeplearning.ai/courses/practical-multi-ai-agents-and-advanced-use-cases-with-crewai/lesson/aix1c/content-creation-at-scale
Multi-Model

### CrewAI CLI - blogpostcrewplayground
- https://learn.deeplearning.ai/courses/practical-multi-ai-agents-and-advanced-use-cases-with-crewai/lesson/omdqe/blog-post-crew-in-production
- https://docs.crewai.com/concepts/cli
```
crewai create crew new_project --provider gemini
crewai install
crewai run
```

> pip install crewai==0.28.8 crewai_tools==0.1.6 langchain_community==0.0.29
