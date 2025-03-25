# Warning control
import warnings
warnings.filterwarnings('ignore')

# Load environment variables
from dotenv import load_dotenv
load_dotenv()
import os
import yaml
from crewai import Agent, Task, Crew, LLM

from pydantic import BaseModel, Field
from typing import List
from crewai_tools import SerperDevTool, ScrapeWebsiteTool, WebsiteSearchTool

class SocialMediaPost(BaseModel):
    platform: str = Field(..., description="The social media platform where the post will be published (e.g., Twitter, LinkedIn).")
    content: str = Field(..., description="The content of the social media post, including any hashtags or mentions.")

class ContentOutput(BaseModel):
    article: str = Field(..., description="The article, formatted in markdown.")
    social_media_posts: List[SocialMediaPost] = Field(..., description="A list of social media posts related to the article.")
    
# Define file paths for YAML configurations
files = {
    'agents': 'agents.yaml',
    'tasks': 'tasks.yaml'
}

# Load configurations from YAML files
configs = {}
for config_type, file_path in files.items():
    with open(file_path, 'r') as file:
        configs[config_type] = yaml.safe_load(file)

# Assign loaded configurations to specific variables
agents_config = configs['agents']
tasks_config = configs['tasks']

GOOGLE_GEMINI_API_KEY = os.getenv('GOOGLE_GEMINI_API_KEY')
os.environ["GOOGLE_API_KEY"] = GOOGLE_GEMINI_API_KEY
os.environ["GEMINI_API_KEY"] = GOOGLE_GEMINI_API_KEY
os.environ["SERPER_API_KEY"] = os.getenv('SERPER_API_KEY')
# https://community.crewai.com/t/issue-with-invalid-api-key-in-crewai-agent-for-gemini-api/4010/5
os.environ["LITELLM_MODEL"] = "gemini/gemini-1.5-flash" 
# _llm = LLM(
#     model="ollama_chat/llama3.2",
#     base_url="http://host.docker.internal:11434",
#     api_key="sk-proj-1111",    
# )
_llm = LLM(
            # model='gemini/gemini-1.5-flash',
            model='gemini/gemini-2.0-flash',
            api_key=GOOGLE_GEMINI_API_KEY
            ) 

# Creating Agents
market_news_monitor_agent = Agent(
    config=agents_config['market_news_monitor_agent'],
    tools=[SerperDevTool(), ScrapeWebsiteTool()],
    llm=_llm,
)

data_analyst_agent = Agent(
    config=agents_config['data_analyst_agent'],
    # tools=[SerperDevTool(), WebsiteSearchTool()],
    tools=[SerperDevTool()],
    llm=_llm,
)

content_creator_agent = Agent(
    config=agents_config['content_creator_agent'],
    # tools=[SerperDevTool(), WebsiteSearchTool()],
    tools=[SerperDevTool()],
    llm=_llm
)

quality_assurance_agent = Agent(
    config=agents_config['quality_assurance_agent'],
)

# Creating Tasks
monitor_financial_news_task = Task(
    config=tasks_config['monitor_financial_news'],
    agent=market_news_monitor_agent
)

analyze_market_data_task = Task(
    config=tasks_config['analyze_market_data'],
    agent=data_analyst_agent
)

create_content_task = Task(
    config=tasks_config['create_content'],
    agent=content_creator_agent,
    context=[monitor_financial_news_task, analyze_market_data_task]
)

quality_assurance_task = Task(
    config=tasks_config['quality_assurance'],
    agent=quality_assurance_agent,
    output_pydantic=ContentOutput    
)

# Creating Crew
content_creation_crew = Crew(
    agents=[
        market_news_monitor_agent,
        data_analyst_agent,
        content_creator_agent,
        quality_assurance_agent
    ],
    tasks=[
        monitor_financial_news_task,
        analyze_market_data_task,
        create_content_task,
        quality_assurance_task
    ],
    verbose=True
)

result = content_creation_crew.kickoff(inputs={
  'subject': 'Inflation in the US and the impact on the stock market in 2024'
})
print(result)