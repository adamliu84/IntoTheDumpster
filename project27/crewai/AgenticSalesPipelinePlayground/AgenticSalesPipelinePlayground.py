# Warning control
import warnings
warnings.filterwarnings('ignore')

import os
from dotenv import load_dotenv
load_dotenv()
import yaml
from crewai import Agent, Task, Crew, LLM
from pydantic import BaseModel, Field
from typing import Dict, Optional, List, Set, Tuple
from crewai_tools import SerperDevTool, ScrapeWebsiteTool
from crewai import Flow
from crewai.flow.flow import listen, start, and_, or_, router
import asyncio
import litellm
litellm._turn_on_debug()

GOOGLE_GEMINI_API_KEY = os.getenv('GOOGLE_GEMINI_API_KEY')
os.environ["GOOGLE_API_KEY"] = GOOGLE_GEMINI_API_KEY
os.environ["GEMINI_API_KEY"] = GOOGLE_GEMINI_API_KEY
os.environ["SERPER_API_KEY"] = os.getenv('SERPER_API_KEY')

async def main():
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

  # Define file paths for YAML configurations
  files = {
      'lead_agents': 'lead_qualification_agents.yaml',
      'lead_tasks': 'lead_qualification_tasks.yaml',
      'email_agents': 'email_engagement_agents.yaml',
      'email_tasks': 'email_engagement_tasks.yaml'
  }

  # Load configurations from YAML files
  configs = {}
  for config_type, file_path in files.items():
      with open(file_path, 'r') as file:
          configs[config_type] = yaml.safe_load(file)

  # Assign loaded configurations to specific variables
  lead_agents_config = configs['lead_agents']
  lead_tasks_config = configs['lead_tasks']
  email_agents_config = configs['email_agents']
  email_tasks_config = configs['email_tasks']
  
  class LeadPersonalInfo(BaseModel):
    name: str = Field(..., description="The full name of the lead.")
    job_title: str = Field(..., description="The job title of the lead.")
    role_relevance: int = Field(..., ge=0, le=10, description="A score representing how relevant the lead's role is to the decision-making process (0-10).")
    professional_background: Optional[str] = Field(..., description="A brief description of the lead's professional background.")

  class CompanyInfo(BaseModel):
      company_name: str = Field(..., description="The name of the company the lead works for.")
      industry: str = Field(..., description="The industry in which the company operates.")
      company_size: int = Field(..., description="The size of the company in terms of employee count.")
      revenue: Optional[float] = Field(None, description="The annual revenue of the company, if available.")
      market_presence: int = Field(..., ge=0, le=10, description="A score representing the company's market presence (0-10).")

  class LeadScore(BaseModel):
      score: int = Field(..., ge=0, le=100, description="The final score assigned to the lead (0-100).")
      scoring_criteria: List[str] = Field(..., description="The criteria used to determine the lead's score.")
      validation_notes: Optional[str] = Field(None, description="Any notes regarding the validation of the lead score.")

  class LeadScoringResult(BaseModel):
      personal_info: LeadPersonalInfo = Field(..., description="Personal information about the lead.")
      company_info: CompanyInfo = Field(..., description="Information about the lead's company.")
      lead_score: LeadScore = Field(..., description="The calculated score and related information for the lead.")

  # Creating Agents
  lead_data_agent = Agent(
    config=lead_agents_config['lead_data_agent'],
    tools=[SerperDevTool(), ScrapeWebsiteTool()],
    llm=_llm
  )

  cultural_fit_agent = Agent(
    config=lead_agents_config['cultural_fit_agent'],
    tools=[SerperDevTool(), ScrapeWebsiteTool()],
    llm=_llm
  )

  scoring_validation_agent = Agent(
    config=lead_agents_config['scoring_validation_agent'],
    tools=[SerperDevTool(), ScrapeWebsiteTool()],
    llm=_llm
  )
  
  # Creating Tasks
  lead_data_task = Task(
    config=lead_tasks_config['lead_data_collection'],
    agent=lead_data_agent
  )

  cultural_fit_task = Task(
    config=lead_tasks_config['cultural_fit_analysis'],
    agent=cultural_fit_agent
  )

  scoring_validation_task = Task(
    config=lead_tasks_config['lead_scoring_and_validation'],
    agent=scoring_validation_agent,
    context=[lead_data_task, cultural_fit_task],
    output_pydantic=LeadScoringResult
  )
  
  # Creating Crew
  lead_scoring_crew = Crew(
    agents=[
      lead_data_agent,
      cultural_fit_agent,
      scoring_validation_agent
    ],
    tasks=[
      lead_data_task,
      cultural_fit_task,
      scoring_validation_task
    ],
    memory=False,
    verbose=True
  )
  
  # Creating Agents
  email_content_specialist = Agent(
    config=email_agents_config['email_content_specialist'],
    llm=_llm
  )

  engagement_strategist = Agent(
    config=email_agents_config['engagement_strategist'],
    llm=_llm
  )

  # Creating Tasks
  email_drafting = Task(
    config=email_tasks_config['email_drafting'],
    agent=email_content_specialist
  )

  engagement_optimization = Task(
    config=email_tasks_config['engagement_optimization'],
    agent=engagement_strategist
  )

  # Creating Crew
  email_writing_crew = Crew(
    agents=[
      email_content_specialist,
      engagement_strategist
    ],
    tasks=[
      email_drafting,
      engagement_optimization
    ],
    memory=False,
    verbose=True
  )
    
  class SalesPipeline(Flow):
    
    @start()
    def fetch_leads(self):
      # Pull our leads from the database
      # This is a mock, in a real-world scenario, this is where you would
      # fetch leads from a database
      leads = [
        {
          "lead_data": {
            "name": "João Moura",
            "job_title": "Director of Engineering",
            "company": "Clearbit",
            "email": "joao@clearbit.com",
            "use_case": "Using AI Agent to do better data enrichment."
          },
        },
      ]
      return leads

    @listen(fetch_leads)
    def score_leads(self, leads):
      scores = lead_scoring_crew.kickoff_for_each(leads)
      self.state["score_crews_results"] = scores
      return scores

    @listen(score_leads)
    def store_leads_score(self, scores):
      # Here we would store the scores in the database
      return scores

    @listen(score_leads)
    def filter_leads(self, scores):
      return [score for score in scores if score['lead_score'].score > 70]

    @listen(and_(filter_leads, store_leads_score))
    def log_leads(self, leads):
      print(f"Leads: {leads}")

    # @router(filter_leads, paths=["high", "medium", "low"])
    @router(filter_leads)
    def count_leads(self, scores):
      if len(scores) > 10:
        return 'high'
      elif len(scores) > 5:
        return 'medium'
      else:
        return 'low'

    @listen('high')
    def store_in_salesforce(self, leads):
      return leads

    @listen('medium')
    def send_to_sales_team(self, leads):
      return leads

    @listen('low')
    def write_email(self, leads):
      scored_leads = [lead.to_dict() for lead in leads]
      emails = email_writing_crew.kickoff_for_each(scored_leads)
      return emails

    @listen(write_email)
    def send_email(self, emails):
      # Here we would send the emails to the leads
      return emails
    
  flow = SalesPipeline()
  # flow.plot()  
  emails = await flow.kickoff_async()
  print(emails)
  scores = flow.state["score_crews_results"]
  print(scores[0].pydantic)

if __name__ == "__main__": 
  # https://community.crewai.com/t/flow-execution-asyncio-run-cannot-be-called-from-a-running-event-loop/2945/3
  loop = asyncio.get_event_loop()
  loop.run_until_complete(main())  