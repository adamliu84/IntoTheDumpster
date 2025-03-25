from crewai import Agent, Crew, Process, Task, LLM
from crewai.project import CrewBase, agent, crew, task
import os

@CrewBase
class Emailcrew():
    # _llm = LLM(
    #     model="ollama_chat/llama3.2",
    #     base_url="http://host.docker.internal:11434",
    #     api_key="sk-proj-1111"
    # )
    
    _llm = LLM(
                # model='gemini/gemini-1.5-flash',
                api_key=os.environ["GOOGLE_GEMINI_API_KEY"],
                model='gemini/gemini-2.0-flash',
    ) 
    
    agents_config = 'config/agents.yaml'
    tasks_config = 'config/tasks.yaml'

    @agent
    def email_content_specialist(self) -> Agent:
        return Agent(
            config=self.agents_config['email_content_specialist'],
            # tools=[MyCustomTool()], # Example of custom tool, loaded on the beginning of file
            verbose=True,
            llm=self._llm
        )

    @agent
    def engagement_strategist(self) -> Agent:
        return Agent(
            config=self.agents_config['engagement_strategist'],
            verbose=True,
            llm=self._llm
        )

    @task
    def email_drafting(self) -> Task:
        return Task(
            config=self.tasks_config['email_drafting'],
        )

    @task
    def engagement_optimization(self) -> Task:
        return Task(
            config=self.tasks_config['engagement_optimization'],
        )

    @crew
    def crew(self) -> Crew:
        """Creates the Emailcrew crew"""
        return Crew(
            agents=self.agents, # Automatically created by the @agent decorator
            tasks=self.tasks, # Automatically created by the @task decorator
            process=Process.sequential,
            async_execution=True,
            verbose=True,
            # process=Process.hierarchical, # In case you wanna use that instead https://docs.crewai.com/how-to/Hierarchical/
        )
