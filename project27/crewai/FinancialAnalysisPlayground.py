import warnings
warnings.filterwarnings('ignore')
import os
from crewai import Agent, Task, Crew, LLM, Process
from crewai_tools import ScrapeWebsiteTool, SerperDevTool

# WARNING Disabling tools usage in Task due to  Invalid response from LLM call - None or empty. May need a stronger LLM
# See https://community.crewai.com/t/why-am-i-getting-the-invalid-response-from-llm-call-none-or-empty-error-with-my-custom-tool-if-using-anthropic-llm-but-not-with-openai-llm/1571/2
SEPER_API_KEY = "xxxxxxxxxxxxxx"
os.environ["SERPER_API_KEY"] = SEPER_API_KEY
# Initialize the tools
search_tool = SerperDevTool()
scrape_tool = ScrapeWebsiteTool()

local_llm = LLM(
    model="ollama_chat/llama3.2",
    base_url="http://host.docker.internal:11434",
    api_key="sk-proj-1111",
)

data_analyst_agent = Agent(
    role="Data Analyst",
    goal="Monitor and analyze market data in real-time "
         "to identify trends and predict market movements.",
    backstory="Specializing in financial markets, this agent "
              "uses statistical modeling and machine learning "
              "to provide crucial insights. With a knack for data, "
              "the Data Analyst Agent is the cornerstone for "
              "informing trading decisions.",
    verbose=True,
    allow_delegation=True,
    tools = [scrape_tool, search_tool],
    llm=local_llm
)

trading_strategy_agent = Agent(
    role="Trading Strategy Developer",
    goal="Develop and test various trading strategies based "
         "on insights from the Data Analyst Agent.",
    backstory="Equipped with a deep understanding of financial "
              "markets and quantitative analysis, this agent "
              "devises and refines trading strategies. It evaluates "
              "the performance of different approaches to determine "
              "the most profitable and risk-averse options.",
    verbose=True,
    allow_delegation=True,
    tools = [scrape_tool, search_tool],
    llm=local_llm
)

execution_agent = Agent(
    role="Trade Advisor",
    goal="Suggest optimal trade execution strategies "
         "based on approved trading strategies.",
    backstory="This agent specializes in analyzing the timing, price, "
              "and logistical details of potential trades. By evaluating "
              "these factors, it provides well-founded suggestions for "
              "when and how trades should be executed to maximize "
              "efficiency and adherence to strategy.",
    verbose=True,
    allow_delegation=True,
    tools = [scrape_tool, search_tool],
    llm=local_llm
)

risk_management_agent = Agent(
    role="Risk Advisor",
    goal="Evaluate and provide insights on the risks "
         "associated with potential trading activities.",
    backstory="Armed with a deep understanding of risk assessment models "
              "and market dynamics, this agent scrutinizes the potential "
              "risks of proposed trades. It offers a detailed analysis of "
              "risk exposure and suggests safeguards to ensure that "
              "trading activities align with the firm’s risk tolerance.",
    verbose=True,
    allow_delegation=True,
    tools = [scrape_tool, search_tool],
    llm=local_llm
)

data_analysis_task = Task(
    description=(
        "Continuously monitor and analyze market data for "
        "the selected stock ({stock_selection}). "
        "Use statistical modeling and machine learning to "
        "identify trends and predict market movements."
    ),
    expected_output=(
        "Insights and alerts about significant market "
        "opportunities or threats for {stock_selection}."
    ),
    agent=data_analyst_agent,
)

strategy_development_task = Task(
    description=(
        "Develop and refine trading strategies based on "
        "the insights from the Data Analyst and "
        "user-defined risk tolerance ({risk_tolerance}). "
        "Consider trading preferences ({trading_strategy_preference})."
    ),
    expected_output=(
        "A set of potential trading strategies for {stock_selection} "
        "that align with the user's risk tolerance."
    ),
    agent=trading_strategy_agent,
)

execution_planning_task = Task(
    description=(
        "Analyze approved trading strategies to determine the "
        "best execution methods for {stock_selection}, "
        "considering current market conditions and optimal pricing."
    ),
    expected_output=(
        "Detailed execution plans suggesting how and when to "
        "execute trades for {stock_selection}."
    ),
    agent=execution_agent,
)

risk_assessment_task = Task(
    description=(
        "Evaluate the risks associated with the proposed trading "
        "strategies and execution plans for {stock_selection}. "
        "Provide a detailed analysis of potential risks "
        "and suggest mitigation strategies."
    ),
    expected_output=(
        "A comprehensive risk analysis report detailing potential "
        "risks and mitigation recommendations for {stock_selection}."
    ),
    agent=risk_management_agent,
)

financial_trading_crew = Crew(
    agents=[data_analyst_agent, 
            trading_strategy_agent, 
            execution_agent, 
            risk_management_agent],
    
    tasks=[data_analysis_task, 
           strategy_development_task, 
           execution_planning_task, 
           risk_assessment_task],
    
    manager_llm=LLM(
        model="ollama_chat/llama3.2",
        base_url="http://host.docker.internal:11434",
        api_key="sk-proj-1111",
        temperature=0.7
    ),
    process=Process.hierarchical,
    verbose=True
)

financial_trading_inputs = {
    'stock_selection': 'AAPL',
    'initial_capital': '100000',
    'risk_tolerance': 'Medium',
    'trading_strategy_preference': 'Day Trading',
    'news_impact_consideration': True
}

result = financial_trading_crew.kickoff(inputs=financial_trading_inputs)
print(result)