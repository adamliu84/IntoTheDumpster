#!/usr/bin/env python
from random import randint

from pydantic import BaseModel

from crewai.flow.flow import Flow, listen, start

# from .crews.sales_crew.sales_crew import SalesCrew
from .crews.leadcrew import leadcrew
from .crews.emailcrew import emailcrew
import os
from dotenv import load_dotenv
load_dotenv()


class SalesState(BaseModel):
    sentence_count: int = 1
    Sales: str = ""


class SalesFlow(Flow[SalesState]):

    @start()
    def fetch_leads(self):        
        lead = {
                "lead_data": {
                    "name": "João Moura",
                    "job_title": "Director of Engineering",
                    "company": "Clearbit",
                    "email": "joao@clearbit.com",
                    "use_case": "Using AI Agent to do better data enrichment."
                },
            }   
        return lead

    @listen(fetch_leads)    
    def score_leads(self, lead):  
        score = (leadcrew.Leadcrew()
                  .crew()
                  .kickoff(lead)                
        )                
        print("Lead crew scoring is: " + str(score))
        return score        

    @listen(score_leads)
    def store_leads_score(self, score):
        print("MOCKING DATABASE STORE")
        return score

    @listen(score_leads)
    def filter_leads(self, score):
        return score
        # return [score for score in scores if score['lead_score'].score > 70]

    @listen(filter_leads)
    def write_email(self, lead):        
        email = (emailcrew.Emailcrew()
                 .crew()
                 .kickoff(lead.to_dict())
        )
        print("DRAFTED EMAIL CONTENT")
        print(email)
        return email

    @listen(write_email)
    def send_email(self, email):
        print("MOCKING SENDING EMAIL")
        return email

def kickoff():
    global os
    GOOGLE_GEMINI_API_KEY = os.getenv('GOOGLE_GEMINI_API_KEY') 
    os.environ["GOOGLE_API_KEY"] = GOOGLE_GEMINI_API_KEY
    os.environ["GEMINI_API_KEY"] = GOOGLE_GEMINI_API_KEY
    os.environ["SERPER_API_KEY"] = os.getenv('SERPER_API_KEY')    
    Sales_flow = SalesFlow()    
    Sales_flow.kickoff()


def plot():
    Sales_flow = SalesFlow()
    Sales_flow.plot()

if __name__ == "__main__":
    kickoff()
