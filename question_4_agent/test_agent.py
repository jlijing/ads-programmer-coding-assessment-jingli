
from question_4 import ClinicalTrialDataAgent

# read the API key from our local file
# using a file instead of hardcoding so we don't accidentally commit the key to git
with open("apikey.txt", "r") as f:
    api_key = f.read().strip()

# create the agent instance
# this loads the AE data and sets up the OpenAI client
agent = ClinicalTrialDataAgent(api_key=api_key, ae_data_path="ae_data.csv")

# these are our test questions - each one tests a different column mapping:
#   1. "severe adverse events" -> should use AESEV column (severity)
#   2. "cardiac disorders" -> should use AESOC column (system organ class)
#   3. "headache" -> should use AETERM column (specific condition)
test_questions = [
    "How many patients had severe adverse events?",
    "Show me subjects with cardiac disorders",
    "Which patients experienced headache?"
]

# print a nice header
print("=" * 60)
print("CLINICAL TRIAL DATA AGENT - TEST RUN")
print("=" * 60)

# loop through each question and run it
for question in test_questions:
    try:
        # ask() sends the question to the LLM and runs the query
        response = agent.ask(question)
        # print_results() formats the output nicely
        agent.print_results(response)
    except Exception as e:
        # if something goes wrong, print the error but keep going
        print(f"\nError processing: {question}")
        print(f"Error: {str(e)}\n")
