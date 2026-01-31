
import os
import json
import pandas as pd
from typing import Dict, List, Tuple, Optional
from openai import OpenAI


# this is the schema that tells the LLM what columns exist in our AE dataset
# the "use_for" lists help the model understand what kinds of questions map to each column
# for example, if someone asks about "severity", the LLM knows to look at AESEV
AE_SCHEMA = {
    "dataset_name": "Adverse Events (AE)",
    "description": "Contains adverse event records from clinical trials",
    "columns": {
        "USUBJID": {
            "label": "Unique Subject Identifier",
            "description": "Unique identifier for each patient/subject in the study",
            "type": "string",
            "use_for": ["subject identification", "patient ID", "participant"]
        },
        "AETERM": {
            "label": "Reported Term for the Adverse Event",
            "description": "The verbatim term used to identify the adverse event, specific condition or symptom",
            "type": "string",
            "use_for": ["specific condition", "symptom", "adverse event name", "what happened", "condition name", "event term"]
        },
        "AEDECOD": {
            "label": "Dictionary-Derived Term",
            "description": "Standardized/coded term for the adverse event from MedDRA dictionary",
            "type": "string",
            "use_for": ["coded term", "standard term", "MedDRA term", "preferred term"]
        },
        "AESOC": {
            "label": "Primary System Organ Class",
            "description": "The body system or organ class affected (e.g., CARDIAC DISORDERS, NERVOUS SYSTEM DISORDERS)",
            "type": "string",
            "use_for": ["body system", "organ class", "system organ class", "SOC", "organ", "body part", "system"]
        },
        "AEBODSYS": {
            "label": "Body System or Organ Class",
            "description": "Body system affected by the adverse event",
            "type": "string",
            "use_for": ["body system", "organ system"]
        },
        "AESEV": {
            "label": "Severity/Intensity",
            "description": "The severity or intensity of the adverse event (MILD, MODERATE, SEVERE)",
            "type": "string",
            "values": ["MILD", "MODERATE", "SEVERE"],
            "use_for": ["severity", "intensity", "how severe", "how bad", "seriousness level", "grade"]
        },
        "AESER": {
            "label": "Serious Event",
            "description": "Whether the adverse event is serious (Y/N)",
            "type": "string",
            "values": ["Y", "N"],
            "use_for": ["serious", "SAE", "serious adverse event"]
        },
        "AEREL": {
            "label": "Causality/Relationship to Treatment",
            "description": "Relationship of adverse event to study treatment (PROBABLE, POSSIBLE, REMOTE, NONE)",
            "type": "string",
            "use_for": ["related", "causality", "relationship", "drug related", "treatment related", "caused by"]
        },
        "AEOUT": {
            "label": "Outcome of Adverse Event",
            "description": "The outcome/resolution of the adverse event",
            "type": "string",
            "use_for": ["outcome", "resolved", "resolution", "result", "recovered"]
        },
        "AEACN": {
            "label": "Action Taken with Study Treatment",
            "description": "Action taken with study treatment due to the adverse event",
            "type": "string",
            "use_for": ["action taken", "treatment action", "drug action", "dose action"]
        },
        "AESTDTC": {
            "label": "Start Date/Time of Adverse Event",
            "description": "When the adverse event started",
            "type": "datetime",
            "use_for": ["start date", "onset date", "when started", "beginning"]
        },
        "AEENDTC": {
            "label": "End Date/Time of Adverse Event",
            "description": "When the adverse event ended/resolved",
            "type": "datetime",
            "use_for": ["end date", "resolution date", "when ended", "when resolved"]
        },
        "AESTDY": {
            "label": "Study Day of Start of Adverse Event",
            "description": "Study day when adverse event started (relative to reference date)",
            "type": "integer",
            "use_for": ["study day", "day of onset", "which day"]
        },
        "AELLT": {
            "label": "Lowest Level Term",
            "description": "MedDRA Lowest Level Term for the adverse event",
            "type": "string",
            "use_for": ["lowest level term", "LLT"]
        },
        "AEHLT": {
            "label": "High Level Term",
            "description": "MedDRA High Level Term grouping",
            "type": "string",
            "use_for": ["high level term", "HLT"]
        },
        "AEHLGT": {
            "label": "High Level Group Term",
            "description": "MedDRA High Level Group Term",
            "type": "string",
            "use_for": ["high level group", "HLGT"]
        }
    }
}


class ClinicalTrialDataAgent:

    def __init__(self, api_key: str, ae_data_path: str = "ae_data.csv"):

        # set up the openai client with the provided key
        self.client = OpenAI(api_key=api_key)

        # load the adverse events data into a pandas dataframe
        self.ae_df = pd.read_csv(ae_data_path)
        self.schema = AE_SCHEMA

        # convert our schema dictionary into a readable string for the LLM
        # this gets included in every prompt so the model knows our data structure
        self.schema_context = self._build_schema_context()

    def _build_schema_context(self) -> str:

        context = "ADVERSE EVENTS DATASET SCHEMA:\n\n"
        for col, info in self.schema["columns"].items():
            context += f"Column: {col}\n"
            context += f"  Label: {info['label']}\n"
            context += f"  Description: {info['description']}\n"

            # only show valid values if they exist (like MILD/MODERATE/SEVERE for severity)
            if 'values' in info:
                context += f"  Valid Values: {', '.join(info['values'])}\n"
            context += f"  Use for questions about: {', '.join(info['use_for'])}\n\n"
        return context

    def parse_question(self, question: str) -> Dict:

        # the system prompt explains to the LLM what its should do
        # we include the full schema so it knows what columns exist
        system_prompt = f"""You are a clinical data analyst assistant. Your job is to parse natural language questions about adverse events data and map them to the correct database columns.

{self.schema_context}

INSTRUCTIONS:
1. Analyze the user's question to understand what they're asking about
2. Identify the most appropriate column from the schema
3. Extract the filter value (what they're searching for)
4. Return a JSON object with exactly these fields:
   - "target_column": The column name to query (must be one from the schema)
   - "filter_value": The value to search for (extracted/normalized from the question)
   - "match_type": Either "exact" (for categorical like severity) or "contains" (for text search)
   - "reasoning": Brief explanation of why you chose this column

IMPORTANT:
- For severity questions (mild, moderate, severe), use AESEV column with UPPERCASE values
- For body system/organ questions, use AESOC column
- For specific conditions/symptoms, use AETERM column
- For serious events (SAE), use AESER column with Y/N values
- For relationship/causality questions, use AEREL column
- Always return valid JSON only, no other text"""

        user_prompt = f"""Parse this question and return JSON:

Question: "{question}"

Return only valid JSON with target_column, filter_value, match_type, and reasoning."""

        # call the openai api
        # using gpt-4o-mini because it's fast and cheap, but still smart enough for this task
        # temperature=0 makes it deterministic (same input = same output)
        # response_format ensures we get valid JSON back
        response = self.client.chat.completions.create(
            model="gpt-4o-mini",
            messages=[
                {"role": "system", "content": system_prompt},
                {"role": "user", "content": user_prompt}
            ],
            temperature=0,
            response_format={"type": "json_object"}
        )

        # parse the JSON response from the model
        result = json.loads(response.choices[0].message.content)
        return result

    def execute_query(self, parsed_query: Dict) -> Tuple[int, List[str], pd.DataFrame]:

        target_column = parsed_query.get("target_column")
        filter_value = parsed_query.get("filter_value")
        match_type = parsed_query.get("match_type", "contains")

        # make sure the column actually exists in our data
        if target_column not in self.ae_df.columns:
            raise ValueError(f"Column '{target_column}' not found in dataset. Available columns: {list(self.ae_df.columns)}")

        # apply the filter based on match type
        if match_type == "exact":
            # exact match - good for categorical stuff like severity (MILD, MODERATE, SEVERE)
            # we uppercase both sides so it's case-insensitive
            mask = self.ae_df[target_column].str.upper() == str(filter_value).upper()
        else:
            # contains match - good for searching text like "headache" in AETERM
            # also case-insensitive, and na=False so we don't crash on missing values
            mask = self.ae_df[target_column].str.upper().str.contains(
                str(filter_value).upper(),
                na=False,
                regex=False
            )

        # apply the filter
        filtered_df = self.ae_df[mask]

        # get the unique subject IDs from the filtered results
        # this tells us how many patients had this type of AE
        unique_subjects = filtered_df["USUBJID"].unique().tolist()
        subject_count = len(unique_subjects)

        return subject_count, unique_subjects, filtered_df

    def ask(self, question: str) -> Dict:

        # step 1: send the question to the LLM to figure out what to query
        parsed_query = self.parse_question(question)

        # step 2: run the actual pandas query
        subject_count, subject_ids, filtered_df = self.execute_query(parsed_query)

        # step 3: package up the results nicely
        response = {
            "question": question,
            "parsed_query": parsed_query,
            "results": {
                "unique_subject_count": subject_count,
                "subject_ids": subject_ids,
                "total_records": len(filtered_df),
                # include first 5 rows as a sample so user can see what the data looks like
                "sample_data": filtered_df.head(5).to_dict(orient="records") if len(filtered_df) > 0 else []
            }
        }

        return response

    def print_results(self, response: Dict) -> None:

        print("\n" + "="*60)
        print(f"QUESTION: {response['question']}")
        print("="*60)

        # show how the LLM interpreted the question
        pq = response['parsed_query']
        print(f"\nPARSED QUERY:")
        print(f"  Target Column: {pq.get('target_column')}")
        print(f"  Filter Value: {pq.get('filter_value')}")
        print(f"  Match Type: {pq.get('match_type')}")
        print(f"  Reasoning: {pq.get('reasoning')}")

        # show the actual results
        results = response['results']
        print(f"\nRESULTS:")
        print(f"  Unique Subjects: {results['unique_subject_count']}")
        print(f"  Total AE Records: {results['total_records']}")

        # list out the subject IDs (just first 10 to keep it readable)
        if results['unique_subject_count'] > 0:
            print(f"\n  Subject IDs (first 10):")
            for subj_id in results['subject_ids'][:10]:
                print(f"    - {subj_id}")
            if len(results['subject_ids']) > 10:
                print(f"    ... and {len(results['subject_ids']) - 10} more")

        print("="*60 + "\n")


if __name__ == "__main__":
    import sys

    # check for API key in environment variable
    api_key = os.environ.get("OPENAI_API_KEY")

    if not api_key:
        print("Please set OPENAI_API_KEY environment variable")
        print("Usage: OPENAI_API_KEY='your-key' python question_4.py")
        sys.exit(1)

    # create the agent
    agent = ClinicalTrialDataAgent(
        api_key=api_key,
        ae_data_path="ae_data.csv"
    )

    # some example questions to test with
    test_questions = [
        "How many patients had severe adverse events?",
        "Show me subjects with cardiac disorders",
        "Which patients experienced headache?"
    ]

    print("\n" + "="*60)
    print("CLINICAL TRIAL DATA AGENT - TEST RUN")
    print("="*60)

    # run each test question
    for question in test_questions:
        try:
            response = agent.ask(question)
            agent.print_results(response)
        except Exception as e:
            print(f"\nError processing question: {question}")
            print(f"Error: {str(e)}\n")
