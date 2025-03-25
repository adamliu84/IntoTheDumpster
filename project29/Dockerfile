# REF from https://github.com/vin67/crewai_docker
# Dockerfile
FROM python:3.11.9

# Set the working directory in the container
WORKDIR /app

# Copy requirements and install dependencies
COPY requirements.txt .
RUN pip install -r requirements.txt

# Set the PYTHONPATH to include src/
ENV PYTHONPATH=/app/src

# Copy the application code
COPY . .

# Expose the app port
EXPOSE 5000

# Specify the command to run on container start
# CMD ["crewai", "run"]

# Set the entrypoint to crewai
ENTRYPOINT ["crewai"]
RUN curl -fsSL https://ollama.com/install.sh | sh

# Default to "run" if no command is provided
CMD ["run"]