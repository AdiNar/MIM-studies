FROM python:3.6-buster
RUN apt install bash gcc g++
COPY ../flask /project
WORKDIR /project

EXPOSE 8080

RUN pip install -r /project/requirements.txt

CMD ["python", "main.py"]
